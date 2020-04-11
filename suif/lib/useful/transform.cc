/* file "transform.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the implementation of the miscellaneous code
 *  transformation routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE libuseful_transform_cc

#include "useful_internal.h"
#include <cstring>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        This file is a place to put general routines to do low-level
        code transformations.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

typedef struct
  {
    boolean expand_floats;
  } rewrite_ops_data;

typedef struct
  {
    boolean expand_floats;
    boolean has_expandable;
  } expandable_data;


static const char *k_useful_bound_const;


static void expand_annote_scope(suif_object *the_object,
                                tree_node_list *new_place);
static block_symtab *get_parent_scope(tree_node_list *node_list);
static in_rrr *new_dead_mark(var_sym *dead_var);
static void replace_on_suif_object(suif_object *the_object,
                                   so_walker *the_walker);
static void replace_on_sym(sym_node *the_sym, so_walker *the_walker);
static void replace_on_type(type_node *the_type, so_walker *the_walker);
static void replace_on_op(operand the_op, so_walker *the_walker);
static void skip_replace_annotes(annote *the_annote, so_walker *the_walker);
static void set_object_to_interfilize(suif_object *the_object);
static void extract_constants_for_operand(operand the_op,
                                          so_walker *the_walker);
static void prep_bound_for_extraction(array_bound original_bound,
                                      boolean *found_one);
static array_bound extraction_fix_bound(array_bound original_bound);
static i_integer breakup_with_size(instruction *the_instr,
                                   i_integer maximum_nodes);


extern void init_transform(void)
  {
    ANNOTE(k_useful_bound_const, "useful bound const", FALSE);
  }

extern void force_dest_not_expr(instruction *the_instr)
  {
    assert(the_instr != NULL);
    if (!(the_instr->dst_op().is_instr()))
        return;

    if (the_instr->parent()->instr() == the_instr)
        return;

    instruction *parent_instr = the_instr->dst_op().instr();
    assert(parent_instr != NULL);

    unsigned num_srcs = parent_instr->num_srcs();
    unsigned src_num;
    for (src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (parent_instr->src_op(src_num) == operand(the_instr))
            break;
      }
    assert(src_num < num_srcs);

    tree_node *owner = the_instr->owner();
    assert(owner != NULL);

    type_node *op_type = the_instr->result_type();
    var_sym *new_symbol = owner->scope()->new_unique_var(op_type);

    the_instr->remove();
    operand the_operand(the_instr);
    tree_node *new_node = create_assignment(new_symbol, the_operand);
    insert_tree_node_before(new_node, owner);

    parent_instr->set_src_op(src_num, operand(new_symbol));
  }

extern void force_sources_not_exprs(instruction *the_instr)
  {
    assert(the_instr != NULL);

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_src = the_instr->src_op(src_num);
        if (this_src.is_expr())
            force_dest_not_expr(this_src.instr());
      }
  }

extern void replace_instruction(instruction *old_instr,
                                instruction *new_instr)
  {
    assert(old_instr != NULL);

    tree_instr *the_tree_instr = old_instr->parent();
    if (the_tree_instr->instr() == old_instr)
      {
        the_tree_instr->remove_instr(old_instr);
        the_tree_instr->set_instr(new_instr);
      }

    if (!old_instr->dst_op().is_instr())
      {
        new_instr->set_dst(old_instr->dst_op());
      }
    else
      {
        instruction *parent_instr = old_instr->dst_op().instr();
        assert(parent_instr != NULL);

        unsigned num_srcs = parent_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            if (parent_instr->src_op(src_num) == operand(old_instr))
              {
                old_instr->remove();
                parent_instr->set_src_op(src_num, operand(new_instr));
                return;
              }
          }
        assert(FALSE);
      }
  }

extern tree_node *create_assignment(var_sym *lval, operand rval)
  {
    operand new_rval = rval;
    if (rval.type() != lval->type()->unqual())
        new_rval = cast_op(new_rval, lval->type()->unqual());
    instruction *the_instr;
    if (new_rval.is_expr())
      {
        the_instr = new_rval.instr();
        the_instr->set_dst(operand(lval));
      }
    else
      {
        the_instr =
                new in_rrr(io_cpy, lval->type()->unqual(), operand(lval),
                           new_rval);
      }
    return new tree_instr(the_instr);
  }

extern void insert_tree_node_before(tree_node *new_node, tree_node *place)
  {
    insert_before(new_node, place);
  }

extern void expand_scope(instruction *the_instr, tree_node_list *new_place)
  {
    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        immed value = the_ldc->value();
        if (value.is_symbol())
            expand_scope(value.symbol(), new_place);
      }

    expand_scope(the_instr->result_type(), new_place);

    expand_annote_scope(the_instr, new_place);

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
        expand_scope(the_instr->src_op(src_num), new_place);
  }

extern void expand_scope(operand the_op, tree_node_list *new_place)
  {
    if (the_op.is_expr())
        expand_scope(the_op.instr(), new_place);
    else if (the_op.is_symbol())
        expand_scope(the_op.symbol(), new_place);
  }

extern void expand_scope(sym_node *the_sym, tree_node_list *new_place)
  {
    if (new_place->scope()->is_visible(the_sym))
        return;

    base_symtab *old_scope = the_sym->parent();
    base_symtab *new_scope = get_parent_scope(new_place);

    var_def *definition = NULL;
    if (the_sym->is_var())
      {
        var_sym *the_var = (var_sym *)the_sym;
        if (the_var->has_var_def())
          {
            definition = the_var->definition();
            assert(definition != NULL);
          }
        if (the_var->is_auto())
          {
            assert(old_scope->is_block());
            block_symtab *old_block_symtab = (block_symtab *)old_scope;
            tree_node_list *old_list = old_block_symtab->block()->body();
            in_rrr *top_mark = new_dead_mark(the_var);
            old_list->push(new tree_instr(top_mark));
            in_rrr *bottom_mark = new_dead_mark(the_var);
            old_list->append(new tree_instr(bottom_mark));
          }
      }

    old_scope->remove_sym(the_sym);
    new_scope->add_sym(the_sym);

    if (definition != NULL)
      {
        old_scope->remove_def(definition);
        new_scope->add_def(definition);
      }

    switch (the_sym->kind())
      {
        case SYM_PROC:
            assert(FALSE);
            break;
        case SYM_LABEL:
            break;
        case SYM_VAR:
          {
            var_sym *the_var = (var_sym *)the_sym;
            expand_scope(the_var->type(), new_place);
            var_sym *parent_var = the_var->parent_var();
            if (parent_var != NULL)
                expand_scope(parent_var, new_place);
            unsigned num_children = the_var->num_children();
            for (unsigned child_num = 0; child_num < num_children; ++child_num)
                expand_scope(the_var->child_var(child_num), new_place);
            break;
          }
        default:
            assert(FALSE);
      }

    expand_annote_scope(the_sym, new_place);
  }

extern void expand_scope(type_node *the_type, tree_node_list *new_place)
  {
    if (new_place->scope()->is_visible(the_type))
        return;

    the_type->parent()->remove_type(the_type);
    get_parent_scope(new_place)->add_type(the_type);

    switch (the_type->op())
      {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_VOID:
            break;
        case TYPE_PTR:
          {
            ptr_type *the_ptr = (ptr_type *)the_type;
            expand_scope(the_ptr->ref_type(), new_place);
            break;
          }
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)the_type;
            expand_scope(the_array->elem_type(), new_place);
            array_bound lower_bound = the_array->lower_bound();
            if (lower_bound.is_variable())
                expand_scope(lower_bound.variable(), new_place);
            array_bound upper_bound = the_array->upper_bound();
            if (upper_bound.is_variable())
                expand_scope(upper_bound.variable(), new_place);
            break;
          }
        case TYPE_FUNC:
          {
            func_type *the_func = (func_type *)the_type;
            expand_scope(the_func->return_type(), new_place);
            unsigned num_args = the_func->num_args();
            for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                expand_scope(the_func->arg_type(arg_num), new_place);
            break;
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)the_type;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
                expand_scope(the_struct->field_type(field_num), new_place);
            break;
          }
        case TYPE_ENUM:
            break;
        case TYPE_CONST:
	case TYPE_RESTRICT:
        case TYPE_VOLATILE:
        case TYPE_CALL_BY_REF:
        case TYPE_NULL:
          {
            modifier_type *the_modifier = (modifier_type *)the_type;
            expand_scope(the_modifier->base(), new_place);
            break;
          }
        default:
            assert(FALSE);
      }

    expand_annote_scope(the_type, new_place);
  }

extern void remove_node(tree_node *the_node)
  {
    tree_node_list *parent_list = the_node->parent();
    if (parent_list != NULL)
      {
        tree_node_list_e *list_e = the_node->list_e();
        parent_list->remove(list_e);
        delete list_e;
      }
  }

extern void kill_node(tree_node *the_node)
  {
    remove_node(the_node);
    delete the_node;
  }

extern void kill_op(operand the_op)
  {
    if (the_op.is_instr())
      {
        instruction *old_instr = the_op.instr();
        if (old_instr->dst_op().is_instr())
            old_instr->remove();
        delete old_instr;
      }
  }

extern void insert_before(tree_node *new_node, tree_node *place)
  {
    place->parent()->insert_before(new_node, place->list_e());
  }

extern void insert_after(tree_node *new_node, tree_node *place)
  {
    place->parent()->insert_after(new_node, place->list_e());
  }

extern void guard_for(tree_for *the_for)
  {
    if (the_for->annotes()->peek_annote(k_guarded) != NULL)
        return;

    if (!operand_reevaluation_ok(the_for->lb_op()))
        make_lb_temp(the_for);
    if (!operand_reevaluation_ok(the_for->ub_op()))
        make_ub_temp(the_for);

    operand condition_op = for_test_done(the_for, the_for->lb_op().clone());

    immed value;
    eval_status status = evaluate_const_expr(condition_op, &value);
    if ((status == EVAL_OK) && (value == immed(0)))
      {
        kill_op(condition_op);
        the_for->append_annote(k_guarded, new immed_list);
        return;
      }

    tree_node_list *new_then = new tree_node_list;
    tree_node_list *new_else = new tree_node_list;

    tree_if *new_if =
            if_node(the_for->scope(), fold_logical_not(condition_op), new_then,
                    new_else);
    insert_before(new_if, the_for);

    remove_node(the_for);
    new_then->append(the_for);

    new_else->append(create_assignment(the_for->index(),
                                       the_for->lb_op().clone()));

    the_for->append_annote(k_guarded, new immed_list);
  }

extern void make_lb_temp(tree_for *the_for)
  {
    operand old_op = the_for->lb_op();
    old_op.remove();
    var_sym *temp_var = the_for->scope()->new_unique_var(old_op.type());
    insert_before(create_assignment(temp_var, old_op), the_for);
    the_for->set_lb_op(operand(temp_var));
  }

extern void make_ub_temp(tree_for *the_for)
  {
    operand old_op = the_for->ub_op();
    old_op.remove();
    var_sym *temp_var = the_for->scope()->new_unique_var(old_op.type());
    insert_before(create_assignment(temp_var, old_op), the_for);
    the_for->set_ub_op(operand(temp_var));
  }

extern void make_step_temp(tree_for *the_for)
  {
    operand old_op = the_for->step_op();
    old_op.remove();
    var_sym *temp_var = the_for->scope()->new_unique_var(old_op.type());
    insert_before(create_assignment(temp_var, old_op), the_for);
    the_for->set_step_op(operand(temp_var));
  }

extern operand for_test_done(tree_for *the_for, operand index_op)
  {
    switch (the_for->test())
      {
        case FOR_SGT:
        case FOR_UGT:
            return fold_sle(index_op, the_for->ub_op().clone());
        case FOR_SGTE:
        case FOR_UGTE:
            return fold_sl(index_op, the_for->ub_op().clone());
        case FOR_SLT:
        case FOR_ULT:
            return fold_sle(the_for->ub_op().clone(), index_op);
        case FOR_SLTE:
        case FOR_ULTE:
            return fold_sl(the_for->ub_op().clone(), index_op);
        default:
            error_line(1, the_for, "illegal test for tree_for");
            return operand();
      }
  }

extern tree_if *if_node(base_symtab *scope, operand test_op,
                        tree_node_list *then_part,
                        tree_node_list *else_part)
  {
    tree_node_list *new_else_part = else_part;
    if (new_else_part == NULL)
        new_else_part = new tree_node_list;
    assert(scope->is_block());
    block_symtab *block_scope = (block_symtab *)scope;
    label_sym *new_jumpto = block_scope->new_unique_label();
    tree_node_list *new_test_part = new tree_node_list;
    new_test_part->append(bfalse_node(new_jumpto, test_op));
    return new tree_if(new_jumpto, new_test_part, then_part, new_else_part);
  }

extern tree_instr *label_node(label_sym *the_label_sym)
  {
    return new tree_instr(new in_lab(the_label_sym));
  }

extern tree_instr *jump_node(label_sym *target)
  {
    return new tree_instr(new in_bj(io_jmp, target));
  }

extern tree_instr *btrue_node(label_sym *target, operand cond_op)
  {
    return new tree_instr(new in_bj(io_btrue, target, cond_op));
  }

extern tree_instr *bfalse_node(label_sym *target, operand cond_op)
  {
    return new tree_instr(new in_bj(io_bfalse, target, cond_op));
  }

extern void do_replacement(suif_object *the_object)
  {
    so_walker the_walker;
    the_walker.set_leaf_function(&replace_on_sym);
    the_walker.set_leaf_function(&replace_on_type);
    the_walker.set_post_function(&replace_on_suif_object);
    the_walker.set_post_function(&replace_on_op);
    the_walker.set_pre_function(&skip_replace_annotes);
    the_walker.walk(the_object);
  }

extern void set_sym_to_interfilize(sym_node *the_sym)
  {
    base_symtab *parent = the_sym->parent();
    if (!parent->is_file())
        return;
    set_object_to_interfilize(the_sym);
  }

extern void set_type_to_interfilize(type_node *the_type)
  {
    base_symtab *parent = the_type->parent();
    if (!parent->is_file())
        return;
    set_object_to_interfilize(the_type);
  }

extern operand string_literal_op(const char *the_string, file_set_entry *the_fse)
  {
    int size = strlen(the_string);
    type_node *the_type =
            new array_type(type_char, array_bound(0), array_bound(size));
    the_type = type_char->parent()->install_type(the_type);
    var_sym *the_var =
            the_fse->symtab()->new_unique_var(the_type, "__tmp_string_");
    var_def *the_def =
            the_fse->symtab()->define_var(the_var, get_alignment(the_type));
    immed_list *init_immeds = new immed_list(target.size[C_char]);
    const char *follow = the_string;
    while (*follow != 0)
      {
        init_immeds->append(*follow);
        ++follow;
      }
    init_immeds->append(immed(0));
    the_def->append_annote(k_multi_init, init_immeds);
    return cast_op(addr_op(the_var), type_char->ptr_to());
  }

extern void kill_redundant_line_marks(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            if (the_instr->opcode() != io_mrk)
                return;
            annote_list *the_annotes = the_instr->annotes();
            if (the_annotes->is_empty())
                return;
            if (the_annotes->head()->next() != NULL)
                return;
            annote *the_annote = the_annotes->head()->contents;
            if (strcmp(the_annote->name(), k_line) != 0)
                return;
            tree_node_list_e *this_e = the_node->list_e();
            if (this_e == NULL)
                return;
            tree_node_list_e *next_e = this_e->next();
            if (next_e == NULL)
                return;
            tree_node *next_node = next_e->contents;
            if (next_node->kind() != TREE_INSTR)
                return;
            tree_instr *next_tree_instr = (tree_instr *)next_node;
            instruction *next_instr = next_tree_instr->instr();
            if (next_instr->opcode() != io_mrk)
                return;
            if (next_instr->peek_annote(k_line) == NULL)
                return;
            kill_node(the_node);
            break;
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            kill_redundant_line_marks(the_loop->body());
            kill_redundant_line_marks(the_loop->test());
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            kill_redundant_line_marks(the_for->body());
            kill_redundant_line_marks(the_for->landing_pad());
            break;
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            kill_redundant_line_marks(the_if->header());
            kill_redundant_line_marks(the_if->then_part());
            kill_redundant_line_marks(the_if->else_part());
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            kill_redundant_line_marks(the_block->body());
            break;
          }
        default:
            assert(FALSE);
      }
  }

extern void kill_redundant_line_marks(tree_node_list *the_list)
  {
    tree_node_list_iter the_iter(the_list);
    while (!the_iter.is_empty())
      {
        tree_node *this_node = the_iter.step();
        kill_redundant_line_marks(this_node);
      }
  }

extern void mark_var_and_subs_replace(var_sym *base_var, immed replacement)
  {
    immed new_immed;
    switch (replacement.kind())
      {
        case im_symbol:
            new_immed = replacement;
            break;
        case im_op:
            new_immed = immed(replacement.op().clone());
            break;
        default:
            error_line(1, base_var,
                       "bad immed in mark_var_and_subs_replace()");
      }
    base_var->append_annote(k_replacement, new immed_list(new_immed));

    unsigned num_children = base_var->num_children();
    for (unsigned child_num = 0; child_num < num_children; ++child_num)
      {
        var_sym *this_child = base_var->child_var(child_num);
        var_sym *repl_var = NULL;
        if (replacement.is_symbol())
          {
            sym_node *repl_sym = replacement.symbol();
            if (!repl_sym->is_var())
              {
                error_line(1, repl_sym,
                           "replacement of var_sym child by non-variable"
                           " symbol");
              }
            repl_var = (var_sym *)repl_sym;
          }
        else
          {
            operand old_op = replacement.op();
            if (old_op.is_symbol())
                repl_var = old_op.symbol();
          }
        int new_offset = this_child->offset();
        type_node *new_type = this_child->type();
        if (repl_var != NULL)
          {
            var_sym *new_child = repl_var->find_child(new_offset, new_type);
            if (new_child == NULL)
              {
                new_child =
                        repl_var->build_child(new_offset, new_type,
                                              this_child->name());
              }
            assert(new_child != NULL);
            mark_var_and_subs_replace(this_child, immed(new_child));
          }
        else
          {
            assert(replacement.is_op());
            operand old_op = replacement.op();
            assert(old_op.is_expr());
            instruction *old_instr = old_op.instr();
            assert(old_instr->opcode() == io_lod);
            in_rrr *old_load = (in_rrr *)old_instr;
            operand old_addr = old_load->src_addr_op().clone();
            operand new_addr = old_addr + new_offset / target.addressable_size;
            operand new_op = fold_load(cast_op(new_addr, new_type->ptr_to()));
            mark_var_and_subs_replace(this_child, immed(new_op));
            kill_op(new_op);
          }
      }
  }

extern void try_ub_from_var_extraction(in_array *the_aref)
  {
    operand base_op = the_aref->base_op();
    sym_node *base_sym = operand_address_root_symbol(base_op);
    if ((base_sym == NULL) || (!base_sym->is_var()))
        return;
    var_sym *base_var = (var_sym *)base_sym;
    if (base_var->parent_var() != NULL)
        return;
    type_node *var_type = base_var->type()->unqual();
    if (!var_type->is_array())
        return;
    array_type *var_array_type = (array_type *)var_type;

    type_node *op_type = base_op.type();
    assert(op_type->is_ptr());
    ptr_type *op_ptr = (ptr_type *)op_type;
    type_node *op_ref = op_ptr->ref_type()->unqual();
    assert(op_ref->is_array());
    array_type *op_array_type = (array_type *)op_ref;

    if (var_array_type->elem_type() != op_array_type->elem_type())
        return;
    if (var_array_type->lower_bound() != op_array_type->lower_bound())
        return;
    if (!op_array_type->upper_bound().is_unknown())
        return;

    base_op.remove();
    the_aref->set_base_op(cast_op(base_op, var_array_type->ptr_to()));

    operand old_ub = the_aref->bound(0);
    old_ub.remove();
    kill_op(old_ub);
    operand new_ub =
            operand_from_array_bound(var_array_type->upper_bound()) + 1;
    the_aref->set_bound(0, new_ub);
  }

extern void extract_constants(tree_node *the_node)
  {
    walk(the_node, &extract_constants_for_operand);
  }

extern void extract_array_bounds(tree_block *the_block)
  {
    block_symtab *the_symtab = the_block->symtab();
    type_node_list_iter type_iter(the_symtab->types());
    boolean found_one = FALSE;
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->op() != TYPE_ARRAY)
            continue;
        array_type *the_array_type = (array_type *)this_type;
        prep_bound_for_extraction(the_array_type->lower_bound(), &found_one);
        prep_bound_for_extraction(the_array_type->upper_bound(), &found_one);
      }

    if (!found_one)
        return;

    found_one = FALSE;
    tree_node_list_iter node_iter(the_block->body());
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        if (!this_node->is_instr())
            break;
        tree_instr *this_tree_instr = (tree_instr *)this_node;
        instruction *this_instr = this_tree_instr->instr();
        if (this_instr->opcode() != io_ldc)
            continue;
        in_ldc *this_ldc = (in_ldc *)this_instr;
        if (!this_ldc->dst_op().is_symbol())
            continue;
        var_sym *dest_var = this_ldc->dst_op().symbol();
        annote *bound_annote =
                dest_var->annotes()->peek_annote(k_useful_bound_const);
        if (bound_annote == NULL)
            continue;
        found_one = TRUE;
        bound_annote->immeds()->append(this_ldc->value());
      }

    if (!found_one)
        return;

    type_iter.reset(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->op() != TYPE_ARRAY)
            continue;
        array_type *the_array_type = (array_type *)this_type;
        the_array_type->set_upper_bound(extraction_fix_bound(
                the_array_type->upper_bound()));
        the_array_type->set_lower_bound(extraction_fix_bound(
                the_array_type->lower_bound()));
      }
  }

extern void breakup_large_expressions(instruction *the_instr,
                                      i_integer maximum_nodes)
  {
    assert(maximum_nodes >= 1);
    (void)breakup_with_size(the_instr, maximum_nodes);
  }


static void expand_annote_scope(suif_object *the_object,
                                tree_node_list *new_place)
  {
    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        immed_list *immeds = this_annote->immeds();
        if (immeds != NULL)
          {
            immed_list_iter immed_iter(immeds);
            while (!immed_iter.is_empty())
              {
                immed this_immed = immed_iter.step();
                switch (this_immed.kind())
                  {
                    case im_symbol:
                        expand_scope(this_immed.symbol(), new_place);
                        break;
                    case im_type:
                        expand_scope(this_immed.type(), new_place);
                        break;
                    case im_op:
                        expand_scope(this_immed.op(), new_place);
                        break;
                    case im_instr:
                        expand_scope(this_immed.instr(), new_place);
                        break;
                    case im_int:
                    case im_extended_int:
                    case im_string:
                    case im_float:
                    case im_extended_float:
                    case im_undef:
                        break;
                    default:
                        assert(FALSE);
                  }
              }
          }
      }
  }

static block_symtab *get_parent_scope(tree_node_list *node_list)
  {
    tree_node *parent_node = node_list->parent();
    if (parent_node->is_block())
      {
        tree_block *parent_block = (tree_block *)parent_node;
        return parent_block->symtab();
      }

    tree_node_list *new_node_list = new tree_node_list;
    switch (parent_node->kind())
      {
        case TREE_INSTR:
            assert(FALSE);
            break;
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)parent_node;
            if (the_loop->body() == node_list)
                the_loop->set_body(new_node_list);
            else if (the_loop->test() == node_list)
                the_loop->set_test(new_node_list);
            else
                assert(FALSE);
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)parent_node;
            if (the_for->body() == node_list)
              {
                the_for->set_body(new_node_list);
              }
            else if (the_for->landing_pad() == node_list)
              {
                the_for->set_landing_pad(new_node_list);
              }
            else
              {
                assert((the_for->lb_list() == node_list) ||
                       (the_for->ub_list() == node_list) ||
                       (the_for->step_list() == node_list));
                delete new_node_list;
                return get_parent_scope(the_for->parent());
              }
            break;
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)parent_node;
            if (the_if->header() == node_list)
                the_if->set_header(new_node_list);
            else if (the_if->then_part() == node_list)
                the_if->set_then_part(new_node_list);
            else if (the_if->else_part() == node_list)
                the_if->set_else_part(new_node_list);
            else
                assert(FALSE);
            break;
          }
        case TREE_BLOCK:
            assert(FALSE);
            break;
        default:
            assert(FALSE);
      }

    base_symtab *old_parent_scope = node_list->scope();
    block_symtab *new_symtab = new block_symtab("");
    tree_block *new_block = new tree_block(node_list, new_symtab);
    new_node_list->append(new_block);

    base_symtab_list_iter child_iter(old_parent_scope->children());
    while (!child_iter.is_empty())
      {
        base_symtab *this_child = child_iter.step();
        assert(this_child->is_block());
        block_symtab *child_block_symtab = (block_symtab *)this_child;
        base_symtab *new_scope = child_block_symtab->block()->scope();
        if (new_scope != old_parent_scope)
          {
            assert(new_scope == new_symtab);
            old_parent_scope->remove_child(this_child);
            new_symtab->add_child(this_child);
          }
      }

    old_parent_scope->add_child(new_symtab);

    return new_symtab;
  }

static in_rrr *new_dead_mark(var_sym *dead_var)
  {
    immed_list *new_immeds = new immed_list;
    new_immeds->append(immed(dead_var));
    in_rrr *new_mark = new in_rrr(io_mrk);
    new_mark->append_annote(k_dead, new_immeds);
    return new_mark;
  }

static void replace_on_suif_object(suif_object *the_object,
                                   so_walker *the_walker)
  {
    if (!the_object->is_instr_obj())
        return;
    instruction *the_instr = (instruction *)the_object;
    instruction *old_instr = NULL;
    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        immed value = the_ldc->value();
        if (value.is_symbol())
          {
            sym_node *the_sym = value.symbol();
            annote *the_annote =
                    the_sym->annotes()->peek_annote(k_replacement);
            if (the_annote != NULL)
              {
                immed_list *the_data = the_annote->immeds();
                assert(the_data != NULL);
                assert(!the_data->is_empty());
                immed first_data = the_data->head()->contents;
                if (first_data.is_op())
                  {
                    operand new_op = first_data.op();
                    if (new_op.is_expr())
                      {
                        operand new_dest;
                        if (!the_instr->dst_op().is_instr())
                            new_dest = the_instr->dst_op();
                        instruction *new_instr = new_op.instr();
                        assert(new_instr->opcode() == io_lod);
                        in_rrr *new_load = (in_rrr *)new_instr;
                        operand addr_op = new_load->src_addr_op().clone();
                        type_node *result_type = the_instr->result_type();
                        old_instr = the_instr;
                        if (value.offset() == 0)
                          {
                            the_instr =
                                    new in_rrr(io_cpy, result_type, new_dest,
                                               addr_op);
                          }
                        else
                          {
                            immed byte_offset =
                                    value.offset() / target.addressable_size;
                            operand offset_op =
                                    const_op(immed(byte_offset),
                                             type_ptr_diff);
                            the_instr =
                                    new in_rrr(io_add, result_type, new_dest,
                                               addr_op, offset_op);
                          }
                      }
                    else if (new_op.is_symbol())
                      {
                        sym_node *new_sym = new_op.symbol();
                        the_ldc->set_value(immed(new_sym, value.offset()));
                      }
                    else
                      {
                        the_ldc->set_value(immed());
                      }
                  }
              }
          }
      }
    operand dest_op = the_instr->dst_op();
    if (dest_op.is_symbol())
      {
        var_sym *the_var = dest_op.symbol();
        annote *the_annote = the_var->annotes()->peek_annote(k_replacement);
        if (the_annote != NULL)
          {
            immed_list *the_data = the_annote->immeds();
            assert(the_data != NULL);
            assert(!the_data->is_empty());
            immed first_data = the_data->head()->contents;
            if (first_data.is_op())
              {
                operand new_op = first_data.op();
                if (new_op.is_expr())
                  {
                    instruction *new_instr = new_op.instr();
                    assert(new_instr->opcode() == io_lod);
                    in_rrr *new_load = (in_rrr *)new_instr;
                    operand addr_op = new_load->src_addr_op().clone();
                    in_rrr *new_store =
                            new in_rrr(io_str, type_void, operand(), addr_op);
                    the_walker->replace_object(new_store);
                    the_instr->set_dst(operand());
                    new_store->set_src2(operand(the_instr));
                    if (old_instr != NULL)
                        delete old_instr;
                    old_instr = NULL;
                  }
                else
                  {
                    the_instr->set_dst(new_op);
                  }
              }
          }
      }
    if (old_instr != NULL)
      {
        the_walker->replace_object(the_instr);
        delete old_instr;
      }
  }

static void replace_on_sym(sym_node *the_sym, so_walker *the_walker)
  {
    annote *the_annote = the_sym->annotes()->peek_annote(k_replacement);
    if (the_annote == NULL)
        return;
    immed_list *the_data = the_annote->immeds();
    assert(the_data != NULL);
    assert(!the_data->is_empty());
    immed first_data = the_data->head()->contents;
    if (!first_data.is_symbol())
        return;
    the_walker->replace_sym(first_data.symbol());
  }

static void replace_on_type(type_node *the_type, so_walker *the_walker)
  {
    annote *the_annote = the_type->annotes()->peek_annote(k_replacement);
    if (the_annote == NULL)
        return;
    immed_list *the_data = the_annote->immeds();
    assert(the_data != NULL);
    assert(!the_data->is_empty());
    immed first_data = the_data->head()->contents;
    if (!first_data.is_type())
        return;
    the_walker->replace_type(first_data.type());
  }

static void replace_on_op(operand the_op, so_walker *the_walker)
  {
    if (the_walker->in_dest_op())
        return;
    if (!the_op.is_symbol())
        return;
    var_sym *the_var = the_op.symbol();
    annote *the_annote = the_var->annotes()->peek_annote(k_replacement);
    if (the_annote == NULL)
        return;
    immed_list *the_data = the_annote->immeds();
    assert(the_data != NULL);
    assert(!the_data->is_empty());
    immed first_data = the_data->head()->contents;
    if (!first_data.is_op())
        return;
    operand new_op = first_data.op();
    the_walker->replace_op(new_op.clone());
  }

static void skip_replace_annotes(annote *the_annote, so_walker *the_walker)
  {
    if (the_annote->name() == k_replacement)
        the_walker->set_skip();
  }

static void set_object_to_interfilize(suif_object *the_object)
  {
    if (the_object->peek_annote(k_globalize) != NULL)
        return;
    the_object->append_annote(k_globalize);
    so_walker the_walker;
    the_walker.set_leaf_function(&set_sym_to_interfilize);
    the_walker.set_leaf_function(&set_type_to_interfilize);
    the_walker.walk(the_object);
  }

static void extract_constants_for_operand(operand the_op,
                                          so_walker *the_walker)
  {
    if (!the_op.is_symbol())
        return;
    var_sym *the_var = the_op.symbol();
    if (the_var->peek_annote(k_is_constant) == NULL)
        return;
    if (!the_var->has_var_def())
        return;
    var_def *the_def = the_var->definition();
    base_init_struct_list *init_data = read_init_data(the_def);
    if (init_data->count() != 1)
      {
        deallocate_init_data(init_data);
        return;
      }
    base_init_struct *init_struct = init_data->head()->contents;
    assert(init_struct != NULL);
    repeat_init_struct *repeat_init = init_struct->the_repeat_init();
    if ((repeat_init == NULL) || (repeat_init->repetitions != 1) ||
        (repeat_init->size != the_op.type()->size()))
      {
        deallocate_init_data(init_data);
        return;
      }
    immed the_data = repeat_init->data;
    deallocate_init_data(init_data);
    if (the_data.is_int_const())
      {
        if ((the_op.type()->unqual()->op() != TYPE_INT) &&
            (the_op.type()->unqual()->op() != TYPE_ENUM))
          {
            return;
          }
      }
    else if (the_data.is_float_const())
      {
        if (the_op.type()->unqual()->op() != TYPE_FLOAT)
            return;
      }
    else
      {
        return;
      }
    in_ldc *new_ldc = new in_ldc(the_op.type(), operand(), the_data);
    the_walker->replace_op(operand(new_ldc));
  }

static void prep_bound_for_extraction(array_bound original_bound,
                                      boolean *found_one)
  {
    if (!original_bound.is_variable())
        return;
    var_sym *original_var = original_bound.variable();
    if (original_var->peek_annote(k_useful_bound_const) != NULL)
        return;
    original_var->append_annote(k_useful_bound_const);
    *found_one = TRUE;
  }

static array_bound extraction_fix_bound(array_bound original_bound)
  {
    if (!original_bound.is_variable())
        return original_bound;
    var_sym *original_var = original_bound.variable();
    annote *bound_annote =
            original_var->annotes()->peek_annote(k_useful_bound_const);
    if (bound_annote == NULL)
        return original_bound;
    immed_list *bound_immeds = bound_annote->immeds();
    if (bound_immeds->count() < 1)
        return original_bound;
    immed the_immed = bound_immeds->head()->contents;
    if (!the_immed.is_integer())
        return original_bound;
    return array_bound(the_immed.integer());
  }

static i_integer breakup_with_size(instruction *the_instr,
                                   i_integer maximum_nodes)
  {
    assert(maximum_nodes >= 1);
    unsigned num_srcs = the_instr->num_srcs();
    if (num_srcs == 0)
        return 1;
    i_integer total = 1;
    i_integer *table = new i_integer[num_srcs];
      {
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            operand this_src = the_instr->src_op(src_num);
            if (this_src.is_expr())
              {
                i_integer this_size =
                        breakup_with_size(this_src.instr(), maximum_nodes);
                total += this_size;
                table[src_num] = this_size;
              }
            else
              {
                table[src_num] = 0;
              }
          }
      }
    while (total > maximum_nodes)
      {
        unsigned max_src_num = 0;
        i_integer max_value = 0;
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            if (!the_instr->src_op(src_num).type()->is_ptr())
              {
                if (table[src_num] > max_value)
                  {
                    max_value = table[src_num];
                    max_src_num = src_num;
                  }
              }
          }
        if (max_value == 0)
            break;
        operand max_op = the_instr->src_op(max_src_num);
        assert(max_op.is_expr());
        assert(!max_op.type()->is_ptr());
        force_dest_not_expr(max_op.instr());
        table[max_src_num] = 0;
        total -= max_value;
        assert(total >= 1);
      }
    delete[] table;
    return total;
  }
