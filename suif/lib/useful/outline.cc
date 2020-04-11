/* file "outline.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of ``outlining'' routines (the reverse
 * of inlining) for the SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE outline_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


static sym_node_list *make_mutually_exclusive(sym_node_list *in_list);
static sym_node_list *find_exposed_syms(tree_block *the_block,
                                        base_symtab *scope);
static type_node_list *find_exposed_types(tree_block *the_block,
                                          base_symtab *scope);
static sym_node_list *reference_needed(tree_block *the_block,
                                       sym_node_list *in_list);
static void find_writes(tree_block *the_block, temp_marker *candidate_marker,
                        temp_marker *direct_ref_marker, boolean *calls,
                        boolean *pointer_writes);
static void write_finding_walkee(suif_object *the_object,
                                 so_walker *the_walker);
static void make_ref_param(var_sym *original_var, operand_list *arguments,
                           tree_proc *new_body);
static void fix_all_bounds(tree_proc *new_body, operand_list *arguments);
static void bound_fix_walkee(suif_object *the_object, so_walker *the_walker);
static array_bound fix_bound(array_bound original_bound, base_symtab *scope,
                             operand_list *arguments, tree_proc *new_body);
static boolean is_cbr_var(var_sym *the_var);
static operand load_op_shift(operand original_load_op, int offset,
                             type_node *new_type);
static type_node *screen_type(type_node *in_type,
                              type_node_list *types_with_screens);
static var_sym *closest_joint_cover(var_sym *var1, var_sym *var2);
static void kill_op_replaced(sym_node *this_sym, so_walker *the_walker);


boolean zero_out_ref_converted_in_annotes = TRUE;

extern proc_sym *outline(tree_block *the_block, var_sym *return_value,
                         boolean do_write_out)
  {
    assert(!the_block->is_proc());

    block_symtab *old_symtab = the_block->symtab();

    proc_sym *old_psym = the_block->proc();
    file_set_entry *the_fse = old_psym->file();
    type_node_list types_with_screens;
    type_node *result_type;
    operand dest_op;
    if (return_value == NULL)
      {
        result_type = type_void;
        dest_op = operand();
      }
    else
      {
        result_type = screen_type(return_value->type()->unqual(),
                                  &types_with_screens);
        dest_op = operand(return_value);
      }
    func_type *new_type = new func_type(result_type);
    new_type = (func_type *)(result_type->parent()->install_type(new_type));
    proc_sym *new_psym =
            the_fse->symtab()->new_proc(new_type, old_psym->src_lang(), "");

    proc_symtab *new_proc_syms = new proc_symtab(new_psym->name());
    tree_proc *new_body = new tree_proc(new tree_node_list, new_proc_syms);
    the_fse->symtab()->add_child(new_proc_syms);
    new_psym->set_block(new_body);

    sym_node_list *exposed_syms =
            find_exposed_syms(the_block, the_fse->symtab());
    sym_node_list *refs_needed = reference_needed(the_block, exposed_syms);
    sym_node_list *ref_bases = make_mutually_exclusive(refs_needed);

    operand_list arguments;

    sym_node_list_iter bases_iter(ref_bases);
    while (!bases_iter.is_empty())
      {
        sym_node *this_base = bases_iter.step();
        assert(this_base->is_var());
        var_sym *this_var = (var_sym *)this_base;
        make_ref_param(this_var, &arguments, new_body);
      }

    sym_node_list_iter refs_iter(refs_needed);
    while (!refs_iter.is_empty())
      {
        sym_node *this_ref = refs_iter.step();
        if (this_ref->peek_annote(k_replacement) == NULL)
          {
            assert(this_ref->is_var());
            var_sym *ref_var = (var_sym *)this_ref;
            var_sym *follow_var = ref_var;
            int offset = 0;
            while (TRUE)
              {
                assert(follow_var != NULL);
                offset += follow_var->offset();
                follow_var = follow_var->parent_var();
                assert(follow_var != NULL);
                immed_list *replacement_data =
                        (immed_list *)(follow_var->peek_annote(k_replacement));
                if (replacement_data != NULL)
                  {
                    assert(replacement_data->count() == 1);
                    immed head = replacement_data->head()->contents;
                    assert(head.is_op());
                    operand old_op = head.op();
                    operand new_op =
                            load_op_shift(old_op, offset, ref_var->type());
                    this_ref->append_annote(k_replacement,
                                            new immed_list(new_op));
                    break;
                  }
              }
          }
      }

    sym_node_list_iter all_iter(exposed_syms);
    while (!all_iter.is_empty())
      {
        sym_node *this_ref = all_iter.step();
        if (this_ref->peek_annote(k_replacement) == NULL)
          {
            switch (this_ref->kind())
              {
                case SYM_LABEL:
		{
		    label_sym *new_label = new_proc_syms->new_unique_label();
                    this_ref->append_annote(k_replacement,
                                            new immed_list(immed(new_label)));
                    break;
		}
                case SYM_PROC:
                    assert(FALSE);
                    break;
                case SYM_VAR:
                  {
                    var_sym *this_var = (var_sym *)this_ref;
                    arguments.append(operand(this_var));
                    /* Note that if this_var is call-by-ref, this_param is
                     * too. */
                    var_sym *this_param =
                            new_proc_syms->new_unique_var(this_var->type(),
                                                          this_var->name());
                    this_param->set_param();
                    new_proc_syms->params()->append(this_param);
                    this_ref->append_annote(k_replacement,
                                            new immed_list(this_param));
                    break;
                  }
                default:
                    assert(FALSE);
              }
          }
      }

    the_block->body()->clear_numbers();
    new_body->body()->append(the_block->body());

    while (!old_symtab->symbols()->is_empty())
      {
        sym_node *this_sym = old_symtab->symbols()->head()->contents;
        old_symtab->remove_sym(this_sym);
        new_proc_syms->add_sym(this_sym);
      }

    while (!old_symtab->var_defs()->is_empty())
      {
        var_def *this_def = old_symtab->var_defs()->head()->contents;
        old_symtab->remove_def(this_def);
        new_proc_syms->add_def(this_def);
      }

    while (!old_symtab->types()->is_empty())
      {
        type_node *this_type = old_symtab->types()->head()->contents;
        old_symtab->remove_type(this_type);
        new_proc_syms->add_type(this_type);
      }

    while (!old_symtab->children()->is_empty())
      {
        base_symtab *this_child = old_symtab->children()->head()->contents;
        old_symtab->remove_child(this_child);
        new_proc_syms->add_child(this_child);
      }

    type_node_list *exposed_types =
            find_exposed_types(new_body, the_fse->symtab());

    type_node_list_iter type_iter(exposed_types);
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        type_node *this_copy = this_type->copy();
        new_proc_syms->add_type(this_copy);
        this_type->append_annote(k_replacement, new immed_list(this_copy));
      }

    fix_all_bounds(new_body, &arguments);

    do_replacement(new_body);
    walk(new_body, &kill_op_replaced);

    /* @@@ *
     * To get good type information, we could go through args and find
     * nearest good types, mark them.
     * @@@ */

    if (do_write_out)
        new_psym->write_proc(the_fse);

    unsigned num_args = arguments.count();
    in_cal *callsite =
            new in_cal(result_type, dest_op, addr_op(new_psym), num_args);
    for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
      {
        operand this_op = arguments.pop();
        callsite->set_argument(arg_num, this_op);
      }
    assert(arguments.is_empty());

    operand result_op;
    if (return_value != NULL)
        result_op = operand(return_value);
    in_rrr *new_return = new in_rrr(io_ret, type_void, operand(), result_op);
    new_body->body()->append(new tree_instr(new_return));

    the_block->parent()->insert_after(new tree_instr(callsite),
                                      the_block->list_e());
    kill_node(the_block);

    while (!exposed_syms->is_empty())
      {
        sym_node *this_sym = exposed_syms->pop();
        annote *replacement_annote =
                this_sym->annotes()->get_annote(k_replacement);
        assert(replacement_annote != NULL);
        delete replacement_annote;
      }
    delete exposed_syms;
    delete refs_needed;

    while (!ref_bases->is_empty())
      {
        sym_node *this_sym = ref_bases->pop();
        annote *replacement_annote =
                this_sym->annotes()->get_annote(k_replacement);
        if (replacement_annote != NULL)
            delete replacement_annote;
      }
    delete ref_bases;

    while (!exposed_types->is_empty())
      {
        type_node *this_type = exposed_types->pop();
        annote *replacement_annote =
                this_type->annotes()->get_annote(k_replacement);
        if (replacement_annote != NULL)
            delete replacement_annote;
      }
    delete exposed_types;

    return new_psym;
  }


static sym_node_list *make_mutually_exclusive(sym_node_list *in_list)
  {
    /* @@@ -- A more efficient algorithm should be used. */
    sym_node_list *result = new sym_node_list;
    sym_node_list_iter in_iter(in_list);
    while (!in_iter.is_empty())
      {
        sym_node *this_sym = in_iter.step();
        if (this_sym->is_var())
          {
            var_sym *this_var = (var_sym *)this_sym;
            sym_node_list_e *out_e = result->head();
            while (TRUE)
              {
                if (out_e == NULL)
                  {
                    result->append(this_sym);
                    break;
                  }
                sym_node *out_sym = out_e->contents;
                if (out_sym->is_var())
                  {
                    var_sym *out_var = (var_sym *)out_sym;
                    if (out_var->overlaps(this_var))
                      {
                        out_e->contents =
                                closest_joint_cover(out_var, this_var);
                        break;
                      }
                  }
                out_e = out_e->next();
              }
          }
        else
          {
            result->append(this_sym);
          }
      }
    return result;
  }

static sym_node_list *find_exposed_syms(tree_block *the_block,
                                        base_symtab *scope)
  {
    /* @@@ A better algorithm should be used here. */
    replacements replacement_data;
    the_block->find_exposed_refs(scope, &replacement_data);
    sym_node_list *result = new sym_node_list;
    sym_node_list_iter the_iter(&(replacement_data.oldsyms));
    while (!the_iter.is_empty())
      {
        sym_node *this_sym = the_iter.step();
        result->append(this_sym);
      }
    return result;
  }

static type_node_list *find_exposed_types(tree_block *the_block,
                                          base_symtab *scope)
  {
    /* @@@ A better algorithm should be used here. */
    replacements replacement_data;
    the_block->find_exposed_refs(scope, &replacement_data);
    type_node_list *result = new type_node_list;
    type_node_list_iter the_iter(&(replacement_data.oldtypes));
    while (!the_iter.is_empty())
      {
        type_node *this_type = the_iter.step();
        result->append(this_type);
      }
    return result;
  }

static sym_node_list *reference_needed(tree_block *the_block,
                                       sym_node_list *in_list)
  {
    temp_marker candidate_marker;
    temp_marker direct_ref_marker;
    sym_node_list_iter in_iter(in_list);
    while (!in_iter.is_empty())
      {
        sym_node *this_sym = in_iter.step();
        candidate_marker.mark(this_sym);
      }

    boolean found_calls = FALSE;
    boolean found_pointer_writes = FALSE;
    find_writes(the_block, &candidate_marker, &direct_ref_marker, &found_calls,
                &found_pointer_writes);

    sym_node_list *result = new sym_node_list;

    sym_node_list_iter exposed_iter(in_list);
    while (!exposed_iter.is_empty())
      {
        sym_node *this_sym = exposed_iter.step();
        if (this_sym->is_var())
          {
            var_sym *this_var = (var_sym *)this_sym;
            boolean ref_needed = FALSE;
            if (is_cbr_var(this_var))
              {
                assert(!direct_ref_marker.is_marked(this_var));
                assert(!this_var->is_addr_taken());
              }
            else
              {
                if (direct_ref_marker.is_marked(this_var))
                  {
                    ref_needed = TRUE;
                  }
                else if (found_pointer_writes &&
                         addr_might_be_stored(this_var))
                  {
                    ref_needed = TRUE;
                  }
                else if (found_calls &&
                         (addr_might_be_stored(this_var) ||
                          this_var->is_global()))
                  {
                    ref_needed = TRUE;
                  }
              }
            if (ref_needed)
                result->append(this_var);
          }
      }

    return result;
  }

static void find_writes(tree_block *the_block, temp_marker *candidate_marker,
                        temp_marker *direct_ref_marker, boolean *calls,
                        boolean *pointer_writes)
  {
    so_walker the_walker;
    the_walker.set_data(0, candidate_marker);
    the_walker.set_data(1, direct_ref_marker);
    the_walker.set_data(2, calls);
    the_walker.set_data(3, pointer_writes);
    the_walker.walk(the_block, &write_finding_walkee);
  }

static void write_finding_walkee(suif_object *the_object,
                                 so_walker *the_walker)
  {
    if (!the_object->is_instr_obj())
        return;
    instruction *the_instr = (instruction *)the_object;

    temp_marker *candidate_marker =
            (temp_marker *)(the_walker->get_data(0).ptr);
    temp_marker *direct_ref_marker =
            (temp_marker *)(the_walker->get_data(1).ptr);
    boolean *calls = (boolean *)(the_walker->get_data(2).ptr);
    boolean *pointer_writes = (boolean *)(the_walker->get_data(3).ptr);

    operand dest_op = the_instr->dst_op();
    if (dest_op.is_symbol())
      {
        var_sym *dest_var = dest_op.symbol();
        if (candidate_marker->is_marked(dest_var))
            direct_ref_marker->mark(dest_var);
      }

    if (instr_is_impure_call(the_instr))
        *calls = TRUE;

    switch (the_instr->opcode())
      {
        case io_ldc:
          {
            in_ldc *the_ldc = (in_ldc *)the_instr;
            immed the_value = the_ldc->value();
            if (the_value.is_symbol())
              {
                sym_node *the_sym = the_value.symbol();
                if (candidate_marker->is_marked(the_sym))
                    direct_ref_marker->mark(the_sym);
              }
            break;
          }
        case io_str:
        case io_memcpy:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            operand addr_op = the_rrr->dst_addr_op();
            while (TRUE)
              {
                if (addr_op.is_expr())
                  {
                    instruction *addr_instr = addr_op.instr();
                    boolean do_continue = FALSE;
                    switch (addr_instr->opcode())
                      {
                        case io_cvt:
                        case io_cpy:
                          {
                            in_rrr *the_rrr = (in_rrr *)addr_instr;
                            addr_op = the_rrr->src_op();
                            do_continue = addr_op.type()->unqual()->is_ptr();
                            break;
                          }
                        case io_add:
                        case io_sub:
                          {
                            in_rrr *the_rrr = (in_rrr *)addr_instr;
                            operand src1_op = the_rrr->src1_op();
                            operand src2_op = the_rrr->src2_op();
                            if (src1_op.type()->unqual()->is_ptr())
                              {
                                if (!src2_op.type()->unqual()->is_ptr())
                                  {
                                    addr_op = src1_op;
                                    do_continue = TRUE;
                                  }
                              }
                            else
                              {
                                if (src2_op.type()->unqual()->is_ptr())
                                  {
                                    addr_op = src2_op;
                                    do_continue = TRUE;
                                  }
                              }
                            break;
                          }
                        case io_array:
                          {
                            in_array *the_aref = (in_array *)addr_instr;
                            do_continue = TRUE;
                            addr_op = the_aref->base_op();
                            break;
                          }
                        default:
                            break;
                      }
                    if (do_continue)
                        continue;
                    *pointer_writes = TRUE;
                    break;
                  }
                else if (addr_op.is_symbol())
                  {
                    var_sym *addr_var = addr_op.symbol();
                    if (!is_cbr_var(addr_var))
                        *pointer_writes = TRUE;
                    break;
                  }
                else
                  {
                    *pointer_writes = TRUE;
                    break;
                  }
              }
            break;
          }
        default:
            break;
      }
  }

static void make_ref_param(var_sym *original_var, operand_list *arguments,
                           tree_proc *new_body)
  {
    arguments->append(addr_op(original_var));
    type_node *new_type = new ptr_type(original_var->type());
    new_type->append_annote(k_call_by_ref);
    new_type = original_var->type()->parent()->install_type(new_type);
    var_sym *this_param =
            new_body->proc_syms()->new_unique_var(new_type,
                                                  original_var->name());
    this_param->set_param();
    new_body->proc_syms()->params()->append(this_param);
    original_var->append_annote(k_replacement,
                                new immed_list(fold_load(this_param)));
  }

static void fix_all_bounds(tree_proc *new_body, operand_list *arguments)
  {
    so_walker the_walker;
    the_walker.set_data(0, arguments);
    the_walker.set_data(1, new_body);
    the_walker.walk(new_body, bound_fix_walkee);
  }

static void bound_fix_walkee(suif_object *the_object, so_walker *the_walker)
  {
    if (!the_object->is_type_obj())
        return;
    type_node *the_type = (type_node *)the_object;
    if (the_type->is_array())
      {
        base_symtab *scope = the_type->parent();
        operand_list *arguments =
                (operand_list *)(the_walker->get_data(0).ptr);
        tree_proc *new_body = (tree_proc *)(the_walker->get_data(1).ptr);
        array_type *the_array_type = (array_type *)the_type;

        array_bound new_lower =
                fix_bound(the_array_type->lower_bound(), scope, arguments,
                          new_body);
        the_array_type->set_lower_bound(new_lower);

        array_bound new_upper =
                fix_bound(the_array_type->upper_bound(), scope, arguments,
                          new_body);
        the_array_type->set_upper_bound(new_upper);
      }
  }

static array_bound fix_bound(array_bound original_bound, base_symtab *scope,
                             operand_list *arguments, tree_proc *new_body)
  {
    if (original_bound.is_variable())
      {
        var_sym *original_var = original_bound.variable();
        if (!scope->is_visible(original_var))
          {
            if (original_var->peek_annote(k_replacement) == NULL)
                make_ref_param(original_var, arguments, new_body);
            immed_list *replacement_immeds =
                    (immed_list *)(original_var->peek_annote(k_replacement));
            assert(replacement_immeds != NULL);
            assert(replacement_immeds->count() == 1);
            immed head = replacement_immeds->head()->contents;
            if (head.is_symbol())
              {
                sym_node *new_sym = head.symbol();
                assert(new_sym->is_var());
                var_sym *new_var = (var_sym *)new_sym;
                return array_bound(new_var);
              }
            else
              {
                assert(head.is_op());
                operand top_op = head.op();
                operand addr_op = get_address(top_op);
                assert(!top_op.is_null());
                sym_node *root_symbol = operand_address_root_symbol(top_op);
                assert(root_symbol != NULL);
                assert(root_symbol->is_var());
                var_sym *new_var = (var_sym *)root_symbol;
                return array_bound(new_var);
              }
          }
      }
    return original_bound;
  }

static boolean is_cbr_var(var_sym *the_var)
  {
    if (!the_var->is_param())
        return FALSE;
    type_node *the_type = the_var->type();
    if (!the_type->is_ptr())
        return FALSE;
    return (the_type->peek_annote(k_call_by_ref) != NULL);
  }

static operand load_op_shift(operand original_load_op, int offset,
                             type_node *new_type)
  {
    operand address = get_address(original_load_op);
    assert(!address.is_null());
    if (offset == 0)
      {
        address = cast_op(address, new_type->ptr_to());
      }
    else
      {
        address = fold_add(new_type->ptr_to(), address,
                           const_op(i_integer(offset), type_ptr_diff));
      }
    return fold_load(address);
  }

static type_node *screen_type(type_node *in_type,
                              type_node_list *types_with_screens)
  {
    /* @@@ */
    assert(types_with_screens != NULL);
    return in_type;
    /* @@@ */
  }

static var_sym *closest_joint_cover(var_sym *var1, var_sym *var2)
  {
    var_sym *follow1 = var1;
    var_sym *follow2 = var2;
    var_sym *result = NULL;
    temp_marker the_marker;
    while (TRUE)
      {
        assert((follow1 != NULL) || (follow2 != NULL));
        if (follow1 != NULL)
          {
            if (the_marker.is_marked(follow1))
              {
                result = follow1;
                break;
              }
            the_marker.mark(follow1);
            follow1 = follow1->parent_var();
          }

        if (follow2 != NULL)
          {
            if (the_marker.is_marked(follow2))
              {
                result = follow2;
                break;
              }
            the_marker.mark(follow2);
            follow2 = follow2->parent_var();
          }
      }
    assert(result != NULL);
    if ((result == var1) || (result == var2))
        return result;
    if (result->num_children() == 2)
        return result;

    follow1 = var1;
    while (follow1->parent_var() != result)
        follow1 = follow1->parent_var();

    follow2 = var2;
    while (follow2->parent_var() != result)
        follow2 = follow2->parent_var();

    int offset1 = follow1->offset();
    int offset2 = follow2->offset();
    int min_offset;
    if (offset1 <= offset2)
        min_offset = offset1;
    else
        min_offset = offset2;
    int end1 = offset1 + follow1->type()->size();
    int end2 = offset2 + follow2->type()->size();
    int end_offset;
    if (end1 >= end2)
        end_offset = end1;
    else
        end_offset = end2;
    struct_type *new_type =
            new struct_type(TYPE_GROUP, end_offset - min_offset, "", 2);
    new_type->set_field_name(0, "_1");
    new_type->set_field_name(1, "_2");
    new_type->set_field_type(0, follow1->type());
    new_type->set_field_type(1, follow2->type());
    new_type->set_offset(0, offset1 - min_offset);
    new_type->set_offset(1, offset2 - min_offset);
    new_type = (struct_type *)(result->parent()->install_type(new_type));
    var_sym *new_var = result->build_child(min_offset, new_type, "suif_merge");
    result->remove_child(follow1);
    result->remove_child(follow2);
    new_var->add_child(follow1, offset1 - min_offset);
    new_var->add_child(follow2, offset2 - min_offset);
    return new_var;
  }

/*
 *  If the replacement for a symbol is an operand, uses of the symbol
 *  in annotations will remain unchanged by do_replacements, but these
 *  symbols will be out-of-scope, so we want them removed from
 *  annotations.  So this function is used to simply remove such
 *  references.
 */
static void kill_op_replaced(sym_node *this_sym, so_walker *the_walker)
  {
    annote *replacement_annote =
            this_sym->annotes()->peek_annote(k_replacement);
    if (replacement_annote == NULL)
        return;
    immed_list *data = replacement_annote->immeds();
    assert(data->count() == 1);
    immed head = data->head()->contents;
    if (head.is_op())
      {
        if (zero_out_ref_converted_in_annotes)
          {
            the_walker->replace_sym(NULL);
          }
        else
          {
            operand head_op = head.op();
            assert(head_op.is_expr());
            instruction *head_instr = head_op.instr();
            assert(head_instr->opcode() == io_lod);
            in_rrr *the_load = (in_rrr *)head_instr;
            operand src_op = the_load->src_addr_op();
            if (src_op.is_symbol())
                the_walker->replace_sym(src_op.symbol());
            else
                the_walker->replace_sym(NULL);
          }
      }
  }
