/*  Main program for the checking library. */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libcheck.a"

#pragma implementation "check.h"

#define RCS_BASE_FILE check_cc

#include "check.h"
#include "check_internal.h"
#include <cstring>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        The SUIF ``check'' library provides functions for user
        programs to check the legality and consistency of various SUIF
        objects for debugging purposes.  An entire SUIF file can be
        checked by using the ``checksuif'' program, which uses the
        routines in this library.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

DECLARE_DLIST_CLASS(label_sym_list, label_sym *);
DECLARE_DLIST_CLASS(label_sym_list_list, label_sym_list *);
DECLARE_DLIST_CLASS(string_index_list, string_index *);

boolean checkfail = FALSE;
boolean checkabort = FALSE;
boolean pointer_freedom = FALSE;

base_symtab *current_scope = NULL;
tree_node *current_node = NULL;

static instruction_list *pending_instr_ops = NULL;
static string_index *defined_labs = NULL;
static label_sym_list *used_labs = NULL;
static string_index_list *old_defined_labs = NULL;
static label_sym_list_list *old_used_labs = NULL;
static boolean in_annote = FALSE;

static void check_node_list(tree_node_list *the_node_list);
static void check_suif_object(suif_object *the_suif_object);
static void check_annote_list(annote_list *the_annotes);
static void check_annote(annote *the_annote);
static void check_immed_list(immed_list *the_immeds);
static void check_immed(immed *the_immed);
static void check_sym_addr(sym_addr *the_addr);
static void check_fse_ref(file_set_entry *the_fse);
static void check_tree_instr_ref(tree_instr *the_tree_instr);
static void check_tree_block_ref(tree_block *the_block);
static void check_tree_proc_ref(tree_proc *the_tree_proc);
static void check_tree_node_ref(tree_node *the_node);
static void check_instruction_ref(instruction *the_instr);
static void check_symtab_ref(base_symtab *the_symtab);
static void check_var_sym_ref(var_sym *the_var);
static void check_proc_sym_ref(proc_sym *the_proc);
static void check_label_sym_ref(label_sym *the_label);
static void check_sym_node_ref(sym_node *symbol);
static void check_array_bound(array_bound the_bound);
static void check_func_type_ref(func_type *the_func_type);
static boolean check_object_type_ref(type_node *the_type);
static boolean check_type_ref(type_node *the_type);
static boolean scope_in_scope(base_symtab *outer, base_symtab *inner);
static void push_label_scope(void);
static void pop_label_scope(void);
static void register_label_def(label_sym *the_label);
static void register_label_use(label_sym *the_label);

extern void init_check(int & /* argc */, char * /* argv */ [])
  {
    return;
  }

extern void exit_check(void)
  {
    return;
  }

extern void check_global_symtab(global_symtab *the_global_symtab)
  {
    if (the_global_symtab == NULL)
      {
        problem("NULL pointer for global_symtab *");
        return;
      }

    push_clue(the_global_symtab);

    base_symtab *the_base_symtab = the_global_symtab;

    if (the_base_symtab->kind() != SYMTAB_GLOBAL)
      {
        problem("object passed as global_symtab isn't a global_symtab");
        pop_clue(the_global_symtab);
        return;
      }

    check_symtab(the_base_symtab);

    pop_clue(the_global_symtab);
  }

extern void check_file_symtab(file_symtab *the_file_symtab)
  {
    if (the_file_symtab == NULL)
      {
        problem("NULL pointer used as file_symtab *");
        return;
      }

    push_clue(the_file_symtab);

    base_symtab *the_base_symtab = the_file_symtab;

    if (the_base_symtab->kind() != SYMTAB_FILE)
      {
        problem("object passed as file_symtab isn't a file_symtab");
        pop_clue(the_file_symtab);
        return;
      }

    check_symtab(the_base_symtab);

    pop_clue(the_file_symtab);
  }

extern void check_symtab(base_symtab *the_symtab)
  {
    if (the_symtab == NULL)
      {
        problem("NULL pointer used for base_symtab *");
        return;
      }

    push_clue(the_symtab);

    suif_object *the_suif_object = the_symtab;

    if (the_suif_object->object_kind() != SYMTAB_OBJ)
      {
        problem("object passed as symtab isn't a symtab");
        pop_clue(the_symtab);
        return;
      }

    base_symtab *old_scope = current_scope;
    current_scope = the_symtab;

    check_suif_object(the_suif_object);

    if (the_symtab->name() == NULL)
        problem("NULL name in symtab");

    base_symtab *parent_symtab = the_symtab->parent();
    if (parent_symtab != NULL)
      {
        check_symtab_ref(parent_symtab);
        base_symtab_list_iter child_iter(parent_symtab->children());
        while (TRUE)
          {
            if (child_iter.is_empty())
              {
                problem("child symtab not in parent's children() list");
                break;
              }
            base_symtab *test_symtab = child_iter.step();
            if (test_symtab == the_symtab)
                break;
          }
      }

    if (the_symtab->symbols() == NULL)
      {
        problem("NULL symbol list in symbol table");
        current_scope = old_scope;
        pop_clue(the_symtab);
        return;
      }

    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_symbol = sym_iter.step();
        check_suif_object(this_symbol);
        check_sym_node_ref(this_symbol);

        push_clue(this_symbol);

        if (this_symbol->parent() != the_symtab)
            problem("symbol's parent() != the table containing it");

        switch (this_symbol->kind())
          {
            case SYM_PROC:
              {
                proc_sym *this_proc = (proc_sym *)this_symbol;

                file_set_entry *this_fse = this_proc->file();
                if (this_fse != NULL)
                    check_fse_ref(this_fse);

                check_func_type_ref(this_proc->type());

                tree_proc *the_tree_proc = this_proc->block();
                if (the_tree_proc != NULL)
                    check_tree_proc_ref(the_tree_proc);

                if (!the_symtab->is_global())
                    problem("proc_sym in non-global symbol table");

                break;
              }
            case SYM_LABEL:
              {
                break;
              }
            case SYM_VAR:
              {
                var_sym *this_var = (var_sym *)this_symbol;

                check_object_type_ref(this_var->type());
                if ((this_var->type()->size() == 0) &&
                    (this_var->is_auto() || this_var->has_var_def()))
                  {
                    problem("var_sym with zero size");
                  }

                var_sym *parent_var = this_var->parent_var();
                if (parent_var != NULL)
                  {
                    check_var_sym_ref(parent_var);
                    if (parent_var->parent() != the_symtab)
                        problem("parent_var() in different table");
                    unsigned num_children = parent_var->num_children();
                    unsigned child_num;
                    for (child_num = 0; child_num < num_children; ++child_num)
                      {
                        if (parent_var->child_var(child_num) == this_var)
                            break;
                      }
                    if (child_num == num_children)
                        problem("parent_var() doesn't contain child");
                  }

                unsigned num_children = this_var->num_children();
                for (unsigned child_num = 0; child_num < num_children;
                     ++child_num)
                  {
                    var_sym *this_child = this_var->child_var(child_num);
                    check_var_sym_ref(this_child);
                    if (this_child->parent() != the_symtab)
                        problem("child_var() in different table");
                    if (this_child->parent_var() != this_var)
                        problem("child_var() with different parent_var()");
                  }

                if ((this_var->parent_var() == NULL) &&
                    ((the_symtab->is_block() && this_var->is_static()) ||
                     the_symtab->is_file()))
                  {
                    if (!this_var->has_var_def())
                        problem("`%s' has no definition", this_var->name());
                  }
                else
                  {
                    if (((parent_var != NULL) ||
                         (the_symtab->kind() != SYMTAB_GLOBAL)) &&
                        this_var->has_var_def())
                      {
                        problem("`%s' shouldn't have a definition, but does",
                                this_var->name());
		      }
                  }

                if (this_var->has_var_def())
                  {
                    var_def *this_def = this_var->definition();
                    if (this_def == NULL)
                      {
                        problem("var_def for `%s' not found",
                                this_var->name());
                      }
                  }

                if (this_var->is_param())
                  {
                    if (!the_symtab->is_proc())
                      {
                        problem("parameter symbol in non-procedure symtab");
                      }
                    else
                      {
                        proc_symtab *the_proc_tab = (proc_symtab *)the_symtab;
                        sym_node_list_iter param_iter(the_proc_tab->params());
                        while (TRUE)
                          {
                            if (param_iter.is_empty())
                              {
                                problem("symbol `%s' marked param is not in"
                                        " parameter list", this_var->name());
                                break;
                              }
                            sym_node *test_sym = param_iter.step();
                            if (test_sym == this_var)
                                break;
                          }
                      }
                  }

                break;
              }
            default:
                problem("illegal symbol type");
          }

        pop_clue(this_symbol);
      }

    if (the_symtab->var_defs() == NULL)
      {
        problem("NULL var_def list in symbol table");
        current_scope = old_scope;
        pop_clue(the_symtab);
        return;
      }

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();

        if (this_def == NULL)
          {
            problem("NULL var_def * in symtab var_def list");
            continue;
          }

        push_clue(this_def);

        suif_object *the_suif_object = this_def;

        if (the_suif_object->object_kind() != DEF_OBJ)
          {
            problem("object in symtab var_def list isn't a var_def");
            continue;
          }

        check_suif_object(the_suif_object);

        if (this_def->parent() != the_symtab)
            problem("var_def's parent() != the table containing it");

        var_sym *this_var = this_def->variable();
        check_var_sym_ref(this_var);

        if ((this_var->parent() != the_symtab) &&
            ((this_var->parent() != fileset->globals()) ||
             (the_symtab->parent() != fileset->globals())))
          {
            problem("var_sym for `%s' in different symbol table than its"
                    " var_def", this_var->name());
          }

        if (this_def->alignment() != get_alignment(this_var->type()))
            problem("wrong alignment for variable `%s'", this_var->name());

        if (!type_is_compatible_with_initializations(this_var->type(),
                                                     this_def))
          {
            problem("misalignment of initialization data");
          }

        pop_clue(this_def);
      }

    if (the_symtab->types() == NULL)
      {
        problem("NULL type list in symbol table");
        current_scope = old_scope;
        pop_clue(the_symtab);
        return;
      }

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        check_suif_object(this_type);
        check_type_ref(this_type);

        push_clue(this_type);

        if (this_type->parent() != the_symtab)
            problem("type's parent() != the table containing it");

        switch (this_type->op())
          {
            case TYPE_INT:
              {
                base_type *this_base_type = (base_type *)this_type;
                if (this_base_type->size() < 0)
                    problem("integer type with negative size");
                if (this_base_type->size() == 0)
                    problem("integer type with zero size");
                break;
              }
            case TYPE_FLOAT:
              {
                base_type *this_base_type = (base_type *)this_type;
                if (this_base_type->size() < 0)
                    problem("floating-point type with negative size");
                if (this_base_type->size() == 0)
                    problem("floating-point type with zero size");
                if (!this_base_type->is_signed())
                    problem("unsigned floating-point type");
                break;
              }
            case TYPE_VOID:
              {
                base_type *this_base_type = (base_type *)this_type;
                if (this_base_type->size() != 0)
                    problem("void type with non-zero size");
                break;
              }
            case TYPE_PTR:
              {
                ptr_type *this_ptr = (ptr_type *)this_type;
                check_type_ref(this_ptr->ref_type());
                break;
              }
            case TYPE_ARRAY:
              {
                array_type *this_array = (array_type *)this_type;
                type_node *elem_type = this_array->elem_type();
                check_object_type_ref(elem_type);
                check_array_bound(this_array->lower_bound());
                check_array_bound(this_array->upper_bound());
                if (this_array->lower_bound().is_unknown())
                    problem("array type with unknown lower bound");
                if (elem_type->unqual()->is_array())
                  {
                    array_type *elem_array =
                            (array_type *)(elem_type->unqual());
                    if (elem_array->upper_bound().is_unknown())
                        problem("sub-array with unknown upper bound");
                  }
                break;
              }
            case TYPE_FUNC:
              {
                func_type *this_func = (func_type *)this_type;
                check_object_type_ref(this_func->return_type());
                if (!this_func->args_known())
                  {
                    if (this_func->has_varargs())
                      {
                        problem("function with unknown args but varargs flag"
                                " set");
                      }
                    if (this_func->num_args() != 0)
                        problem("function with unknown args num_args() != 0");
                  }
                unsigned num_args = this_func->num_args();
                for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                    check_object_type_ref(this_func->arg_type(arg_num));
                break;
              }
            case TYPE_GROUP:
            case TYPE_STRUCT:
            case TYPE_UNION:
              {
                struct_type *this_struct = (struct_type *)this_type;
                if (this_struct->name() == NULL)
                    problem("struct type with NULL name");
                int total_size = this_struct->size();
                if (total_size < 0)
                    problem("struct_type with negative size");
                int last_field_end = 0;
                unsigned num_fields = this_struct->num_fields();
                for (unsigned field_num = 0; field_num < num_fields;
                     ++field_num)
                  {
                    int this_offset = this_struct->offset(field_num);
                    type_node *field_type = this_struct->field_type(field_num);
                    check_object_type_ref(field_type);
                    int field_size = field_type->size();
                    if (this_offset + field_size > total_size)
                        problem("field goes past end of struct type");
                    if (this_offset < 0)
                        problem("field with negative offset in struct type");
                    if (field_size < 0)
                        problem("struct type with field of negative size");
                    else if (field_size == 0)
                        problem("struct type with field of zero size");
                    if (this_struct->op() == TYPE_STRUCT)
                      {
                        if (this_offset < last_field_end)
                            problem("out-of-order fields in struct type");
                        last_field_end = this_offset + field_size;
                      }
                    else if (this_struct->op() == TYPE_UNION)
                      {
                        if (this_offset != 0)
                            problem("union with non-zero field offset");
                      }
                  }
                break;
              }
            case TYPE_ENUM:
              {
                enum_type *this_enum = (enum_type *)this_type;
                if (this_enum->size() < 0)
                    problem("enumerated type with negative size");
                if (this_enum->size() == 0)
                    problem("enumerated type with zero size");
                break;
              }
            case TYPE_CONST:
            case TYPE_RESTRICT:
            case TYPE_VOLATILE:
            case TYPE_CALL_BY_REF:
            case TYPE_NULL:
              {
                modifier_type *this_modifier = (modifier_type *)this_type;
                check_object_type_ref(this_modifier->base());
                break;
              }
            default:
                problem("unknown type op()");
          }

        pop_clue(this_type);
      }

    if (the_symtab->children() == NULL)
      {
        problem("NULL children list in symbol table");
        current_scope = old_scope;
        pop_clue(the_symtab);
        return;
      }

    base_symtab_list_iter child_iter(the_symtab->children());
    while (!child_iter.is_empty())
      {
        base_symtab *this_child = child_iter.step();
        check_symtab_ref(this_child);
        push_clue(this_child);
        if (this_child->parent() != the_symtab)
            problem("child symtab with wrong parent");
        pop_clue(this_child);
      }

    switch (the_symtab->kind())
      {
        case SYMTAB_GLOBAL:
            break;
        case SYMTAB_FILE:
          {
            file_symtab *the_file_symtab = (file_symtab *)the_symtab;
            check_fse_ref(the_file_symtab->fse());
            break;
          }
        case SYMTAB_PROC:
          {
            proc_symtab *the_proc_symtab = (proc_symtab *)the_symtab;
            tree_block *the_tree_block = the_proc_symtab->block();
            if (!the_tree_block->is_proc())
                problem("proc_symtab with non-proc block");
            tree_proc *the_tree_proc = (tree_proc *)the_tree_block;
            check_tree_proc_ref(the_tree_proc);
            if (the_tree_proc->proc_syms() != the_proc_symtab)
                problem("block() of proc_symtab has different proc_syms()");
            sym_node_list_iter param_iter(the_proc_symtab->params());
            while (!param_iter.is_empty())
              {
                sym_node *this_param = param_iter.step();
                if (!this_param->is_var())
                  {
                    problem("non-variable in parameter list");
                  }
                else
                  {
                    var_sym *this_var = (var_sym *)this_param;
                    check_var_sym_ref(this_var);
                    if (this_var->parent() != the_proc_symtab)
                      {
                        problem("variable in parameter list is from a "
                                "different symbol table");
                      }
                    if (!this_var->is_param())
                        problem("non-parameter in parameter list");
                  }
              }
            break;
          }
        case SYMTAB_BLOCK:
          {
            block_symtab *the_block_symtab = (block_symtab *)the_symtab;
            tree_block *the_tree_block = the_block_symtab->block();
            check_tree_block_ref(the_tree_block);
            if (the_tree_block->symtab() != the_block_symtab)
                problem("block() of block_symtab has different symtab()");
            break;
          }
        default:
            problem("unknown symtab kind()");
      }

    current_scope = old_scope;
    pop_clue(the_symtab);
  }

extern void check_proc(tree_proc *the_proc)
  {
    tree_node *the_tree_node = (tree_node *)the_proc;
    check_node(the_tree_node);
    push_clue(the_proc);
    if (!the_tree_node->is_proc())
        problem("object used as a tree_proc isn't a tree_proc");
    pop_clue(the_proc);
  }

extern void check_node(tree_node *the_node)
  {
    if (the_node == NULL)
      {
        problem("NULL pointer used as tree_node *");
        return;
      }

    push_clue(the_node);

    suif_object *the_suif_object = the_node;

    if (the_suif_object->object_kind() != TREE_OBJ)
      {
        problem("object used as a tree_node isn't a tree_node");
        pop_clue(the_node);
        return;
      }

    check_suif_object(the_suif_object);

    tree_node *old_node = current_node;
    current_node = the_node;

    tree_node_list_e *list_e = the_node->list_e();
    if (list_e != NULL)
      {
        if (list_e->contents != the_node)
            problem("list_e() of tree_node does not contain it");
      }

    tree_node_list *parent = the_node->parent();
    if (parent != NULL)
      {
        if (list_e == NULL)
            problem("tree_node with non-NULL parent() but NULL list_e()");
        tree_node_list_e *follow_e = parent->head();
        while (TRUE)
          {
            if (follow_e == NULL)
              {
                problem("parent of tree_node does not contain it");
                break;
              }
            if (follow_e == list_e)
                break;
            follow_e = follow_e->next();
          }
      }

    base_symtab *node_scope = the_node->scope();
    if (node_scope != NULL)
      {
        suif_object *scope_suif_object = node_scope;
        if (scope_suif_object->object_kind() != SYMTAB_OBJ)
          {
            problem("scope() in tree_node isn't really a base_symtab");
            node_scope = NULL;
          }
      }

    base_symtab *old_scope = current_scope;
    if (node_scope != NULL)
      {
        if (current_scope == NULL)
          {
            current_scope = node_scope;
          }
        else
          {
            if (current_scope != node_scope)
                problem("scope() in tree_node is wrong");
          }
      }

    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            check_instruction(the_instr);
            if (the_instr->parent() != the_tree_instr)
              {
                problem("instr() of tree_instr does not have the tree_instr"
                        " as parent()");
              }
            break;
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;

            check_label_sym_ref(the_loop->contlab());
            check_label_sym_ref(the_loop->brklab());
            check_label_sym_ref(the_loop->toplab());

            push_label_scope();
            register_label_def(the_loop->contlab());
            register_label_def(the_loop->brklab());
            check_node_list(the_loop->body());
            pop_label_scope();

            push_label_scope();
            register_label_def(the_loop->toplab());
            check_node_list(the_loop->test());
            pop_label_scope();

            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;

            var_sym *index = the_for->index();
            boolean index_signed = FALSE;
            boolean index_type_known = FALSE;
            if (index == NULL)
              {
                problem("NULL index in tree_for");
              }
            else
              {
                check_var_sym_ref(index);
                if (check_object_type_ref(index->type()) &&
                    check_object_type_ref(index->type()->unqual()))
                  {
                    type_node *ind_type_unqual = index->type()->unqual();
                    if (ind_type_unqual->op() == TYPE_INT)
                      {
                        base_type *the_base = (base_type *)ind_type_unqual;
                        index_type_known = TRUE;
                        index_signed = the_base->is_signed();
                      }
                    else
                      {
                        problem("type of tree_for index is not an integer");
                      }
                  }
              }

            switch (the_for->test())
              {
                case FOR_EQ:
                case FOR_NEQ:
                    break;
                case FOR_SGELE:
                case FOR_SGT:
                case FOR_SGTE:
                case FOR_SLT:
                case FOR_SLTE:
                    if (index_type_known && !index_signed)
                        problem("signed test on unsigned index in tree_for");
                    break;
                case FOR_UGELE:
                case FOR_UGT:
                case FOR_UGTE:
                case FOR_ULT:
                case FOR_ULTE:
                    if (index_type_known && index_signed)
                        problem("unsigned test on signed index in tree_for");
                    break;
                default:
                    problem("illegal tree_node kind");
              }

            if (!the_for->lb_op().type()->is_same(index->type()))
                problem("bad lb_op() type");
            if (!the_for->ub_op().type()->is_same(index->type()))
                problem("bad ub_op() type");
            if (!the_for->step_op().type()->op() == TYPE_INT)
                problem("type of tree_for step_op() is not an integer");

            check_label_sym_ref(the_for->contlab());
            check_label_sym_ref(the_for->brklab());

            push_label_scope();
            check_node_list(the_for->lb_list());
            pop_label_scope();

            push_label_scope();
            check_node_list(the_for->ub_list());
            pop_label_scope();

            push_label_scope();
            check_node_list(the_for->step_list());
            pop_label_scope();

            push_label_scope();
            register_label_def(the_for->contlab());
            register_label_def(the_for->brklab());
            check_node_list(the_for->body());
            pop_label_scope();

            push_label_scope();
            check_node_list(the_for->landing_pad());
            pop_label_scope();

            break;
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;

            check_label_sym_ref(the_if->jumpto());

            push_label_scope();
            register_label_def(the_if->jumpto());
            check_node_list(the_if->header());
            pop_label_scope();

            push_label_scope();
            check_node_list(the_if->then_part());
            pop_label_scope();

            push_label_scope();
            check_node_list(the_if->else_part());
            pop_label_scope();

            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;

            block_symtab *the_symtab = the_block->symtab();
            check_symtab(the_symtab);
            if (the_symtab->block() != the_block)
                problem("block with symtab() with wrong block() pointer");

            base_symtab *the_base_symtab = the_symtab;
            if (!the_base_symtab->is_block())
                problem("symtab of block is not block_symtab");

            if (the_block->is_proc())
                push_label_scope();

            base_symtab *save_scope = current_scope;
            current_scope = the_block->symtab();
            check_node_list(the_block->body());
            current_scope = save_scope;

            if (the_block->is_proc())
                pop_label_scope();

            if (the_block->is_proc())
              {
                tree_proc *the_tree_proc = (tree_proc *)the_block;
                proc_sym *the_proc_sym = the_tree_proc->proc();
                check_proc_sym_ref(the_proc_sym);
                if (!the_symtab->is_proc())
                    problem("symtab of proc is not proc_symtab");

                func_type *the_func_type = the_proc_sym->type();
                if (the_func_type->args_known())
                  {
                    proc_symtab *the_proc_symtab = (proc_symtab *)the_symtab;
                    sym_node_list_iter param_iter(the_proc_symtab->params());
                    unsigned num_args = the_func_type->num_args();
                    for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                      {
                        sym_node *this_sym = param_iter.step();
                        if (this_sym == NULL)
                          {
                            problem("number of parameters in formal argument "
                                    "list (%lu) is less than the number in "
                                    "the function type (%lu)",
                                    (unsigned long)arg_num,
                                    (unsigned long)num_args);
                            break;
                          }
                        if (!this_sym->is_var())
                            continue;
                        var_sym *this_param = (var_sym *)this_sym;
                        if (this_param->type() !=
                            the_func_type->arg_type(arg_num))
                          {
                            problem("mismatch between the type of formal "
                                    "argument %u and the type in the function "
                                    "type", arg_num);
                          }
                      }
                    if (!param_iter.is_empty())
                      {
                        problem("number of parameters in formal argument list "
                                "(%lu) is greater than the number in the "
                                "function type (%lu)",
                                (unsigned long)
                                        (the_proc_symtab->params()->count()),
                                (unsigned long)num_args);
                      }
                  }
              }

            break;
          }
        default:
            problem("illegal tree_node kind");
      }

    current_scope = old_scope;
    current_node = old_node;
    pop_clue(the_node);
  }

extern void check_operand(operand the_operand)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            break;
        case OPER_SYM:
            check_var_sym_ref(the_operand.symbol());
            break;
        case OPER_INSTR:
          {
            instruction *this_instr = the_operand.instr();
            if (the_operand.is_expr())
                check_instruction(this_instr);
            else
                check_instruction_ref(this_instr);
            if (this_instr->dst_op().is_symbol())
                problem("instruction used as operand with symbol destination");
            break;
          }
        default:
            problem("unknown kind() for operand");
      }
  }

extern void check_instruction(instruction *the_instr)
  {
    if (the_instr == NULL)
      {
        problem("NULL pointer used as instruction *");
        return;
      }

    push_clue(the_instr);

    suif_object *the_suif_object = the_instr;

    if (the_suif_object->object_kind() != INSTR_OBJ)
      {
        problem("object passed as an instruction isn't an instruction");
        pop_clue(the_instr);
        return;
      }

    check_suif_object(the_suif_object);

    check_object_type_ref(the_instr->result_type());

    tree_instr *parent = the_instr->parent();
    if (parent != NULL)
        check_tree_instr_ref(parent);

    operand dest_op = the_instr->dst_op();
    switch (dest_op.kind())
      {
        case OPER_NULL:
            break;
        case OPER_SYM:
            check_var_sym_ref(dest_op.symbol());
            break;
        case OPER_INSTR:
          {
            instruction *dest_instr = dest_op.instr();
            check_instruction_ref(dest_instr);
            unsigned num_dest_srcs = dest_instr->num_srcs();
            unsigned src_num;
            for (src_num = 0; src_num < num_dest_srcs; ++src_num)
              {
                if (dest_instr->src_op(src_num) == operand(the_instr))
                    break;
              }
            if (src_num == num_dest_srcs)
                problem("destination does not use instruction as source");
            if ((!operand(the_instr).is_expr()) && (pending_instr_ops != NULL))
                pending_instr_ops->append(the_instr);
            break;
          }
        default:
            problem("unknown kind() for destination operand");
      }

    switch (the_instr->format())
      {
        case inf_none:
          {
            problem("instruction with format() ``inf_none''");
            break;
          }
        case inf_rrr:
          {
            break;
          }
        case inf_bj:
          {
            in_bj *the_bj = (in_bj *)the_instr;
            check_label_sym_ref(the_bj->target());
            register_label_use(the_bj->target());
            break;
          }
        case inf_ldc:
          {
            in_ldc *the_ldc = (in_ldc *)the_instr;

            immed value = the_ldc->value();
            check_immed(&value);

            type_ops result_base_op = the_instr->result_type()->unqual()->op();
            switch (value.kind())
              {
                case im_int:
                case im_extended_int:
                    if ((result_base_op != TYPE_INT) &&
                        (result_base_op != TYPE_ENUM) &&
                        (result_base_op != TYPE_PTR))
                      {
                        problem("ldc of integer constant with non-integer,"
                                " non-pointer type");
                      }
                    break;
                case im_float:
                case im_extended_float:
                    if (result_base_op != TYPE_FLOAT)
                      {
                        problem("ldc of floating-point constant with "
                                "non-floating-point type");
                      }
                    break;
                case im_symbol:
                  {
                    sym_node *the_symbol = value.symbol();
                    if (the_symbol->is_var())
                      {
                        var_sym *the_var = (var_sym *)the_symbol;
                        if (!the_var->is_addr_taken())
                          {
                            problem("ldc of address of symbol `%s' with "
                                    "is_addr_taken() flag FALSE",
                                    the_var->name());
                          }
                      }
                    if (!the_instr->result_type()->unqual()->is_ptr())
                        problem("ldc of symbol with non-pointer result type");
                    break;
                  }
                default:
                    problem("illegal immed kind `%s' in ldc instruction",
                            value.kind());
                    break;
              }

            if (!immed_fits(value, the_instr->result_type()))
                problem("value of ldc does not fit result type");

            break;
          }
        case inf_cal:
          {
            break;
          }
        case inf_array:
          {
            break;
          }
        case inf_mbr:
          {
            in_mbr *the_mbr = (in_mbr *)the_instr;
            check_label_sym_ref(the_mbr->default_lab());
            register_label_use(the_mbr->default_lab());
            unsigned num_labs = the_mbr->num_labs();
            for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
              {
                check_label_sym_ref(the_mbr->label(lab_num));
                register_label_use(the_mbr->label(lab_num));
              }
            break;
          }
        case inf_lab:
          {
            in_lab *the_lab_instr = (in_lab *)the_instr;
            check_label_sym_ref(the_lab_instr->label());
            register_label_def(the_lab_instr->label());
            break;
          }
        case inf_gen:
          {
            in_gen *the_gen_instr = (in_gen *)the_instr;
            const char *name = the_gen_instr->name();
            if (name == NULL)
              {
                problem("in_gen with NULL name");
              }
            else if (name != lexicon->enter(name)->sp)
              {
                problem("in_gen instruction with name not in lexicon");
              }
            break;
          }
        default:
          {
            problem("unknown format() for instruction");
            break;
          }
      }

    check_type_instr(the_instr, the_instr->parent());

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_op = the_instr->src_op(src_num);
        check_operand(this_op);
        if (this_op.is_instr())
          {
            instruction *this_src_instr = this_op.instr();
            if (this_src_instr->dst_op() != operand(the_instr))
              {
                problem("instruction source operand does not use instruction"
                        " as destination");
              }
            if ((!this_op.is_expr()) && (pending_instr_ops != NULL))
              {
                instruction_list_e *follow_instr = pending_instr_ops->head();
                while (TRUE)
                  {
                    if (follow_instr == NULL)
                      {
                        problem("instruction used as source is not live");
                        break;
                      }
                    if (follow_instr->contents == this_src_instr)
                      {
                        pending_instr_ops->remove(follow_instr);
                        break;
                      }
                    follow_instr = follow_instr->next();
                  }
              }
          }
      }

    pop_clue(the_instr);
  }

extern void problem(char *fmt, ...)
  {
    va_list ap;

    va_start(ap, fmt);
    vproblem(fmt, ap);
    va_end(ap);
  }

extern void vproblem(char *fmt, va_list ap)
  {
    char *new_fmt = new char[strlen(fmt) + 13];
    sprintf(new_fmt, "%s%s", (in_annote ? "[in annote] " : ""), fmt);
    if (checkfail)
        verror_line(checkabort ? 0 : 1, current_node, new_fmt, ap);
    else
        vwarning_line(current_node, new_fmt, ap);
    delete[] new_fmt;
    if (checkabort)
        abort();
  }

extern void problem_instr(tree_node *the_node, instruction *the_instr,
                          char *fmt, ...)
  {
    va_list ap;

    va_start(ap, fmt);
    vproblem_instr(the_node, the_instr, fmt, ap);
    va_end(ap);
  }

extern void vproblem_instr(tree_node *the_node, instruction *the_instr,
                           char *fmt, va_list ap)
  {
    tree_node *old_current = current_node;
    if (the_node != NULL)
        current_node = the_node;
    char *new_fmt = new char[strlen(fmt) + 30];
    sprintf(new_fmt, "instruction (%u) - %s", the_instr->number(), fmt);
    vproblem(new_fmt, ap);
    delete[] new_fmt;
    current_node = old_current;
  }

static void check_node_list(tree_node_list *the_node_list)
  {
    if (the_node_list == NULL)
      {
        problem("NULL pointer used as tree_node_list *");
        return;
      }

    tree_node *parent = the_node_list->parent();
    if (parent != NULL)
      {
        check_tree_node_ref(parent);
        boolean not_used = FALSE;
        switch (parent->kind())
          {
            case TREE_INSTR:
                problem("tree_node_list with tree_instr as parent");
                break;
            case TREE_LOOP:
              {
                tree_loop *the_loop = (tree_loop *)parent;
                not_used = ((the_loop->body() != the_node_list) &&
                            (the_loop->test() != the_node_list));
                break;
              }
            case TREE_FOR:
              {
                tree_for *the_for = (tree_for *)parent;
                not_used = ((the_for->body() != the_node_list) &&
                            (the_for->landing_pad() != the_node_list) &&
                            (the_for->lb_list() != the_node_list) &&
                            (the_for->ub_list() != the_node_list) &&
                            (the_for->step_list() != the_node_list));
                break;
              }
            case TREE_IF:
              {
                tree_if *the_if = (tree_if *)parent;
                not_used = ((the_if->header() != the_node_list) &&
                            (the_if->then_part() != the_node_list) &&
                            (the_if->else_part() != the_node_list));
                break;
              }
            case TREE_BLOCK:
              {
                tree_block *the_block = (tree_block *)parent;
                not_used = (the_block->body() != the_node_list);
                break;
              }
            default:
                problem("unknown kind() for parent of tree_node_list");
          }

        if (not_used)
            problem("parent of tree_node_list does not use it");
      }

    if ((pending_instr_ops != NULL) && (!pending_instr_ops->is_empty()))
        problem("instruction operand live across list beginning");
    instruction_list *old_pending_instrs = pending_instr_ops;
    pending_instr_ops = new instruction_list;

    tree_node_list_iter node_iter(the_node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        check_node(this_node);
      }

    if (!pending_instr_ops->is_empty())
        problem("instruction operand live across list end");
    delete pending_instr_ops;
    pending_instr_ops = old_pending_instrs;
  }

static void check_suif_object(suif_object *the_suif_object)
  {
    if (the_suif_object == NULL)
      {
        problem("NULL pointer used as suif_object *");
        return;
      }

    push_clue(the_suif_object);

    object_kinds this_kind = the_suif_object->object_kind();
    switch (this_kind)
      {
        case FILE_OBJ:
        case TREE_OBJ:
        case INSTR_OBJ:
        case SYMTAB_OBJ:
        case SYM_OBJ:
        case DEF_OBJ:
        case TYPE_OBJ:
            break;
        default:
            problem("object passed as a suif_object isn't a suif_object");
            pop_clue(the_suif_object);
            return;
      }

    annote_list *the_annotes = the_suif_object->annotes();
    check_annote_list(the_annotes);

    pop_clue(the_suif_object);
  }

static void check_annote_list(annote_list *the_annotes)
  {
    if (the_annotes == NULL)
      {
        problem("NULL pointer used as annote_list *");
        return;
      }

    annote_list_iter the_iter(the_annotes);
    while (!the_iter.is_empty())
      {
        annote *this_annote = the_iter.step();
        check_annote(this_annote);
      }
  }

static void check_annote(annote *the_annote)
  {
    boolean old_in_annote = in_annote;
    in_annote = TRUE;

    if (the_annote == NULL)
      {
        problem("NULL annote");
        return;
      }

    push_clue(the_annote);

    const char *name = the_annote->name();
    if (name == NULL)
      {
        problem("annote with NULL name");
        pop_clue(the_annote);
        return;
      }

    if (name != lexicon->enter(name)->sp)
      {
        problem("annote with name not in lexicon");
        pop_clue(the_annote);
        return;
      }

    annote_def *definition = lookup_annote(name);
    if ((definition != NULL) && (!definition->is_structured()))
      {
        immed_list *the_immeds = (immed_list *)the_annote->data();
        if (the_immeds != NULL)
            check_immed_list(the_immeds);
      }

    in_annote = old_in_annote;
    pop_clue(the_annote);
  }

static void check_immed_list(immed_list *the_immeds)
  {
    if (the_immeds == NULL)
      {
        problem("NULL pointer used as immed_list *");
        return;
      }

    immed_list_iter the_iter(the_immeds);
    while (!the_iter.is_empty())
      {
        immed this_immed = the_iter.step();
        check_immed(&this_immed);
      }
  }

static void check_immed(immed *the_immed)
  {
    switch (the_immed->kind())
      {
        case im_int:
            break;
        case im_extended_int:
            if (the_immed->ext_integer() == NULL)
              {
                problem("NULL data for extended integer immed");
              }
            else if (the_immed->ext_integer() !=
                     lexicon->enter(the_immed->ext_integer())->sp)
              {
                problem("extended integer immed data not in lexicon");
              }
            else
              {
                const char *follow = the_immed->ext_integer();
                if (*follow == '-')
                    ++follow;
                if (*follow == 0)
                    problem("empty digit string for extended integer immed");
                while (*follow != 0)
                  {
                    if (*follow == '-')
                      {
                        problem("dash in extended integer not in the first"
                                " position");
                        break;
                      }
                    if ((*follow < '0') || (*follow > '9'))
                      {
                        problem("non-digit `%c' in extended integer", *follow);
                        break;
                      }
                    ++follow;
                  }
              }
            break;
        case im_string:
            if (the_immed->string() == NULL)
              {
                problem("NULL for string immed");
              }
            else if (the_immed->string() !=
                     lexicon->enter(the_immed->string())->sp)
              {
                problem("string immed with data that is not entered in the"
                        " lexicon");
              }
            break;
        case im_float:
            break;
        case im_extended_float:
            if (the_immed->ext_flt() == NULL)
              {
                problem("NULL data for extended floating-point immed");
              }
            else if (the_immed->ext_flt() !=
                     lexicon->enter(the_immed->ext_flt())->sp)
              {
                problem("extended floating-point immed data not in lexicon");
              }
            else
              {
                const char *follow = the_immed->ext_flt();
                if (*follow == '-')
                    ++follow;
                if ((*follow == 0) || (*follow == 'e') || (*follow == 'E'))
                  {
                    problem("empty mantissa for extended floating-point"
                            " immed");
                  }
                boolean decimal_point_seen = FALSE;
                boolean in_exponent = FALSE;
                while (*follow != 0)
                  {
                    if (*follow == '-')
                      {
                        problem("dash in %s of extended floating-point immed"
                                " not in the first position",
                                (in_exponent ? "exponent" : "mantissa"));
                        break;
                      }
                    else if (*follow == '.')
                      {
                        if (in_exponent)
                          {
                            problem("decimal point in exponent of extended"
                                    " floating-point immed");
                            break;
                          }
                        else
                          {
                            if (decimal_point_seen)
                              {
                                problem("more than one decimal point in "
                                        "mantissa of extended floating-point"
                                        " immed");
                                break;
                              }
                            else
                              {
                                decimal_point_seen = TRUE;
                              }
                          }
                      }
                    else if ((*follow == 'e') || (*follow == 'E'))
                      {
                        if (in_exponent)
                          {
                            problem("multiple exponents in extended "
                                    "floating-point immed");
                            break;
                          }
                        else
                          {
                            in_exponent = TRUE;
                            if (follow[1] == '-')
                                ++follow;
                            if (follow[1] == 0)
                              {
                                problem("empty exponent in extended "
                                        "floating-point immed");
                              }
                          }
                      }
                    else if ((*follow < '0') || (*follow > '9'))
                      {
                        problem("illegal character `%c' in extended "
                                "floating-point immed", *follow);
                        break;
                      }
                    ++follow;
                  }
              }
            break;
        case im_symbol:
          {
            sym_addr this_addr = the_immed->addr();
            check_sym_addr(&this_addr);
            break;
          }
        case im_type:
            check_type_ref(the_immed->type());
            break;
        case im_op:
            check_operand(the_immed->op());
            break;
        case im_instr:
            check_instruction(the_immed->instr());
            break;
        case im_undef:
            break;
        default:
            problem("illegal immed kind `%c'", the_immed->kind());
            break;
      }
  }

static void check_sym_addr(sym_addr *the_addr)
  {
    sym_node *the_sym = the_addr->symbol();
    check_sym_node_ref(the_sym);
    if (the_addr->offset() != 0)
      {
        if (!the_sym->is_var())
          {
            problem("address of non-var sym `%s' used with non-zero offset",
                    the_sym->name());
          }
      }
  }

static void check_fse_ref(file_set_entry *the_fse)
  {
    if (the_fse == NULL)
      {
        problem("NULL pointer used as file_set_entry *");
        return;
      }

    push_clue(the_fse);

    suif_object *the_suif_object = the_fse;

    if (the_suif_object->object_kind() != FILE_OBJ)
      {
        problem("object used as file_set_entry isn't a file_set_entry");
        pop_clue(the_fse);
        return;
      }

    pop_clue(the_fse);
  }

static void check_tree_instr_ref(tree_instr *the_tree_instr)
  {
    tree_node *the_tree_node = the_tree_instr;
    check_tree_node_ref(the_tree_node);
    push_clue(the_tree_instr);
    if (!the_tree_node->is_instr())
        problem("object used as tree_instr isn't a tree_instr");
    pop_clue(the_tree_instr);
  }

static void check_tree_block_ref(tree_block *the_block)
  {
    tree_node *the_tree_node = the_block;
    check_tree_node_ref(the_tree_node);
    push_clue(the_block);
    if (!the_tree_node->is_block())
        problem("object used as tree_block isn't a tree_block");
    pop_clue(the_block);
  }

static void check_tree_proc_ref(tree_proc *the_tree_proc)
  {
    tree_block *the_block = the_tree_proc;
    check_tree_block_ref(the_block);
    push_clue(the_tree_proc);
    if (!the_block->is_proc())
        problem("object used as tree_proc isn't a tree_proc");
    pop_clue(the_tree_proc);
  }

static void check_tree_node_ref(tree_node *the_node)
  {
    if (the_node == NULL)
      {
        problem("NULL pointer used as tree_node *");
        return;
      }

    push_clue(the_node);

    suif_object *the_suif_object = the_node;

    if (the_suif_object->object_kind() != TREE_OBJ)
      {
        problem("object used as tree_node isn't a tree_node");
        pop_clue(the_node);
        return;
      }

    pop_clue(the_node);
  }

static void check_instruction_ref(instruction *the_instr)
  {
    if (the_instr == NULL)
      {
        problem("NULL pointer used as instruction *");
        return;
      }

    push_clue(the_instr);

    suif_object *the_suif_object = the_instr;

    if (the_suif_object->object_kind() != INSTR_OBJ)
        problem("object used as instruction isn't an instruction");

    pop_clue(the_instr);
  }

static void check_symtab_ref(base_symtab *the_symtab)
  {
    if (the_symtab == NULL)
      {
        problem("NULL pointer used as base_symtab *");
        return;
      }

    push_clue(the_symtab);

    suif_object *the_suif_object = the_symtab;

    if (the_suif_object->object_kind() != SYMTAB_OBJ)
      {
        problem("object used as base_symtab isn't a base_symtab");
        pop_clue(the_symtab);
        return;
      }

    pop_clue(the_symtab);
  }

static void check_var_sym_ref(var_sym *the_var)
  {
    sym_node *this_sym = the_var;
    check_sym_node_ref(the_var);
    push_clue(the_var);
    if (this_sym->kind() != SYM_VAR)
        problem("object used as var_sym isn't a var_sym");
    pop_clue(the_var);
  }

static void check_proc_sym_ref(proc_sym *the_proc)
  {
    sym_node *this_sym = the_proc;
    check_sym_node_ref(the_proc);
    push_clue(the_proc);
    if (this_sym->kind() != SYM_PROC)
        problem("object used as proc_sym isn't a proc_sym");
    pop_clue(the_proc);
  }

static void check_label_sym_ref(label_sym *the_label)
  {
    sym_node *this_sym = the_label;
    check_sym_node_ref(the_label);
    push_clue(the_label);
    if (this_sym->kind() != SYM_LABEL)
        problem("object used as label_sym isn't a label_sym");
    pop_clue(the_label);
  }

static void check_sym_node_ref(sym_node *symbol)
  {
    if (symbol == NULL)
      {
        problem("NULL pointer used as sym_node *");
        return;
      }

    push_clue(symbol);

    suif_object *the_suif_object = symbol;

    if (the_suif_object->object_kind() != SYM_OBJ)
      {
        problem("object used as sym_node isn't a sym_node");
        pop_clue(symbol);
        return;
      }

    if (symbol->name() == NULL)
      {
        problem("symbol has NULL name");
        pop_clue(symbol);
        return;
      }

    base_symtab *parent = symbol->parent();
    if (parent == NULL)
      {
        problem("symbol `%s' has no symbol table", symbol->name());
        pop_clue(symbol);
        return;
      }

    check_symtab_ref(parent);

    sym_node_list_iter sym_iter(parent->symbols());
    while (TRUE)
      {
        if (sym_iter.is_empty())
          {
            problem("symbol `%s' is not in the list of symbols for its parent",
                    symbol->name());
            pop_clue(symbol);
            return;
          }
        sym_node *test_sym = sym_iter.step();
        if (test_sym == symbol)
            break;
      }

    if (current_scope == NULL)
      {
        pop_clue(symbol);
        return;
      }

    if (!scope_in_scope(parent, current_scope))
        problem("symbol `%s' used out of its scope", symbol->name());

    pop_clue(symbol);
  }

static void check_array_bound(array_bound the_bound)
  {
    if (the_bound.is_variable())
        check_var_sym_ref(the_bound.variable());
  }

static void check_func_type_ref(func_type *the_func_type)
  {
    type_node *the_type_node = (type_node *)the_func_type;
    check_type_ref(the_type_node);
    push_clue(the_func_type);
    if (!the_type_node->is_func())
        problem("non-function type used as function type");
    pop_clue(the_func_type);
  }

static boolean check_object_type_ref(type_node *the_type)
  {
    boolean ok = check_type_ref(the_type);
    if (!ok)
        return FALSE;
    push_clue(the_type);
    if (the_type->is_func())
      {
        problem("function type used as object type");
        pop_clue(the_type);
        return FALSE;
      }
    pop_clue(the_type);
    return TRUE;
  }

static boolean check_type_ref(type_node *the_type)
  {
    if (the_type == NULL)
      {
        problem("NULL pointer used as type_node *");
        return FALSE;
      }

    push_clue(the_type);

    suif_object *the_suif_object = the_type;

    if (the_suif_object->object_kind() != TYPE_OBJ)
      {
        problem("object passed as type_node isn't a type_node");
        pop_clue(the_type);
        return FALSE;
      }

    base_symtab *parent = the_type->parent();
    if (parent == NULL)
      {
        problem("type has no symbol table");
        pop_clue(the_type);
        return FALSE;
      }

    check_symtab_ref(parent);

    type_node_list_iter type_iter(parent->types());
    while (TRUE)
      {
        if (type_iter.is_empty())
          {
            problem("type `%u' is not in the list of types for its parent",
                    the_type->type_id());
            pop_clue(the_type);
            return FALSE;
          }
        type_node *test_type = type_iter.step();
        if (test_type == the_type)
            break;
      }

    if (current_scope == NULL)
      {
        pop_clue(the_type);
        return TRUE;
      }

    if (!scope_in_scope(parent, current_scope))
        problem("type used out of its scope");

    pop_clue(the_type);
    return TRUE;
  }

static boolean scope_in_scope(base_symtab *outer, base_symtab *inner)
  {
    if ((outer == NULL) || (inner == NULL))
        return FALSE;

    base_symtab *follow = inner;

    while (follow != NULL)
      {
        if (follow == outer)
            return TRUE;
        follow = follow->parent();
      }

    return FALSE;
  }

static void push_label_scope(void)
  {
    if (defined_labs != NULL)
      {
        if (old_defined_labs == NULL)
            old_defined_labs = new string_index_list;
        old_defined_labs->push(defined_labs);
      }
    defined_labs = new tree_string_index;

    if (used_labs != NULL)
      {
        if (old_used_labs == NULL)
            old_used_labs = new label_sym_list_list;
        old_used_labs->push(used_labs);
      }
    used_labs = new label_sym_list;
  }

static void pop_label_scope(void)
  {
    assert(used_labs != NULL);
    assert(defined_labs != NULL);
    while (!used_labs->is_empty())
      {
        label_sym *this_lab = used_labs->pop();
        if (!defined_labs->exists(this_lab->name()))
          {
            problem("label `%s' used as target outside of its legal range",
                    this_lab->name());
          }
      }

    delete used_labs;
    delete defined_labs;

    if (old_defined_labs != NULL)
      {
        assert(old_used_labs != NULL);
        assert(!old_defined_labs->is_empty());
        assert(!old_used_labs->is_empty());
        defined_labs = old_defined_labs->pop();
        used_labs = old_used_labs->pop();
        if (old_defined_labs->is_empty())
          {
            assert(old_used_labs->is_empty());
            delete old_defined_labs;
            delete old_used_labs;
            old_defined_labs = NULL;
            old_used_labs = NULL;
          }
        else
          {
            assert(!old_used_labs->is_empty());
          }
      }
    else
      {
        assert(old_used_labs == NULL);
        defined_labs = NULL;
        used_labs = NULL;
      }
  }

static void register_label_def(label_sym *the_label)
  {
    if (defined_labs != NULL)
        defined_labs->enter(the_label->name(), the_label);
  }

static void register_label_use(label_sym *the_label)
  {
    if (used_labs != NULL)
        used_labs->append(the_label);
  }
