/* file "preprocess.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements some pre-processing of the SUIF code for the s2c
 *  program for the SUIF system.
 */

#define RCS_BASE_FILE preprocess_cc

#include "s2c.h"
#include <cctype>
#include <cstring>

RCS_BASE(
    "$Id$")

INCLUDE_SUIF_COPYRIGHT

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Type Definitions
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Type Definitions
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Global Variables
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Public Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Global Variables
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void do_local_group_layouts(tree_node *the_node, void *);
static void preprocess_node(tree_node *the_node, void *);
static void preprocess_instr(instruction *the_instr, void *);
static void compact_structs(base_symtab *the_symtab);
static void order_types(base_symtab *the_symtab);
static void put_type_and_contents_before(type_node_list *type_list,
                                         type_node_list_e *place,
                                         type_node *the_type);
static void put_containing_types_before(type_node_list *type_list,
                                        type_node_list_e *place,
                                        type_node *the_type);
static void fix_refs_in_type(type_node_list *type_list,
                             type_node_list_e *place, type_node *the_type);
static boolean type_after(type_node *the_type, type_node_list *type_list,
                          type_node_list_e *place);
static void fix_labels(base_symtab *the_symtab);
static void order_symbols(base_symtab *the_symtab);
static void put_def_and_refs_before(var_def_list *def_list,
                                    var_def_list_e *place,
                                    var_def_list_e *base,
                                    var_def *the_def);
static void put_referenced_defs_before(var_def_list *def_list,
                                       var_def_list_e *place,
                                       var_def_list_e *base,
                                       var_def *the_def);
static void comment_objects_in_symtab(base_symtab *the_symtab);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void preprocess_symtab(base_symtab *the_symtab)
  {
    if (!array_exprs)
        fix_function_array_types(the_symtab);
    fix_labels(the_symtab);
    if (!write_pseudo)
        fix_names(the_symtab);
    compact_structs(the_symtab);
    order_types(the_symtab);
    order_symbols(the_symtab);
    comment_objects_in_symtab(the_symtab);
    comment_object(the_symtab);
  }

extern void preprocess_proc(tree_proc *the_proc)
  {
    /*
     * All local groups must be layed out first because the layout is
     * used by everything else.
     */
    do_local_group_layouts(the_proc, NULL);

    preprocess_node(the_proc, NULL);
    the_proc->map(&preprocess_node, NULL, FALSE);
  }

extern void comment_object(suif_object *the_object)
  {
    no_annote_sub_vars(the_object);

    immed_list *comments =
            (immed_list *)(the_object->get_annote(k_s2c_comments));
    if (comments == NULL)
        comments = new immed_list;

    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        const char *this_name = this_annote->name();
        boolean show_this = all_annotes_to_comments;
        if (!show_this)
          {
            annote_place this_place = ap_last;
            switch (the_object->object_kind())
              {
                case FILE_OBJ:
                    this_place = ap_fses;
                    break;
                case TREE_OBJ:
                  {
                    tree_node *the_node = (tree_node *)the_object;
                    switch (the_node->kind())
                      {
                        case TREE_INSTR:
                            break;
                        case TREE_LOOP:
                            this_place = ap_loops;
                            break;
                        case TREE_FOR:
                            this_place = ap_fors;
                            break;
                        case TREE_IF:
                            this_place = ap_ifs;
                            break;
                        case TREE_BLOCK:
                            this_place = ap_blocks;
                            break;
                        default:
                            assert(FALSE);
                      }
                    break;
                  }
                case INSTR_OBJ:
                  {
                    this_place = ap_all_instrs;
                    instruction *the_instr = (instruction *)the_object;
                    if (show_annote_opcodes[the_instr->opcode()])
                        show_this = TRUE;
                    break;
                  }
                case SYMTAB_OBJ:
                    this_place = ap_symtabs;
                    break;
                case SYM_OBJ:
                  {
                    sym_node *the_sym = (sym_node *)the_object;
                    switch (the_sym->kind())
                      {
                        case SYM_PROC:
                            this_place = ap_proc_syms;
                            break;
                        case SYM_LABEL:
                            this_place = ap_labels;
                            break;
                        case SYM_VAR:
                            this_place = ap_vars;
                            break;
                        default:
                            assert(FALSE);
                      }
                    break;
                  }
                case DEF_OBJ:
                    this_place = ap_var_defs;
                    break;
                case TYPE_OBJ:
                    this_place = ap_types;
                    break;
                default:
                    assert(FALSE);
              }

            if (show_annote_places[this_place])
                show_this = TRUE;
          }

        if ((!show_this) && (show_annote_names != NULL))
          {
            for (int name_num = 0; show_annote_names[name_num] != NULL;
                 ++name_num)
              {
                if (strcmp(show_annote_names[name_num], this_annote->name())
                    == 0)
                  {
                    show_this = TRUE;
                    break;
                  }
              }
          }

        if (show_this && (strncmp(this_name, "s2c ", 4) != 0))
          {
            comments->append(comment_for_annote(this_annote));
          }
      }

    if (comments->is_empty())
        delete comments;
    else
        the_object->append_annote(k_s2c_comments, comments);
  }

/*
 *  When writing out structure types, if there are gaps between the
 *  fields, s2c uses unnamed bit fields to put the gaps in the C
 *  structure.  Since unnamed bit fields can only be as large as
 *  integers, though, for very large structures, this approach can
 *  lead to a huge number of unnamed bit fields, giving an enormous C
 *  file.  To avoid this, we'd like to put in an array where there is
 *  a large gap.  We want to add it to the type, not just print it, so
 *  that initialization will work right.  So we do so here.
 */
extern void compact_a_struct(struct_type *the_struct)
  {
    assert(the_struct->op() == TYPE_STRUCT);

    if (target.size[C_char] == 0)
        return;

    int filler_num = 0;

    unsigned num_fields = the_struct->num_fields();
    for (unsigned field_num = 0; field_num <= num_fields; ++field_num)
      {
        int end_of_field;
        if (field_num == 0)
          {
            end_of_field = 0;
          }
        else
          {
            end_of_field =
                the_struct->offset(field_num - 1) +
                the_struct->field_type(field_num - 1)->size();
          }
        int next_offset;
        if (field_num == num_fields)
            next_offset = the_struct->size();
        else
            next_offset = the_struct->offset(field_num);

        int space = next_offset - end_of_field;
        if (((space > (target.size[C_char] * 40)) &&
             (get_alignment(the_struct) >= target.align[C_char])) ||
            (gcc_bug_flag && (space > 0)))
          {
            int fill_align = ((target.array_align > target.align[C_char]) ?
                              target.array_align : target.align[C_char]);
            int space_mod = end_of_field % fill_align;
            if (space_mod > 0)
              {
                space -= (fill_align - space_mod);
                end_of_field += (fill_align - space_mod);
              }
            int filler_size = space / target.size[C_char];
            if (filler_size > 0)
              {
                char *filler_name;

                while (TRUE)
                  {
                      {
                        string_io the_io(&filler_name);
                        the_io.printf("__filler%d", filler_num);
                      }

                    unsigned existing_field =
                            the_struct->find_field_by_name(filler_name);
                    if (existing_field >= the_struct->num_fields())
                        break;
                    delete filler_name;
                    ++filler_num;
                  }

                ++num_fields;
                the_struct->set_num_fields(num_fields);
                for (unsigned move_field = num_fields - 1;
                     move_field > field_num; --move_field)
                  {
                    the_struct->set_field_name(move_field,
                            the_struct->field_name(move_field - 1));
                    the_struct->set_field_type(move_field,
                            the_struct->field_type(move_field - 1));
                    the_struct->set_offset(move_field,
                                           the_struct->offset(move_field - 1));
                  }

                type_node *filler_type =
                        new array_type(type_char, array_bound(0),
                                       array_bound(filler_size - 1));
                filler_type = type_char->parent()->install_type(filler_type);

                the_struct->set_field_name(field_num, filler_name);
                the_struct->set_field_type(field_num, filler_type);
                the_struct->set_offset(field_num, end_of_field);

                delete filler_name;
                ++filler_num;
                ++field_num;
              }
          }
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void do_local_group_layouts(tree_node *the_node, void *)
  {
    if (the_node->is_block())
      {
        tree_block *the_block = (tree_block *)the_node;
        init_summaries_for_symtab(the_block->symtab(), FALSE);
      }

    the_node->map(&do_local_group_layouts, NULL, TRUE, FALSE);

    if (the_node->is_block())
      {
        tree_block *the_block = (tree_block *)the_node;
        layout_groups_for_symtab(the_block->symtab());
      }
  }

static void preprocess_node(tree_node *the_node, void *)
  {
    comment_object(the_node);
    if (the_node->is_instr())
      {
        tree_instr *the_tree_instr = (tree_instr *)the_node;
        instr_no_sub_vars(the_tree_instr->instr());
        the_tree_instr->instr_map(&preprocess_instr, NULL, FALSE);
      }
    else if (the_node->is_for())
      {
        tree_for *the_for = (tree_for *)the_node;
        fix_re_eval_for(the_for);
      }
  }

static void preprocess_instr(instruction *the_instr, void *)
  {
    comment_object(the_instr);
    fix_re_eval_instr(the_instr);
    if (!array_exprs)
        fix_array_copy_on_instr(the_instr);
  }

static void compact_structs(base_symtab *the_symtab)
  {
    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->op() == TYPE_STRUCT)
            compact_a_struct((struct_type *)this_type);
      }
  }

/*
 *  We must make sure that struct or union types are defined only after all
 *  their field types that contain struct, union, or enumerated types have
 *  those types defined, and after all their field types that reference
 *  struct, union, or enumerated types are declared, if not defined.  Here, by
 *  ``containing'' we mean types with size that depend on the other type,
 *  which means the type itself or an array of a type containing it.  By
 *  ``referencing'' we mean pointers, which use the names of things they refer
 *  to but whose size doesn't depend on the other type.  This is the difference
 *  between the situations that require a complete type and those that allow an
 *  incomplete type.  Since structures can have pointers to each other
 *  recursively, we need to make this distinction and figure out when we can
 *  and must use incomplete types and when we need the complete type
 *  information before another type can be defined.
 *
 *  The algorithm to implement this has two steps.  First, go through the
 *  types in order and look at all the types contained in each structure or
 *  union type.  For each of these that is a structure, union, or enumerated,
 *  type, if it comes later in the list, move it just before the type under
 *  consideration and recursively put all types it contains in front of it.
 *  Second, go through the newly re-ordered type list and find any struct,
 *  union, or enumerated types that are referenced by a structure or union
 *  field type that comes earlier in the new list.  Move any such enumerated
 *  types just in front of their first reference and mark any such structure
 *  or union types with an annotation.  When writing the symbol table, all
 *  these marked types must be given forward declarations.
 */
static void order_types(base_symtab *the_symtab)
  {
    type_node_list *type_list = the_symtab->types();

    type_node_list_e *this_element = type_list->head();
    while (this_element != NULL)
      {
        type_node_list_e *next_element = this_element->next();
        type_node *this_type = this_element->contents;
        switch (this_type->op())
          {
            case TYPE_GROUP:
            case TYPE_STRUCT:
            case TYPE_UNION:
                put_containing_types_before(type_list, this_element,
                                            this_type);
                break;
            default:
                break;
          }
        this_element = next_element;
      }

    this_element = type_list->head();
    while (this_element != NULL)
      {
        type_node_list_e *next_element = this_element->next();
        type_node *this_type = this_element->contents;
        switch (this_type->op())
          {
            case TYPE_GROUP:
            case TYPE_STRUCT:
            case TYPE_UNION:
              {
                struct_type *the_struct = (struct_type *)this_type;
                unsigned num_fields = the_struct->num_fields();
                for (unsigned field_num = 0; field_num < num_fields;
                     ++field_num)
                  {
                    fix_refs_in_type(type_list, this_element,
                                     the_struct->field_type(field_num));
                  }
                break;
              }
            default:
                break;
          }
        this_element = next_element;
      }
  }

static void put_type_and_contents_before(type_node_list *type_list,
                                         type_node_list_e *place,
                                         type_node *the_type)
  {
    switch (the_type->op())
      {
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ENUM:
          {
            boolean past = FALSE;
            type_node_list_e *the_element = type_list->head();
            while (the_element != NULL)
              {
                type_node_list_e *next_element = the_element->next();
                if (the_element->contents == the_type)
                  {
                    if (!past)
                        break;
                    type_list->remove(the_element);
                    type_list->insert_before(the_element, place);
                    put_containing_types_before(type_list, the_element,
                                                the_type);
                    return;
                  }
                if (the_element == place)
                    past = TRUE;
                the_element = next_element;
              }
            break;
          }
        default:
            break;
      }

    put_containing_types_before(type_list, place, the_type);
  }

static void put_containing_types_before(type_node_list *type_list,
                                        type_node_list_e *place,
                                        type_node *the_type)
  {
    if (the_type->is_modifier())
      {
        put_type_and_contents_before(type_list, place, the_type->unqual());
        return;
      }

    switch (the_type->op())
      {
        case TYPE_FUNC:
            assert(FALSE);
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)the_type;
            put_type_and_contents_before(type_list, place,
                                         the_array->elem_type());
            break;
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)the_type;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                put_type_and_contents_before(type_list, place,
                        the_struct->field_type(field_num));
              }
            break;
          }
        default:
            break;
      }
  }

static void fix_refs_in_type(type_node_list *type_list,
                             type_node_list_e *place, type_node *the_type)
  {
    if (the_type->is_modifier())
      {
        fix_refs_in_type(type_list, place, the_type->unqual());
        return;
      }

    switch (the_type->op())
      {
        case TYPE_PTR:
          {
            ptr_type *the_ptr = (ptr_type *)the_type;
            fix_refs_in_type(type_list, place, the_ptr->ref_type());
            break;
          }
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)the_type;
            fix_refs_in_type(type_list, place, the_array->elem_type());
            break;
          }
        case TYPE_FUNC:
          {
            func_type *the_func = (func_type *)the_type;
            fix_refs_in_type(type_list, place, the_func->return_type());
            if (the_func->args_known())
              {
                unsigned num_args = the_func->num_args();
                for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                  {
                    fix_refs_in_type(type_list, place,
                                     the_func->arg_type(arg_num));
                  }
              }
            break;
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            if (type_after(the_type, type_list, place) &&
                (the_type->annotes()->peek_annote(
                         k_s2c_needs_forward_declaration) == NULL))
              {
                the_type->append_annote(k_s2c_needs_forward_declaration, NULL);
              }
            break;
          }
        case TYPE_ENUM:
            put_type_and_contents_before(type_list, place, the_type);
            break;
        default:
            break;
      }
  }

static boolean type_after(type_node *the_type, type_node_list *type_list,
                          type_node_list_e *place)
  {
    boolean past = FALSE;
    type_node_list_e *the_element = type_list->head();
    while (the_element != NULL)
      {
        type_node_list_e *next_element = the_element->next();
        if (the_element->contents == the_type)
          {
            if (past)
                return TRUE;
            else
                return FALSE;
          }
        if (the_element == place)
            past = TRUE;
        the_element = next_element;
      }
    return FALSE;
  }

/*
 *  In SUIF labels can have either block scope or procedure scope, but in C
 *  they all have procedure scope.  So we must fix the label names so that no
 *  two labels in the procedure have the same name.  To do this, we move all
 *  SUIF label symbols to the procedure symbol table and rename duplicates as
 *  they enter the procedure symbol table.
 */
static void fix_labels(base_symtab *the_symtab)
  {
    assert(the_symtab != NULL);
    if (the_symtab->kind() != SYMTAB_BLOCK)
        return;

    base_symtab *follow = the_symtab->parent();
    assert(follow != NULL);
    while (!follow->is_proc())
      {
        follow = follow->parent();
        assert(follow != NULL);
      }
    proc_symtab *the_proc_symtab = (proc_symtab *)follow;

    sym_node_list_iter the_iter(the_symtab->symbols());
    while (!the_iter.is_empty())
      {
        sym_node *this_sym = the_iter.step();

        if (!this_sym->is_label())
            continue;
        label_sym *this_label = (label_sym *)this_sym;
        the_symtab->remove_sym(this_label);

        label_sym *new_label =
                the_proc_symtab->new_unique_label(this_label->name());
        this_label->set_name(new_label->name());
        the_proc_symtab->remove_sym(new_label);
        delete new_label;

        the_proc_symtab->add_sym(this_label);
      }
  }

/*
 *  In C there are no forward declarations for statically linked
 *  variables, so we must be sure that if some statically linked
 *  variables have initializations that reference others, the
 *  declarations come before the references.  In SUIF it is possible
 *  to have a cycle of such initialization references; this cannot be
 *  represented in C so we must note this situation and give an
 *  appropriate error message.
 */
static void order_symbols(base_symtab *the_symtab)
  {
    var_def_list *def_list = the_symtab->var_defs();

    var_def_list_e *this_element = def_list->head();
    while (this_element != NULL)
      {
        var_def_list_e *next_element = this_element->next();
        var_def *this_def = this_element->contents;
        put_referenced_defs_before(def_list, this_element, this_element,
                                   this_def);
        this_element = next_element;
      }
  }

static void put_def_and_refs_before(var_def_list *def_list,
                                    var_def_list_e *place,
                                    var_def_list_e *base,
                                    var_def *the_def)
  {
    boolean past_place = FALSE;
    boolean past_base = FALSE;
    var_def_list_e *the_element = def_list->head();
    while (the_element != NULL)
      {
        var_def_list_e *next_element = the_element->next();
        if (the_element->contents == the_def)
          {
            if (!past_place)
                return;
            if (!past_base)
              {
                static boolean warned = FALSE;
                mistake(&warned, the_def,
                        "there is a cycle in static variable initialization "
                        "references\nthat goes through `%s';\nsuch a cycle "
                        "cannot be represented in C",
                        the_def->variable()->name());
                return;
              }
            assert(the_element != place);
            def_list->remove(the_element);
            def_list->insert_before(the_element, place);
            put_referenced_defs_before(def_list, the_element, base, the_def);
            return;
          }
        if (the_element == place)
            past_place = TRUE;
        if (the_element == base)
            past_base = TRUE;
        the_element = next_element;
      }
  }

static void put_referenced_defs_before(var_def_list *def_list,
                                       var_def_list_e *place,
                                       var_def_list_e *base,
                                       var_def *the_def)
  {
    annote_list_iter annote_iter(the_def->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        if ((this_annote->name() == k_multi_init) ||
            (this_annote->name() == k_repeat_init))
          {
            immed_list_iter immed_iter(this_annote->immeds());
            while (!immed_iter.is_empty())
              {
                immed this_immed = immed_iter.step();
                if (this_immed.is_symbol())
                  {
                    sym_node *this_symbol = this_immed.symbol();
                    if (this_symbol->is_var())
                      {
                        var_sym *this_var = (var_sym *)this_symbol;
                        this_var = this_var->root_ancestor();
                        if (this_var->has_var_def() &&
                            ((this_var->is_global() &&
                              this_var->is_private()) ||
                             ((!this_var->is_global()) &&
                              (!this_var->is_auto()))) &&
                            (this_var != the_def->variable()))
                          {
                            put_def_and_refs_before(def_list, place, base,
                                                    this_var->definition());
                          }
                      }
                  }
              }
          }
      }
  }

static void comment_objects_in_symtab(base_symtab *the_symtab)
  {
    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_symbol = sym_iter.step();
        comment_object(this_symbol);
      }

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        comment_object(this_def);
      }

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        comment_object(this_type);
      }
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
