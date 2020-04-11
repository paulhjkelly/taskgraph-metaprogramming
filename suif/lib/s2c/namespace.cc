/* file "namespace.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements routines to check for and fix name conflicts in the
 *  same C namespace for the s2c program for the SUIF system.
 */

#define RCS_BASE_FILE namespace_cc

#include "s2c.h"
#include <cctype>
#include <cstring>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Type Definitions
 *----------------------------------------------------------------------*/

typedef enum
  {
    LABEL_NAMES,
    TAG_NAMES,
    OTHER_NAMES
  } name_space;

typedef enum
  {
    STRUCT_NAME_TYPE = 0,
    UNION_NAME_TYPE,
    ENUM_NAME_TYPE,
    LABEL_NAME_TYPE,
    VAR_NAME_TYPE,
    FUNC_NAME_TYPE,
    ENUM_VAL_NAME_TYPE,
    MEMBER_NAME_TYPE
  } name_types;

typedef struct
  {
    name_space space;
    const char *prefix;
    int next_num;
  } name_data;

typedef struct
  {
    name_types the_name_type;

    union
      {
        type_node *the_type;
        sym_node *the_symbol;
        struct
          {
            enum_type *the_enum;
            unsigned number;
          } the_enum_val;
        struct
          {
            struct_type *the_struct;
            unsigned number;
          } the_member;
      } data;
  } name_instance;

typedef struct
  {
    type_node *type;
    boolean found;
  } type_ref_data;

typedef struct
  {
    sym_node *symbol;
    boolean found;
  } sym_ref_data;

typedef struct
  {
    enum_type *the_enum;
    unsigned member_num;
    boolean found;
  } enum_ref_data;

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

static name_data name_table[] =
  {
    {TAG_NAMES,   "__tmp_struct",   1},
    {TAG_NAMES,   "__tmp_union",    1},
    {TAG_NAMES,   "__tmp_enum",     1},
    {LABEL_NAMES, "__tmp_label",    1},
    {OTHER_NAMES, "__tmp_var",      1},
    {OTHER_NAMES, "__tmp_function", 1},
    {OTHER_NAMES, "__tmp_enum_val", 1},
    {OTHER_NAMES, "__tmp_member",   1}
  };

void reset_namespace ( ) {
	for ( int a = 0; a < 8; ++a ) {
		name_table[a].next_num = 1;
	}
}

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void fix_a_name(name_instance *the_instance, tree_block *scope);
static boolean name_conflict_for_instance(name_instance *the_instance,
                                          const char *name, tree_block *scope,
                                          boolean for_new);
static boolean name_conflict_in_inner_scope(base_symtab *the_symtab,
                                            tree_block *scope,
                                            name_space the_space, const char *name,
                                            suif_object *stopping_point);
static boolean name_conflict_with_reserved_or_macro(const char *name);
static boolean name_conflict_in_type(type_node *the_type, const char *name,
                                     name_space the_space, tree_block *scope);
static boolean name_conflict_in_symbol(sym_node *the_symbol, const char *name,
                                       name_space the_space,
                                       tree_block *scope);
static boolean direct_type_ref_in_scope(type_node *the_type,
                                        tree_block *scope);
static boolean direct_type_ref_in_symtab(type_node *the_type,
                                         base_symtab *the_symtab);
static void direct_type_ref_on_node(tree_node *the_node, void *data);
static void direct_type_ref_on_instr(instruction *the_instr, void *data);
static boolean direct_type_ref_in_type(type_node *to_ref, type_node *in_type);
static boolean type_ref_in_type_or_init_member(type_node *to_ref,
                                               type_node *in_type);
static boolean direct_symbol_ref_in_scope(sym_node *the_symbol,
                                          tree_block *scope);
static boolean direct_symbol_ref_in_symtab(sym_node *the_symbol,
                                           base_symtab *the_symtab);
static void direct_symbol_ref_on_node(tree_node *the_node, void *data);
static void direct_symbol_ref_on_instr(instruction *the_instr, void *data);
static boolean enum_const_ref_in_scope(enum_type *the_enum,
                                       unsigned member_num, tree_block *scope);
static boolean enum_const_ref_in_symtab(enum_type *the_enum,
                                        unsigned member_num,
                                        base_symtab *the_symtab);
static void enum_const_ref_on_node(tree_node *the_node, void *data);
static void enum_const_ref_on_instr(instruction *the_instr, void *data);
static boolean bad_c_id_name(const char *name);
static const char *get_instance_name(name_instance *the_instance);
static void set_instance_name(name_instance *the_instance, const char *name);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

/*
 *  We must fix up the names used by SUIF for the tags of struct, union, and
 *  enum types; symbols; enumeration values; and members of structures and
 *  unions.  First of all, the name must be of the form a letter or underscore
 *  followed by a string of letters, digits, and underscores.  Anything not of
 *  that form (for example, structures with all digits for names, created by
 *  the front end to represent anonymous structures) is given a name of the
 *  form ``__tmp_struct'', ``__tmp_union'', ``__tmp_enum'', ``__tmp_label'',
 *  ``__tmp_var'', ``__tmp_function'', ``__tmp_enum_val'', or ``__tmp_member''
 *  followed by some digits.  The digits represent the first integer, starting
 *  with 1, not already used with that prefix and not conflicting with another
 *  name in that name space used in that scope.
 *
 *  The second thing to do is insure that a name in an inner scope does not
 *  conflict with anything in the same name space in an enclosing scope that
 *  might be referenced in the inner scope, or with anything else in the same
 *  name space and scope.  In case of a conflict, the name in the inner scope
 *  has digits representing integers starting with 1 tacked onto the end of
 *  the name until a name is found that does not conflict.
 *
 *  Note that the tags of structures, unions, and enums form one name space,
 *  labels another, and everything else together forms another name space,
 *  except for the members of structures and unions -- each structure or union
 *  forms a separate namespace for its members.
 */
extern void fix_names(base_symtab *the_symtab)
  {
    tree_block *scope = NULL;
    if (the_symtab->is_block())
      {
        block_symtab *the_block_symtab = (block_symtab *)the_symtab;
        scope = the_block_symtab->block();
      }

    if (the_symtab->is_global() && (!the_symtab->is_file()))
      {
        global_symtab *the_global_symtab = (global_symtab *)the_symtab;
        map_namespace_to_alphanumeric(the_global_symtab);
      }

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if ((this_type->op() == TYPE_GROUP) ||
            (this_type->op() == TYPE_STRUCT) ||
            (this_type->op() == TYPE_UNION) || (this_type->op() == TYPE_ENUM))
          {
            name_instance the_instance;

            switch (this_type->op())
              {
                case TYPE_GROUP:
                case TYPE_STRUCT:
                case TYPE_UNION:
                    if (struct_is_union((struct_type *)this_type))
                        the_instance.the_name_type = UNION_NAME_TYPE;
                    else
                        the_instance.the_name_type = STRUCT_NAME_TYPE;
                    break;
                case TYPE_ENUM:
                    the_instance.the_name_type = ENUM_NAME_TYPE;
                    break;
                default:
                    assert(FALSE);
              }

            the_instance.data.the_type = this_type;
            fix_a_name(&the_instance, scope);

            if (this_type->op() == TYPE_ENUM)
              {
                enum_type *the_enum = (enum_type *)this_type;
                name_instance the_instance;

                the_instance.the_name_type = ENUM_VAL_NAME_TYPE;
                the_instance.data.the_enum_val.the_enum = the_enum;

                unsigned num_values = the_enum->num_values();
                for (unsigned value_num = 0; value_num < num_values;
                     ++value_num)
                  {
                    the_instance.data.the_enum_val.number = value_num;
                    fix_a_name(&the_instance, scope);
                  }
              }
            else
              {
                struct_type *the_struct = (struct_type *)this_type;
                name_instance the_instance;

                the_instance.the_name_type = MEMBER_NAME_TYPE;
                the_instance.data.the_member.the_struct = the_struct;

                unsigned num_fields = the_struct->num_fields();
                for (unsigned field_num = 0; field_num < num_fields;
                     ++field_num)
                  {
                    the_instance.data.the_member.number = field_num;
                    fix_a_name(&the_instance, scope);
                  }
              }
          }
      }

    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_symbol = sym_iter.step();

        name_instance the_instance;

        switch (this_symbol->kind())
          {
            case SYM_PROC:
                the_instance.the_name_type = FUNC_NAME_TYPE;
                break;
            case SYM_LABEL:
                the_instance.the_name_type = LABEL_NAME_TYPE;
                break;
            case SYM_VAR:
              {
                var_sym *the_var = (var_sym *)this_symbol;
                /* Child variables don't really show up in the C code,
                 * so ignore their names.  */
                if (the_var->parent_var() != NULL)
                    continue;
                the_instance.the_name_type = VAR_NAME_TYPE;
                break;
              }
            default:
                assert(FALSE);
          }

        the_instance.data.the_symbol = this_symbol;
        fix_a_name(&the_instance, scope);
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void fix_a_name(name_instance *the_instance, tree_block *scope)
  {
    const char *this_name = get_instance_name(the_instance);

    if (bad_c_id_name(this_name))
      {
        const char *new_base = name_table[the_instance->the_name_type].prefix;
        char *new_name = new char[strlen(new_base) + 20];
        strcpy(new_name, new_base);
        char *num_place = &(new_name[strlen(new_base)]);
        do
          {
            int number = name_table[the_instance->the_name_type].next_num;
            ++(name_table[the_instance->the_name_type].next_num);
            sprintf(num_place, "%d", number);
          } while (name_conflict_for_instance(the_instance, new_name, scope,
                                              TRUE));
        set_instance_name(the_instance, new_name);
        delete[] new_name;
        return;
      }

    if (name_conflict_for_instance(the_instance, this_name, scope, FALSE))
      {
        char *new_name = new char[strlen(this_name) + 20];
        strcpy(new_name, this_name);
        char *num_place = &(new_name[strlen(this_name)]);
        int number = 1;
        do
          {
            sprintf(num_place, "%d", number);
            ++number;
          } while (name_conflict_for_instance(the_instance, new_name, scope,
                                              TRUE));
        set_instance_name(the_instance, new_name);
        delete[] new_name;
      }
  }

static boolean name_conflict_for_instance(name_instance *the_instance,
                                          const char *name, tree_block *scope,
                                          boolean for_new)
  {
    name_space the_space = name_table[the_instance->the_name_type].space;

    switch (the_instance->the_name_type)
      {
        case STRUCT_NAME_TYPE:
        case UNION_NAME_TYPE:
        case ENUM_NAME_TYPE:
          {
            type_node *the_type = the_instance->data.the_type;
            return name_conflict_in_inner_scope(the_type->parent(), scope,
                                                the_space, name,
                                                for_new ? NULL : the_type);
          }
        case LABEL_NAME_TYPE:
        case VAR_NAME_TYPE:
        case FUNC_NAME_TYPE:
          {
            sym_node *the_symbol = the_instance->data.the_symbol;
            return name_conflict_in_inner_scope(the_symbol->parent(), scope,
                                                the_space, name,
                                                for_new ? NULL : the_symbol);
          }
        case ENUM_VAL_NAME_TYPE:
          {
            enum_type *the_enum = the_instance->data.the_enum_val.the_enum;
            unsigned this_member = the_instance->data.the_enum_val.number;

            assert(this_member < the_enum->num_values());
            unsigned top_member =
                    for_new ? the_enum->num_values() : this_member;
            for (unsigned value_num = 0; value_num < top_member; ++value_num)
              {
                if (strcmp(the_enum->member(value_num), name) == 0)
                    return TRUE;
              }

            return name_conflict_in_inner_scope(the_enum->parent(), scope,
                                                the_space, name,
                                                for_new ? NULL : the_enum);
          }
        case MEMBER_NAME_TYPE:
          {
            struct_type *the_struct = the_instance->data.the_member.the_struct;
            unsigned this_member = the_instance->data.the_member.number;

            assert(this_member < the_struct->num_fields());
            unsigned top_member =
                    for_new ? the_struct->num_fields() : this_member;
            for (unsigned field_num = 0; field_num < top_member; ++field_num)
              {
                if (strcmp(the_struct->field_name(field_num), name) == 0)
                    return TRUE;
              }

            return name_conflict_with_reserved_or_macro(name);
          }
        default:
            assert(FALSE);
            return TRUE;
      }
  }

static boolean name_conflict_in_inner_scope(base_symtab *the_symtab,
                                            tree_block *scope,
                                            name_space the_space, const char *name,
                                            suif_object *stopping_point)
  {
    if (the_symtab == NULL)
        return name_conflict_with_reserved_or_macro(name);

    boolean stopped = FALSE;

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type == stopping_point)
          {
            stopped = TRUE;
            break;
          }
        if (name_conflict_in_type(this_type, name, the_space, scope))
            return TRUE;
      }

    if (!stopped)
      {
        sym_node_list_iter sym_iter(the_symtab->symbols());
        while (!sym_iter.is_empty())
          {
            sym_node *this_symbol = sym_iter.step();
            if (this_symbol == stopping_point)
                break;
            if (name_conflict_in_symbol(this_symbol, name, the_space, scope))
                return TRUE;
          }
      }

    return name_conflict_in_inner_scope(the_symtab->parent(), scope,
                                        the_space, name, NULL);
  }

static boolean name_conflict_with_reserved_or_macro(const char *name)
  {
    for (const char **follow = reserved_words; *follow != NULL; ++follow)
      {
        if (strcmp(*follow, name) == 0)
            return TRUE;
      }

    for (int index = 0; macro_table[index].replacement != NULL; ++index)
      {
        if ((strcmp(macro_table[index].op_name, name) == 0) &&
            macro_table[index].used)
          {
            return TRUE;
          }
      }

    return FALSE;
  }

static boolean name_conflict_in_type(type_node *the_type, const char *name,
                                     name_space the_space, tree_block *scope)
  {
    assert(the_type != NULL);

    switch (the_space)
      {
        case LABEL_NAMES:
            return FALSE;
        case TAG_NAMES:
          {
            const char *this_name;
            switch (the_type->op())
              {
                case TYPE_GROUP:
                case TYPE_STRUCT:
                case TYPE_UNION:
                  {
                    struct_type *the_struct = (struct_type *)the_type;
                    this_name = the_struct->name();
                    break;
                  }
                case TYPE_ENUM:
                  {
                    enum_type *the_enum = (enum_type *)the_type;
                    this_name = the_enum->name();
                    break;
                  }
                default:
                    return FALSE;
              }

            if ((strcmp(this_name, name) == 0) &&
                ((scope == NULL) || direct_type_ref_in_scope(the_type, scope)))
              {
                return TRUE;
              }
            return FALSE;
          }
        case OTHER_NAMES:
          {
            if (the_type->op() != TYPE_ENUM)
                return FALSE;

            enum_type *the_enum = (enum_type *)the_type;
            unsigned num_members = the_enum->num_values();
            for (unsigned member_num = 0; member_num < num_members;
                 ++member_num)
              {
                if ((strcmp(the_enum->member(member_num), name) == 0) &&
                    ((scope == NULL) ||
                     enum_const_ref_in_scope(the_enum, member_num, scope)))
                  {
                    return TRUE;
                  }
              }

            return FALSE;
          }
        default:
            assert(FALSE);
            return FALSE;
      }
  }

static boolean name_conflict_in_symbol(sym_node *the_symbol, const char *name,
                                       name_space the_space,
                                       tree_block *scope)
  {
    assert(the_symbol != NULL);

    switch (the_space)
      {
        case LABEL_NAMES:
            /*
             * Note that all labels that might conflict have the same scope,
             * so our ``scope'' block is always NULL and any name match is a
             * conflict.
             */

            if (!the_symbol->is_label())
                return FALSE;

            return (strcmp(the_symbol->name(), name) == 0);

        case TAG_NAMES:
            return FALSE;

        case OTHER_NAMES:
            if (the_symbol->is_label())
                return FALSE;

            if ((strcmp(the_symbol->name(), name) == 0) &&
                ((scope == NULL) ||
                 direct_symbol_ref_in_scope(the_symbol, scope) ||
                 (scope->symtab() == the_symbol->parent())))
              {
                return TRUE;
              }
            return FALSE;

        default:
            assert(FALSE);
            return FALSE;
      }
  }

static boolean direct_type_ref_in_scope(type_node *the_type,
                                        tree_block *scope)
  {
    assert(scope != NULL);

    if (direct_type_ref_in_symtab(the_type, scope->symtab()))
        return TRUE;

    type_ref_data the_data;
    the_data.type = the_type;
    the_data.found = FALSE;
    scope->map(&direct_type_ref_on_node, &the_data);
    return the_data.found;
  }

static boolean direct_type_ref_in_symtab(type_node *the_type,
                                         base_symtab *the_symtab)
  {
    assert(the_symtab != NULL);

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (direct_type_ref_in_type(the_type, this_type))
            return TRUE;
      }

    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_symbol = sym_iter.step();
        switch (this_symbol->kind())
          {
            case SYM_PROC:
              {
                proc_sym *the_proc = (proc_sym *)this_symbol;
                if (direct_type_ref_in_type(the_type, the_proc->type()))
                    return TRUE;
                break;
              }
            case SYM_VAR:
              {
                var_sym *the_var = (var_sym *)this_symbol;
                if (direct_type_ref_in_type(the_type, the_var->type()))
                    return TRUE;
                break;
              }
            default:
                break;
          }
      }

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        if (type_ref_in_type_or_init_member(the_type,
                                            this_def->variable()->type()))
          {
            return TRUE;
          }
      }

    return FALSE;
  }

static void direct_type_ref_on_node(tree_node *the_node, void *data)
  {
    assert(data != NULL);
    assert(the_node != NULL);

    type_ref_data *ref_data = (type_ref_data *)data;
    if (ref_data->found)
        return;

    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            the_tree_instr->instr_map(&direct_type_ref_on_instr, data);
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            if (direct_type_ref_in_symtab(ref_data->type, the_block->symtab()))
                ref_data->found = TRUE;
            break;
          }
        default:
            break;
      }
  }

static void direct_type_ref_on_instr(instruction *the_instr, void *data)
  {
    assert(data != NULL);
    assert(the_instr != NULL);

    type_ref_data *ref_data = (type_ref_data *)data;
    if (ref_data->found)
        return;

    if (direct_type_ref_in_type(ref_data->type, the_instr->result_type()))
        ref_data->found = TRUE;
  }

static boolean direct_type_ref_in_type(type_node *to_ref, type_node *in_type)
  {
    if (to_ref == in_type)
        return TRUE;

    switch (in_type->op())
      {
        case TYPE_PTR:
          {
            ptr_type *the_ptr = (ptr_type *)in_type;
            return direct_type_ref_in_type(to_ref, the_ptr->ref_type());
          }
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)in_type;
            return direct_type_ref_in_type(to_ref, the_array->elem_type());
          }
        case TYPE_FUNC:
          {
            func_type *the_func = (func_type *)in_type;
            unsigned num_args = the_func->num_args();
            for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
              {
                if (direct_type_ref_in_type(to_ref,
                                            the_func->arg_type(arg_num)))
                  {
                    return TRUE;
                  }
              }
            return direct_type_ref_in_type(to_ref, the_func->return_type());
          }
        case TYPE_CONST:
        case TYPE_VOLATILE:
        case TYPE_CALL_BY_REF:
	case TYPE_RESTRICT:
        case TYPE_NULL:
          {
            modifier_type *the_modifier = (modifier_type *)in_type;
            return direct_type_ref_in_type(to_ref, the_modifier->base());
          }
        default:
            return FALSE;
      }
  }

static boolean type_ref_in_type_or_init_member(type_node *to_ref,
                                               type_node *in_type)
  {
    if (direct_type_ref_in_type(to_ref, in_type))
        return TRUE;

    switch (in_type->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)in_type;
            return type_ref_in_type_or_init_member(to_ref,
                                                   the_array->elem_type());
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)in_type;
            unsigned num_fields = the_struct->num_fields();

            if (num_fields == 0)
                return FALSE;

            if (in_type->op() == TYPE_UNION)
              {
                type_node *field_type = the_struct->field_type(0);
                return type_ref_in_type_or_init_member(to_ref, field_type);
              }
            else if (in_type->op() == TYPE_STRUCT)
              {
                for (unsigned field_num = 0; field_num < num_fields;
                     ++field_num)
                  {
                    type_node *field_type = the_struct->field_type(field_num);
                    if (type_ref_in_type_or_init_member(to_ref, field_type))
                        return TRUE;
                  }

                return FALSE;
              }
            else
              {
                assert(in_type->op() == TYPE_GROUP);
                type_node *temp_type = init_view(the_struct);
                boolean result =
                        type_ref_in_type_or_init_member(to_ref, temp_type);
                delete temp_type;
                return result;
              }
          }
        case TYPE_CONST:
        case TYPE_VOLATILE:
        case TYPE_CALL_BY_REF:
	case TYPE_RESTRICT:
        case TYPE_NULL:
          {
            modifier_type *the_modifier = (modifier_type *)in_type;
            return type_ref_in_type_or_init_member(to_ref,
                                                   the_modifier->base());
          }
        default:
            return FALSE;
      }
  }

static boolean direct_symbol_ref_in_scope(sym_node *the_symbol,
                                          tree_block *scope)
  {
    assert(scope != NULL);

    if (direct_symbol_ref_in_symtab(the_symbol, scope->symtab()))
        return TRUE;

    sym_ref_data the_data;
    the_data.symbol = the_symbol;
    the_data.found = FALSE;
    scope->map(&direct_symbol_ref_on_node, &the_data);
    return the_data.found;
  }

static boolean direct_symbol_ref_in_symtab(sym_node *the_symbol,
                                           base_symtab *the_symtab)
  {
    assert(the_symtab != NULL);

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        if (sym_ref_in_initializers(this_def->annotes(), the_symbol))
            return TRUE;
      }

    return FALSE;
  }

static void direct_symbol_ref_on_node(tree_node *the_node, void *data)
  {
    assert(data != NULL);
    assert(the_node != NULL);

    sym_ref_data *ref_data = (sym_ref_data *)data;
    if (ref_data->found)
        return;

    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            the_tree_instr->instr_map(&direct_symbol_ref_on_instr, data);
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            if (the_for->index() == ref_data->symbol)
                ref_data->found = TRUE;
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            if (direct_symbol_ref_in_symtab(ref_data->symbol,
                                            the_block->symtab()))
              {
                ref_data->found = TRUE;
              }
            break;
          }
        default:
            break;
      }
  }

static void direct_symbol_ref_on_instr(instruction *the_instr, void *data)
  {
    assert(data != NULL);
    assert(the_instr != NULL);

    sym_ref_data *ref_data = (sym_ref_data *)data;
    if (ref_data->found)
        return;

    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        if (the_ldc->value().is_symbol())
          {
            if (the_ldc->value().symbol() == ref_data->symbol)
              {
                ref_data->found = TRUE;
                return;
              }
          }
      }

    if (!ref_data->symbol->is_var())
        return;

    var_sym *the_var = (var_sym *)(ref_data->symbol);

    if (operand(the_var) == the_instr->dst_op())
      {
        ref_data->found = TRUE;
        return;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand(the_var) == the_instr->src_op(src_num))
          {
            ref_data->found = TRUE;
            return;
          }
      }
  }

static boolean enum_const_ref_in_scope(enum_type *the_enum,
                                       unsigned member_num, tree_block *scope)
  {
    assert(scope != NULL);

    if (enum_const_ref_in_symtab(the_enum, member_num, scope->symtab()))
        return TRUE;

    enum_ref_data the_data;
    the_data.the_enum = the_enum;
    the_data.member_num = member_num;
    the_data.found = FALSE;
    scope->map(&enum_const_ref_on_node, &the_data);
    return the_data.found;
  }

static boolean enum_const_ref_in_symtab(enum_type *the_enum,
                                        unsigned member_num,
                                        base_symtab *the_symtab)
  {
    assert(the_symtab != NULL);
    assert(the_enum != NULL);

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        if (enum_const_ref_in_initializers(this_def, the_enum, member_num))
            return TRUE;
      }

    return FALSE;
  }

static void enum_const_ref_on_node(tree_node *the_node, void *data)
  {
    assert(data != NULL);
    assert(the_node != NULL);

    enum_ref_data *ref_data = (enum_ref_data *)data;
    if (ref_data->found)
        return;

    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            the_tree_instr->instr_map(&enum_const_ref_on_instr, data);
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            if (enum_const_ref_in_symtab(ref_data->the_enum,
                                         ref_data->member_num,
                                         the_block->symtab()))
              {
                ref_data->found = TRUE;
              }
            break;
          }
        default:
            break;
      }
  }

static void enum_const_ref_on_instr(instruction *the_instr, void *data)
  {
    assert(data != NULL);
    assert(the_instr != NULL);

    enum_ref_data *ref_data = (enum_ref_data *)data;
    if (ref_data->found)
        return;

    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        if (the_ldc->value().is_integer() &&
            (the_ldc->result_type() == ref_data->the_enum) &&
            (the_ldc->value().integer() ==
             ref_data->the_enum->value(ref_data->member_num)))
          {
            ref_data->found = TRUE;
          }
        return;
      }
  }

static boolean bad_c_id_name(const char *name)
  {
    if (name == NULL)
        return TRUE;
    if ((!isalpha(*name)) && (*name != '_'))
        return TRUE;

    const char *follow = name;
        ++follow;
    while (*follow != 0)
      {
        if ((!isalpha(*follow)) && (*follow != '_') && (!isdigit(*follow)))
            return TRUE;
        ++follow;
      }

    return FALSE;
  }

static const char *get_instance_name(name_instance *the_instance)
  {
    switch (the_instance->the_name_type)
      {
        case STRUCT_NAME_TYPE:
        case UNION_NAME_TYPE:
          {
            type_node *the_type = the_instance->data.the_type;

            assert(the_type->is_struct());
            struct_type *the_struct = (struct_type *)the_type;

            return the_struct->name();
          }
        case ENUM_NAME_TYPE:
          {
            type_node *the_type = the_instance->data.the_type;

            assert(the_type->is_enum());
            enum_type *the_enum = (enum_type *)the_type;

            return the_enum->name();
          }
        case LABEL_NAME_TYPE:
        case VAR_NAME_TYPE:
        case FUNC_NAME_TYPE:
          {
            return the_instance->data.the_symbol->name();
          }
        case ENUM_VAL_NAME_TYPE:
          {
            enum_type *the_enum = the_instance->data.the_enum_val.the_enum;
            unsigned this_member = the_instance->data.the_enum_val.number;

            assert(this_member < the_enum->num_values());
            return the_enum->member(this_member);
          }
        case MEMBER_NAME_TYPE:
          {
            struct_type *the_struct = the_instance->data.the_member.the_struct;
            unsigned this_member = the_instance->data.the_member.number;

            assert(this_member < the_struct->num_fields());
            return the_struct->field_name(this_member);
          }
        default:
            assert(FALSE);
            return NULL;
      }
  }

static void set_instance_name(name_instance *the_instance, const char *name)
  {
    switch (the_instance->the_name_type)
      {
        case STRUCT_NAME_TYPE:
        case UNION_NAME_TYPE:
          {
            type_node *the_type = the_instance->data.the_type;

            assert(the_type->is_struct());
            struct_type *the_struct = (struct_type *)the_type;

            the_struct->set_name(name);
            break;
          }
        case ENUM_NAME_TYPE:
          {
            type_node *the_type = the_instance->data.the_type;

            assert(the_type->is_enum());
            enum_type *the_enum = (enum_type *)the_type;

            the_enum->set_name(name);
            break;
          }
        case LABEL_NAME_TYPE:
        case VAR_NAME_TYPE:
        case FUNC_NAME_TYPE:
          {
            sym_node *the_symbol = the_instance->data.the_symbol;
            if (!unreferenced_outside_fileset(the_symbol))
              {
                /* We make ``warned'' auto here, not static, so that
                 * the warning is re-issued every time this situation
                 * is encountered. */
                boolean warned = FALSE;

                mistake(&warned, the_symbol,
                        "global symbol with external linkage must have its "
                        "name changed\n           from `%s' to `%s'; this may "
                        "cause a linker failure", the_symbol->name(), name);
              }
            the_symbol->set_name(name);
            break;
          }
        case ENUM_VAL_NAME_TYPE:
          {
            enum_type *the_enum = the_instance->data.the_enum_val.the_enum;
            unsigned this_member = the_instance->data.the_enum_val.number;

            assert(this_member < the_enum->num_values());
            the_enum->set_member(this_member, name);
            break;
          }
        case MEMBER_NAME_TYPE:
          {
            struct_type *the_struct = the_instance->data.the_member.the_struct;
            unsigned this_member = the_instance->data.the_member.number;

            assert(this_member < the_struct->num_fields());

            annote *original_names_annote =
                    the_struct->annotes()->peek_annote(
                            k_s2c_original_field_names);
            if (original_names_annote == NULL)
              {
                original_names_annote = new annote(k_s2c_original_field_names);
                the_struct->annotes()->append(original_names_annote);
              }
            original_names_annote->immeds()->append(
                    the_struct->field_name(this_member));
            original_names_annote->immeds()->append(this_member);

            the_struct->set_field_name(this_member, name);

            break;
          }
        default:
            assert(FALSE);
      }
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
