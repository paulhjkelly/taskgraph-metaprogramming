/* file "init.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements the conversion of SUIF initialization annotations to
 *  ctrees for the s2c program for the SUIF system.
 */

#define RCS_BASE_FILE init_cc

#include "s2c.h"

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

class init_summary_item
  {
public:
    int offset;
    int size;
    type_ops type;

    init_summary_item(int new_offset, int new_size, type_ops new_type)
      { offset = new_offset; size = new_size; type = new_type; }
    ~init_summary_item(void) { }

    boolean operator==(const init_summary_item &other)
      {
        return ((offset == other.offset) && (size == other.size) &&
                (type == other.type));
      }
  };

DECLARE_DLIST_CLASS(init_summary_item_list, init_summary_item *);

class init_summary : public init_summary_item_list
  {
public:
    init_summary(void) { }
    ~init_summary(void)
      {
        while (!is_empty())
          {
            init_summary_item_list_e *the_elem = head();
            remove(the_elem);
            delete the_elem->contents;
            delete the_elem;
          }
      }

    void add_item(int offset, int size, type_ops type)
      {
        init_summary_item_list_e *the_elem = tail();
        while (the_elem != NULL)
          {
            if (*(the_elem->contents) == init_summary_item(offset, size, type))
                return;
            if (the_elem->contents->offset < offset)
              {
                insert_after(new init_summary_item(offset, size, type),
                             the_elem);
                return;
              }
            the_elem = the_elem->prev();
          }
        push(new init_summary_item(offset, size, type));
      }
  };

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

static const char *k_s2c_init_summary;

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void remove_initializers(int size, base_init_struct_list *initializers);
static immed get_immed_initializer(int size,
                                   base_init_struct_list *initializers);
static immed pop_unit(base_init_struct_list *initializers, int *size);
static boolean end_of_initializers(base_init_struct_list *initializers);
static void add_init_summaries(var_def *the_def, boolean global);
static void init_summary_for_type(type_node *the_type, int offset, int size,
                                  type_ops type, boolean global, boolean must);
static type_ops type_op_from_immed(immed the_immed);
static type_node *mark_type_init_field(type_node *the_type);
static boolean type_matches_op(type_node *the_type, type_ops the_op);
static void free_init_summary(void *data);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void init_initialization(void)
  {
    STRUCT_ANNOTE(k_s2c_init_summary, "s2c init summary", FALSE, NULL, NULL,
                  &free_init_summary, NULL);
  }

/*
 *  This routine returns a tree to initialize a variable of the given type.
 *  It uses as much of the initialization data from ``initializers'' as needed
 *  and leaves initializers containing the remaining initialization, if any.
 *  The initializers list must contain only initialization annotations.
 */
extern ctree *build_initializers(type_node *the_type,
                                 base_init_struct_list *initializers)
  {
    if (end_of_initializers(initializers))
        return NULL;

    if (the_type->size() == 0)
      {
        error_line(1, NULL,
                   "attempted initialization of object with unknown size");
      }

    switch (the_type->op())
      {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_PTR:
        case TYPE_ENUM:
            return ldc_tree(the_type,
                            get_immed_initializer(the_type->size(),
                                                  initializers));
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)the_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            if ((!the_array->lower_bound().is_constant()) ||
                (!the_array->upper_bound().is_constant()))
              {
                error_line(1, NULL,
                           "attempted initialization of array with unknown "
                           "bounds");
              }
            int num_elems = the_array->upper_bound().constant() -
                            the_array->lower_bound().constant() + 1;
            if (num_elems <= 0)
                error_line(1, NULL, "illegal array bounds");

            ctree *comma_tree = new ctree(ctree_comma);
            int position = 0;
            if ((elem_type->op() == TYPE_INT) &&
                (elem_type->size() == target.size[C_char]) &&
                (((base_type *)elem_type)->is_signed() ==
                 target.char_is_signed))
              {
                int *data = new int[num_elems];
                while (position < num_elems)
                  {
                    immed this_value =
                            get_immed_initializer(target.size[C_char],
                                                  initializers);
                    if ((!this_value.is_integer()) ||
                        (!expressable_as_char_const(this_value.integer())))
                      {
                        for (int index = 0; index < position; ++index)
                          {
                            comma_tree->addchild(ldc_tree(elem_type,
                                                          immed(data[index])));
                          }
                        delete[] data;
                        comma_tree->addchild(ldc_tree(elem_type, this_value));
                        break;
                      }

                    data[position] = this_value.integer();
                    ++position;
                  }

                if (position == num_elems)
                  {
                    delete comma_tree;

                    char *c_string = new char[(num_elems * 4) + 1];
                    c_string[0] = 0;
                    char *follow = c_string;
                    for (position = 0; position < num_elems; ++position)
                      {
                        /*
                         *  If there is a trigraph sequence embedded
                         *  in the string literal, we must put a
                         *  backslash before the middle question mark.
                         */
                        if ((position > 0) && (position + 1 < num_elems) &&
                            (data[position - 1] == '?') &&
                            (data[position] == '?') &&
                            ((data[position + 1] == '=') ||
                             (data[position + 1] == '(') ||
                             (data[position + 1] == '/') ||
                             (data[position + 1] == ')') ||
                             (data[position + 1] == '\'') ||
                             (data[position + 1] == '<') ||
                             (data[position + 1] == '!') ||
                             (data[position + 1] == '>') ||
                             (data[position + 1] == '-')))
                          {
                            *follow = '\\';
                            ++follow;
                            *follow = 0;
                          }
                        get_c_char(data[position], &follow, TRUE);
                      }
                    delete[] data;

                    remove_trailing_zero(c_string);
                    const char *entry = lexicon->enter(c_string)->sp;
                    delete[] c_string;

                    ctree *result = new ctree(ctree_strconst, entry);
                    return result;
                  }

                ++position;
              }

            while (position < num_elems)
              {
                ctree *elem_tree =
                        build_initializers(elem_type, initializers);
                if (elem_tree == NULL)
                    break;
                comma_tree->addchild(elem_tree);
                ++position;
              }

            if (comma_tree->N() == 0)
              {
                delete comma_tree;
                return NULL;
              }

            ctree *result = new ctree(ctree_block);
            result->addchild(comma_tree);
            return result;
          }
        case TYPE_GROUP:
          {
            struct_type *the_struct = (struct_type *)the_type;
            struct_type *temp_struct = init_view(the_struct);
            ctree *result = build_initializers(temp_struct, initializers);
            delete temp_struct;
            if (init_is_union(the_struct))
              {
                ctree *new_tree = new ctree(ctree_block);
                new_tree->addchild(result);
                result = new_tree;
              }
            return result;
          }
        case TYPE_STRUCT:
          {
            struct_type *the_struct = (struct_type *)the_type;
            if (the_struct->num_fields() == 0)
              {
                remove_initializers(the_type->size(), initializers);
                return NULL;
              }

            ctree *comma_tree = new ctree(ctree_comma);

            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type = the_struct->field_type(field_num);
                ctree *field_tree =
                        build_initializers(field_type, initializers);
                if (field_tree == NULL)
                    break;
                comma_tree->addchild(field_tree);
                int extra;
                if (field_num == num_fields - 1)
                    extra = the_type->size();
                else
                    extra = the_struct->offset(field_num + 1);
                extra -= the_struct->offset(field_num);
                extra -= field_type->size();
                remove_initializers(extra, initializers);
              }

            if (comma_tree->N() == 0)
              {
                delete comma_tree;
                return NULL;
              }

            ctree *result = new ctree(ctree_block);
            result->addchild(comma_tree);
            return result;
          }
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)the_type;
            if (the_struct->num_fields() == 0)
              {
                remove_initializers(the_type->size(), initializers);
                return NULL;
              }

            type_node *first_field = the_struct->field_type(0);
            ctree *first_tree = build_initializers(first_field, initializers);
            if (first_tree == NULL)
                return NULL;
            remove_initializers(the_type->size() - first_field->size(),
                                initializers);

            ctree *result = new ctree(ctree_block);
            result->addchild(first_tree);
            return result;
          }
	case TYPE_RESTRICT:
        case TYPE_CONST:
        case TYPE_VOLATILE:
        case TYPE_CALL_BY_REF:
        case TYPE_NULL:
            return build_initializers(the_type->unqual(), initializers);
        default:
            assert(FALSE);
            return NULL;
      }
  }

/*
 *  This function returns TRUE iff the given initializer list contains an
 *  initialization annotation that uses the given symbol.
 */
extern boolean sym_ref_in_initializers(annote_list *initializers,
                                       sym_node *the_symbol)
  {
    annote_list_iter the_iter(initializers);
    while (!the_iter.is_empty())
      {
        annote *this_annote = the_iter.step();
        if ((this_annote->name() == k_multi_init) ||
            (this_annote->name() == k_repeat_init) ||
            (this_annote->name() == k_fill))
          {
            immed_list_iter immed_iter(this_annote->immeds());
            while (!immed_iter.is_empty())
              {
                immed this_immed = immed_iter.step();
                if (this_immed.is_symbol() &&
                    (this_immed.symbol() == the_symbol))
                  {
                    return TRUE;
                  }
              }
          }
      }
    return FALSE;
  }

/*
 *  This function returns TRUE iff the given initializer list contains an
 *  initialization annotation that will generate an enumerated constant which
 *  is the member number member_num of the given enumerated type.
 */
extern boolean enum_const_ref_in_initializers(var_def *the_def,
                                              enum_type *the_enum,
                                              unsigned member_num)
  {
    /*
     *  We do this by building the entire initialization tree and then
     *  checking to see if we used the given enumerated constant.  In any
     *  case, we delete the initialization tree when we're done.  This takes a
     *  lot of time to run, but this is only run when there is a name conflict
     *  between an enumeration constant and some other identifier in an inner
     *  scope.  This is such an uncommon occurance that building an extra
     *  initialization tree in this case will have negligible impact on
     *  overall performance.  So we just put all that existing initialization
     *  code to work for this case.
     */

    watch_enum_type = the_enum;
    watch_enum_member = member_num;
    watch_enum_used = FALSE;

    base_init_struct_list *init_list = read_init_data(the_def);
    ctree *init_tree =
            build_initializers(the_def->variable()->type(), init_list);
    deallocate_init_data(init_list);
    if (init_tree != NULL)
        delete init_tree;

    watch_enum_type = NULL;
    return watch_enum_used;
  }

extern void init_summaries_for_symtab(base_symtab *the_symtab, boolean global)
  {
    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        add_init_summaries(this_def, global);
      }
  }

extern void add_and_mark_init_fields(struct_type *the_struct)
  {
    assert(the_struct->op() == TYPE_GROUP);

    sort_fields_by_offset(the_struct);

    init_summary *type_summary =
            (init_summary *)(the_struct->peek_annote(k_s2c_init_summary));
    if (type_summary == NULL)
        return;

    unsigned num_fields = the_struct->num_fields();
    unsigned new_num_fields = num_fields;
    unsigned field_num = 0;
    int next_offset = 0;
    int new_field_num = 0;
    init_summary_item_list_iter item_iter(type_summary);
    while (!item_iter.is_empty())
      {
        init_summary_item *this_item = item_iter.step();
        if (this_item->offset < next_offset)
            continue;

        boolean added = FALSE;
        while ((field_num < num_fields) &&
               (the_struct->offset(field_num) <= this_item->offset))
          {
            if ((the_struct->offset(field_num) == this_item->offset) &&
                (the_struct->size() == this_item->size) && 
                type_matches_op(the_struct->field_type(field_num),
                                this_item->type))
              {
                type_node *field_type = the_struct->field_type(field_num);
                field_type = mark_type_init_field(field_type);
                the_struct->set_field_type(field_num, field_type);
                added = TRUE;
                break;
              }
            ++field_num;
          }

        if (!added)
          {
            type_node *new_type;
            switch (this_item->type)
              {
                case TYPE_FLOAT:
                case TYPE_INT:
                    new_type = new base_type(this_item->type, this_item->size);
                    break;
                case TYPE_PTR:
                    new_type = new ptr_type(type_void);
                    break;
                default:
                    new_type = NULL;
                    assert(FALSE);
              }
            new_type = fileset->globals()->install_type(new_type);
            new_type = mark_type_init_field(new_type);

            the_struct->set_num_fields(new_num_fields + 1);

            char *new_field_name;
            string_io *the_io = new string_io(&new_field_name);
            the_io->printf("__init%d", new_field_num);
            delete the_io;
            the_struct->set_field_name(new_num_fields, new_field_name);
            delete[] new_field_name;
            ++new_field_num;

            the_struct->set_field_type(new_num_fields, new_type);
            the_struct->set_offset(new_num_fields, this_item->offset);
            ++new_num_fields;
          }

        next_offset = this_item->offset + this_item->size;
      }
  }

extern boolean is_init_field(type_node *field_type)
  {
    return (field_type->peek_annote(k_s2c_init_field) != NULL);
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

/*
 *  Remove from the beginning of initializers the ``initializers''
 *  corresponding to the first ``size'' bits.  The ``initializers'' list must
 *  contain only initialization annotations.
 */
static void remove_initializers(int size, base_init_struct_list *initializers)
  {
    if (size <= 0)
        return;

    int extra = size;
    while (!initializers->is_empty())
      {
        int part_size;
        (void)pop_unit(initializers, &part_size);
        if (part_size > extra)
            error_line(1, NULL, "misalignment of initialization data");
        if (part_size >= extra)
            return;
        extra -= part_size;
      }
  }

/*
 *  Extract the first ``size'' bits of initialization as a single immed.
 */
static immed get_immed_initializer(int size,
                                   base_init_struct_list *initializers)
  {
    if (initializers->is_empty())
        return immed(0);

    int chunk_size;
    immed chunk_immed = pop_unit(initializers, &chunk_size);

    if (chunk_size == size)
        return chunk_immed;

    // PGI: Required to support initialization of global pointer variables
    //      when pointer size different from integer size
    if ((chunk_size < size) && chunk_immed.is_symbol()) {
	return chunk_immed;
    }


    if ((chunk_size < size) && chunk_immed.is_integer() &&
        (chunk_immed.integer() == 0))
      {
        chunk_immed = get_immed_initializer(size - chunk_size, initializers);
        if (chunk_immed.is_integer() && (chunk_immed.integer() == 0))
            return immed(0);
      }

    error_line(1, NULL, "misalignment of initialization data");
    return immed(0);
  }

/*
 *  Remove the smallest piece of initialization from the beginning of the
 *  initialization data.  ``*size'' is set to the size in bits of the unit and
 *  the return value contains the data.
 */
static immed pop_unit(base_init_struct_list *initializers, int *size)
  {
    assert(!initializers->is_empty());
    base_init_struct *first_init_struct = initializers->pop();

    if (first_init_struct->the_multi_init() != NULL)
      {
        multi_init_struct *the_multi_init =
                first_init_struct->the_multi_init();

        if (the_multi_init->data->is_empty())
          {
            delete the_multi_init->data;
            delete the_multi_init;
            return pop_unit(initializers, size);
          }

        *size = the_multi_init->size;
        immed result = the_multi_init->data->pop();
        if (the_multi_init->data->is_empty())
          {
            delete the_multi_init->data;
            delete the_multi_init;
          }
        else
          {
            initializers->push(the_multi_init);
          }

        return result;
      }
    else if (first_init_struct->the_repeat_init() != NULL)
      {
        repeat_init_struct *the_repeat_init =
                first_init_struct->the_repeat_init();

        if (the_repeat_init->repetitions == 0)
          {
            delete the_repeat_init;
            return pop_unit(initializers, size);
          }

        if ((the_repeat_init->repetitions < 0) || (the_repeat_init->size <= 0))
            error_line(1, NULL, "bad format for \"%s\" annote", k_repeat_init);

        --the_repeat_init->repetitions;
        *size = the_repeat_init->size;
        immed result = the_repeat_init->data;

        if (the_repeat_init->repetitions > 0)
            initializers->push(the_repeat_init);
        else
            delete the_repeat_init;

        return result;
      }
    else if (first_init_struct->the_fill_init() != NULL)
      {
        fill_init_struct *the_fill_init = first_init_struct->the_fill_init();

        if ((the_fill_init->data != 0) || (the_fill_init->size < 0) ||
            ((the_fill_init->size % target.addressable_size) != 0))
          {
            error_line(1, NULL, "bad format for \"%s\" annote", k_fill);
          }

        if (the_fill_init->size == 0)
          {
            delete the_fill_init;
            return pop_unit(initializers, size);
          }

        the_fill_init->size -= target.addressable_size;
        if (the_fill_init->size > 0)
            initializers->push(the_fill_init);
        else
            delete the_fill_init;

        *size = target.addressable_size;
        return immed(0);
      }
    else
      {
        assert(FALSE);
        return immed(0);
      }
  }

/*
 *  This fucntion returns TRUE iff no remaining initializers are
 *  needed.  That is, it returns TRUE if all remaining initializers
 *  give all zero values, so even if no explicit initializations are
 *  given, the rules for C would give the same initialization values
 *  (ANSI/ISO 9899-1990, section 6.5.7, Semantics, paragraph three).
 */
static boolean end_of_initializers(base_init_struct_list *initializers)
  {
    base_init_struct_list_iter init_iter(initializers);
    while (!init_iter.is_empty())
      {
        if (explicit_zero_init)
            return FALSE;
        base_init_struct *this_init_struct = init_iter.step();
        if (this_init_struct->the_multi_init() != NULL)
          {
            multi_init_struct *the_multi_init =
                    this_init_struct->the_multi_init();

            immed_list_iter data_iter(the_multi_init->data);
            while (!data_iter.is_empty())
              {
                immed this_value = data_iter.step();
                switch (this_value.kind())
                  {
                    case im_int:
                    case im_extended_int:
                        if (this_value != immed(0))
                            return FALSE;
                        break;
                    case im_float:
                    case im_extended_float:
                        if (this_value != immed(0.0))
                            return FALSE;
                        break;
                    default:
                        return FALSE;
                  }
              }
          }
        else if (this_init_struct->the_repeat_init() != NULL)
          {
            repeat_init_struct *the_repeat_init =
                    this_init_struct->the_repeat_init();

            immed this_value = the_repeat_init->data;
            switch (this_value.kind())
              {
                case im_int:
                case im_extended_int:
                    if (this_value != immed(0))
                        return FALSE;
                    break;
                case im_float:
                case im_extended_float:
                    if (this_value != immed(0.0))
                        return FALSE;
                    break;
                default:
                    return FALSE;
              }
          }
        else if (this_init_struct->the_fill_init() != NULL)
          {
            continue;
          }
        else
          {
            assert(FALSE);
            return FALSE;
          }
      }
    return TRUE;
  }

static void add_init_summaries(var_def *the_def, boolean global)
  {
    init_summary *var_summary = NULL;
    base_init_struct_list *init_data = read_init_data(the_def);

    int offset = 0;
    base_init_struct_list_iter data_iter(init_data);
    while (!data_iter.is_empty())
      {
        base_init_struct *this_data = data_iter.step();
        if (this_data->the_multi_init() != NULL)
          {
            int size = this_data->the_multi_init()->size;
            immed_list_iter immed_iter(this_data->the_multi_init()->data);
            while (!immed_iter.is_empty())
              {
                immed this_value = immed_iter.step();
                type_ops this_type = type_op_from_immed(this_value);

                if (global == the_def->variable()->is_global())
                  {
                    if (var_summary == NULL)
                        var_summary = new init_summary;
                    var_summary->add_item(offset, size, this_type);
                  }

                init_summary_for_type(the_def->variable()->type(), offset,
                                      size, this_type, global, TRUE);

                offset += size;
              }
          }
        else if (this_data->the_repeat_init() != NULL)
          {
            int size = this_data->the_repeat_init()->size;
            type_ops this_type =
                    type_op_from_immed(this_data->the_repeat_init()->data);
            int repetitions = this_data->the_repeat_init()->repetitions;
            for (int index = 0; index < repetitions; ++index)
              {
                if (global == the_def->variable()->is_global())
                  {
                    if (var_summary == NULL)
                        var_summary = new init_summary;
                    var_summary->add_item(offset + (index * size), size,
                                          this_type);
                  }

                init_summary_for_type(the_def->variable()->type(),
                                      offset + (index * size), size, this_type,
                                      global, TRUE);
              }
            offset += repetitions * size;
          }
        else if (this_data->the_fill_init() != NULL)
          {
            offset += this_data->total_size();
          }
        else
          {
            assert(FALSE);
          }
      }
    deallocate_init_data(init_data);

    if (var_summary != NULL)
        the_def->variable()->append_annote(k_s2c_init_summary, var_summary);
  }

static void init_summary_for_type(type_node *the_type, int offset, int size,
                                  type_ops type, boolean global, boolean must)
  {
    type_node *unqualed = the_type->unqual();

    if ((!global) && unqualed->parent()->is_global())
        return;

    switch (unqualed->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)unqualed;
            int elem_size = the_array->elem_type()->size();
            if (elem_size == 0)
                return;
            init_summary_for_type(the_array->elem_type(), offset % elem_size,
                                  size, type, global, must);
            break;
          }
        case TYPE_GROUP:
          {
            if (global == unqualed->parent()->is_global())
              {
                init_summary *type_summary =
                        (init_summary *)
                                (unqualed->peek_annote(k_s2c_init_summary));
                boolean append_needed = FALSE;
                if (type_summary == NULL)
                  {
                    type_summary = new init_summary;
                    append_needed = TRUE;
                  }
                type_summary->add_item(offset, size, type);
                if (append_needed)
                    unqualed->append_annote(k_s2c_init_summary, type_summary);
              }
            /* fall through */
          }
        case TYPE_STRUCT:
          {
            boolean new_must;
            if (unqualed->op() == TYPE_STRUCT)
                new_must = must;
            else
                new_must = FALSE;
            struct_type *the_struct = (struct_type *)unqualed;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *this_type = the_struct->field_type(field_num);
                int this_offset = the_struct->offset(field_num);
                if ((offset + size > this_offset) &&
                    (offset < this_offset + this_type->size()))
                  {
                    init_summary_for_type(this_type, offset - this_offset,
                                          size, type, global, new_must);
                  }
              }
            break;
          }
        case TYPE_UNION:
          {
            struct_type *the_union = (struct_type *)unqualed;
            if (the_union->num_fields() > 0)
            init_summary_for_type(the_union->field_type(0), offset, size, type,
                                  global, must);
            break;
          }
        default:
            break;
      }
  }

static type_ops type_op_from_immed(immed the_immed)
  {
    switch (the_immed.kind())
      {
        case im_float:
        case im_extended_float:
            return TYPE_FLOAT;
        case im_symbol:
            return TYPE_PTR;
        case im_int:
        case im_extended_int:
        default:
            return TYPE_INT;
      }
  }

static type_node *mark_type_init_field(type_node *the_type)
  {
    type_node *new_type = new modifier_type(TYPE_NULL, the_type);
    new_type->append_annote(k_s2c_init_field, new immed_list);
    return the_type->parent()->install_type(new_type);
  }

static boolean type_matches_op(type_node *the_type, type_ops the_op)
  {
    switch (the_type->unqual()->op())
      {
        case TYPE_INT:
        case TYPE_PTR:
        case TYPE_ENUM:
            return ((the_op == TYPE_PTR) || (the_op == TYPE_INT));
        case TYPE_FLOAT:
            return (the_op == TYPE_FLOAT);
        default:
            return FALSE;
      }
  }

static void free_init_summary(void *data)
  {
    delete (init_summary *)data;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
