/* file "util.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#include <cstdlib>
#include <cstring>
#include "s2c.h"
#include <cstdarg>

class auto_string;

static void make_c_type_helper(type_node *tn, auto_string *basetype,
                               auto_string *pretype, auto_string *posttype);
static void append_type_comments(type_node *the_type, auto_string *the_string,
                                 int prefix_space, int suffix_space);
static void check_function_type(func_type *the_type);

class auto_string
  {
  private:
    char *the_string;
    int space;

    void make_space(int new_size);

  public:
    auto_string(void)                 { space = 10;
                                        the_string = new char[10];
                                        the_string[0] = 0; }
    auto_string(const char *initial_string)
                                      { space = strlen(initial_string) + 1;
                                        the_string = new char[space];
                                        strcpy(the_string, initial_string); }

    ~auto_string()                    { delete[] the_string; }

    char *value(void)                 { return the_string; }
    void set(const char *new_string);
    void append(const char *new_string);
    void prepend(const char *new_string);
    void append_int(int new_int);
  };

struct format_unit
  {
    int num_args;
    const char *format_string;

    format_unit &operator=(const format_unit &other)
      {
        num_args = other.num_args;
        format_string = other.format_string;
        return *this;
      }
    boolean operator==(const format_unit &other) const
      {
        return ((num_args == other.num_args) &&
                (format_string == other.format_string));
      }
  };

DECLARE_DLIST_CLASS(format_unit_list, format_unit);

struct format_record
  {
    const char *op_name;
    const char *default_format;
    format_unit_list *unit_list;

    format_record &operator=(const format_record &other)
      {
        op_name = other.op_name;
        default_format = other.default_format;
        unit_list = other.unit_list;
        return *this;
      }
    boolean operator==(const format_record &other) const
      {
        return ((op_name == other.op_name) &&
                (default_format == other.default_format) &&
                (unit_list == other.unit_list));
      }
  };

DECLARE_DLIST_CLASS(format_record_list, format_record);


/*
 *  The following is a pointer to a format_record_list instead of just
 *  being the list as a work-around for a common bug in C++
 *  implementations: global constructors don't always get called
 *  correctly.  This is a problem in the current g++ (2.5.8) for SGI's
 *  IRIX 5.2 as well as the current g++ for Solaris running on SPARCs.
 */
format_record_list *record_list = NULL;


void auto_string::make_space(int new_size)
  {
    if (new_size <= space)
        return;

    char *new_string = new char[new_size];
    strcpy(new_string, the_string);
    delete [] the_string;
    the_string = new_string;
    space = new_size;
  }

void auto_string::set(const char *new_string)
  {
    int new_size = strlen(new_string) + 1;
    if (space < new_size)
        make_space(new_size);

    strcpy(the_string, new_string);
  }

void auto_string::append(const char *new_string)
  {
    int new_size = strlen(new_string) + strlen(the_string) + 1;
    if (space < new_size)
        make_space(new_size);

    strcat(the_string, new_string);
  }

void auto_string::prepend(const char *new_string)
  {
    int old_size = strlen(the_string);
    int new_size = strlen(new_string) + old_size + 1;
    if (space < new_size)
        make_space(new_size);

    char *temp_string = new char[old_size + 1];
    strcpy(temp_string, the_string);
    strcpy(the_string, new_string);
    strcat(the_string, temp_string);
    delete [] temp_string;
  }

void auto_string::append_int(int new_int)
  {
    static char num_buffer[20];
    sprintf(num_buffer, "%d", new_int);
    append(num_buffer);
  }

extern boolean expressable_as_char_const(i_integer c)
  {
    return ((c >= 0) && (c < 0777));
  }

extern void get_c_char(int c, char **p, boolean is_string)
{
    assert(expressable_as_char_const(c));

    if (limit_escape_sequences)
      {
        switch (c)
          {
            case '\a':
            case '\b':
            case '\f':
            case '\r':
            case '\t':
            case '\v':
                sprintf(*p, "\\%03o", (unsigned)c);
                (*p) += 4;
                **p = '\0';
                return;
            default:
                break;
          }
      }

    switch (c) {
    case '\a':
        strcpy(*p, "\\a");
        (*p) += 2;
        break;
    case '\b':
        strcpy(*p, "\\b");
        (*p) += 2;
        break;
    case '\f':
        strcpy(*p, "\\f");
        (*p) += 2;
        break;
    case '\n':
        strcpy(*p, "\\n");
        (*p) += 2;
        break;
    case '\r':
        strcpy(*p, "\\r");
        (*p) += 2;
        break;
    case '\t':
        strcpy(*p, "\\t");
        (*p) += 2;
        break;
    case '\v':
        strcpy(*p, "\\v");
        (*p) += 2;
        break;
    case '\'':
        if (is_string)
          {
            **p = c;
            (*p)++;
          }
        else
          {
            strcpy(*p, "\\\'");
            (*p) += 2;
          }
        break;
    case '"':
        if (is_string)
          {
            strcpy(*p, "\\\"");
            (*p) += 2;
          }
        else
          {
            **p = c;
            (*p)++;
          }
        break;
    case '\\':
        strcpy(*p, "\\\\");
        (*p) += 2;
        break;
    default:
        if (c >= 32 && c < 127) {
            **p = c;
            (*p)++;
        } else  {
            sprintf(*p, "\\%03o", (unsigned)c);
            (*p) += 4;
        }
        break;
    }
    **p = '\0';
}

void print_c_char(io_class *out, i_integer c)
{
    char bfr[16];
    char *ptr = bfr;

    assert(c.is_c_int());
    get_c_char(c.c_int(), &ptr, FALSE);
    out->printf("%s", bfr);
}

extern void remove_trailing_zero(char *string)
  {
    char *new_end = string;
    char *follow = new_end;
    while (*follow != 0)
      {
        if (*follow == '\\')
          {
            if ((follow[1] == '0') && (follow[2] == '0') && (follow[3] == '0'))
              {
                follow += 4;
              }
            else
              {
                follow += 2;
                new_end = follow;
              }
          }
        else
          {
            ++follow;
            new_end = follow;
          }
      }
    if (new_end != follow)
        *(follow - 4) = 0;
  }

extern void comment_operand(operand the_op)
  {
    if (the_op.is_instr())
        comment_instr(the_op.instr());
  }

extern void comment_instr(instruction *the_instr)
  {
    comment_object(the_instr);

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
        comment_operand(the_instr->src_op(src_num));
  }

/*
 * Convert a type to a nice C type-decl string with the variable
 * name in the right place.
 */

static void make_c_type_helper(type_node *tn, auto_string *basetype,
                               auto_string *pretype, auto_string *posttype)
{
    switch (tn->op()) {
    case TYPE_CONST:
    case TYPE_RESTRICT:
    case TYPE_VOLATILE:
    case TYPE_CALL_BY_REF:
    case TYPE_NULL: {
        auto_string qualification_string;

        if (tn->is_const())
            qualification_string.append("const ");
        if (tn->is_volatile())
            qualification_string.append("volatile ");
	if (tn->is_restrict())
	    qualification_string.append("restrict ");

        type_node *tn_unqual = tn;
        while (tn_unqual->is_modifier())
          {
            modifier_type *this_modifier = (modifier_type *)tn_unqual;
            append_type_comments(this_modifier, &qualification_string, 0, 1);
            tn_unqual = this_modifier->base();
          }

        type_ops tn_op = tn_unqual->op();
        switch (tn_op)
          {
            case TYPE_INT:
            case TYPE_FLOAT:
            case TYPE_VOID:
            case TYPE_ENUM:
            case TYPE_GROUP:
            case TYPE_STRUCT:
            case TYPE_UNION:
            case TYPE_ARRAY:
                /* Note for array types: though C doesn't allow
                 * qualified array types, any qualifications on the
                 * array type specification instead apply to the
                 * element type.  This is a reasonable thing to do for
                 * types that are qualified array types in SUIF. */
                basetype->append(qualification_string.value());
                break;
            case TYPE_PTR:
                pretype->prepend(qualification_string.value());
                break;
            case TYPE_FUNC:
              {
                /* Only const or volatile qualifications are not
                 * allowed -- other qualifications aren't part of C,
                 * so they don't matter */
                if (tn->is_const() || tn->is_volatile())
                  {
                    static boolean array_warn = FALSE;
                    static boolean func_warn = FALSE;

                    boolean *warning =
                            (tn_op == TYPE_ARRAY) ? &array_warn : &func_warn;
                    mistake(warning, tn,
                            "qualified %s type cannot be represented in C",
                            (tn_op == TYPE_ARRAY) ? "array" : "function");
                    basetype->append(qualification_string.value());
                  }
                break;
              }
            default:
                assert(FALSE);
          }

        make_c_type_helper(tn_unqual, basetype, pretype, posttype);
        return;
    }
    case TYPE_INT: {
        base_type *btn = (base_type *) tn;

        enum C_types the_c_type = c_int_type(tn);
        if (the_c_type == C_char)
          {
            if (target.char_is_signed && !btn->is_signed())
                basetype->append("unsigned ");
            if ((!target.char_is_signed) && btn->is_signed())
                basetype->append("signed ");

            basetype->append("char");
            append_type_comments(tn, basetype, 1, 0);
            return;
          }

        if (!btn->is_signed())
            basetype->append("unsigned ");

        boolean use_intnum = FALSE;

        if (the_c_type == C_int)
          {
            basetype->append("int");
          }
        else if (the_c_type == C_short)
          {
            basetype->append("short");
          }
        else if (the_c_type == C_long)
          {
            basetype->append("long");
          }
        else if (the_c_type == C_longlong)
          {
            // if (!no_warn)
            //     warning_line(NULL, "non-ANSI ``long long int'' type used");
            basetype->append("long long");
          }
        else
          {
            static boolean warned = FALSE;

            mistake(&warned, tn,
                    "integer type with size that does not match any target "
                    "integer types");
            use_intnum = TRUE;
          }

        if (use_intnum)
          {
            basetype->append("int");
            basetype->append_int(tn->size());
          }
        append_type_comments(tn, basetype, 1, 0);
        return;
    }
    case TYPE_FLOAT: {
        enum C_types the_c_type = c_float_type(tn);
        if (the_c_type == C_float)
          {
            basetype->append("float");
          }
        else if (the_c_type == C_double)
          {
            basetype->append("double");
          }
        else if (the_c_type == C_longdouble)
          {
            basetype->append("long double");
          }
        else
          {
            static boolean warned = FALSE;

            mistake(&warned, tn,
                    "floating point type with size that does not match any "
                    "target floating point types");

            basetype->append("float");
            basetype->append_int(tn->size());
          }

        append_type_comments(tn, basetype, 1, 0);
        return;
    }
    case TYPE_VOID:
        basetype->append("void");
        append_type_comments(tn, basetype, 1, 0);
        return;
    case TYPE_PTR: {
        ptr_type *ptn = (ptr_type *)tn;
        pretype->prepend("*");
        append_type_comments(tn, pretype, 1, 1);
        make_c_type_helper(ptn->ref_type(), basetype, pretype, posttype);
        return;
    }
    case TYPE_ARRAY: {
        array_type *atn = (array_type *) tn;

        if ((pretype->value()[0] != 0) || (posttype->value()[0] != 0))
          {
            pretype->prepend("(");
            posttype->append(")");
          }

        posttype->append("[");
        if (atn->size() != 0)
            posttype->append_int(atn->size() / atn->elem_type()->size());
        posttype->append("]");

        /*
         * From Fortran we can get types which are arrays of arrays where the
         * size of the second-level array is not known at compile time.  In
         * terms of C, we want to consider such things a single flat array of
         * unknown size of the ultimate element type with known size.
         */
        type_node *elem_type = atn->elem_type();
        while (elem_type->unqual()->is_array() && (elem_type->size() == 0))
          {
            array_type *elem_array = (array_type *)(elem_type->unqual());
            elem_type = elem_array->elem_type();
          }

        append_type_comments(tn, posttype, 1, 0);
        make_c_type_helper(elem_type, basetype, pretype, posttype);
        return;
    }
    case TYPE_ENUM: {
        enum_type *etn = (enum_type *) tn;
        basetype->append("enum ");
        basetype->append(etn->name());
        append_type_comments(tn, basetype, 1, 0);
        return;
    }
    case TYPE_GROUP:
    case TYPE_STRUCT:
    case TYPE_UNION: {
        struct_type *stn = (struct_type *) tn;
        basetype->append(struct_is_union(stn) ? (char*)"union " : (char*)"struct ");
        basetype->append(stn->name());
        append_type_comments(tn, basetype, 1, 0);
        return;
    }
    case TYPE_FUNC: {
        func_type *ftn = (func_type *) tn;

        if ((pretype->value()[0] != 0) || (posttype->value()[0] != 0))
          {
            pretype->prepend("(");
            posttype->append(")");
          }

        posttype->append("(");
        if (ftn->args_known())
          {
            unsigned num_args = ftn->num_args();
            if (num_args == 0)
              {
                if (ftn->has_varargs())
                  {
                    static boolean warned = FALSE;
                    mistake(&warned, ftn,
                            "function with no specified arguments may not "
                            "have varargs");
                    posttype->append("...");
                  }
                else
                  {
                    posttype->append("void");
                  }
              }
            else
              {
                for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                  {
                    if (arg_num != 0)
                        posttype->append(", ");

                    auto_string *arg_basetype = new auto_string;
                    auto_string *arg_pretype = new auto_string;
                    auto_string *arg_posttype = new auto_string;

                    make_c_type_helper(ftn->arg_type(arg_num), arg_basetype,
                                       arg_pretype, arg_posttype);

                    posttype->append(arg_basetype->value());
                    if ((arg_pretype->value()[0] != 0) ||
                        (arg_posttype->value()[0] != 0))
                      {
                        posttype->append(" ");
                        posttype->append(arg_pretype->value());
                        posttype->append(arg_posttype->value());
                      }

                    delete arg_basetype;
                    delete arg_pretype;
                    delete arg_posttype;
                  }
                if (ftn->has_varargs())
                    posttype->append(", ...");
              }
          }
        posttype->append(")");

        check_function_type(ftn);
        append_type_comments(tn, posttype, 1, 0);
        make_c_type_helper(ftn->return_type(), basetype, pretype, posttype);
        return;
    }
    default:
        assert_msg(FALSE, ("make_c_type_helper - unknown type"));
        return;
    }
}


static void append_type_comments(type_node *the_type, auto_string *the_string,
                                 int prefix_space, int suffix_space)
  {
    immed_list *comment_list =
            (immed_list *)(the_type->peek_annote(k_s2c_comments));
    if ((comment_list == NULL) || (comment_list->is_empty()))
        return;

    int space_count;
    for (space_count = 0; space_count < prefix_space; ++space_count)
        the_string->append(" ");

    immed_list_iter comment_iter(comment_list);
    while (!comment_iter.is_empty())
      {
        immed this_immed = comment_iter.step();
        if (!this_immed.is_string())
          {
            error_line(1, NULL, "non-string in \"%s\" annotation",
                       k_s2c_comments);
          }
        the_string->append("/*");
        the_string->append(this_immed.string());
        the_string->append("*/");
        if (!comment_iter.is_empty())
            the_string->append(" ");
      }

    for (space_count = 0; space_count < suffix_space; ++space_count)
        the_string->append(" ");
  }


static auto_string ct_bfr, ct_basetype, ct_pretype, ct_posttype;

const char *make_c_type(type_node *tn)
{
    ct_basetype.set("");
    ct_pretype.set("");
    ct_posttype.set("");

    make_c_type_helper(tn, &ct_basetype, &ct_pretype, &ct_posttype);
    ct_bfr.set(ct_basetype.value());
    if ((ct_pretype.value()[0] != 0) || (ct_posttype.value()[0] != 0))
      {
        ct_bfr.append(" ");
        ct_bfr.append(ct_pretype.value());
        ct_bfr.append(ct_posttype.value());
      }
    return ct_bfr.value();
}

const char *make_c_sym_type(var_sym *sym)
{
    ct_bfr.set("");
    ct_basetype.set("");
    ct_pretype.set("");
    ct_posttype.set("");

    make_c_type_helper(sym->type(), &ct_basetype, &ct_pretype, &ct_posttype);
    ct_bfr.append(ct_basetype.value());
    ct_bfr.append(" ");
    ct_bfr.append(ct_pretype.value());
    ct_bfr.append(sym->name());
    ct_bfr.append(ct_posttype.value());

    return ct_bfr.value();
}

const char *make_c_proc_type(proc_sym *sym, const char **posttype)
{
    ct_bfr.set("");
    ct_basetype.set("");
    ct_pretype.set("");
    ct_posttype.set("");

    if (sym->is_private())
        ct_bfr.append("static ");
    else if (c_style.extern_on_func_decl)
        ct_bfr.append("extern ");

    check_function_type(sym->type());
    make_c_type_helper(sym->type()->return_type(), &ct_basetype, &ct_pretype,
                       &ct_posttype);
    ct_bfr.append(ct_basetype.value());
    ct_bfr.append(" ");
    ct_bfr.append(ct_pretype.value());
    ct_bfr.append(sym->name());
    *posttype = ct_posttype.value();
    return ct_bfr.value();
}

extern const char *make_c_proc_prototype(proc_sym *psym)
  {
    ct_bfr.set("");
    ct_basetype.set("");
    ct_pretype.set("");
    ct_posttype.set("");

    if (psym->is_private())
        ct_bfr.append("static ");
    else if (c_style.extern_on_func_decl)
        ct_bfr.append("extern ");

    make_c_type_helper(psym->type(), &ct_basetype, &ct_pretype, &ct_posttype);
    ct_bfr.append(ct_basetype.value());
    ct_bfr.append(" ");
    ct_bfr.append(ct_pretype.value());
    ct_bfr.append(psym->name());
    ct_bfr.append(ct_posttype.value());

    return ct_bfr.value();
  }


const char *make_c_agg_type(type_node *t, const char *nm)
{
    ct_bfr.set("");
    ct_basetype.set("");
    ct_pretype.set("");
    ct_posttype.set("");

    make_c_type_helper(t, &ct_basetype, &ct_pretype, &ct_posttype);
    ct_bfr.append(ct_basetype.value());
    ct_bfr.append(" ");
    ct_bfr.append(ct_pretype.value());
    ct_bfr.append(nm);
    ct_bfr.append(ct_posttype.value());

    return ct_bfr.value();
}

extern void print_float(io_class *out, immed float_value)
  {
    /*
     *  When printing a floating point constant, we want to use a compact
     *  notation so it's easy to read (i.e. use exponential form iff it is
     *  really needed).  The printf() "%g" option would be perfect except
     *  that if everything after the decimal point is zero it elinates the
     *  decimal point.  So it prints 5.0 as just 5.  That turns the
     *  floating point constant into an integer constant when it is parsed
     *  back in as C code.  So we need to explicitly add the ``.0'' in
     *  this case.
     */
    static char buffer[sizeof(double) * 10];
    static char format[100];

    const char *string = NULL;
    if (float_value.is_flt())
      {
        sprintf(format, "%%.%lulg", (unsigned long)(sizeof(double) * 3));
        sprintf(buffer, format, float_value.flt());
        string = buffer;
      }
    else if (float_value.is_ext_flt())
      {
        string = float_value.ext_flt();
      }
    else
      {
        assert(FALSE);
      }

    out->printf("%s", string);
    if ((strchr(string, '.') == NULL) && (strchr(string, 'e') == NULL) &&
        (strchr(string, 'E') == NULL))
      {
        out->printf(".0");
      }

    return;
  }

extern boolean array_flattening_needed(in_array *the_array,
                                       array_type *base_array_type)
  {
    if (!the_array->offset_op().is_null())
      {
        immed result;
        eval_status status =
                evaluate_const_expr(the_array->offset_op(), &result);
        if ((status != EVAL_OK) || (result != immed(0)))
            return TRUE;
      }

    type_node *current_type = base_array_type;
    unsigned num_dims = the_array->dims();
    for (unsigned dim_num = 0; dim_num < num_dims; ++dim_num)
      {
        if (!current_type->unqual()->is_array())
          {
            error_line(1, the_array->parent(),
                       "array type mismatches array reference instruction");
          }
        array_type *this_array = (array_type *)(current_type->unqual());
        if (dim_num == 0)
          {
            current_type = this_array->elem_type();
            continue;
          }

        if ((!this_array->lower_bound().is_constant()) ||
            (!this_array->upper_bound().is_constant()))
          {
            return TRUE;
          }
        operand this_bound = the_array->bound(dim_num);
        if (!this_bound.is_expr())
            return TRUE;
        if (this_bound.instr()->opcode() != io_ldc)
            return TRUE;
        in_ldc *this_ldc = (in_ldc *)(this_bound.instr());
        if (!this_ldc->value().is_integer())
            return TRUE;
        int int_bound = this_ldc->value().integer();
        if ((this_array->lower_bound().constant() != 0) ||
            (this_array->upper_bound().constant() != int_bound - 1))
          {
            return TRUE;
          }
        current_type = this_array->elem_type();
        if (current_type->size() == 0)
            return TRUE;
      }

    if (((unsigned)current_type->size()) != the_array->elem_size())
        return TRUE;

    return FALSE;
  }

/*
 * This function makes sure a ctree gives a particular type by putting in a
 * convert only when necessary (really, it always puts in a convert but marks
 * it as removable and later it is optimized away if not needed).
 */
extern ctree *force_type(ctree *the_ctree, type_node *the_type)
  {
    ctree *conv_tree = new ctree(ctree_conv, TRUE, the_type);
    conv_tree->addchild(the_ctree);
    return conv_tree;
  }

extern void mistake(boolean *warned_var, suif_object *location, const char *message,
                    ...)
  {
    va_list ap;

    va_start(ap, message);
    if (write_pseudo)
      {
        if ((!(*warned_var)) && !no_warn)
          {
            vwarning_line(location, message, ap);
            *warned_var = TRUE;
          }
      }
    else
      {
        verror_line(1, location, message, ap);
      }
    va_end(ap);
  }

extern immed comment_for_annote(annote *the_annote)
  {
    static char buffer[100];

    auto_string comment_string(" ");

    char *new_string;
    string_io *the_io = new string_io(&new_string);

    the_io->printf("%s", the_annote->name());

    immed_list *data = the_annote->immeds();
    if (!data->is_empty())
      {
        the_io->printf(":");
        immed_list_iter data_iter(data);
        while (!data_iter.is_empty())
          {
            the_io->printf(" ");
            immed this_immed = data_iter.step();
            switch (this_immed.kind())
              {
                case im_int:
                    the_io->printf("%d", this_immed.integer());
                    break;
                case im_extended_int:
                    the_io->printf("%s", this_immed.ext_integer());
                    break;
                case im_string:
                    the_io->printf("\"%s\"", this_immed.string());
                    break;
                case im_float:
                    the_io->printf("%g", this_immed.flt());
                    break;
                case im_extended_float:
                    the_io->printf("%s", this_immed.ext_flt());
                    break;
                case im_symbol:
                    if (this_immed.offset() != 0)
                        the_io->printf("<");
                    the_io->printf("%s", this_immed.symbol()->name());
                    if (this_immed.offset() != 0)
                        the_io->printf(",%d>", this_immed.offset());
                    break;
                case im_type:
                    the_io->printf("%s", make_c_type(this_immed.type()));
                    break;
                case im_op:
                case im_instr:
                  {
                    boolean old_in_comment = in_comment;
                    in_comment = TRUE;

                    ctree *the_ctree;
                    if (this_immed.is_op())
                      {
                        comment_operand(this_immed.op());
                        the_ctree = operand_to_tree(this_immed.op());
                      }
                    else
                      {
                        comment_instr(this_immed.instr());
                        the_ctree = process_solo_instr(this_immed.instr());
                      }
                    transform_and_print_ctree(the_io, the_ctree);
                    delete the_ctree;

                    in_comment = old_in_comment;

                    break;
                  }
                case im_undef:
                    the_io->printf("?");
                    break;
                default:
                    assert(FALSE);
              }
          }
      }

    delete the_io;
    comment_string.append(new_string);
    delete[] new_string;

    comment_string.append(" ");

    auto_string filtered_string;
    char *follow = comment_string.value();
    boolean was_star = FALSE;
    buffer[1] = 0;
    while (*follow != 0)
      {
        if (was_star && (*follow == '/'))
            filtered_string.append(" ");
        buffer[0] = *follow;
        filtered_string.append(buffer);
        if (*follow == '*')
            was_star = TRUE;
        else
            was_star = FALSE;
        ++follow;
      }

    return immed(filtered_string.value());
  }

extern const char *lookup_gen_op(const char *op_name, int num_args)
  {
    if (record_list == NULL)
        record_list = new format_record_list;

    format_record_list_iter record_iter(record_list);
    while (!record_iter.is_empty())
      {
        format_record this_record = record_iter.step();
        if (strcmp(this_record.op_name, op_name) == 0)
          {
            format_unit_list_iter unit_iter(this_record.unit_list);
            while (!unit_iter.is_empty())
              {
                format_unit this_unit = unit_iter.step();
                if (this_unit.num_args == num_args)
                    return this_unit.format_string;
              }
            return this_record.default_format;
          }
      }

    auto_string new_format;
    new_format.set(op_name);
    if (num_args > 0)
        new_format.append("(%n, %m)");
    const char *format_result = lexicon->enter(new_format.value())->sp;
    return format_result;
  }

extern void register_gen_op(const char *name, const char *format, boolean is_default,
                            int num_args)
  {
    if (record_list == NULL)
        record_list = new format_record_list;

    format_record_list_iter record_iter(record_list);
    while (!record_iter.is_empty())
      {
        format_record this_record = record_iter.step();
        if (strcmp(this_record.op_name, name) == 0)
          {
            if (is_default)
              {
                this_record.default_format = format;
              }
            else
              {
                format_unit_list_iter unit_iter(this_record.unit_list);
                while (!unit_iter.is_empty())
                  {
                    format_unit this_unit = unit_iter.step();
                    if (this_unit.num_args == num_args)
                      {
                        this_unit.format_string = format;
                        return;
                      }
                  }

                format_unit new_unit;
                new_unit.num_args = num_args;
                new_unit.format_string = format;
                this_record.unit_list->append(new_unit);
              }
            return;
          }
      }

    format_record new_record;
    new_record.op_name = name;
    new_record.unit_list = new format_unit_list;
    if (is_default)
      {
        new_record.default_format = format;
      }
    else
      {
        new_record.default_format = name;
        format_unit new_unit;
        new_unit.num_args = num_args;
        new_unit.format_string = format;
        new_record.unit_list->append(new_unit);
      }

    record_list->append(new_record);
  }

/*
 * The following finds the ``composite'' of two types as specified in
 * ANSI/ISO 9899-1990 section 6.1.2.6.  If the types are not ``compatible'',
 * NULL is returned.
 */
extern type_node *composite(type_node *type_1, type_node *type_2)
  {
    boolean is_const = type_1->is_const();
    boolean is_volatile = type_1->is_volatile();
    boolean is_restrict = type_1->is_restrict();

    if (is_const != type_2->is_const())
        return NULL;
    if (is_volatile != type_2->is_volatile())
        return NULL;
    if (is_restrict != type_2->is_restrict())
        return NULL;

    type_node *unqual_1 = type_1->unqual();
    type_node *unqual_2 = type_2->unqual();

    if (unqual_1->is_enum())
      {
        if (unqual_2->is_enum())
          {
            if (unqual_1 != unqual_2)
                return NULL;
          }
        else
          {
            base_type *old_base = (base_type *)unqual_1;
            base_type *new_base =
                    new base_type(TYPE_INT, old_base->size(),
                                  old_base->is_signed());
            unqual_1 = unqual_1->parent()->install_type(new_base);
          }
      }
    else if (unqual_2->is_enum())
      {
        base_type *old_base = (base_type *)unqual_2;
        base_type *new_base =
                new base_type(TYPE_INT, old_base->size(),
                              old_base->is_signed());
        unqual_2 = unqual_2->parent()->install_type(new_base);
      }

    type_node *result;
    if (unqual_1->is_same(unqual_2))
      {
        result = unqual_1;
      }
    else
      {
        if (unqual_1->op() != unqual_2->op())
            return NULL;

        switch (unqual_1->op())
          {
            case TYPE_PTR:
              {
                ptr_type *ptr_1 = (ptr_type *)unqual_1;
                ptr_type *ptr_2 = (ptr_type *)unqual_2;
                result = composite(ptr_1->ref_type(), ptr_2->ref_type());
                if (result == NULL)
                    return NULL;
                result = result->ptr_to();
                break;
              }
            case TYPE_ARRAY:
              {
                array_type *array_1 = (array_type *)unqual_1;
                array_type *array_2 = (array_type *)unqual_2;
                array_bound upper_bound;
                if (array_1->size() != 0)
                  {
                    if (array_2->size() != 0)
                        return NULL;
                    upper_bound = array_bound(array_1->size() /
                                              array_1->elem_type()->size());
                  }
                else if (array_2->size() != 0)
                  {
                    upper_bound = array_bound(array_2->size() /
                                              array_2->elem_type()->size());
                  }
                type_node *elem_1 = array_1->elem_type();
                type_node *elem_2 = array_2->elem_type();
                while (elem_1->unqual()->is_array() && (elem_1->size() == 0))
                  {
                    array_1 = (array_type *)(elem_1->unqual());
                    elem_1 = array_1->elem_type();
                  }
                while (elem_2->unqual()->is_array() && (elem_2->size() == 0))
                  {
                    array_2 = (array_type *)(elem_2->unqual());
                    elem_2 = array_2->elem_type();
                  }
                result = composite(elem_1, elem_2);
                if (result == NULL)
                    return NULL;
                array_type *new_array =
                        new array_type(result, array_bound(0), upper_bound);
                result = result->parent()->install_type(new_array);
                break;
              }
            case TYPE_FUNC:
              {
                func_type *func_1 = (func_type *)unqual_1;
                func_type *func_2 = (func_type *)unqual_2;
                type_node *return_type =
                        composite(func_1->return_type(),
                                  func_2->return_type());
                if (return_type == NULL)
                    return NULL;
                if (!func_1->args_known())
                  {
                    func_type *temp_func = func_1;
                    func_1 = func_2;
                    func_2 = temp_func;
                  }
                if (!func_1->args_known())
                  {
                    func_type *new_func = new func_type(return_type);
                    return return_type->parent()->install_type(new_func);
                  }
                if (!func_2->args_known())
                  {
                    unsigned num_args = func_1->num_args();
                    func_type *new_func =
                            new func_type(return_type, num_args,
                                          func_1->has_varargs());
                    base_symtab *the_symtab = return_type->parent();
                    for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                      {
                        type_node *arg_type = func_1->arg_type(arg_num);
                        the_symtab =
                                joint_symtab(the_symtab, arg_type->parent());
                        if (the_symtab == NULL)
                          {
                            delete new_func;
                            return NULL;
                          }
                        new_func->set_arg_type(arg_num, arg_type);
                      }
                    return the_symtab->install_type(new_func);
                  }
                unsigned num_args = func_1->num_args();
                if (num_args != func_2->num_args())
                    return NULL;
                boolean has_varargs = func_1->has_varargs();
                if (has_varargs != func_2->has_varargs())
                    return NULL;
                func_type *new_func =
                        new func_type(return_type, num_args, has_varargs);
                base_symtab *the_symtab = return_type->parent();
                for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                  {
                    type_node *arg_type =
                            composite(func_1->arg_type(arg_num),
                                      func_2->arg_type(arg_num));
                    if (arg_type == NULL)
                      {
                        delete new_func;
                        return NULL;
                      }
                    the_symtab = joint_symtab(the_symtab, arg_type->parent());
                    if (the_symtab == NULL)
                      {
                        delete new_func;
                        return NULL;
                      }
                    new_func->set_arg_type(arg_num, arg_type);
                  }
                return the_symtab->install_type(new_func);
              }
            default:
                return NULL;
          }
      }

    if (is_const)
      {
        type_node *new_type = new modifier_type(TYPE_CONST, result);
        result = result->parent()->install_type(new_type);
      }
    if (is_volatile)
      {
        type_node *new_type = new modifier_type(TYPE_VOLATILE, result);
        result = result->parent()->install_type(new_type);
      }
    if (is_restrict)
      {
        type_node *new_type = new modifier_type(TYPE_RESTRICT, result);
        result = result->parent()->install_type(new_type);
      }

    return result;
  }

static void check_function_type(func_type *the_type)
  {
    type_node *return_type = the_type->return_type()->unqual();

    if (return_type->is_func())
      {
        static boolean warned = FALSE;
        mistake(&warned, the_type,
                "functions returning function type are illegal in C");
      }

    if (return_type->is_array())
      {
        static boolean warned = FALSE;
        mistake(&warned, the_type,
                "functions returning array type are illegal in C");
      }
  }
