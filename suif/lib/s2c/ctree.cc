/* file "ctree.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * Implementation of the C abstract syntax tree class.
 */


#pragma implementation "ctree.h"

#include "s2c.h"
#include <cstring>

/*
 * This is the table of ctree operators, their associativity,
 * commutivity, and printable representation.
 */

static type_node *op_type_conv(type_node *the_type);
static type_node *usual_arith_conv(type_node *type_1, type_node *type_2);
static type_node *integral_promotions(type_node *the_type);
static enum C_types c_int_promotions(enum C_types c_type, boolean *is_signed);
static boolean assign_convert_implicit(type_node *dest_type,
                                       type_node *source_type);
static boolean call_convert_implicit(type_node *dest_type,
                                     type_node *source_type);
static enum C_types get_c_arith_type(type_node *the_type);

struct _ctree_oper_attr ctree_oper_attr[] = {
    { 16, ctree_leftassoc, 0, "" },
    { 15, ctree_leftassoc, 0, "int" },
    { 15, ctree_leftassoc, 0, "char" },
    { 15, ctree_leftassoc, 0, "float" },
    { 15, ctree_leftassoc, 0, "enum" },
    { 15, ctree_leftassoc, 0, "string" },
    { 15, ctree_leftassoc, 0, "symbol" },
    { 15, ctree_leftassoc, 0, "funcdecl" },
    { 15, ctree_leftassoc, 0, "vardecl" },
    { 15, ctree_leftassoc, 0, "vardef" },
    { 15, ctree_leftassoc, 0, "typedecl" },
    { 15, ctree_leftassoc, 0, "forward typedecl" },
    { 15, ctree_leftassoc, 0, "goto" },
    { 15, ctree_leftassoc, 0, "break" },
    { 15, ctree_leftassoc, 0, "continue" },
    { 15, ctree_leftassoc, 0, "return" },
    { 15, ctree_leftassoc, 0, "label" },
    { 15, ctree_leftassoc, 0, "case" },
    { 15, ctree_leftassoc, 0, "default" },
    { 15, ctree_leftassoc, 0, "for" },
    { 15, ctree_leftassoc, 0, "if" },
    { 15, ctree_leftassoc, 0, "do" },
    { 15, ctree_leftassoc, 0, "switch" },
    { 15, ctree_leftassoc, 0, "{" },
    { 14, ctree_leftassoc, 0, "->" },
    { 14, ctree_leftassoc, 0, "." },
    { 14, ctree_leftassoc, 0, "[]" },
    { 14, ctree_leftassoc, 0, "CALL" },
    { 14, ctree_leftassoc, 0, "macro" },
    { 13, ctree_leftassoc, 0, "++" },
    { 13, ctree_leftassoc, 0, "++" },
    { 13, ctree_leftassoc, 0, "--" },
    { 13, ctree_leftassoc, 0, "--" },
    { 13, ctree_leftassoc, 0, "~" },
    { 13, ctree_leftassoc, 0, "!" },
    { 13, ctree_leftassoc, 0, "-" },
    { 13, ctree_leftassoc, 0, "&" },
    { 13, ctree_leftassoc, 0, "*" },
    { 13, ctree_leftassoc, 0, "()" },
    { 12, ctree_leftassoc, 1, "*" },
    { 12, ctree_leftassoc, 0, "/" },
    { 12, ctree_leftassoc, 0, "%" },
    { 11, ctree_leftassoc, 1, "+" },
    { 11, ctree_leftassoc, 0, "-" },
    { 10, ctree_leftassoc, 0, "<<" },
    { 10, ctree_leftassoc, 0, ">>" },
    {  9, ctree_leftassoc, 0, "<" },
    {  9, ctree_leftassoc, 0, "<=" },
    {  9, ctree_leftassoc, 0, ">" },
    {  9, ctree_leftassoc, 0, ">=" },
    {  8, ctree_leftassoc, 0, "==" },
    {  8, ctree_leftassoc, 0, "!=" },
    {  7, ctree_leftassoc, 0, "&" },
    {  6, ctree_leftassoc, 0, "^" },
    {  5, ctree_leftassoc, 0, "|" },
    {  4, ctree_leftassoc, 1, "&&" },
    {  3, ctree_leftassoc, 1, "||" },
    {  2, ctree_leftassoc, 0, "?:" },
    {  1, ctree_leftassoc, 0, "=" },
    {  1, ctree_leftassoc, 0, "+=" },
    {  1, ctree_leftassoc, 0, "-=" },
    {  0, ctree_leftassoc, 1, "," },
    {  0, ctree_leftassoc, 1, ";" },
};


#define grandchild(i,j) ((ctree*)(*(child(i)->children))[j])


/*
 * Convert a SUIF op to a ctree op.
 */

ctree_op ctree::ctree_io_to_op(if_ops n)
{
    switch (n) {
    case io_add: return ctree_add;
    case io_sub: return ctree_sub;
    case io_neg: return ctree_neg;
    case io_mul: return ctree_mult;
    case io_div: return ctree_div;
    case io_rem: return ctree_mod;
    case io_not: return ctree_compl;
    case io_and: return ctree_bitand;
    case io_ior: return ctree_bitor;
    case io_xor: return ctree_bitxor;
    case io_asr:
    case io_lsr: return ctree_rshift;
    case io_lsl: return ctree_lshift;
    case io_cvt: return ctree_conv;
    case io_lod: return ctree_deref;
    case io_seq: return ctree_eq;
    case io_sne: return ctree_neq;
    case io_sl: return ctree_lt;
    case io_sle: return ctree_lte;
    default: assert(FALSE);
    }
    assert(FALSE);
    return ctree_semi;
}


/*
 * Indent for readability.
 */

static void ctree_indent(io_class* out, int n)
{
    for (int i=0; i<n; i++)
        out->printf(" ");
}


/*
 * ctree constructors.
 */

ctree::ctree(void)
{
    op = ctree_semi;
    type = type_error;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop)
{
    op = theop;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop, boolean is_foldable, type_node *thetype)
{
    op = theop;
    type = thetype;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = is_foldable;
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop, type_node *thetype, const i_integer &val)
{
    op = theop;
    type = thetype;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    set_integer_val(val);
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(type_node *thetype, immed float_immed)
{
    op = ctree_floatconst;
    type = thetype;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    set_float_val(float_immed);
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop, const char *val)
{
    op = theop;
    type = NULL;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    leafval.s = val;
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop, const char *val, type_node *the_type)
{
    op = theop;
    type = the_type;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    leafval.s = val;
    comments = NULL;
    comment_space = 0;
}

ctree::ctree(ctree_op theop, sym_node *val, type_node *t)
{
    op = theop;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    leafval.y = val;
    type = t;
    comments = NULL;
    comment_space = 0;

    if ((theop == ctree_vardecl) || (theop == ctree_vardef) ||
        (theop == ctree_funcdecl) || (theop == ctree_funcdef) ||
        (theop == ctree_label))
      {
        add_object_comments(val);
      }
}

ctree::ctree(ctree_op theop, type_node *val)
{
    op = theop;
    children = new ctree_list;
    cached_child = NULL;
    cached_size = 0;
    foldable = FALSE;
    leafval.t = val;
    comments = NULL;
    comment_space = 0;
}


/*
 * ctree destructor
 */

ctree::~ctree()
{
    for (int i=0; i<N(); i++)
        delete child(i);
    delete children;

    if (comments != NULL)
        delete[] comments;
}


/*
 * Return number of elements.  This value is cached because recalculating it
 * requires iterating through the entire list of children.
 */
int ctree::N(void)
  {
    if (cached_size == 0)
        cached_size = children->count();
    return cached_size;
  }


/*
 * Return child number n.
 *
 * The method of iterating through children used all over the code is a loop
 * of the form:
 *
 *     for (int i=0; i<N(); i++)
 *         <function>(child(i));
 *
 * If ctree:child() and ctree::N() just called the corresponding functions on
 * the ``children'' list every time, this would be an N^2 operation where N is
 * the size of the list.  This was the original approach, but it turns out
 * that large lists sometimes occur and lead to unacceptable performance.
 * In particular, some Fortran benchmarks have arrays of 150,000 or more
 * elements initialized, leading to comma lists of that many elements.  N^2
 * performance is unacceptable in that case; the program can run all night and
 * not finish.
 *
 * To fix this problem, both the last values of N() and the element of the
 * list used by child() are cached.  Then the performance for this typical
 * iteration becomes linear.
 */
ctree *ctree::child(int n)
  {
    if ((cached_child == NULL) || (cached_child_num > n))
      {
        cached_child = children->head();
        cached_child_num = 0;
      }

    while (cached_child_num < n)
      {
        assert(cached_child != NULL);
        cached_child = cached_child->next();
        ++cached_child_num;
      }

    assert(cached_child != NULL);
    return cached_child->contents;
  }


i_integer ctree::integer_val(void) const
  {
    if (leafval.i.is_c_int)
        return leafval.i.u.i;
    else
        return immed_to_ii(immed(im_extended_int, leafval.i.u.str));
  }


void ctree::set_integer_val(const i_integer &new_value)
  {
    if (new_value.is_c_int())
      {
        leafval.i.is_c_int = TRUE;
        leafval.i.u.i = new_value.c_int();
      }
    else
      {
        leafval.i.is_c_int = FALSE;
        immed the_immed = ii_to_immed(new_value);
        assert(the_immed.kind() == im_extended_int);
        leafval.i.u.str = the_immed.ext_integer();
      }
  }


immed ctree::float_val(void) const
  {
    assert(op == ctree_floatconst);
    if (leafval.f.is_c_double)
        return immed(leafval.f.u.f);
    else
        return immed(im_extended_float, leafval.f.u.str);
  }


void ctree::set_float_val(immed new_value)
  {
    assert(op == ctree_floatconst);
    if (new_value.is_flt())
      {
        leafval.f.is_c_double = TRUE;
        leafval.f.u.f = new_value.flt();
      }
    else if (new_value.is_ext_flt())
      {
        leafval.f.is_c_double = FALSE;
        leafval.f.u.str = new_value.ext_flt();
      }
    else
      {
        assert(FALSE);
      }
  }


void ctree::replace_child(int child_num, ctree *new_child)
  {
    if ((cached_child == NULL) || (cached_child_num > child_num))
      {
        cached_child = children->head();
        cached_child_num = 0;
      }

    while (cached_child_num < child_num)
      {
        assert(cached_child != NULL);
        cached_child = cached_child->next();
        ++cached_child_num;
      }

    assert(cached_child != NULL);
    cached_child->contents = new_child;
  }


void ctree::add_comment(const char *comment)
  {
    if (comments != NULL)
      {
        for (int space_num = 0; space_num + 1 < comment_space; ++space_num)
          {
            if (comments[space_num] == NULL)
              {
                comments[space_num] = comment;
                comments[space_num + 1] = NULL;
                return;
              }
          }
      }

    if (comment_space == 0)
        comment_space = 1;
    const char **new_comments = new const char *[comment_space * 2];
    int space_num = 0;
    if (comments != NULL)
      {
        for (; space_num + 1 < comment_space; ++space_num)
            new_comments[space_num] = comments[space_num];
        delete[] comments;
      }
    comments = new_comments;

    comments[space_num] = comment;
    comments[space_num + 1] = NULL;
    comment_space *= 2;
  }


void ctree::add_object_comments(suif_object *the_object)
  {
    immed_list *comment_list =
            (immed_list *)(the_object->peek_annote(k_s2c_comments));
    if ((comment_list == NULL) || (comment_list->is_empty()))
        return;

    immed_list_iter comment_iter(comment_list);
    while (!comment_iter.is_empty())
      {
        immed this_immed = comment_iter.step();
        if (!this_immed.is_string())
          {
            error_line(1, NULL, "non-string in \"%s\" annotation",
                       k_s2c_comments);
          }
        add_comment(this_immed.string());
      }
  }


void ctree::copy_comments_from(ctree *other_ctree)
  {
    if (other_ctree->comments == NULL)
        return;
    int space_num = 0;
    while (other_ctree->comments[space_num] != NULL)
      {
        add_comment(other_ctree->comments[space_num]);
        ++space_num;
      }
  }


/*
 * Compare two ctrees.
 */

boolean ctree::operator==(ctree &c)
{
    if (op != c.op || type != c.type ||
        ((op == ctree_intconst || op == ctree_case) &&
         integer_val() != c.integer_val()) ||
        (op == ctree_floatconst && float_val() != c.float_val()) ||
        ((op == ctree_strconst || op == ctree_enumconst) &&
         leafval.s != c.leafval.s) ||
        ((op == ctree_symconst || op == ctree_vardecl || op == ctree_vardef ||
          op == ctree_funcdecl || op == ctree_funcdef ||
          op == ctree_goto || op == ctree_label) &&
         leafval.y != c.leafval.y) ||
        (op == ctree_typedecl && leafval.t != c.leafval.t) ||
        (op == ctree_typeforward && leafval.t != c.leafval.t))
        return FALSE;

    if (N() != c.N())
        return FALSE;

    for (int i=0; i<N(); i++)
        if (*child(i) != *c.child(i))
            return FALSE;

    return TRUE;
}


void ctree::swap(ctree *other)
  {
    static char swapspace[sizeof(ctree)];

    memcpy(swapspace, this, sizeof(ctree));
    memcpy(this, other, sizeof(ctree));
    memcpy(other, swapspace, sizeof(ctree));
  }


boolean ctree::is_int_const(i_integer *value)
  {
    if (op == ctree_intconst)
      {
        *value = integer_val();
        return TRUE;
      }
    else
      {
        return FALSE;
      }
  }

/*
 * Return the type of a C expression.
 */
type_node *ctree::expr_type(void)
  {
    switch (op)
      {
        case ctree_intconst:
        case ctree_floatconst:
            return type;
        case ctree_charconst:
        case ctree_enumconst:
            return type_signed;
        case ctree_strconst:
          {
            array_type *new_array =
                    new array_type(type_char, array_bound(0),
                                   array_bound(strlen(leafval.s) + 1));
            return type_char->parent()->install_type(new_array);
          }
        case ctree_symconst:
            if (leafval.y->is_var())
              {
                var_sym *the_var = (var_sym *)(leafval.y);
                return the_var->type();
              }
            else if (leafval.y->is_proc())
              {
                proc_sym *the_proc = (proc_sym *)(leafval.y);
                return the_proc->type();
              }
            else
              {
                assert(FALSE);
                return NULL;
              }
        case ctree_strptr:
        case ctree_strsel:
          {
            assert(N() == 1);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type);
            if (op == ctree_strptr)
              {
                this_type = this_type->unqual();
                assert(this_type->is_ptr());
                ptr_type *the_ptr_type = (ptr_type *)this_type;
                this_type = the_ptr_type->ref_type();
              }
            assert(this_type->unqual()->is_struct());
            struct_type *the_struct_type =
                    (struct_type *)(this_type->unqual());
            const char *field_name = leafval.s;
            if ((the_struct_type->op() == TYPE_GROUP) &&
                struct_is_union(the_struct_type))
              {
                field_name = strchr(field_name, '.');
                assert(field_name != NULL);
                ++field_name;
              }
            unsigned field_num =
                    the_struct_type->find_field_by_name(field_name);
            assert(field_num < the_struct_type->num_fields());
            type_node *field_type = the_struct_type->field_type(field_num);
            if (this_type->is_const() && !field_type->is_const())
              {
                type_node *new_type =
                        new modifier_type(TYPE_CONST, field_type);
                field_type = field_type->parent()->install_type(new_type);
              }
            if (this_type->is_volatile() && !field_type->is_volatile())
              {
                type_node *new_type =
                        new modifier_type(TYPE_VOLATILE, field_type);
                field_type = field_type->parent()->install_type(new_type);
              }
             if (this_type->is_restrict() && !field_type->is_restrict())
              {
                type_node *new_type =
                        new modifier_type(TYPE_RESTRICT, field_type);
                field_type = field_type->parent()->install_type(new_type);
              }
            return field_type;
          }
        case ctree_subscr:
          {
            assert(N() > 0);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            assert(this_type->is_ptr());
            ptr_type *this_ptr = (ptr_type *)this_type;
            for (int child_num = 2; child_num < N(); ++child_num)
              {
                this_type = this_ptr->ref_type();
                this_type = op_type_conv(this_type->unqual());
                assert(this_type->is_ptr());
                this_ptr = (ptr_type *)this_type;
              }
            return this_ptr->ref_type();
          }
        case ctree_funcall:
          {
            assert(N() > 0);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            assert(this_type->is_ptr());
            ptr_type *this_ptr = (ptr_type *)this_type;
            this_type = this_ptr->ref_type()->unqual();
            assert(this_type->is_func());
            func_type *this_func = (func_type *)this_type;
            return this_func->return_type();
          }
        case ctree_macro:
          {
            if (type != NULL)
                return type;
            if (N() == 0)
                return type_signed;
            assert(N() > 0);
            type_node *type_1 = child(0)->expr_type();
            type_1 = op_type_conv(type_1->unqual());
            type_node *type_2;
            if (N() == 1)
              {
                type_2 = type_1;
              }
            else
              {
                type_2 = child(1)->expr_type();
                type_2 = op_type_conv(type_2->unqual());
              }
            return usual_arith_conv(type_1, type_2);
          }
        case ctree_postinc:
        case ctree_postdec:
            assert(N() == 1);
            return op_type_conv(child(0)->expr_type()->unqual());
        case ctree_preinc:
        case ctree_predec:
          {
            assert(N() == 1);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            if (this_type->is_ptr())
                return this_type;
            return usual_arith_conv(this_type, type_signed);
          }
        case ctree_compl:
        case ctree_neg:
          {
            assert(N() == 1);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            return integral_promotions(this_type);
          }
        case ctree_not:
        case ctree_lt:
        case ctree_lte:
        case ctree_gt:
        case ctree_gte:
        case ctree_eq:
        case ctree_neq:
        case ctree_logand:
        case ctree_logor:
            return type_signed;
        case ctree_addrof:
          {
            assert(N() == 1);
            return child(0)->expr_type()->ptr_to();
          }
        case ctree_deref:
          {
            assert(N() == 1);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            assert(this_type->is_ptr());
            ptr_type *this_ptr = (ptr_type *)this_type;
            return this_ptr->ref_type();
          }
        case ctree_conv:
            return type;
        case ctree_mult:
        case ctree_div:
        case ctree_mod:
        case ctree_bitand:
        case ctree_bitxor:
        case ctree_bitor:
          {
            assert(N() > 0);
            type_node *this_type = child(0)->expr_type();
            if (N() == 1)
                return this_type;
            this_type = op_type_conv(this_type->unqual());
            for (int child_num = 1; child_num < N(); ++child_num)
              {
                type_node *op_type = child(child_num)->expr_type();
                op_type = op_type_conv(op_type->unqual());
                this_type = usual_arith_conv(this_type, op_type);
              }
            return this_type;
          }
        case ctree_add:
        case ctree_sub:
          {
            assert(N() > 0);
            type_node *this_type = child(0)->expr_type();
            if (N() == 1)
                return this_type;
            this_type = op_type_conv(this_type->unqual());
            for (int child_num = 1; child_num < N(); ++child_num)
              {
                type_node *op_type = child(child_num)->expr_type();
                op_type = op_type_conv(op_type->unqual());
                if (this_type->is_ptr() && op_type->is_ptr())
                    this_type = type_ptr_diff;
                else if (op_type->is_ptr())
                    this_type = op_type;
                else if (!this_type->is_ptr())
                    this_type = usual_arith_conv(this_type, op_type);
              }
            return this_type;
          }
        case ctree_lshift:
        case ctree_rshift:
          {
            assert(N() > 0);
            type_node *this_type = child(0)->expr_type();
            this_type = op_type_conv(this_type->unqual());
            return integral_promotions(this_type);
          }
        case ctree_ternary:
          {
            assert(N() == 3);
            type_node *type_1 = child(1)->expr_type();
            type_1 = op_type_conv(type_1->unqual());
            type_node *type_2 = child(2)->expr_type();
            type_2 = op_type_conv(type_2->unqual());
            if (((type_1->op() == TYPE_INT) || (type_1->op() == TYPE_FLOAT)) &&
                ((type_2->op() == TYPE_INT) || (type_2->op() == TYPE_FLOAT)))
              {
                return usual_arith_conv(type_1, type_2);
              }
            if (type_1 == type_2)
                return type_1;
            assert(type_1->is_ptr() && type_2->is_ptr());
            ptr_type *ptr_1 = (ptr_type *)type_1;
            ptr_type *ptr_2 = (ptr_type *)type_2;
            type_node *result;
            if (ptr_1->ref_type()->unqual()->op() == TYPE_VOID)
              {
                result = ptr_2->ref_type()->unqual();
              }
            else if (ptr_2->ref_type()->unqual()->op() == TYPE_VOID)
              {
                result = ptr_1->ref_type()->unqual();
              }
            else
              {
                result = composite(ptr_1->ref_type()->unqual(),
                                   ptr_2->ref_type()->unqual());
                assert(result != NULL);
              }
            if (ptr_1->ref_type()->is_const() || ptr_2->ref_type()->is_const())
              {
                type_node *new_type = new modifier_type(TYPE_CONST, result);
                result = result->parent()->install_type(new_type);
              }
            if (ptr_1->ref_type()->is_volatile() ||
                ptr_2->ref_type()->is_volatile())
              {
                type_node *new_type = new modifier_type(TYPE_VOLATILE, result);
                result = result->parent()->install_type(new_type);
              }
            if (ptr_1->ref_type()->is_restrict() ||
                ptr_2->ref_type()->is_restrict())
              {
                type_node *new_type = new modifier_type(TYPE_RESTRICT, result);
                result = result->parent()->install_type(new_type);
              }

            return result->ptr_to();
          }
        case ctree_assign:
        case ctree_addassign:
        case ctree_subassign:
          {
            assert(N() == 2);
            type_node *this_type = child(0)->expr_type();
            return op_type_conv(this_type->unqual());
          }
        case ctree_comma:
          {
            assert(N() > 0);
            type_node *this_type = child(N() - 1)->expr_type();
            return op_type_conv(this_type->unqual());
          }

        case ctree_funcdef:
        case ctree_funcdecl:
        case ctree_vardecl:
        case ctree_vardef:
        case ctree_typedecl:
        case ctree_typeforward:
        case ctree_goto:
        case ctree_break:
        case ctree_continue:
        case ctree_return:
        case ctree_label:
        case ctree_case:
        case ctree_default:
        case ctree_for:
        case ctree_if:
        case ctree_do:
        case ctree_switch:
        case ctree_block:
        case ctree_semi:
        case ctree_blank_line:
        case ctree_pound_line:
        case ctree_type_ref:
            assert_msg(FALSE, ("ctree::expr_type() called on non-expression"));
            return NULL;
      }
    return NULL;
  }

/*
 * Print a ctree in list form.
 */

void ctree::print(FILE *out, int nindent)
{
    file_io the_io(out);

    switch (op) {
    case ctree_intconst:
        integer_val().print(out);
        return;
    case ctree_charconst:
      {
        fprintf(out, "'");
        print_c_char(&the_io, integer_val());
        fprintf(out, "'");
        return;
      }
    case ctree_floatconst:
        print_float(&the_io, float_val());
        return;
    case ctree_enumconst:
        fprintf(out, "%s", leafval.s);
        return;
    case ctree_strconst:
        fprintf(out, "\"%s\"", leafval.s);
        return;
    case ctree_symconst:
    case ctree_vardecl:
    case ctree_vardef:
        leafval.y->print(out);
        return;
    case ctree_typedecl:
        leafval.t->print_full(out);
        return;
    case ctree_typeforward:
        leafval.t->print_full(out);
        return;
    default:
        fprintf(out, "(%s", ctree_oper_attr[op].sym);
        fprintf(out, " ");
        nindent += strlen(ctree_oper_attr[op].sym)+7;
        for (int i=0; i<N(); i++) {
            if (i != 0)
                ctree_indent(&the_io, nindent+4);
            child(i)->print(out, nindent+4);
            if (i != N()-1)
                fprintf(out, "\n");
        }
        fprintf(out, ")");
        return;
    }
}


static boolean last_was_minus = FALSE;
static boolean last_was_slash = FALSE;

/*
 * Print a ctree with the proper precedence relationship.  This may
 * mean placing ()s around the expression.
 */

void ctree::print_with_prec(io_class *out, int nindent, int parentop,
                            boolean is_left)
  {
    int this_prec = ctree_oper_attr[op].prec;
    int parent_prec = ctree_oper_attr[parentop].prec;

    boolean parens_needed;
    if ((op == ctree_neg) && last_was_minus)
      {
        /*
         * We don't want ``x - -y'' to be written ``x--y'' or
         * ``-(-x)'' to be written ``--x''; this would give a ``--''
         * token.
         */
        parens_needed = TRUE;
      }
    else if ((op == ctree_deref) && last_was_slash)
      {
        /*
         * We don't want ``x / *y'' to be written without spaces or
         * parentheses; this would be interpretted as the beginning of
         * a comment.
         */
        parens_needed = TRUE;
      }
    else if (this_prec < parent_prec)
      {
        parens_needed = TRUE;
      }
    else if (this_prec > parent_prec)
      {
        parens_needed = FALSE;
      }
    else
      {
        if ((ctree_oper_attr[op].assoc == ctree_leftassoc) && is_left)
            parens_needed = FALSE;
        else if ((op == parentop) && ctree_oper_attr[op].commutes)
            parens_needed = FALSE;
        else
            parens_needed = TRUE;
      }

    if (parens_needed)
      {
        out->printf("(");
        last_was_minus = FALSE;
        last_was_slash = FALSE;
      }
    print_as_c(out, nindent);
    if (parens_needed)
        out->printf(")");
  }


/*
 * Print the body of a ``for'', ``while'', ``do ... while'', or
 * ``switch'' statement, or either the ``then'' or ``else'' body of an
 * ``if'' statement.
 */
void ctree::print_as_body(io_class *out, int nindent)
  {
    int new_indent = nindent;
    if (op != ctree_block)
      {
        if (c_style.indent_single_statement_bodies)
          {
            out->printf("\n");
            new_indent = nindent + c_style.single_statement_body_indent;
            ctree_indent(out, new_indent);
          }
        else
          {
            out->printf(" ");
          }
      }
    print_as_c(out, new_indent);
  }


/*
 * Print a ctree as C.
 */

void ctree::print_as_c(io_class *out, int nindent)
{
    if (op == ctree_neg)
      {
        assert(!last_was_minus);
      }
    if (op == ctree_deref)
      {
        assert(!last_was_slash);
      }

    /*
     * If this op will print something before making a recursive call
     * to a print function, reset last_was_minus and last_was_slash.
     */
    switch (op)
      {
        case ctree_strptr:
        case ctree_strsel:
        case ctree_subscr:
        case ctree_funcall:
        case ctree_postinc:
        case ctree_postdec:
        case ctree_mult:
        case ctree_div:
        case ctree_mod:
        case ctree_add:
        case ctree_sub:
        case ctree_lshift:
        case ctree_rshift:
        case ctree_lt:
        case ctree_lte:
        case ctree_gt:
        case ctree_gte:
        case ctree_eq:
        case ctree_neq:
        case ctree_bitand:
        case ctree_bitxor:
        case ctree_bitor:
        case ctree_logand:
        case ctree_logor:
        case ctree_ternary:
        case ctree_assign:
        case ctree_addassign:
        case ctree_subassign:
        case ctree_comma:
        case ctree_semi:
            break;
        default:
            last_was_minus = FALSE;
            last_was_slash = FALSE;
      }

    int i;

    switch (op) {
        /* Unimplemented */
    default:
        assert_msg(FALSE, ("ctree::print_as_c - Unknown op %d", op));

        /* Function declaration */
    case ctree_funcdef:
      {
        const char *posttype;
        proc_sym *the_proc_sym = (proc_sym *)leafval.y;
        const char *pretype = make_c_proc_type(the_proc_sym, &posttype);
        out->printf("%s(", pretype);

        /*
         * posttype points to a scratch buffer that can be
         * over-written by the calls to make_c_sym_type.  So we need
         * to save it.  But the vast majority of the time it will be
         * an empty string, so we optimize for that case.
         */
        if (*posttype == '0')
          {
            posttype = "";
          }
        else
          {
            char *new_string = new char[strlen(posttype) + 1];
            strcpy(new_string, posttype);
            posttype = new_string;
          }

        assert(N()>0);
        func_type *the_func_type = the_proc_sym->type();
        boolean new_style = the_func_type->args_known();
        for (i=0; i<N()-1; i++) {
            assert(child(i)->op == ctree_vardecl);
            if (new_style)
              {
                out->printf("%s",
                            make_c_sym_type((var_sym *)(child(i)->leafval.y)));
              }
            else
              {
                out->printf("%s", child(i)->leafval.y->name());
              }
            if (i != N()-2)
                out->printf(", ");
        }
        if (the_func_type->has_varargs())
          {
            if (N() == 1)
              {
                static boolean warned = FALSE;
                mistake(&warned, the_func_type,
                        "function with no specified arguments may not "
                        "have varargs");
              }
            out->printf(", ...");
          }
        else if (new_style && (N() == 1))
          {
            out->printf("void");
          }
        out->printf(")%s\n", posttype);
        if (*posttype != '0')
            delete[] posttype;
        if (!new_style)
          {
            for (i=0; i<N()-1; i++)
              {
                ctree_indent(out, nindent + c_style.param_indent);
                out->printf("%s;\n",
                            make_c_sym_type((var_sym *)child(i)->leafval.y));
              }
          }
        if (comments != NULL)
            full_line_output_comments(out, nindent + c_style.statement_indent);
        ctree_indent(out, nindent + c_style.brace_indent);
        out->printf("{\n");
        child(N()-1)->print_as_c(out, nindent + c_style.statement_indent);
        ctree_indent(out, nindent + c_style.brace_indent);
        out->printf("}\n");
        return;
      }

        /* Ternary operator */
    case ctree_ternary:
        assert(N() == 3);
        child(0)->print_with_prec(out, nindent, op, TRUE);
        out->printf(" ? ");
        if (comments != NULL)
            in_line_output_comments(out, 0, 1);
        child(1)->print_with_prec(out, nindent, op, FALSE);
        out->printf(" : ");
        child(2)->print_with_prec(out, nindent, op, FALSE);
        return;

        /* Binary operators */
    case ctree_add:
    case ctree_sub:
    case ctree_mult:
    case ctree_div:
    case ctree_mod:
    case ctree_lshift:
    case ctree_rshift:
    case ctree_lt:
    case ctree_lte:
    case ctree_gt:
    case ctree_gte:
    case ctree_eq:
    case ctree_neq:
    case ctree_bitand:
    case ctree_bitxor:
    case ctree_bitor:
    case ctree_logand:
    case ctree_logor:
    case ctree_assign:
    case ctree_addassign:
    case ctree_subassign:
        for (i = 0; i < N(); i++)
          {
            child(i)->print_with_prec(out, nindent, op, (i == 0));
            if (i != N()-1)
              {
                if ((op == ctree_assign) ||
                    (op == ctree_addassign) ||
                    (op == ctree_subassign))
                  {
                    if (c_style.space_around_assignments)
                        out->printf(" ");
                    out->printf("%s", ctree_oper_attr[op].sym);
                    if (c_style.space_around_assignments)
                        out->printf(" ");
                  }
                else
                  {
                    if (c_style.space_around_binops)
                        out->printf(" ");
                    out->printf("%s", ctree_oper_attr[op].sym);
                    if (c_style.space_around_binops)
                        out->printf(" ");
                    else if (op == ctree_sub)
                        last_was_minus = TRUE;
                    else if (op == ctree_div)
                        last_was_slash = TRUE;
                  }
              }
            if ((i == 0) && (comments != NULL))
                in_line_output_comments(out, 1, 1);
          }
        return;

    case ctree_comma:
      {
        int mod = 0;
        for (i = 0; i < N(); i++)
          {
            child(i)->print_with_prec(out, nindent, op, (i == 0));
            if (i != N()-1)
              {
                out->printf("%s", ctree_oper_attr[op].sym);
                if (c_style.space_after_commas &&
                    (mod != c_style.max_comma_items_per_line - 1))
                  {
                    out->printf(" ");
                  }
              }
            if ((i == 0) && (comments != NULL))
                in_line_output_comments(out, 1, 1);
            ++mod;
            if ((mod == c_style.max_comma_items_per_line) && (i + 1 < N()))
              {
                out->printf("\n");
                ctree_indent(out, nindent);
                mod = 0;
              }
          }
        return;
      }

        /* Unary pre-operators */
    case ctree_compl:
    case ctree_not:
    case ctree_neg:
    case ctree_addrof:
    case ctree_deref:
    case ctree_preinc:
    case ctree_predec:
        assert(N() == 1);
        out->printf("%s", ctree_oper_attr[op].sym);
        if (op == ctree_neg)
            last_was_minus = TRUE;
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        child(0)->print_with_prec(out, nindent, op, TRUE);
        return;

    case ctree_conv:
        assert(N() == 1);
        out->printf("(%s)", make_c_type(type));
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        child(0)->print_with_prec(out, nindent, op, TRUE);
        return;

        /* Unary post-operators */
    case ctree_postinc:
    case ctree_postdec:
        assert(N() == 1);
        child(0)->print_with_prec(out, nindent, op, TRUE);
        out->printf("%s", ctree_oper_attr[op].sym);
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

        /* Misc */
    case ctree_strptr:
    case ctree_strsel:
        assert(N() == 1);
        child(0)->print_with_prec(out, nindent, op, TRUE);
        out->printf("%s%s", ctree_oper_attr[op].sym, leafval.s);
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_subscr:
        child(0)->print_with_prec(out, nindent, op, TRUE);
        for (i=1; i<N(); i++) {
            out->printf("[");
            child(i)->print_as_c(out, nindent);
            out->printf("]");
        }
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_macro:
      {
        const char *format_string = lookup_gen_op(leafval.s, N());
        assert(format_string != NULL);

        const char *follow;
        int num_matches = 0;
        for (follow = format_string; *follow != 0; ++follow)
          {
            if (*follow == '%')
              {
                ++follow;
                if (*follow == 'a')
                    ++num_matches;
              }
          }
        if (num_matches > N())
          {
            error_line(1, NULL,
                       "format for `%s' generic op requires %d operands, but "
                       "it is used with %d", leafval.s, num_matches, N());
          }

        int match_num = 0;
        for (follow = format_string; *follow != 0; ++follow)
          {
            if (*follow == '%')
              {
                ++follow;
                switch (*follow)
                  {
                    case '%':
                        out->io_putc('%');
                        break;
                    case 'a':
                        child(match_num)->print_as_c(out, nindent);
                        ++match_num;
                        break;
                    case 'n':
                      {
                        ++follow;
                        const char *begin_sep = follow;
                        for (; *follow != 0; ++follow)
                          {
                            if (*follow == '%')
                              {
                                ++follow;
                                if (*follow != '%')
                                    break;
                              }
                          }
                        if (*follow != 'm')
                          {
                            error_line(1, NULL,
                                       "badly formed ``%%n'' directive in "
                                       "format for generic op `%s'",
                                       leafval.s);
                          }

                        int max_child = N() - (num_matches - match_num);
                        while (match_num < max_child)
                          {
                            child(match_num)->print_as_c(out, nindent);
                            ++match_num;
                            if (match_num < max_child)
                              {
                                for (const char *sep = begin_sep; sep < follow - 1;
                                     ++sep)
                                  {
                                    out->io_putc(*sep);
                                    if (*sep == '%')
                                        ++sep;
                                  }
                              }
                          }

                        break;
                      }
                    default:
                        error_line(1, NULL,
                                   "illegal escape sequence `%%%c' in format"
                                   " string for `%s'", *follow, leafval.s);
                  }
              }
            else
              {
                out->io_putc(*follow);
              }
          }

        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_funcdecl:
      {
        proc_sym *psym = (proc_sym *)leafval.y;
        out->printf("%s", make_c_proc_prototype(psym));
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_vardecl:
      {
        var_sym *the_var = (var_sym *)leafval.y;
        assert(the_var->is_global() && (!the_var->is_private()));
        out->printf("extern ");
        out->printf("%s", make_c_sym_type(the_var));
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_vardef:
      {
        var_sym *the_var = (var_sym *)leafval.y;
        if (((!the_var->is_global()) && (!the_var->is_auto())) ||
            (the_var->is_global() && the_var->is_private()))
          {
            out->printf("static ");
          }
        out->printf("%s", make_c_sym_type(the_var));
        if (N()) {
            assert(N() == 1);
            out->printf(" =");
            if (child(0)->op != ctree_block)
                out->printf(" ");
            child(0)->print_as_c(out, nindent);
        }
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_typedecl: {
        type_node *t = leafval.t;
        if (t->op() == TYPE_ENUM) {
            enum_type *et = (enum_type *) t;
            out->printf("enum %s { ", et->name());
            for (unsigned value_num=0; value_num<et->num_values();
                 value_num++) {
                if (value_num > 0)
                    out->printf(", ");
                out->printf("%s", et->member(value_num));
                if (((value_num > 0) &&
                     (et->value(value_num) !=
                      (et->value(value_num - 1) + 1))) ||
                    ((value_num == 0) && (et->value(value_num) != 0)))
                  {
                    out->printf(" = %d", et->value(value_num));
                  }
            }
            out->printf(" }");
        } else if ((t->op() == TYPE_GROUP) || (t->op() == TYPE_STRUCT) ||
                   (t->op() == TYPE_UNION)) {
            struct_type *st = (struct_type *) t;
            print_struct_with_fields(st, out, nindent);
        }
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        break;
    }

    case ctree_typeforward: {
        type_node *t = leafval.t;
        assert((t->op() == TYPE_GROUP) || (t->op() == TYPE_STRUCT) ||
               (t->op() == TYPE_UNION));
        struct_type *st = (struct_type *) t;
        print_struct_short(st, out);
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        break;
    }

    case ctree_goto:
        out->printf("goto %s", leafval.y->name());
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_break:
        out->printf("break");
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_continue:
        out->printf("continue");
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_return:
        out->printf("return");
        if (N()) {
            out->printf(" ");
            child(0)->print_as_c(out, nindent);
        }
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_label:
        out->printf("%s:", leafval.y->name());
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        return;

    case ctree_case:
        out->printf("case ");
        out->print_ii(integer_val());
        out->printf(":");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        return;

    case ctree_default:
        out->printf("default:");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        return;

    case ctree_semi:
        for (i=0; i<N(); i++) {
            ctree_op cop = child(i)->op;

            int indent_amount = nindent;
            if ((cop == ctree_label) || (cop == ctree_case) ||
                (cop == ctree_default))
              {
                indent_amount = indent_amount + c_style.label_indent;
                indent_amount = ((indent_amount > 0) ? indent_amount : 0);
              }
            if ((cop != ctree_block) && (cop != ctree_blank_line) &&
                (cop != ctree_pound_line))
              {
                ctree_indent(out, indent_amount);
              }

            child(i)->print_as_c(out, nindent);
            if ((((cop != ctree_if) && (cop != ctree_for) &&
                  (cop != ctree_switch)) ||
                 (child(i)->child(child(i)->N() - 1)->op != ctree_block)) &&
                (cop != ctree_funcdef) && (cop != ctree_block) &&
                (cop != ctree_blank_line) && (cop != ctree_pound_line) &&
                (((cop != ctree_label) && (cop != ctree_case) &&
                  (cop != ctree_default)) || remainder_is_blanks(i + 1))) {
                out->printf("%s\n", ctree_oper_attr[op].sym);
            } else if ((cop != ctree_blank_line) &&
                       (cop != ctree_pound_line)) {
                out->printf("\n");
            }
            if ((i == 0) && (comments != NULL))
                full_line_output_comments(out, nindent);
            if ((cop == ctree_vardecl || cop == ctree_vardef ||
                 cop == ctree_funcdecl || cop == ctree_funcdef ||
                 cop == ctree_typedecl || cop == ctree_typeforward ||
                 cop == ctree_block) &&
                i < N()-1 && child(i+1)->op != cop &&
                child(i+1)->op != ctree_block)
              {
                out->printf("\n");
              }
        }
        return;

        /* Nested scope */
    case ctree_block:
      {
        if (c_style.new_line_for_open_brace)
          {
            out->printf("\n");
            ctree_indent(out, nindent + c_style.brace_indent);
          }
        else
          {
            out->printf(" ");
          }
        out->printf("{");
        int new_indent = nindent + c_style.statement_indent;
        if (comments != NULL)
            in_line_output_comments(out, 2, 0);
        if (child(0)->getop() == ctree_comma)
          {
            boolean contains_block = FALSE;

            for (i=0; i<child(0)->N(); i++)
              {
                if (child(0)->child(i)->op == ctree_block)
                  {
                    contains_block = TRUE;
                    break;
                  }
              }

            int mod = 0;
            for (i=0; i<child(0)->N(); i++)
              {
                if (!contains_block)
                  {
                    if (i == 0)
                      {
                        out->printf("\n");
                        ctree_indent(out, new_indent);
                      }
                    else if (mod == c_style.max_comma_items_per_line)
                      {
                        out->printf("\n");
                        ctree_indent(out, new_indent);
                        mod = 0;
                      }
                    else
                      {
                        out->printf(" ");
                      }
                  }
                else if (child(0)->child(i)->op != ctree_block)
                  {
                    out->printf("\n");
                    ctree_indent(out, new_indent);
                  }
                child(0)->child(i)->print_as_c(out, new_indent);
                if (i < child(0)->N() - 1)
                    out->printf("%s", ctree_oper_attr[ctree_comma].sym);
                ++mod;
              }
            out->printf("\n");
          }
        else
          {
            if (child(0)->op != ctree_block)
                out->printf("\n");
            child(0)->print_as_c(out, new_indent);
            if (child(0)->op == ctree_block)
                out->printf("\n");
          }
        ctree_indent(out, nindent + c_style.brace_indent);
        out->printf("}");
        return;
      }

        /* FOR statement */
    case ctree_for: {
        assert(N() == 4);
        out->printf("for (");
        child(0)->print_as_c(out, nindent);
        out->printf("; ");
        child(1)->print_as_c(out, nindent);
        out->printf("; ");
        child(2)->print_as_c(out, nindent);
        out->printf(")");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        child(3)->print_as_body(out, nindent);
        return;
    }

        /* IF statement */
    case ctree_if: {
        assert(N() == 2 || N() == 3);

        out->printf("if (");
        child(0)->print_as_c(out, nindent);
        out->printf(")");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        child(1)->print_as_body(out, nindent);
        if (N() == 3)
          {
            if (c_style.new_line_for_else)
              {
                out->printf("\n");
                ctree_indent(out, nindent);
              }
            else
              {
                out->printf(" ");
              }
            out->printf("else");
            child(2)->print_as_body(out, nindent);
          }
        return;
    }

        /* DO...WHILE statement */
    case ctree_do:
        assert(N() == 2);
        out->printf("do");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        child(0)->print_as_body(out, nindent);
        out->printf(" ");
        out->printf("while (");
        child(1)->print_as_c(out, nindent);
        out->printf(")");
        return;

        /* SWITCH statement */
    case ctree_switch:
        assert(N() == 2);
        out->printf("switch (");
        child(0)->print_as_c(out, nindent);
        out->printf(")");
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        child(1)->print_as_body(out, nindent);
        return;

        /* Function call */
    case ctree_funcall:
        child(0)->print_with_prec(out, nindent, op, TRUE);
        out->printf("(");
        for (i=1; i<N(); i++) {
            child(i)->print_as_c(out, nindent);
            if (i != N()-1)
                out->printf(", ");
        }
        out->printf(")");
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

        /* Various constants */
    case ctree_intconst:
      {
        assert(type->op() == TYPE_INT);
        base_type *the_base = (base_type *)type;
        enum C_types the_c_type = c_int_type(the_base);
        assert((integer_val() >= 0) || always_intnum);
        out->print_ii(integer_val());
        if ((the_c_type == C_int) || always_intnum)
          {
            if (!the_base->is_signed())
                out->printf("u");
          }
        else if (the_c_type == C_long)
          {
            if (the_base->is_signed())
                out->printf("l");
            else
                out->printf("ul");
          }
        else if (the_c_type == C_longlong)
          {
            assert(ll_suffix);
            if (the_base->is_signed())
                out->printf("ll");
            else
                out->printf("ull");
          }
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_charconst:
      {
        out->printf("'");
        print_c_char(out, integer_val());
        out->printf("'");
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_floatconst:
      {
        immed float_value = float_val();

        boolean is_negative = false;
        if (float_value.is_flt())
            is_negative = (float_value.flt() < 0);
        else if (float_value.is_ext_flt())
            is_negative = (*(float_value.ext_flt()) == '-');
        else
            assert(FALSE);

        if (is_negative)
            out->printf("(");

        assert(type->op() == TYPE_FLOAT);
        enum C_types the_c_type = c_float_type(type);
        print_float(out, float_value);
        if (the_c_type == C_float)
            out->printf("F");
        else if (the_c_type == C_longdouble)
            out->printf("L");
        else
            assert(the_c_type == C_double);

        if (is_negative)
            out->printf(")");
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
      }

    case ctree_enumconst:
        out->printf("%s", leafval.s);
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_strconst:
        out->printf("\"%s\"", leafval.s);
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_symconst:
        out->printf("%s", leafval.y->name());
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;

    case ctree_blank_line:
        if (comments != NULL)
            full_line_output_comments(out, nindent);
        else
            out->printf("\n");
        return;

    case ctree_pound_line:
        out->printf("#%s", leafval.s);
        if (comments != NULL)
            in_line_output_comments(out, 1, 0);
        out->printf("\n");
        return;

    case ctree_type_ref:
        assert(N() == 0);
        out->printf("%s", make_c_type(type));
        if (comments != NULL)
            in_line_output_comments(out, 1, 1);
        return;
    }
}


/***********************************************************************
 *
 * Tree rewriting routines, used to turn the C tree into legal,
 * readable C.
 *
 ***********************************************************************/

/*
 * Do some nice cleanup on the tree, like collapsing *& into nothing.
 */

void ctree::do_basic_folding()
{
    for (int i=0; i<N(); i++)
        child(i)->do_basic_folding();

    for (;;) {
        if ((op == ctree_addrof) && (N() == 1) &&
            (child(0)->op == ctree_deref)) {
            assert(child(0)->N() == 1);
            ctree *old_child = child(0);
            ctree *replacement = old_child->child(0);
            old_child->reset_children();
            swap(replacement);
            copy_comments_from(old_child);
            copy_comments_from(replacement);
            delete replacement;
            continue;
        }
        if ((op == ctree_deref) && (N() == 1) &&
            (child(0)->op == ctree_addrof)) {
            assert(child(0)->N() == 1);
            ctree *old_child = child(0);
            ctree *replacement = old_child->child(0);
            old_child->reset_children();
            swap(replacement);
            copy_comments_from(old_child);
            copy_comments_from(replacement);
            delete replacement;
            continue;
        }
        if ((op == ctree_strsel) && (N() == 1) &&
            (child(0)->op == ctree_deref)) {
            assert(child(0)->N() == 1);
            ctree *old_child = child(0);
            replace_child(0, old_child->child(0));
            old_child->children = new ctree_list;
            old_child->flush_child_cache();
            copy_comments_from(old_child);
            delete old_child;
            op = ctree_strptr;
            continue;
        }
        if ((op == ctree_strptr) && (N() == 1) &&
            (child(0)->op == ctree_addrof)) {
            assert(child(0)->N() == 1);
            ctree *old_child = child(0);
            replace_child(0, old_child->child(0));
            old_child->children = new ctree_list;
            old_child->flush_child_cache();
            copy_comments_from(old_child);
            delete old_child;
            op = ctree_strsel;
            continue;
        }
        if ((op == ctree_addrof) && (N() == 1) &&
            (child(0)->op == ctree_subscr) && (child(0)->N() == 2) &&
            (child(0)->child(1)->op == ctree_intconst) &&
            (child(0)->child(1)->integer_val() == 0)) {
            ctree *old_child = child(0);
            ctree *replacement = old_child->child(0);
            copy_comments_from(old_child->child(1));
            delete old_child->child(1);
            old_child->reset_children();
            swap(replacement);
            copy_comments_from(old_child);
            copy_comments_from(replacement);
            delete replacement;
            continue;
        }
        if ((op == ctree_not) && (N() == 1) &&
            (child(0)->op == ctree_not)) {
            assert(child(0)->N() == 1);
            ctree *old_child = child(0);
            ctree *replacement = old_child->child(0);
            old_child->reset_children();
            swap(replacement);
            copy_comments_from(old_child);
            copy_comments_from(replacement);
            delete replacement;
            continue;
        }
        return;
    }
}


/*
 * Get rid of unnecessary type casts.
 */
void ctree::fold_converts(void)
  {
    for (int child_num = 0; child_num < N(); ++child_num)
      {
        child(child_num)->fold_converts();

        boolean do_not_repeat = FALSE;

        if ((child(child_num)->op == ctree_conv) && child(child_num)->foldable)
          {
            assert(child(child_num)->N() == 1);
            type_node *implicit_type = child(child_num)->child(0)->expr_type();
            if ((op != ctree_addrof) && (op != ctree_postinc) &&
                (op != ctree_preinc) && (op != ctree_postdec) &&
                (op != ctree_predec) && (op != ctree_strsel) &&
                (((op != ctree_assign) && (op != ctree_addassign) &&
                  (op != ctree_subassign)) || (child_num != 0)))
              {
                implicit_type = implicit_type->unqual();
              }
            if (op != ctree_addrof)
                implicit_type = op_type_conv(implicit_type);
            boolean do_fold = FALSE;
            type_node *child_type = child(child_num)->type;
            if (implicit_type->is_same(child_type))
              {
                do_fold = TRUE;
              }
            else if (write_pseudo)
              {
                do_fold = FALSE;
              }
            else if ((op == ctree_vardef) ||
                     (((op == ctree_assign) || (op == ctree_addassign) ||
                       (op == ctree_subassign)) && (child_num != 0)))
              {
                do_fold = assign_convert_implicit(child_type, implicit_type);
                do_not_repeat = TRUE;
              }
            else if ((op == ctree_funcall) && (child_num != 0))
              {
                type_node *func_base = child(0)->expr_type()->unqual();
                if (func_base->is_ptr())
                  {
                    ptr_type *func_ptr = (ptr_type *)func_base;
                    func_base = func_ptr->ref_type()->unqual();
                  }
                assert(func_base->is_func());
                func_type *the_func_type = (func_type *)func_base;
                if (the_func_type->args_known())
                  {
                    do_fold =
                            assign_convert_implicit(child_type, implicit_type);
                    do_not_repeat = TRUE;
                  }
                else
                  {
                    do_fold = call_convert_implicit(child_type, implicit_type);
                  }
              }
            else if ((op == ctree_compl) || (op == ctree_neg) ||
                     (op == ctree_lshift) || (op == ctree_rshift))
              {
                do_fold = (integral_promotions(implicit_type)->is_same(
                                                    child_type));
              }
            else if (op == ctree_conv)
              {
                if (type->unqual()->is_ptr() && child_type->unqual()->is_ptr())
                  {
                    do_fold = TRUE;
                  }
                else if ((type->unqual()->op() == TYPE_FLOAT) &&
                         (child_type->unqual()->op() == TYPE_FLOAT) &&
                         (implicit_type->unqual()->op() == TYPE_FLOAT))
                  {
                    if (child_type->size() >= implicit_type->size())
                        do_fold = TRUE;
                  }
                else if ((type->unqual()->op() == TYPE_INT) &&
                         (child_type->unqual()->op() == TYPE_INT) &&
                         (implicit_type->unqual()->op() == TYPE_INT))
                  {
                    base_type *base_0 = (base_type *)(type->unqual());
                    base_type *base_1 = (base_type *)(child_type->unqual());
                    base_type *base_2 = (base_type *)(implicit_type->unqual());
                    boolean is_signed_0 = base_0->is_signed();
                    boolean is_signed_1 = base_1->is_signed();
                    boolean is_signed_2 = base_2->is_signed();
                    if (is_signed_1 == is_signed_2)
                      {
                        if (child_type->size() >= implicit_type->size())
                            do_fold = TRUE;
                      }
                    else if ((is_signed_0 == is_signed_2) && is_signed_1)
                      {
                        if (child_type->size() > implicit_type->size())
                            do_fold = TRUE;
                      }
                  }
              }
            if (do_fold)
              {
                ctree *this_child = child(child_num);
                replace_child(child_num, this_child->child(0));
                this_child->reset_children();
                copy_comments_from(this_child);
                delete this_child;
                if (!do_not_repeat)
                  {
                    /* now look at this child again */
                    --child_num;
                  }
              }
          }
      }
  }


/*
 * Find initialized symbols in vardefs and break out the data into
 * children of the vardef.
 */

void ctree::extract_data()
{
    if (op == ctree_vardef) {
        var_sym *vs = (var_sym*)leafval.y;
        if (vs->has_var_def() && vs->definition()->are_annotations()) {

            base_init_struct_list *init_list =
                    read_init_data(vs->definition());
            ctree *init_tree = build_initializers(vs->type(), init_list);
            deallocate_init_data(init_list);

            if (init_tree != NULL)
                addchild(init_tree);
        }
    } else {
        for (int i=0; i<N(); i++)
            child(i)->extract_data();
    }
}

void ctree::full_line_output_comments(io_class *out, int nindent)
{
    assert(comments != NULL);

    const char **follow = comments;
    assert(*follow != NULL);
    while (TRUE)
      {
        ctree_indent(out, nindent);
        output_a_comment(out, *follow);
        out->printf("\n");
        ++follow;
        if (*follow == NULL)
            break;
      }
}

void ctree::in_line_output_comments(io_class *out, int prefix_space,
                                    int suffix_space)
{
    assert(comments != NULL);

    int ind_i;
    for (ind_i = 0; ind_i < prefix_space; ++ind_i)
        out->printf(" ");

    const char **follow = comments;
    assert(*follow != NULL);
    while (TRUE)
      {
        output_a_comment(out, *follow);
        ++follow;
        if (*follow == NULL)
            break;
        out->printf(" ");
      }

    for (ind_i = 0; ind_i < suffix_space; ++ind_i)
        out->printf(" ");
}

void ctree::output_a_comment(io_class *out, const char *comment)
  {
    if (in_comment)
        out->printf("/ *");
    else
        out->printf("/*");
    out->printf("%s", comment);
    if (in_comment)
        out->printf("* /");
    else
        out->printf("*/");
  }

/*
 *  Truncate all strings with trailing zeros.  This is called by
 *  ctree::truncate_zeros_in_static_inits() only on static initializer
 *  trees.  It is legal in static initializers because the size is
 *  already determined by the definition and all parts not explicitly
 *  initialized are implicitly initialized to zero.  It is not legal
 *  in general for string literals in executable expressions, because
 *  it shortens the length of the memory area reserved for the string.
 */
void ctree::truncate_all_zeros(void)
  {
    if (getop() == ctree_strconst)
      {
        const char *old_string = leafval.s;
        const char *new_end = old_string;
        const char *follow = new_end;
        while (*follow != 0)
          {
            if (*follow == '\\')
              {
                if ((follow[1] == '0') && (follow[2] == '0') &&
                    (follow[3] == '0'))
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

        if (follow != new_end)
          {
            char *temp_string = new char[(new_end - old_string) + 1];
            strncpy(temp_string, old_string, (new_end - old_string));
            temp_string[new_end - old_string] = 0;
            leafval.s = lexicon->enter(temp_string)->sp;
            delete[] temp_string;
          }
      }

    for (int child_num = 0; child_num < N(); ++child_num)
        child(child_num)->truncate_all_zeros();
  }

boolean ctree::remainder_is_blanks(int starting_point)
  {
    for (int child_num = starting_point; child_num < N(); ++child_num)
      {
        if ((child(child_num)->getop() != ctree_blank_line) &&
            (child(child_num)->getop() != ctree_pound_line))
          {
            return FALSE;
          }
      }
    return TRUE;
  }

/*
 * Do some basic logical simplications:
 *      !(! <>) -> <>
 *      !0 -> 1
 *      !1 -> 0
 *      !(<> relop <>) -> (<> !relop <>)
 *      <> == 0 -> !<>
 *      <> != 0 -> <>
 */

void ctree::log_simp()
{
    for (int i=0; i<N(); i++)
        child(i)->log_simp();

    int changed;
    do {
        changed = 0;
        if (op == ctree_not) {
            assert(N() == 1);
            if (child(0)->op == ctree_not) {
                assert(child(0)->N() == 1);
                ctree *old_child = child(0);
                ctree *replacement = old_child->child(0);
                old_child->reset_children();
                swap(replacement);
                copy_comments_from(old_child);
                copy_comments_from(replacement);
                delete replacement;
                changed = 1;
                continue;
            } else if (child(0)->op == ctree_intconst) {
                ctree *old_child = child(0);
                reset_children();
                swap(old_child);
                copy_comments_from(old_child);
                delete old_child;
                set_integer_val(!integer_val());
                changed = 1;
                continue;
            } else {
                ctree_op newop = ctree_semi;
                switch (child(0)->op) {
                case ctree_lt:
                    newop = ctree_gte;
                    break;
                case ctree_lte:
                    newop = ctree_gt;
                    break;
                case ctree_gt:
                    newop = ctree_lte;
                    break;
                case ctree_gte:
                    newop = ctree_lt;
                    break;
                case ctree_eq:
                    newop = ctree_neq;
                    break;
                case ctree_neq:
                    newop = ctree_eq;
                    break;
                default:
                    break;
                }
                if (newop != ctree_semi) {
                    ctree *old_child = child(0);
                    reset_children();
                    swap(old_child);
                    copy_comments_from(old_child);
                    delete old_child;
                    op = newop;
                    changed = 1;
                    continue;
                }
            }
        }
        if (op == ctree_eq &&
            child(1)->op == ctree_intconst &&
            (child(1)->integer_val() == 0)) {
            op = ctree_not;
            ctree *newchild = child(0);
            copy_comments_from(child(1));
            delete child(1);
            reset_children();
            addchild(newchild);
            changed = 1;
            continue;
        }
        if (op == ctree_neq &&
            child(1)->op == ctree_intconst &&
            (child(1)->integer_val() == 0)) {
            ctree *old_child = child(0);
            copy_comments_from(child(1));
            delete(child(1));
            reset_children();
            swap(old_child);
            copy_comments_from(old_child);
            delete old_child;
            changed = 1;
            continue;
        }
    } while (changed);
}

/*
 * Detect ``?:'' operations:
 *
 *     if (<e1>)
 *         <var> = <e2>;
 *     else
 *         <var> = <e3>;
 *
 * becomes
 *
 *     <var> = <e1> ? <e2> : <e3>;
 *
 * (and if <e2> and <e3> are zero and one, this reduces further to
 *
 *     <var> = <e1>;
 *
 * or
 *
 *     <var> = !<e1>;
 *
 */

void ctree::cond_simp()
  {
    for (int i = 0; i < N(); i++)
        child(i)->cond_simp();

    if (op != ctree_if)
        return;
    if (N() != 3)
        return;
    ctree *then_block = child(1);
    ctree *else_block = child(2);
    if ((then_block->op != ctree_block) || (else_block->op != ctree_block) ||
        (then_block->N() != 1) || (else_block->N() != 1))
      {
        return;
      }
    ctree *then_semi = then_block->child(0);
    ctree *else_semi = else_block->child(0);
    if ((then_semi->op != ctree_semi) || (else_semi->op != ctree_semi) ||
        (then_semi->N() != 1) || (else_semi->N() != 1))
      {
        return;
      }
    ctree *then_assign = then_semi->child(0);
    ctree *else_assign = else_semi->child(0);
    if ((then_assign->op != ctree_assign) || (else_assign->op != ctree_assign))
        return;
    ctree *then_var_ref = then_assign->child(0);
    ctree *else_var_ref = else_assign->child(0);
    if (*then_var_ref != *else_var_ref)
        return;
    ctree *ifexp = child(0);
    ctree *ifval1 = then_assign->child(1);
    ctree *ifval2 = else_assign->child(1);
    ctree *newassign = new ctree(ctree_assign);
    newassign->addchild(then_var_ref);
    if ((ifval1->op == ctree_intconst) && (ifval1->integer_val() == 0) &&
        (ifval2->op == ctree_intconst) && (ifval2->integer_val() == 1))
      {
        ctree *cnot = new ctree(ctree_not);
        cnot->addchild(ifexp);
        newassign->addchild(cnot);
        newassign->copy_comments_from(ifval1);
        newassign->copy_comments_from(ifval2);
        delete ifval1;
        delete ifval2;
      }
    else if ((ifval1->op == ctree_intconst) && (ifval1->integer_val() == 1) &&
             (ifval2->op == ctree_intconst) && (ifval2->integer_val() == 0))
      {
        newassign->addchild(ifexp);
        newassign->copy_comments_from(ifval1);
        newassign->copy_comments_from(ifval2);
        delete ifval1;
        delete ifval2;
      }
    else
      {
        ctree *ter = new ctree(ctree_ternary);
        newassign->addchild(ter);
        ter->addchild(ifexp);
        ter->addchild(ifval1);
        ter->addchild(ifval2);
      }
    reset_children();
    swap(newassign);
    then_var_ref->copy_comments_from(else_var_ref);
    copy_comments_from(then_block);
    copy_comments_from(then_semi);
    copy_comments_from(then_assign);
    copy_comments_from(else_block);
    copy_comments_from(else_semi);
    copy_comments_from(else_assign);
    then_assign->reset_children();
    else_assign->reset_children();
    delete newassign;
    delete then_block;
    delete else_block;
    delete else_var_ref;
  }

/*
 *  Truncate strings with trailing zeros in static initializers.
 */
void ctree::truncate_zeros_in_static_inits(void)
  {
    if (getop() == ctree_vardef)
      {
        assert(leafval.y->is_var());
        var_sym *the_var = (var_sym *)(leafval.y);
        if ((!the_var->is_auto()) && (N() == 1))
            child(0)->truncate_all_zeros();
      }
    else
      {
        for (int child_num = 0; child_num < N(); ++child_num)
            child(child_num)->truncate_zeros_in_static_inits();
      }
  }

/*
 * Flatten the tree a little.  If a tree has the same op as one of its
 * children, and the operator commutes, merge the child's children in
 * with the current node's children (got that?).  Also do this for
 * blocks containing only another block.
 */

void ctree::flatten(boolean in_initializer)
{
    boolean changed;
    do {
        changed = FALSE;
        ctree_list *newchildren = new ctree_list;
        for (int i=0; i<N(); i++) {
            if ((op == child(i)->op && ctree_oper_attr[op].commutes) ||
                ((!in_initializer) && op == ctree_block && N() == 1 &&
                 child(i)->op == ctree_block) ||
                ((!in_initializer) && op == ctree_block &&
                 child(i)->op == ctree_semi && child(i)->N() == 1 &&
                 grandchild(i,0)->op == ctree_block)) {
                changed = TRUE;
                for (int j=0; j<child(i)->N(); j++)
                    newchildren->append(child(i)->child(j));
                child(i)->reset_children();
                copy_comments_from(child(i));
                delete child(i);
            }
            else
                newchildren->append(child(i));
        }
        if (changed) {
            delete children;
            children = newchildren;
            flush_child_cache();
        } else
            delete newchildren;
    } while (changed);

    boolean children_in_initializer = (in_initializer || (op == ctree_vardef));
    for (int i=0; i<N(); i++)
        child(i)->flatten(children_in_initializer);
}

void ctree::try_const_div(int amount, boolean *folded)
  {
    if (amount == 0)
        return;

    switch (getop())
      {
        case ctree_intconst:
          {
            if (integer_val() % amount == 0)
              {
                set_integer_val(integer_val() / amount);
                *folded = TRUE;
              }
            break;
          }
        case ctree_mult:
          {
            for (int i = 0; i < N(); ++i)
              {
                child(i)->try_const_div(amount, folded);
                if (*folded)
                  {
                    if ((child(i)->getop() == ctree_intconst) &&
                        (child(i)->integer_val() == 1))
                      {
                        ctree *this_child = child(i);
                        ctree_list_e *list_e = children->lookup(this_child);
                        assert(list_e != NULL);
                        children->remove(list_e);
                        flush_child_cache();
                        delete list_e;
                        copy_comments_from(this_child);
                        delete this_child;
                      }
                    break;
                  }
              }
            break;
          }
        default:
            break;
      }
    return;
  }

/*
 * This function does the implicit conversion specified in ANSI/ISO 9899-1990
 * section 6.2.2.1, paragraphs three and four: it converts ``array of x'' to
 * ``pointer to x'' and ``<function type y>'' to
 * ``pointer to <function type y>''.
 */
static type_node *op_type_conv(type_node *the_type)
  {
    if (the_type->is_array())
      {
        array_type *the_array = (array_type *)the_type;
        type_node *elem_type = the_array->elem_type();
        while (elem_type->unqual()->is_array() && (elem_type->size() == 0))
          {
            the_array = (array_type *)(elem_type->unqual());
            elem_type = the_array->elem_type();
          }
        return elem_type->ptr_to();
      }
    else if (the_type->is_func())
      {
        return the_type->ptr_to();
      }
    else
      {
        return the_type;
      }
  }

/*
 * This function does the implicit ``usual arithmetic conversions'' specified
 * in ANSI/ISO 9899-1990 section 6.2.1.5.
 */
static type_node *usual_arith_conv(type_node *type_1, type_node *type_2)
  {
    enum C_types c_type1 = get_c_arith_type(type_1);
    enum C_types c_type2 = get_c_arith_type(type_2);
    if ((c_type1 == num_C_types) || (c_type2 == num_C_types))
        return type_1;

    base_type *base_1 = (base_type *)type_1;
    base_type *base_2 = (base_type *)type_2;

    boolean is_signed_1 = base_1->is_signed();
    boolean is_signed_2 = base_2->is_signed();

    if ((c_type1 == C_longdouble) || (c_type2 == C_longdouble))
        return c_type_to_suif(C_longdouble);
    if ((c_type1 == C_double) || (c_type2 == C_double))
        return c_type_to_suif(C_double);
    if ((c_type1 == C_float) || (c_type2 == C_float))
        return c_type_to_suif(C_float);

    c_type1 = c_int_promotions(c_type1, &is_signed_1);
    c_type2 = c_int_promotions(c_type2, &is_signed_2);

    if (((c_type1 == C_long) && !is_signed_1) ||
        ((c_type2 == C_long) && !is_signed_2))
      {
        return c_type_to_suif(C_long, FALSE);
      }
    if (((c_type1 == C_long) || (c_type2 == C_long)) &&
        ((!is_signed_1) || (!is_signed_2)))
      {
        if (target.size[C_long] > target.size[C_int])
            return c_type_to_suif(C_long, TRUE);
        else
            return c_type_to_suif(C_long, FALSE);
      }
    if ((c_type1 == C_long) || (c_type2 == C_long))
        return c_type_to_suif(C_long, TRUE);
    if ((!is_signed_1) || (!is_signed_2))
        return c_type_to_suif(C_int, FALSE);
    return c_type_to_suif(C_int, TRUE);
  }

/*
 * The following two functions do the ``integral promotions'' specified in
 * ANSI/ISO 9899-1990 section 6.2.1.1.
 */
static type_node *integral_promotions(type_node *the_type)
  {
    if ((the_type->op() != TYPE_INT) && (the_type->op() != TYPE_ENUM))
        return the_type;
    if (c_int_type(the_type) == num_C_types)
      {
        static boolean warned = FALSE;
        mistake(&warned, the_type,
                "integral type does not match any integral types on the "
                "target machine");
        return the_type;
      }
    base_type *the_base = (base_type *)the_type;
    boolean is_signed = the_base->is_signed();
    enum C_types c_type = c_int_promotions(c_int_type(the_type), &is_signed);
    return c_type_to_suif(c_type, is_signed);
  }

static enum C_types c_int_promotions(enum C_types c_type, boolean *is_signed)
  {
    if ((c_type == C_char) || (c_type == C_short))
      {
        if (target.size[c_type] < target.size[C_int])
            *is_signed = TRUE;
        return C_int;
      }
    return c_type;
  }

static boolean assign_convert_implicit(type_node *dest_type,
                                       type_node *source_type)
  {
    type_node *dest_unqual = dest_type->unqual();
    type_node *source_unqual = source_type->unqual();

    if (((dest_unqual->op() == TYPE_INT) || (dest_unqual->op() == TYPE_ENUM) ||
         (dest_unqual->op() == TYPE_FLOAT)) &&
        ((source_unqual->op() == TYPE_INT) ||
         (source_unqual->op() == TYPE_ENUM) ||
         (source_unqual->op() == TYPE_FLOAT)))
      {
        return TRUE;
      }
    if (dest_unqual->is_ptr() && source_unqual->is_ptr())
      {
        ptr_type *dest_ptr = (ptr_type *)dest_unqual;
        ptr_type *source_ptr = (ptr_type *)source_unqual;
        if ((source_ptr->ref_type()->is_const() &&
             !dest_ptr->ref_type()->is_const()) ||
            (source_ptr->ref_type()->is_volatile() &&
             !dest_ptr->ref_type()->is_volatile()))
          {
            return FALSE;
          }
        type_node *dest_ref = dest_ptr->ref_type()->unqual();
        type_node *source_ref = source_ptr->ref_type()->unqual();
        if ((dest_ref->op() == TYPE_VOID) || (source_ref->op() == TYPE_VOID))
            return TRUE;
        return (composite(dest_ref, source_ref) != NULL);
      }
    return FALSE;
  }

static boolean call_convert_implicit(type_node *dest_type,
                                     type_node *source_type)
  {
    if (source_type->op() == TYPE_INT)
      {
        if (dest_type->op() == TYPE_INT)
          {
            base_type *source_base = (base_type *)source_type;
            base_type *dest_base = (base_type *)dest_type;
            if ((dest_base->size() == type_signed->size()) &&
                dest_base->is_signed())
              {
                if (source_base->size() < dest_base->size())
                    return TRUE;
              }
          }
      }
    else if (source_type->op() == TYPE_FLOAT)
      {
        if (dest_type->op() == TYPE_FLOAT)
          {
            if ((source_type->size() == type_float->size()) &&
                (dest_type->size() == type_double->size()))
              {
                return TRUE;
              }
          }
      }
    return FALSE;
  }

static enum C_types get_c_arith_type(type_node *the_type)
  {
    if ((the_type->op() == TYPE_INT) || (the_type->op() == TYPE_ENUM))
        return c_int_type(the_type);
    else if (the_type->op() == TYPE_FLOAT)
        return c_float_type(the_type);

    static boolean warned = FALSE;
    mistake(&warned, the_type, "arithmetic type expected");
    return num_C_types;
  }
