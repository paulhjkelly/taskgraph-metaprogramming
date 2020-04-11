/*  High-Level Type Classes */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef TYPES_H
#define TYPES_H

#pragma interface

RCS_HEADER(types_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class var_sym;
class annote;
class modifier_type;
class ptr_type;
struct replacements;


enum type_ops {
    TYPE_INT, TYPE_FLOAT, TYPE_VOID,	  /* base_type */
    TYPE_PTR,				  /* ptr_type */
    TYPE_ARRAY,				  /* array_type */
    TYPE_FUNC,				  /* func_type */
    TYPE_GROUP, TYPE_STRUCT, TYPE_UNION,  /* struct_type */
    TYPE_ENUM,				  /* enum_type */
    /* modifier_types: */
    TYPE_CONST,				  /* const modifier */
    TYPE_VOLATILE,			  /* volatile modifier */
    TYPE_CALL_BY_REF,			  /* call-by-reference modifier */
    TYPE_RESTRICT,			  /* restrict modifier */
    TYPE_NULL				  /* null modifier */
};


/*
 *  A type_node represents a high-level type.  The base class cannot be
 *  directly instantiated but provides functions shared by all of the
 *  derived classes.
 *
 *  Each type_node has an ID number and a back pointer to the symtab which
 *  contains it.  The ID numbers for types within the global_symtab are
 *  unique, but the IDs for types in a file_symtab are only unique within the
 *  file.  Local types have unique ID numbers within procedures (i.e. types in
 *  separate procedures may have the same ID number but types in different
 *  scopes of the same procedure will always have different numbers).
 *  
 *  Although methods are provided for modifying the types, these should only
 *  be used before a type_node is added to a symtab.  Otherwise, duplicate
 *  types may be created in the same symtab (that is not a fatal problem but
 *  is better avoided).  There are three different print methods.  The
 *  "print_abbrev" method is used when printing the result_types of
 *  instructions.  It prints the type ID along with a single character to
 *  identify the type operator followed by a period and the size (e.g.
 *  "i.32" for a 32-bit integer).  The "print" method shows the ID number
 *  in the form "type(ID)".  The "print_full" method prints all of the type
 *  information and is used when listing symbol tables.  The "is_same" method
 *  (and operator==) compares the type_node with another and returns TRUE if
 *  they are the same.  We assume that named types use name equivalence, and
 *  thus struct_types and enum_types are only the same if they are the same
 *  object.
 */

class type_node : public suif_object {
    friend class base_symtab;
    friend class global_symtab;
    friend class file_symtab;
    friend class block_symtab;

private:
    type_ops oper;			/* the type operator */
    base_symtab *table;			/* symtab containing this type_node */
    unsigned id;			/* identifier used for i/o */

    void set_parent(base_symtab *st)	{ table = st; }

protected:
    type_node();

    void set_type_id(unsigned i)	{ id = i; }
    void set_op(type_ops o)		{ oper = o; }
    virtual void print_helper(FILE *, int /* depth */) { }
    boolean is_same_annotes(type_node *t);

    static type_node *scan_from_annote(annote *an);
    virtual void cvt_from_annote(annote *an, base_symtab *symtab);
    virtual annote *cvt_to_annote() = 0;
    void cvt_to_annote_base(annote *an);

    virtual type_node *clone_internal(void) = 0;

public:
    virtual ~type_node()		{ }

    object_kinds object_kind()		{ return TYPE_OBJ; }

    type_ops op()			{ return oper; }
    base_symtab *parent()		{ return table; }
    unsigned type_id()			{ return id; }
    virtual int size() = 0;		/* return the size in bits */

    void clear_type_id();

    boolean is_modifier()		{ return ((op() == TYPE_CONST) ||
						  (op() == TYPE_VOLATILE) ||
						  (op() == TYPE_CALL_BY_REF) ||
						  (op() == TYPE_NULL) ||
						  (op() == TYPE_RESTRICT)); }
    boolean is_base()			{ return ((op() == TYPE_INT) ||
						  (op() == TYPE_FLOAT) ||
						  (op() == TYPE_VOID) ||
						  (op() == TYPE_ENUM)); }
    boolean is_ptr()			{ return (op() == TYPE_PTR); }
    boolean is_array()			{ return (op() == TYPE_ARRAY); }
    boolean is_func()			{ return (op() == TYPE_FUNC); }
    boolean is_struct()			{ return ((op() == TYPE_GROUP) ||
						  (op() == TYPE_STRUCT) ||
						  (op() == TYPE_UNION)); }
    boolean is_enum()			{ return (op() == TYPE_ENUM); }
    boolean is_named();			/* struct, union, or enum */
    boolean is_scalar();

    virtual boolean is_const()		{ return FALSE; }
    virtual boolean is_volatile()	{ return FALSE; }
    virtual boolean is_call_by_ref()	{ return FALSE; }
    virtual boolean is_restrict()	{ return FALSE; }

    virtual unsigned num_ref_types()	{ return 0; }
    virtual type_node *ref_type(unsigned /* num */)
      { assert(FALSE); return NULL; }
    virtual void set_ref_type(unsigned /* num */, type_node * /* new_type */)
      { assert(FALSE); }

    modifier_type *find_modifier(type_ops mod);

    virtual type_node *unqual()		{ return this; }
    ptr_type *ptr_to(void);		/* installed pointer to this type */

    virtual boolean is_same(type_node *t);
    virtual boolean compatible(type_node *t) = 0;
    boolean operator==(type_node &t)    { return is_same(&t); }
    boolean operator!=(type_node &t)    { return !((*this)==t); }

    virtual type_node *copy() = 0;	/* make a copy of this type_node */

    type_node *clone(void)		{ return clone_internal(); }

    type_node *clone_helper(replacements *r);

    void print_abbrev(FILE *f=stdout);
    void print(FILE *f=stdout);
    void print_full(FILE *f=stdout, int depth=0);

    static type_node *read(in_stream *is, base_symtab *symtab);
    void write(out_stream *os);
    void write_check(void);
};

DECLARE_DLIST_CLASS(type_node_list, type_node*);


/*
 *  Modifier types are used to add various attributes to other types.
 *  Each attribute is represented by a different type operator.  The
 *  "unqual" method skips over any modifier types and returns the
 *  unqualified base type.  The meanings of the TYPE_CONST and TYPE_VOLATILE
 *  modifiers should be obvious.  The TYPE_CALL_BY_REF modifier is used to
 *  indicate a parameter that is passed by reference -- see the code to
 *  convert to call-by-reference form for more details.  The TYPE_NULL
 *  modifier has no effect on the type but simply provides a place to
 *  attach annotations; this is needed for named types because copying
 *  them would create completely new types.
 */

class modifier_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    type_node *typ;

    modifier_type() : typ(0)			{ }

protected:
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);
    boolean is_same_helper(type_ops mod, type_node *t);

    type_node *clone_internal(void);

public:
    modifier_type(type_ops o,		/* modifier operator */
		  type_node *t);	/* base type */

    type_node *base()			{ return typ; }
    void set_base(type_node *t)		{ typ = t; }

    int size();

    boolean is_const();
    boolean is_volatile();
    boolean is_call_by_ref();
    boolean is_restrict();

    virtual unsigned num_ref_types()	{ return 1; }
    virtual type_node *ref_type(unsigned num);
    virtual void set_ref_type(unsigned num, type_node *new_type);

    type_node *unqual();

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    modifier_type *clone(void);
};


/*
 *  Base types are used for the TYPE_INT, TYPE_FLOAT, and TYPE_VOID type
 *  operators.  The only added fields are the size (in bits) and a flag
 *  to indicate if INTs are signed.
 */

class base_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    int sz;
    boolean sgn;

protected:
    base_type() : sz(0), sgn(FALSE)		{ }

    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    virtual type_node *clone_internal(void);

public:
    base_type(type_ops o,		/* type operator: INT, FLOAT, VOID */
	      int s,			/* size in bits */
	      boolean b = TRUE);	/* is it signed? (INTs only) */

    int size()				{ return sz; }
    boolean is_signed()			{ return sgn; }

    void set_size(int s)		{ sz = s; }
    void set_signed(boolean b)		{ sgn = b; }

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    base_type *clone(void);
};


/*
 *  Pointer types are used for the TYPE_PTR type operator.  The size of
 *  a pointer is the same throughout the system; it cannot be set for a
 *  specific type.
 */

class ptr_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    type_node *ref;

    ptr_type() : ref(0)			{ }

protected:
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    type_node *clone_internal(void);

public:
    ptr_type(type_node *r);		/* referent type */

    int size()				{ return target.size[C_ptr]; }
    type_node *ref_type()		{ return ref; }
    void set_ref_type(type_node *r)	{ ref = r; }

    virtual unsigned num_ref_types()	{ return 1; }
    virtual type_node *ref_type(unsigned num);
    virtual void set_ref_type(unsigned num, type_node *new_type);

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    ptr_type *clone(void);
};


/*
 *  Array types are used for the TYPE_ARRAY type operator.  They include the
 *  lower and upper bounds and the element type.  Multi-dimensional arrays are
 *  treated as arrays of arrays.  Each array bound may be either a constant
 *  integer, a variable, or the special value "unknown_bound".  If the lower
 *  bound is unknown, the upper bound must also be unknown.  If the bounds are
 *  both constant, the size of the entire array is computed from the size of
 *  the elements; this assumes that all structures are padded to tile arrays
 *  without violating alignment restrictions (guaranteed for ANSI C).
 */

class array_bound {
    friend class array_type;

private:
    boolean is_cnst;
    union {
	int cnst;
	var_sym *var;
    } u;

protected:
    array_bound(annote *an, base_symtab *symtab);
    void add_to_annote(annote *an);

public:
    array_bound() : is_cnst(0) { u.var = NULL; }
    array_bound(int c) : is_cnst(TRUE)  {  u.cnst = c; }
    array_bound(var_sym *v) : is_cnst(FALSE)  { u.var = v; }
    array_bound(const array_bound &b) : is_cnst(b.is_cnst), u(b.u) { }

    boolean is_constant() const		{ return is_cnst; }
    boolean is_variable() const		{ return (!is_cnst && u.var); }
    boolean is_unknown() const		{ return (!is_cnst && !u.var); }

    int constant() const;
    var_sym *variable() const;

    array_bound& operator=(const array_bound &b);
    boolean operator==(const array_bound &b);
    boolean operator!=(const array_bound &b)	{ return !(*this == b); }

    void print(FILE *f=stdout);
};

EXPORTED_BY_SUIF const array_bound unknown_bound;


class array_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    type_node *elemt;
    array_bound low;
    array_bound uppr;

    array_type() : elemt(0), low(), uppr()			{ }

protected:
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    type_node *clone_internal(void);

public:
    array_type(type_node *elem,			/* element type */
	       array_bound lb = unknown_bound,	/* lower bound */
	       array_bound ub = unknown_bound);	/* upper bound */

    type_node *elem_type()		{ return elemt; }
    array_bound lower_bound()		{ return low; }
    array_bound upper_bound()		{ return uppr; }
    boolean are_bounds_unknown();

    void set_elem_type(type_node *t)	{ elemt = t; }
    void set_lower_bound(array_bound b)	{ low = b; }
    void set_upper_bound(array_bound b) { uppr = b; }

    int size();

    virtual unsigned num_ref_types()	{ return 1; }
    virtual type_node *ref_type(unsigned num);
    virtual void set_ref_type(unsigned num, type_node *new_type);

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    array_type *clone(void);
};


/*
 *  Function types are used for the TYPE_FUNC type operator.  They
 *  include the return type and an array of types for the arguments.
 *  The number of arguments may be changed at any time; if necessary,
 *  additional space will be allocated.
 */

class func_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    type_node *ret;
    unsigned nargs;
    type_node **argts;
    boolean vargs;
    boolean arg_info;

    func_type()	: ret(0), nargs(0), argts(0), vargs(FALSE), arg_info(FALSE)
	{ }

protected:
    void check_range(unsigned n);
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    type_node *clone_internal(void);

public:
    func_type(type_node *r);		/* return type (assume args unknown) */
    func_type(boolean varargs,		/* does it have varargs? */
	      type_node *r);		/* return type (assume zero args) */
    func_type(type_node *r,		/* return type */
	      unsigned n,		/* number of arguments */
	      boolean varargs = FALSE,	/* does it have varargs? */
	      boolean know_args = TRUE); /* are argument types known? */
    func_type(type_node *r,		/* return type */
	      type_node *arg,		/* argument */
	      boolean varargs = FALSE);	/* does it have varargs? */
    func_type(type_node *r,		/* return type */
	      type_node *arg1,		/* argument 1 */
	      type_node *arg2,		/* argument 2 */
	      boolean varargs = FALSE);	/* does it have varargs? */
    func_type(type_node *r,		/* return type */
	      type_node *arg1,		/* argument 1 */
	      type_node *arg2,		/* argument 2 */
	      type_node *arg3,		/* argument 3 */
	      boolean varargs = FALSE);	/* does it have varargs? */
    func_type(type_node *r,		/* return type */
	      type_node *arg1,		/* argument 1 */
	      type_node *arg2,		/* argument 2 */
	      type_node *arg3,		/* argument 3 */
	      type_node *arg4,		/* argument 4 */
	      boolean varargs = FALSE);	/* does it have varargs? */
    func_type(type_node *r,		/* return type */
	      type_node *arg1,		/* argument 1 */
	      type_node *arg2,		/* argument 2 */
	      type_node *arg3,		/* argument 3 */
	      type_node *arg4,		/* argument 4 */
	      type_node *arg5,		/* argument 5 */
	      boolean varargs = FALSE);	/* does it have varargs? */

    ~func_type();

    int size()				{ return 0; }
    type_node *return_type()		{ return ret; }
    boolean args_known()		{ return arg_info; }
    unsigned num_args()			{ return nargs; }
    type_node *arg_type(unsigned n)	{ check_range(n); return argts[n]; }
    boolean has_varargs()		{ return vargs; }

    void set_return_type(type_node *r)	{ ret = r; }
    void set_num_args(unsigned n);
    void set_args_known()		{ arg_info = TRUE; }
    void set_args_unknown();
    void set_arg_type(unsigned n, type_node *t);
    void set_varargs(boolean b)		{ if (b) set_args_known(); vargs = b; }

    virtual unsigned num_ref_types()	{ return 1 + nargs; }
    virtual type_node *ref_type(unsigned num);
    virtual void set_ref_type(unsigned num, type_node *new_type);

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    func_type *clone(void);
};


/*
 *  Struct types are used for the TYPE_GROUP, TYPE_STRUCT, and
 *  TYPE_UNION type operators.  They include the type name and for
 *  each field, the field name, field type, and offset.  The total
 *  size is not determined by the component types -- it may be set to
 *  anything so long as it is big enough to cover all the components.
 *  This allows extra trailing padding for alignment restrictions,
 *  unnamed bit fields, or for any other reason that it might be
 *  convenient.  For TYPE_GROUPs, there is no limitation on the
 *  positions or overlapping of fields.  TYPE_STRUCTs and TYPE_UNIONs
 *  are used for special cases where the fields and their offsets meet
 *  certain conditions.  For TYPE_STRUCTs, the fields must be in order
 *  of increasing offsets and may not overlap at all.  The field
 *  offsets for TYPE_UNION types should all be zero.  Note that
 *  anything that would be legal as a TYPE_UNION or TYPE_STRUCT would
 *  also be legal as a TYPE_GROUP.  The "find_field_by_name" method
 *  can be used to lookup a particular field.  A
 *  "find_field_by_offset" method is also provided, though if more
 *  than one field has the same offset, an arbitrary one of them will
 *  be returned.  Hence the result will always be unique for
 *  TYPE_STRUCT, sometimes for TYPE_GROUP, and never for a TYPE_UNION
 *  with more than a single member.  The number of fields may be
 *  changed at any time; if necessary, additional space will be
 *  allocated internally to keep the field information.
 */

class struct_type : public type_node {
    friend class type_node;
    friend class base_symtab;

private:
    const char *my_name;
    int sz;
    unsigned ntypes;
    type_node **types;
    const char **names;
    int *offsets;

    struct_type() : my_name(0), sz(0), ntypes(0), types(0), names(0),
	offsets(0) { }

protected:
    void check_range(unsigned n);
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    type_node *clone_internal(void);

public:
    struct_type(type_ops o,		/* one of TYPE_GROUP, TYPE_STRUCT, */
					/* or TYPE_UNION */
		int s,			/* total size in bits */
		const char *nm,		/* name of this struct */
		unsigned n);		/* number of fields */
    ~struct_type();

    int size()				{ return sz; }
    const char *name()			{ return my_name; }
    unsigned num_fields()		{ return ntypes; }
    const char *field_name(unsigned n)	{ check_range(n); return names[n]; }
    type_node *field_type(unsigned n)	{ check_range(n); return types[n]; }
    int offset(unsigned n)		{ check_range(n); return offsets[n]; }

    void set_size(int s)		{ sz = s; }
    void set_name(const char *nm);
    void set_num_fields(unsigned n);
    void set_field_name(unsigned n, const char *nm);
    void set_field_type(unsigned n, type_node *t);
    void set_offset(unsigned n, int o);

    unsigned find_field_by_offset(int off, int& left);
    unsigned find_field_by_name(const char *nm);

    virtual unsigned num_ref_types()	{ return ntypes; }
    virtual type_node *ref_type(unsigned num);
    virtual void set_ref_type(unsigned num, type_node *new_type);

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    struct_type *clone(void);

    // hack added for convertsuif2to1, not recommended for general use:
    void set_to_union();
    void set_to_struct();
    void set_to_group();
};


/*
 *  Enum types are used for the TYPE_ENUM type operator.  They contain the
 *  name of the enumerated type and arrays of the names and values for the
 *  members of the enumeration.  The number of members may be changed at any
 *  time; if necessary, additional space will be allocated.
 */

class enum_type : public base_type {
    friend class type_node;
    friend class base_symtab;

private:
    const char *my_name;
    unsigned nvals;
    const char **names;
    int *vals;

    enum_type()	: my_name(0), nvals(0), names(0), vals(0) { }

protected:
    void check_range(unsigned n);
    void print_helper(FILE *f, int depth);
    annote *cvt_to_annote();
    void cvt_from_annote(annote *an, base_symtab *symtab);

    type_node *clone_internal(void);

public:
    enum_type(const char *nm,		/* name of this enumeration */
	      int s,			/* size in bits */
	      boolean b,		/* is it signed? */
	      unsigned n);		/* number of values */
    ~enum_type();

    const char *name()			{ return my_name; }
    unsigned num_values()		{ return nvals; }
    const char *member(unsigned n)	{ check_range(n); return names[n]; }
    int value(unsigned n)		{ check_range(n); return vals[n]; }

    void set_name(const char *nm);
    void set_num_values(unsigned n);
    void set_member(unsigned n, const char *nm);
    void set_value(unsigned n, int v);

    unsigned find_member_by_value(int v);
    unsigned find_member_by_name(const char *nm);

    boolean is_same(type_node *t);
    boolean compatible(type_node *t);
    type_node *copy();
    enum_type *clone(void);
};


/*  Predefined types */
EXPORTED_BY_SUIF type_node *type_error;		/* error type */
EXPORTED_BY_SUIF type_node *type_v0;		/* void */
EXPORTED_BY_SUIF type_node *type_s8;		/* signed ints */
EXPORTED_BY_SUIF type_node *type_s16;
EXPORTED_BY_SUIF type_node *type_s32;
EXPORTED_BY_SUIF type_node *type_s64;
EXPORTED_BY_SUIF type_node *type_u8;		/* unsigned ints */
EXPORTED_BY_SUIF type_node *type_u16;
EXPORTED_BY_SUIF type_node *type_u32;
EXPORTED_BY_SUIF type_node *type_u64;
EXPORTED_BY_SUIF type_node *type_f32;		/* floats */
EXPORTED_BY_SUIF type_node *type_f64;
EXPORTED_BY_SUIF type_node *type_f128;

/*  Portable types */
EXPORTED_BY_SUIF type_node *type_void;
EXPORTED_BY_SUIF type_node *type_ptr;
EXPORTED_BY_SUIF type_node *type_char;
EXPORTED_BY_SUIF type_node *type_signed_char;
EXPORTED_BY_SUIF type_node *type_unsigned_char;
EXPORTED_BY_SUIF type_node *type_signed_short;
EXPORTED_BY_SUIF type_node *type_unsigned_short;
EXPORTED_BY_SUIF type_node *type_signed; /* int */
EXPORTED_BY_SUIF type_node *type_unsigned; /* int */
EXPORTED_BY_SUIF type_node *type_signed_long;
EXPORTED_BY_SUIF type_node *type_unsigned_long;
EXPORTED_BY_SUIF type_node *type_signed_longlong;
EXPORTED_BY_SUIF type_node *type_unsigned_longlong;
EXPORTED_BY_SUIF type_node *type_ptr_diff; /* always a signed integer type */
EXPORTED_BY_SUIF type_node *type_float;
EXPORTED_BY_SUIF type_node *type_double;
EXPORTED_BY_SUIF type_node *type_longdouble;

#endif /* TYPES_H */
