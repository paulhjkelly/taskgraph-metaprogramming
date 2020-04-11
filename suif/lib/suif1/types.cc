/*  Implementation of High-Level Types */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "types.h"

#define RCS_BASE_FILE types_cc

#include "suif1.h"
#include "suif_internal.h"

RCS_BASE(
    "$Id$")


/*  predefined types */
type_node *type_error = NULL;
type_node *type_v0 = NULL;
type_node *type_s8 = NULL;
type_node *type_s16 = NULL;
type_node *type_s32 = NULL;
type_node *type_s64 = NULL;
type_node *type_u8 = NULL;
type_node *type_u16 = NULL;
type_node *type_u32 = NULL;
type_node *type_u64 = NULL;
type_node *type_f32 = NULL;
type_node *type_f64 = NULL;
type_node *type_f128 = NULL;

/*  portable types */
type_node *type_void = NULL;
type_node *type_ptr = NULL;
type_node *type_char = NULL;
type_node *type_signed_char = NULL;
type_node *type_unsigned_char = NULL;
type_node *type_signed_short = NULL;
type_node *type_unsigned_short = NULL;
type_node *type_signed = NULL; /* int */
type_node *type_unsigned = NULL; /* int */
type_node *type_signed_long = NULL;
type_node *type_unsigned_long = NULL;
type_node *type_signed_longlong = NULL;
type_node *type_unsigned_longlong = NULL;
type_node *type_ptr_diff = NULL;
type_node *type_float = NULL;
type_node *type_double = NULL;
type_node *type_longdouble = NULL;


type_node::type_node ()
    : oper(TYPE_VOID), table(0), id(0)
{
}


void
type_node::clear_type_id ()
{
    if (parent() != NULL)
        parent()->register_type_id_change(this, 0);
    set_type_id(0);
}


/*
 *  Check if a type is a group, a structure, a union, or an enumeration.
 *  These are all of the named type classes.
 */

boolean
type_node::is_named ()
{
    if (op() == TYPE_GROUP) return TRUE;
    if (op() == TYPE_STRUCT) return TRUE;
    if (op() == TYPE_UNION) return TRUE;
    if (op() == TYPE_ENUM) return TRUE;
    return FALSE;
}


/*
 *  Check if a type is scalar.  This rules out everything except integers,
 *  floats, pointers, and enumerations.
 */

boolean
type_node::is_scalar ()
{
    type_node *t = unqual();
    if ((t->op() == TYPE_INT) ||
	(t->op() == TYPE_FLOAT) ||
	(t->op() == TYPE_PTR) ||
	(t->op() == TYPE_ENUM)) {
	return TRUE;
    }
    return FALSE;
}


/*
 *  Compare the base class fields for two type_nodes.  This is not called
 *  directly by users but may be used in the derived "is_same" methods.
 */

boolean
type_node::is_same (type_node *t)
{
    if (!t) return FALSE;
    if (op() != t->op()) return FALSE;
    if (!is_same_annotes(t)) return FALSE;
    return TRUE;
}


/*
 *  Check if another type node has the same annotations.  Structured
 *  annotations are automatically flattened so they can be compared.
 *  Unregistered annotations are ignored.  Note that this method assumes
 *  that the annotation order is significant.
 */

boolean
type_node::is_same_annotes (type_node *t)
{
    /* compare the annotations */
    if (are_annotations() != t->are_annotations()) return FALSE;
    if (!are_annotations()) return TRUE;
    if (annotes()->count() != t->annotes()->count()) return FALSE;

    annote_list_iter anli(annotes());
    annote_list_iter t_anli(t->annotes());
    while (!anli.is_empty()) {
	assert(!t_anli.is_empty());

	annote *an = anli.step();
	annote *t_an = t_anli.step();

	/* compare the annotation names */
	if (an->name() != t_an->name()) return FALSE;

	immed_list *iml = an->immeds();
	immed_list *t_iml = t_an->immeds();

	/* ignore unregistered annotations */
	if (!iml) continue;

	boolean no_diff = TRUE;

	/* check if the immed lists are the same length */
	if (iml->count() != t_iml->count()) {
	    no_diff = FALSE;
	} else {
	    /* compare the immeds */
	    immed_list_iter imli(iml);
	    immed_list_iter t_imli(t_iml);
	    while (!imli.is_empty()) {
		assert(!t_imli.is_empty());
		if (imli.step() != t_imli.step()) {
		    no_diff = FALSE;
		    break;
		}
	    }
	}

	if (an->is_structured()) {
	    delete iml;
	    delete t_iml;
	}

	if (!no_diff) return FALSE;
    }

    return TRUE;
}


/*
 *  Try to find the specified modifier type.  Return NULL if it does
 *  not exist.
 */

modifier_type *
type_node::find_modifier (type_ops mod)
{
    if (!is_modifier()) return NULL;

    modifier_type *mt = (modifier_type *)this;
    if (op() == mod) return mt;
    return mt->base()->find_modifier(mod);
}


/*
 *  Return a type which is a pointer to this type.  If this type is
 *  installed, the pointer type will be installed in the same place.
 *  If this type is not installed, the pointer type will not be
 *  installed.
 */
ptr_type *type_node::ptr_to(void)
{
    ptr_type *result = new ptr_type(this);
    if (parent() != NULL)
        result = (ptr_type *)(parent()->install_type(result));
    return result;
}


/*
 *  Print a type (short version) -- just use the type ID number.
 */

void
type_node::print (FILE *f)
{
    fputs("t:", f);
    print_id_number(f, type_id());
}


/*
 *  Print the type id, operator, and size.  This abbreviated form is used
 *  when printing the result_types of instructions.
 */

void
type_node::print_abbrev (FILE *f)
{
    /* print the type_id number */
    print(f);

    fputs(" (", f);

    switch (unqual()->op()) {
	case TYPE_INT: {
	    base_type *bt = (base_type *)unqual();
	    if (bt->is_signed()) {
		putc('i', f);
	    } else {
		putc('u', f);
	    }
	    break;
	}
	case TYPE_FLOAT:	{ putc('f', f); break; }
	case TYPE_VOID:		{ putc('v', f); break; }
	case TYPE_PTR:		{ putc('p', f); break; }
	case TYPE_ARRAY:	{ putc('a', f); break; }
	case TYPE_FUNC:		{ putc('n', f); break; }
	case TYPE_GROUP:	{ putc('g', f); break; }
	case TYPE_STRUCT:	{ putc('s', f); break; }
	case TYPE_UNION:	{ putc('n', f); break; }
	case TYPE_ENUM:		{ putc('e', f); break; }
	default: {
	    assert_msg(FALSE, ("type_node::print_abbrev - invalid operator"));
	}
    }

    fprintf(f, ".%d)", size());
}


/*
 *  Print all of the type information.  This method is used for printing
 *  symtabs.  This handles the fields shared by all type_nodes and the
 *  print_helper virtual function is used to print fields specific to the
 *  derived classes.
 */

void
type_node::print_full (FILE *f, int depth)
{
    suif_indent(f, depth);

    /* print the ID number */
    print(f);

    switch (op()) {
	case TYPE_INT:		{ fputs(": int ", f); break; }
	case TYPE_FLOAT:	{ fputs(": float ", f); break; }
	case TYPE_VOID:		{ fputs(": void ", f); break; }
	case TYPE_PTR:		{ fputs(": ptr ", f); break; }
	case TYPE_ARRAY:	{ fputs(": array ", f); break; }
	case TYPE_FUNC:		{ fputs(": function ", f); break; }
	case TYPE_GROUP:	{ fputs(": group ", f); break; }
	case TYPE_STRUCT:	{ fputs(": struct ", f); break; }
	case TYPE_UNION:	{ fputs(": union ", f); break; }
	case TYPE_ENUM:		{ fputs(": enum ", f); break; }
	case TYPE_CONST:	{ fputs(": const ", f); break; }
	case TYPE_VOLATILE:	{ fputs(": volatile ", f); break; }
	case TYPE_CALL_BY_REF:	{ fputs(": call-by-ref ", f); break; }
	case TYPE_NULL:		{ fputs(": null ", f); break; }
	case TYPE_RESTRICT:	{ fputs(": restrict ", f); break; }
	default: {
	    assert_msg(FALSE, ("type_node::print - invalid operator"));
	}
    }

    /* print any fields specific to the type kind */
    print_helper(f, depth);

    print_annotes(f, depth+1);
    putc('\n', f);
}


/*
 *  Read a type_node from an input stream.  The type_node must have already
 *  been created in the symtab or one of its ancestors.
 */

type_node *
type_node::read (in_stream *is, base_symtab *symtab)
{
    unsigned i = is->read_int();
    type_node *result = symtab->lookup_type_id(i);
    assert_msg(result, ("type_node::read - type ID %u not found", i));
    return result;
}


/*
 *  Write a type_node to an output stream.  The type_node must have a
 *  non-zero ID number assigned before it can be written out.
 */

void
type_node::write (out_stream *os)
{
    write_check();
    os->write_int(id);
}


void
type_node::write_check (void)
{
    assert(write_scope != NULL);
    assert_msg(type_id() != 0,
               ("attempt to write reference to type with no type_id"));
    assert_msg(write_scope->is_visible(this),
               ("attempt to write reference to type #%u used outside its "
                "scope", id));
}


/*
 *  Create a type_node from an annotation created by type_node::cvt_to_annote.
 *  This function calls the appropriate constructor based on the name
 *  of the annotation.  This function only creates the type_nodes; it
 *  doesn't read the details of the types (except the ID numbers) from
 *  the annotations because there may be references to type_nodes that
 *  haven't yet been created.
 */

type_node *
type_node::scan_from_annote (annote *an)
{
    type_node *result = NULL;

    if (an->name() == k_int_type) {
	result = new base_type;
	result->set_op(TYPE_INT);

    } else if (an->name() == k_float_type) {
	result = new base_type;
	result->set_op(TYPE_FLOAT);

    } else if (an->name() == k_void_type) {
	result = new base_type;
	result->set_op(TYPE_VOID);

    } else if (an->name() == k_ptr_type) {
	result = new ptr_type;
	result->set_op(TYPE_PTR);

    } else if (an->name() == k_array_type) {
	result = new array_type;
	result->set_op(TYPE_ARRAY);

    } else if (an->name() == k_func_type) {
	result = new func_type;
	result->set_op(TYPE_FUNC);

    } else if (an->name() == k_group_type) {
	result = new struct_type;
	result->set_op(TYPE_GROUP);

    } else if (an->name() == k_struct_type) {
	result = new struct_type;
	result->set_op(TYPE_STRUCT);

    } else if (an->name() == k_union_type) {
	result = new struct_type;
	result->set_op(TYPE_UNION);

    } else if (an->name() == k_enum_type) {
	result = new enum_type;
	result->set_op(TYPE_ENUM);

    } else if (an->name() == k_const_type) {
	result = new modifier_type;
	result->set_op(TYPE_CONST);

    } else if (an->name() == k_volatile_type) {
	result = new modifier_type;
	result->set_op(TYPE_VOLATILE);

    } else if (an->name() == k_restrict_type) {
	result = new modifier_type;
	result->set_op(TYPE_RESTRICT);

    } else if (an->name() == k_call_by_ref_type) {
	result = new modifier_type;
	result->set_op(TYPE_CALL_BY_REF);

    } else if (an->name() == k_null_type) {
	result = new modifier_type;
	result->set_op(TYPE_NULL);
    }
    assert_msg(result, ("type_node::scan_from_annote: unknown type kind"));

    /* get the ID number */
    result->id = (*an->immeds())[0].unsigned_int();

    return result;
}


/*
 *  Read the shared type_node fields from an annotation.  This function is
 *  called by the cvt_from_annote functions in the derived classes.  Since
 *  the ID number has already been read by scan_from_annote, it is just
 *  checked here.
 */

void
type_node::cvt_from_annote (annote *an, base_symtab *)
{
    assert(id == an->immeds()->pop().unsigned_int());
}


/*
 *  Write the shared type_node fields onto an annotation.  This function is
 *  called by the cvt_to_annote functions to append to the annotation the
 *  fields shared by all type_nodes.
 */

void
type_node::cvt_to_annote_base (annote *an)
{
    an->immeds()->append(immed(id));
}


/*
 *  Check if a type_node has been cloned, and if so, return its replacement.
 *  Otherwise, just return this type_node.
 */

type_node *
type_node::clone_helper (replacements *r)
{
    /* check if this type has been replaced */
    type_node_list_iter old_tnli(&r->oldtypes);
    type_node_list_iter new_tnli(&r->newtypes);
    while (!old_tnli.is_empty()) {
	assert(!new_tnli.is_empty());

	type_node *old_tn = old_tnli.step();
	type_node *new_tn = new_tnli.step();

	/* return the replacement */
	if (old_tn == this) return new_tn;
    }

    return this;
}


/*****************************************************************************/


/*
 *  Create a modifier type.
 */

modifier_type::modifier_type (type_ops o, type_node *t)
    : type_node(), typ(t)
{
    assert_msg((o == TYPE_CONST) ||
	       (o == TYPE_VOLATILE) ||
	       (o == TYPE_CALL_BY_REF) ||
	       (o == TYPE_RESTRICT) ||
	       (o == TYPE_NULL),
	       ("attempt to create modifier_type with invalid operator"));
    set_op(o);
}


/*
 *  Return the type size.  Modifiers have no effect on the size.
 */

int
modifier_type::size ()
{
    if (!base()) return 0;
    return base()->size();
}


/*
 *  Unqualify a type (remove the modifiers).  Since we know this is a
 *  modifier, we can just skip to the base type.
 */

type_node *
modifier_type::unqual ()
{
    if (!base()) return NULL;
    return base()->unqual();
}


/*
 *  Virtual methods to check for the various modifiers.  The default return
 *  value for these methods is FALSE.
 */

boolean
modifier_type::is_const ()
{
    if (op() == TYPE_CONST) return TRUE;
    if (base()) return base()->is_const();
    return FALSE;
}


boolean
modifier_type::is_volatile ()
{
    if (op() == TYPE_VOLATILE) return TRUE;
    if (base()) return base()->is_volatile();
    return FALSE;
}

boolean
modifier_type::is_restrict ()
{
    if (op() == TYPE_RESTRICT) return TRUE;
    if (base()) return base()->is_restrict();
    return FALSE;
}

boolean
modifier_type::is_call_by_ref ()
{
    if (op() == TYPE_CALL_BY_REF) return TRUE;
    if (base()) return base()->is_call_by_ref();
    return FALSE;
}


type_node *
modifier_type::ref_type (unsigned num)
{
    assert(num == 0);
    return base();
}


void
modifier_type::set_ref_type (unsigned num, type_node *new_type)
{
    assert(num == 0);
    set_base(new_type);
}


/*
 *  Help print a modifier type.  Just print the base type.
 */

void
modifier_type::print_helper (FILE *f, int /* depth */)
{
    assert_msg(base(), ("modifier_type::print_helper - missing base type"));
    base()->print(f);
}


/*
 *  Make a copy of a modifier_type.  (This isn't very useful!)
 */

type_node *
modifier_type::copy ()
{
    modifier_type *result = new modifier_type(op(), base());
    return result;
}


modifier_type *
modifier_type::clone(void)
{
    modifier_type *result = new modifier_type(op(), base()->clone());
    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is the same as another type_node.  The modifiers
 *  may be in any order, so the "is_same_helper" method is used to check
 *  for each possible modifier.
 */

boolean
modifier_type::is_same (type_node *t)
{
    if (!t) return FALSE;

    type_node *ut = unqual();
    if (!ut || (!ut->is_same(t->unqual()))) return FALSE;

    if (is_same_helper(TYPE_CONST, t) &&
	is_same_helper(TYPE_VOLATILE, t) &&
	is_same_helper(TYPE_CALL_BY_REF, t) &&
	is_same_helper(TYPE_RESTRICT, t) &&
	is_same_helper(TYPE_NULL, t)) {
	return TRUE;
    }

    return FALSE;
}


boolean
modifier_type::is_same_helper (type_ops mod, type_node *t)
{
    modifier_type *mt1 = find_modifier(mod);
    modifier_type *mt2 = t->find_modifier(mod);

    /* check if they are identical or both NULL */
    if (mt1 == mt2) return TRUE;

    /* check if both types have the same modifier with the same annotations */
    if (mt1 && mt2 && mt1->is_same_annotes(mt2)) return TRUE;

    return FALSE;
}


/*
 *  Check if this type is compatible with another type_node.  Modifiers
 *  do not affect compatibility so they are ignored here.
 */

boolean
modifier_type::compatible (type_node *t)
{
    if (!t) return FALSE;
    type_node *ut = unqual();
    if (!ut) return FALSE;
    return ut->compatible(t);
}


/*
 *  Write the modifier_type fields onto an annotation.
 */

annote *
modifier_type::cvt_to_annote ()
{
    annote *an;

    /* determine the annotation name from the type operator */
    switch (op()) {
	case TYPE_CONST:	{ an = new annote(k_const_type); break; }
	case TYPE_VOLATILE:	{ an = new annote(k_volatile_type);  break; }
	case TYPE_CALL_BY_REF:	{ an = new annote(k_call_by_ref_type); break; }
	case TYPE_NULL:		{ an = new annote(k_null_type); break; }
	case TYPE_RESTRICT:	{ an = new annote(k_restrict_type); break; }
	default:		{ an = NULL; break; }
    }
    assert_msg(an == NULL, ("modifier_type::cvt_to_annote - invalid type operator"));

    cvt_to_annote_base(an);
    assert_msg(base(), ("modifier_type::cvt_to_annote - missing base type"));
    if (write_scope != NULL)
	base()->write_check();
    unsigned base_id = base()->type_id();
    an->immeds()->append(immed(base_id));
    return an;
}


/*
 *  Read the modifier_type fields from an annotation.
 */

void
modifier_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    unsigned base_id = an->immeds()->pop().unsigned_int();
    typ = symtab->lookup_type_id(base_id);
    assert_msg(typ, ("modifier_type::cvt_from_annote - type ID %u not found",
		     base_id));
}


type_node *
modifier_type::clone_internal(void)
{
    return clone();
}


/*****************************************************************************/


/*
 *  Create a base type.
 */

base_type::base_type (type_ops o, int s, boolean b)
    : sz(s), sgn(b)
{
    assert_msg((o == TYPE_INT) ||
	       (o == TYPE_FLOAT) ||
	       (o == TYPE_VOID) ||
	       (o == TYPE_ENUM),
	       ("attempt to create base_type with non-base operator"));

    set_op(o);
    if ((o == TYPE_INT) || (o == TYPE_ENUM)) {
	sgn = b;
    } else if (o == TYPE_VOID) {
	sgn = FALSE;
    } else {
	sgn = TRUE;
    }
}


/*
 *  Write the base_type fields onto an annotation.
 */

annote *
base_type::cvt_to_annote ()
{
    annote *an;

    /* determine the annotation name from the type operator */
    switch (op()) {
	case TYPE_INT:		{ an = new annote(k_int_type); break; }
	case TYPE_FLOAT:	{ an = new annote(k_float_type);  break;}
	case TYPE_VOID:		{ an = new annote(k_void_type); break; }
	default:		{ an = NULL; break; }
    }
    assert_msg(an == NULL, ("base_type::cvt_to_annote - invalid type operator"));

    cvt_to_annote_base(an);
    an->immeds()->append(immed(size()));
    an->immeds()->append(immed((int)is_signed()));
    return an;
}


/*
 *  Read the base_type fields from an annotation.
 */

void
base_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    sz = an->immeds()->pop().integer();
    sgn = (boolean)(an->immeds()->pop().integer());
}


type_node *
base_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print a base_type.  The only added fields to check are the
 *  "is_signed" flag and the size.
 */

void
base_type::print_helper (FILE *f, int /* depth */)
{
    if (op() == TYPE_INT) {
	if (is_signed()) {
	    fputs("signed ", f);
	} else {
	    fputs("unsigned ", f);
	}
    }
    fprintf(f, "%d", size());
}


/*
 *  Make a copy of a base_type.
 */

type_node *
base_type::copy ()
{
    base_type *result = new base_type(op(), size(), is_signed());
    return result;
}


base_type *
base_type::clone(void)
{
    base_type *result = new base_type(op(), size(), is_signed());
    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is compatible with another type_node.  This is mostly
 *  like "is_same" except that ENUMs may be compatible with INTs.
 */

boolean
base_type::compatible (type_node *t)
{
    t = t->unqual();

    /* enums and ints may be compatible */
    if ((t->op() == TYPE_ENUM) && (op() == TYPE_INT)) {
	enum_type *et = (enum_type *)t;
	if ((et->is_signed() == is_signed()) &&
	    (et->size() == size())) {
	    return TRUE;
	}
    }

    if (t->op() != op()) return FALSE;
    base_type *bt = (base_type *)t;

    if (bt->size() != size()) return FALSE;
    if (bt->is_signed() != is_signed()) return FALSE;
    return TRUE;
}


/*
 *  Check if this base_type is the same as another type_node.
 */

boolean
base_type::is_same (type_node *t)
{
    if (!type_node::is_same(t)) return FALSE;
    base_type *bt = (base_type *)t;

    if (bt->size() != size()) return FALSE;
    if (bt->is_signed() != is_signed()) return FALSE;
    return TRUE;
}


/*****************************************************************************/


/*
 *  Create a new ptr_type.
 */

ptr_type::ptr_type (type_node *r)
    : ref(r)
{
    set_op(TYPE_PTR);
}


/*
 *  Write the ptr_type fields onto an annotation.
 */

annote *
ptr_type::cvt_to_annote ()
{
    annote *an = new annote(k_ptr_type);
    cvt_to_annote_base(an);
    assert_msg(ref_type(), ("ptr_type::cvt_to_annote - missing ref type"));
    if (write_scope != NULL)
	ref_type()->write_check();
    unsigned ref_id = ref_type()->type_id();
    an->immeds()->append(immed(ref_id));
    return an;
}


/*
 *  Read the ptr_type fields from an annotation.
 */

void
ptr_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    unsigned ref_id = an->immeds()->pop().unsigned_int();
    ref = symtab->lookup_type_id(ref_id);
    assert_msg(ref, ("ptr_type::cvt_from_annote - type ID %u not found",
		     ref_id));
}


type_node *
ptr_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print a ptr_type.  Just print the referent type.
 */

void
ptr_type::print_helper (FILE *f, int /* depth */)
{
    fputs("to ", f);
    assert_msg(ref_type(), ("ptr_type::print - missing ref type"));
    ref_type()->print(f);
}


type_node *
ptr_type::ref_type (unsigned num)
{
    assert(num == 0);
    return ref_type();
}


void
ptr_type::set_ref_type (unsigned num, type_node *new_type)
{
    assert(num == 0);
    set_ref_type(new_type);
}


/*
 *  Make a copy of a ptr_type.  The referent type is not copied.
 */

type_node *
ptr_type::copy ()
{
    ptr_type *result = new ptr_type(ref_type());
    return result;
}


ptr_type *
ptr_type::clone(void)
{
    ptr_type *result = new ptr_type(ref_type()->clone());
    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is compatible with another type_node.  All pointers
 *  are considered to be compatible with one another.
 */

boolean
ptr_type::compatible (type_node *t)
{
    if (!t || !(t = t->unqual())) return FALSE;
    if (t->op() != op()) return FALSE;
    return TRUE;
}


/*
 *  Check if a ptr_type is equivalent to another type_node.
 */

boolean
ptr_type::is_same (type_node *t)
{
    if (!type_node::is_same(t)) return FALSE;
    ptr_type *pt = (ptr_type *)t;
    if (!ref_type() ||
	!ref_type()->is_same(pt->ref_type())) return FALSE;
    return TRUE;
}


/*****************************************************************************/


/*
 *  Global variable used for unknown array bounds.
 */

const array_bound unknown_bound;


/*
 *  Retrieve a constant bound.  An error occurs if the bound is not
 *  a constant.
 */

int
array_bound::constant () const
{
    assert_msg(is_constant(),
	       ("array_bound::constant - bound is not constant"));
    return u.cnst;
}


/*
 *  Retrieve a variable bound.  An error occurs if the bound is not
 *  a variable.
 */

var_sym *
array_bound::variable () const
{
    assert_msg(is_variable(),
	       ("array_bound::variable - bound is not a variable"));
    return u.var;
}


/*
 *  Print an array bound.  Variables include the full chain_name from the
 *  symbol table.  If the bound is unknown, it is printed as an asterisk.
 */

void
array_bound::print (FILE *f)
{
    if (is_constant()) {
	fprintf(f, "%d", constant());
    } else if (is_variable()) {
	variable()->print(f);
    } else {
	fputc('*', f);
    }
}


/*
 *  Array bound operators....
 */

array_bound &
array_bound::operator= (const array_bound &b)
{
    is_cnst = b.is_cnst;
    u = b.u;
    return *this;
}


boolean
array_bound::operator== (const array_bound &b)
{
    if (is_unknown() && b.is_unknown()) return TRUE;
    if (is_constant() && b.is_constant()) return (constant() == b.constant());
    if (is_variable() && b.is_variable()) return (variable() == b.variable());
    return FALSE;
}


/*
 *  Grab an array bound from the list of immeds in an annotation.
 *  This is called by the array_type::cvt_from_annote function.
 */

array_bound::array_bound (annote *an, base_symtab *symtab)
{
    is_cnst = (boolean)(an->immeds()->pop().integer());
    if (is_cnst) {
	u.cnst = an->immeds()->pop().integer();
    } else {
	unsigned var_id = an->immeds()->pop().unsigned_int();
	if (var_id == 0) {
	    u.var = NULL;
	} else {
	    u.var = (var_sym *)symtab->lookup_sym_id(var_id);
	    assert_msg(u.var && u.var->is_var(),
		       ("array_bound - variable ID (%u) not found", var_id));
	}
    }
}


/*
 *  Put an array bound on the list of immeds of an annotation.
 *  This is called by the array_type::cvt_to_annote function.
 */

void
array_bound::add_to_annote (annote *an)
{
    if (is_constant()) {
	an->immeds()->append(immed((int)TRUE));
	an->immeds()->append(immed(constant()));
    } else if (is_variable()) {
	an->immeds()->append(immed((int)FALSE));
	if (write_scope != NULL)
	    variable()->write_check();
	an->immeds()->append(immed(variable()->sym_id()));
    } else {
	an->immeds()->append(immed((int)FALSE));
	an->immeds()->append(immed(0u));
    }
}


/*****************************************************************************/


/*
 *  Create a new array type.  By default, the bounds are unknown.
 */

array_type::array_type (type_node *elem, array_bound lb, array_bound ub)
    : elemt(elem), low(lb), uppr(ub)
{
    set_op(TYPE_ARRAY);

    /* make sure constant bounds are valid */
    if (lb.is_constant() && ub.is_constant()) {
	assert_msg(ub.constant() >= lb.constant(),
		   ("array_type - upper bound smaller than lower bound"));
    }
}


/*
 *  Check if the upper bound is unknown.  As a consistency check, this
 *  method also checks that if the upper bound is known, then the lower
 *  bound must also be known.
 */

boolean
array_type::are_bounds_unknown ()
{
    if (upper_bound().is_unknown()) return TRUE;

    if (lower_bound().is_unknown()) {
	assert_msg(upper_bound().is_unknown(),
		   ("array_type::are_bounds_unknown - "
		    "only upper bound is known"));
    }
    return FALSE;
}


/*
 *  Try to figure out the size (in bits) of an array.  If the array bounds
 *  are not constant, the result is zero.
 */

int
array_type::size ()
{
    if (!lower_bound().is_constant() ||
	!upper_bound().is_constant() ||
	!elem_type()) {
	return 0;
    }

    int lb = lower_bound().constant();
    int ub = upper_bound().constant();
    assert_msg(ub >= lb, ("array_type::size - "
			  "upper bound smaller than lower bound"));
    return (ub - lb + 1) * elem_type()->size();
}


type_node *
array_type::ref_type (unsigned num)
{
    assert(num == 0);
    return elem_type();
}


void
array_type::set_ref_type (unsigned num, type_node *new_type)
{
    assert(num == 0);
    set_elem_type(new_type);
}


/*
 *  Write the array_type fields onto an annotation.
 */

annote *
array_type::cvt_to_annote ()
{
    annote *an = new annote(k_array_type);
    cvt_to_annote_base(an);
    assert_msg(elem_type(),
	       ("array_type::cvt_to_annote - missing element type"));
    if (write_scope != NULL)
	elem_type()->write_check();
    unsigned elemt_id = elem_type()->type_id();
    an->immeds()->append(immed(elemt_id));
    lower_bound().add_to_annote(an);
    upper_bound().add_to_annote(an);
    return an;
}


/*
 *  Read the array_type fields from an annotation.
 */

void
array_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    unsigned elemt_id = an->immeds()->pop().unsigned_int();
    low = array_bound(an, symtab);
    uppr = array_bound(an, symtab);

    elemt = symtab->lookup_type_id(elemt_id);
    assert_msg(elemt, ("array_type::cvt_from_annote - type ID %u not found",
		       elemt_id));
}


type_node *
array_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print an array_type.  Show the bounds and the element type.
 */

void
array_type::print_helper (FILE *f, int /* depth */)
{
    fputc('[', f);
    lower_bound().print(f);
    fputs("..", f);
    upper_bound().print(f);
    fputs("] of ", f);

    assert_msg(elem_type(), ("array_type::print - missing element type"));
    elem_type()->print(f);
}


/*
 *  Make a copy of an array_type.  The element type is not copied.
 */

type_node *
array_type::copy ()
{
    array_type *result = new array_type(elem_type());
    result->set_lower_bound(lower_bound());
    result->set_upper_bound(upper_bound());
    return result;
}


array_type *
array_type::clone(void)
{
    array_type *result =
            new array_type(elem_type()->clone(), lower_bound(), upper_bound());
    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is compatible with another type_node.  Array types
 *  should never need to be compared but this method does something reasonable
 *  just in case.
 */

boolean
array_type::compatible (type_node *t)
{
    if (!t) return FALSE;
    return is_same(t->unqual());
}


/*
 *  Check if an array_type is equivalent to another type_node.
 */

boolean
array_type::is_same (type_node *t)
{
    if (!type_node::is_same(t)) return FALSE;
    array_type *at = (array_type *)t;
    if (lower_bound() != at->lower_bound()) return FALSE;
    if (upper_bound() != at->upper_bound()) return FALSE;
    if (!elem_type() ||
	!elem_type()->is_same(at->elem_type())) return FALSE;
    return TRUE;
}


/*****************************************************************************/


/*
 *  Create a new func_type.  The argument information is assumed
 *  unknown.
 */

func_type::func_type (type_node *r)
    : ret(r), nargs(0), argts(NULL), vargs(FALSE), arg_info(FALSE)
{
    set_op(TYPE_FUNC);
}


/*
 *  Create a new func_type.  The argument array is allocated and initialized.
 *  The individual argument types must be set separately.
 */

func_type::func_type (type_node *r, unsigned n, boolean varargs,
                      boolean know_args)
    : ret(r), nargs(n), argts(0), vargs(varargs), arg_info(know_args)
{
    assert(know_args || ((n == 0) && (!varargs)));

    set_op(TYPE_FUNC);

    if (nargs == 0) {
	argts = NULL;
    } else {
	argts = new type_node*[nargs];
	for (unsigned i = 0; i < nargs; i++) {
	    argts[i] = NULL;
	}
    }
}


func_type::func_type (boolean varargs, type_node *r)
    : ret(r), nargs(0), argts(0), vargs(varargs), arg_info(TRUE)
{
    set_op(TYPE_FUNC);
}


func_type::func_type (type_node *r, type_node *arg, boolean varargs)
    : ret(r), nargs(1), argts(new type_node *[1]), vargs(varargs),
      arg_info(TRUE)
{
    set_op(TYPE_FUNC);
    argts[0] = arg;
}


func_type::func_type (type_node *r, type_node *arg1, type_node *arg2,
                      boolean varargs)
    : ret(r), nargs(2), argts(new type_node*[2]), vargs(varargs),
      arg_info(TRUE)
{
    set_op(TYPE_FUNC);
    argts[0] = arg1;
    argts[1] = arg2;
}


func_type::func_type (type_node *r, type_node *arg1, type_node *arg2,
                      type_node *arg3, boolean varargs)
    : ret(r), nargs(3), argts(new type_node*[3]), vargs(varargs),
      arg_info(TRUE)
{
    set_op(TYPE_FUNC);
    argts[0] = arg1;
    argts[1] = arg2;
    argts[2] = arg3;
}


func_type::func_type (type_node *r, type_node *arg1, type_node *arg2,
                      type_node *arg3, type_node *arg4, boolean varargs)
    : ret(r), nargs(4), argts(new type_node*[4]), vargs(varargs),
      arg_info(TRUE)
{
    set_op(TYPE_FUNC);
    argts[0] = arg1;
    argts[1] = arg2;
    argts[2] = arg3;
    argts[3] = arg4;
}


func_type::func_type (type_node *r, type_node *arg1, type_node *arg2,
                      type_node *arg3, type_node *arg4, type_node *arg5,
                      boolean varargs)
    : ret(r), nargs(5), argts(new type_node*[5]), vargs(varargs),
      arg_info(TRUE)
{
    set_op(TYPE_FUNC);
    argts[0] = arg1;
    argts[1] = arg2;
    argts[2] = arg3;
    argts[3] = arg4;
    argts[4] = arg5;
}


func_type::~func_type ()
{
    if (argts) delete[] argts;
}


/*
 *  Check if an argument number is valid.
 */

void
func_type::check_range (unsigned n)
{
    assert_msg(n < num_args(),
	       ((num_args() > 0) ?
                (char*)"func_type: argument number %u out of range 0 to %u" :
                (char*)"func_type: argument number %u is invalid for function "
                "type with zero arguments",
		n, num_args() - 1));
}


/*
 *  Change the number of arguments.  If the number is larger than before,
 *  the argument type array is reallocated.
 */

void
func_type::set_num_args (unsigned n)
{
    if ((n > 0) && !args_known())
	set_args_known();

    if (n <= nargs) {
	nargs = n;
	return;
    }

    type_node **new_args = new type_node*[n];

    /* copy the old arguments (as much as possible) */
    unsigned i;
    for (i = 0; (i < nargs) && (i < n); i++) {
	new_args[i] = argts[i];
    }

    /* initialize any new arg fields */
    for (unsigned j = nargs; j < n; j++) {
	new_args[i] = NULL;
    }

    nargs = n;
    if (argts != NULL)
        delete[] argts;
    argts = new_args;
}


/*
 *  Set the argument information to be unknown.
 */

void
func_type::set_args_unknown (void)
{
    set_num_args(0);
    set_varargs(FALSE);
    arg_info = FALSE;
}


/*
 *  Set one of the argument types.  The argument number must be valid or an
 *  error will occur.
 */

void
func_type::set_arg_type (unsigned n, type_node *t)
{
    check_range(n);
    argts[n] = t;
}


type_node *
func_type::ref_type (unsigned num)
{
    if (num == 0)
	return return_type();
    else
	return arg_type(num - 1);
}


void
func_type::set_ref_type (unsigned num, type_node *new_type)
{
    if (num == 0)
	set_return_type(new_type);
    else
	set_arg_type(num - 1, new_type);
}


/*
 *  Write the func_type fields onto an annotation.
 */

annote *
func_type::cvt_to_annote ()
{
    annote *an = new annote(k_func_type);
    cvt_to_annote_base(an);
    assert_msg(return_type(),
	       ("func_type::cvt_to_annote - missing return type"));
    if (write_scope != NULL)
	return_type()->write_check();
    unsigned ret_id = return_type()->type_id();
    an->immeds()->append(immed(ret_id));
    an->immeds()->append(immed(num_args()));
    unsigned arg_flag =
            ((unsigned)(has_varargs())) | (((unsigned)(!args_known())) << 1);
    an->immeds()->append(immed(arg_flag));

    for (unsigned i = 0; i < num_args(); i++) {
	assert_msg(arg_type(i), ("func_type::cvt_to_annote - missing type for "
				 "argument %u", i));
	if (write_scope != NULL)
	    arg_type(i)->write_check();
	unsigned arg_id = arg_type(i)->type_id();
	an->immeds()->append(immed(arg_id));
    }

    return an;
}


/*
 *  Read the func_type fields from an annotation.
 */

void
func_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    unsigned ret_id = an->immeds()->pop().unsigned_int();
    ret = symtab->lookup_type_id(ret_id);
    assert_msg(ret, ("func_type::cvt_from_annote - type ID %u not found",
		     ret_id));

    nargs = an->immeds()->pop().unsigned_int();
    unsigned arg_flags = an->immeds()->pop().unsigned_int();
    vargs = (arg_flags & 0x1);
    if (nargs == (unsigned)-1) {
	/* old-style format, for backward compatibility */
	nargs = 0;
	arg_info = FALSE;
    } else {
	arg_info = !((arg_flags & 0x2) >> 1);
    }

    argts = new type_node*[num_args()];
    for (unsigned i = 0; i < num_args(); i++) {
	unsigned arg_id = an->immeds()->pop().unsigned_int();
	argts[i] = symtab->lookup_type_id(arg_id);
	assert_msg(argts[i], ("func_type::cvt_from_annote - type ID %u not "
			      "found", arg_id));
    }
}


type_node *
func_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print a func_type.  Show the argument types, including "..." if
 *  the function has a variable argument list, and the return type.
 */

void
func_type::print_helper (FILE *f, int /* depth */)
{
    /* print the argument types */
    fputs("(", f);
    if (args_known()) {
        unsigned n;
        for (n = 0; n + 1 < num_args(); n++) {
	    if(arg_type(n)) {
		arg_type(n)->print(f);
	    } else {
		fputs("<<MISSING TYPE>>", f);
	    }
	    fputs(", ", f);
	}
	if (num_args() > 0) {
	    if(arg_type(n)) {
		arg_type(n)->print(f);
	    } else {
		fputs("<<MISSING TYPE>>", f);
	    }
	}
	if (has_varargs()) {
	    if (num_args() > 0) fputs(", ", f);
	    fputs("...", f);
	}
    } else {
	fputs("??", f);
    }

    fputs(") returns ", f);

    if(return_type()) {
	return_type()->print(f);
    } else {
	fputs("<<MISSING RETURN TYPE>>", f);
    }
}


/*
 *  Make a copy of a func_type.  The return and argument types are not copied.
 */

type_node *
func_type::copy ()
{
    /* create new func_type */
    func_type *result =
	    new func_type(return_type(), num_args(), has_varargs(),
			  args_known());

    /* set the argument types */
    for (unsigned n = 0; n < num_args(); n++) {
	result->set_arg_type(n, arg_type(n));
    }

    return result;
}


func_type *
func_type::clone(void)
{
    func_type *result =
            new func_type(return_type()->clone(), num_args(), has_varargs(),
                          args_known());

    for (unsigned n = 0; n < num_args(); n++) {
        result->set_arg_type(n, arg_type(n)->clone());
    }

    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is compatible with another type_node.  Function types
 *  should never need to be compared but this method does something reasonable
 *  just in case.
 */

boolean
func_type::compatible (type_node *t)
{
    if (!t) return FALSE;
    return is_same(t->unqual());
}


/*
 *  Check if this func_type is equivalent to another type_node.
 */

boolean
func_type::is_same (type_node *t)
{
    if (!type_node::is_same(t)) return FALSE;
    func_type *ft = (func_type *)t;

    if (args_known() != ft->args_known()) return FALSE;
    if (num_args() != ft->num_args()) return FALSE;
    if (!return_type() ||
	!return_type()->is_same(ft->return_type())) return FALSE;
    if (has_varargs() != ft->has_varargs()) return FALSE;

    /* compare the argument types */
    for (unsigned n = 0; n < num_args(); n++) {
	if (!arg_type(n) ||
	    !arg_type(n)->is_same(ft->arg_type(n))) return FALSE;
    }

    return TRUE;
}


/*****************************************************************************/


/*
 *  Create a new struct_type.  The field arrays are allocated and initialized,
 *  but the individual field names, types, and offset must be set separately.
 */

struct_type::struct_type (type_ops o, int s, const char *nm, unsigned n)
    : my_name(0), sz(s), ntypes(n), types(new type_node*[n]),
      names(new const char*[n]), offsets(new int[n])
{
    assert_msg((o == TYPE_GROUP) || (o == TYPE_STRUCT) || (o == TYPE_UNION),
	       ("attempt to create struct of non-struct type"));
    set_op(o);
    set_name(nm);

    /* initialize the fields */
    for (unsigned i = 0; i < ntypes; i++) {
	types[i] = NULL;
	names[i] = NULL;
	offsets[i] = 0;
    }
}


struct_type::~struct_type ()
{
    if (types) delete[] types;
    if (names) delete[] names;
    if (offsets) delete[] offsets;
}


/*
 *  Check if a field number is valid.
 */

void
struct_type::check_range (unsigned n)
{
    assert_msg(n < num_fields(),
	       ((num_fields() > 0) ?
                (char*)"struct_type: field number %u out of range 0 to %u" :
                (char*)"struct_type: field number %u is invalid for structure "
                "type with zero fields",
		n, num_fields() - 1));
}


/*
 *  Change the number of fields.  If the number is larger than before, the
 *  field arrays are reallocated.
 */

void
struct_type::set_num_fields (unsigned n)
{
    if (n <= ntypes) {
	ntypes = n;
	return;
    }

    type_node **new_types = new type_node*[n];
    const char **new_names = new const char*[n];
    int *new_offsets = new int[n];

    /* copy the old fields (as much as possible) */
    unsigned i;
    for (i = 0; (i < ntypes) && (i < n); i++) {
	new_types[i] = types[i];
	new_names[i] = names[i];
	new_offsets[i] = offsets[i];
    }

    /* initialize any new fields */
    for (unsigned j = ntypes; j < n; j++) {
	new_types[i] = NULL;
	new_names[i] = NULL;
	new_offsets[i] = 0;
    }

    ntypes = n;
    delete[] types;
    delete[] names;
    delete[] offsets;
    types = new_types;
    names = new_names;
    offsets = new_offsets;
}


/*
 *  Set the name of a struct_type.  Make sure that it is entered in the
 *  lexicon.
 */

void
struct_type::set_name (const char *nm)
{
    my_name = nm ? lexicon->enter(nm)->sp : NULL;
}


/*
 *  Set the name of a particular field in a struct_type.  Make sure that it
 *  is entered in the lexicon.  The field number must be valid or an error
 *  will occur.
 */

void
struct_type::set_field_name (unsigned n, const char *nm)
{
    check_range(n);
    names[n] = nm ? lexicon->enter(nm)->sp : NULL;
}


/*
 *  Set the type of a particular field in a struct_type.  The field number
 *  must be valid or an error will occur.
 */

void
struct_type::set_field_type (unsigned n, type_node *t)
{
    check_range(n);
    types[n] = t;
}


/*
 *  Set the offset of a particular field in a struct_type.  The field number
 *  must be valid or an error will occur.
 */

void
struct_type::set_offset (unsigned n, int o)
{
    check_range(n);
    offsets[n] = o;
}


/*
 *  Find a structure field from its offset.  This relies upon the convention
 *  that the fields are stored in order of increasing offsets.  If no fields
 *  match the offset exactly, the field containing the offset is returned and
 *  the number of bits from the start of that field is returned in the "left"
 *  parameter.  This function is useless for TYPE_UNIONs since the fields all
 *  have the same offset.
 */

unsigned
struct_type::find_field_by_offset (int off, int& left)
{
    unsigned closest_field = num_fields();
    int distance = 0;
    for (unsigned field_num = 0; field_num < num_fields(); ++field_num) {
	if (offset(field_num) <= off) {
	    if ((closest_field == num_fields()) ||
		(distance > (off - offset(field_num)))) {
		distance = off - offset(field_num);
		closest_field = field_num;
	    }
	}
    }

    if (closest_field != num_fields())
	left = distance;
    return closest_field;
}


/*
 *  Find a group, structure, or union field by name.  Returns
 *  "(unsigned)-1" if the field is not found.
 */

unsigned
struct_type::find_field_by_name (const char *nm)
{
    const char *nn = nm ? lexicon->enter(nm)->sp : NULL;

    for (unsigned i = 0; i < num_fields(); i++) {
	if (nn == field_name(i)) return i;
    }

    return (unsigned)-1;
}


type_node *
struct_type::ref_type (unsigned num)
{
    return field_type(num);
}


void
struct_type::set_ref_type (unsigned num, type_node *new_type)
{
    set_field_type(num, new_type);
}


/*
 *  Write the struct_type fields onto an annotation.
 */

annote *
struct_type::cvt_to_annote ()
{
    annote *an;
    if (op() == TYPE_GROUP) {
	an = new annote(k_group_type);
    } else if (op() == TYPE_STRUCT) {
	an = new annote(k_struct_type);
    } else {
	an = new annote(k_union_type);
    }

    cvt_to_annote_base(an);
    an->immeds()->append(immed(name()));
    an->immeds()->append(immed(size()));
    an->immeds()->append(immed(num_fields()));

    for (unsigned i = 0; i < num_fields(); i++) {
	assert_msg(field_type(i), ("struct_type::cvt_to_annote - missing type "
				   "for field %u", i));
	if (write_scope != NULL)
	    field_type(i)->write_check();
	unsigned field_id = field_type(i)->type_id();
	an->immeds()->append(immed(field_name(i)));
	an->immeds()->append(immed(field_id));
	an->immeds()->append(immed(offset(i)));
    }

    return an;
}


/*
 *  Read the struct_type fields from an annotation.
 */

void
struct_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    my_name = an->immeds()->pop().string();
    sz = an->immeds()->pop().integer();
    ntypes = an->immeds()->pop().unsigned_int();

    /* allocate storage for the aggregate's elements */
    names = new const char*[ntypes];
    types = new type_node*[ntypes];
    offsets = new int[ntypes];

    for (unsigned i = 0; i < ntypes; i++) {
	names[i] = an->immeds()->pop().string();
	unsigned field_id = an->immeds()->pop().unsigned_int();
	types[i] = symtab->lookup_type_id(field_id);
	assert_msg(types[i], ("struct_type::cvt_from_annote - type ID %u not "
			      "found", field_id));
	offsets[i] = an->immeds()->pop().integer();
    }
}


type_node *
struct_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print a struct_type.  This method includes all of the struct_type
 *  fields.
 */

void
struct_type::print_helper (FILE *f, int depth)
{
    fprintf(f, "%s (%d) { ", name(), size());

    for (unsigned i = 0; i < num_fields(); i++) {
	putc('\n', f);
	suif_indent(f, depth+1);
	if(field_name(i)) {
	    fprintf(f, "%s offset=%d ", field_name(i), offset(i));
	    if(field_type(i)) {
		field_type(i)->print(f);
	    } else {
		fprintf(f, "<<MISSING TYPE>>");
	    }
	} else {
	    fprintf(f, "<<MISSING FIELD>>");
	}
    }
    if (num_fields() > 0) {
	putc('\n', f);
	suif_indent(f, depth);
    }
    putc('}', f);
}


/*
 *  Make a copy of a struct_type.  The field types are not copied.
 */

type_node *
struct_type::copy ()
{
    struct_type *result = new struct_type(op(), size(), name(), num_fields());

    /* set the field names, types, and offsets */
    for (unsigned n = 0; n < num_fields(); n++) {
	result->set_field_name(n, field_name(n));
	result->set_field_type(n, field_type(n));
	result->set_offset(n, offset(n));
    }

    return result;
}


struct_type *
struct_type::clone(void)
{
    annote *clone_annote = annotes()->peek_annote(k_clone);
    if (clone_annote != NULL) {
        immed_list *clone_immeds = clone_annote->immeds();
        assert((clone_immeds->count() == 1) && ((*clone_immeds)[0].is_type()));
        type_node *result = (*clone_immeds)[0].type();
        assert(result->is_struct());
        return (struct_type *)result;
    }

    struct_type *result = new struct_type(op(), size(), name(), num_fields());
    clone_annote = new annote(k_clone);
    clone_annote->immeds()->append(immed(result));
    annotes()->push(clone_annote);

    for (unsigned n = 0; n < num_fields(); n++) {
	result->set_field_name(n, field_name(n));
	result->set_field_type(n, field_type(n)->clone());
	result->set_offset(n, offset(n));
    }

    clone_annote = annotes()->get_annote(k_clone);
    assert(clone_annote != NULL);
    delete clone_annote;

    copy_annotes(result);
    return result;
}

/*
 * Modify the type of struct_type
 * No validation is done here
 */
void
struct_type::set_to_union() {
  set_op(TYPE_UNION);
}

void
struct_type::set_to_struct() {
  set_op(TYPE_STRUCT);
}

void
struct_type::set_to_group() {
  set_op(TYPE_GROUP);
}


/*
 *  Check if this type is compatible with another type_node.  Struct types
 *  should never need to be compared but this method does something reasonable
 *  just in case.
 */

boolean
struct_type::compatible (type_node *t)
{
    if (!t) return FALSE;
    return is_same(t->unqual());
}


/*
 *  Compare struct_types.  In SUIF, the types are the same only if
 *  their pointers match.
 */

boolean
struct_type::is_same (type_node *t)
{
    return (t == (type_node *)this);
}


/*****************************************************************************/


/*
 *  Create a new enum_type.  The member arrays are allocated and initialized,
 *  but the individual values must be set separately.
 */

enum_type::enum_type (const char *nm, int s, boolean b, unsigned n)
    : base_type(TYPE_ENUM, s, b), my_name(0), nvals(n),
      names(new const char*[nvals]), vals(new int[nvals])
{
    set_name(nm);

    /* initialize the members */
    for (unsigned i = 0; i < nvals; i++) {
	names[i] = NULL;
	vals[i] = 0;
    }
}


enum_type::~enum_type ()
{
    if (names) delete[] names;
    if (vals) delete[] vals;
}


/*
 *  Check if an enum_type member number is valid.
 */

void
enum_type::check_range (unsigned n)
{
    assert_msg(n < num_values(),
	       ((num_values() > 0) ?
                "enum_type: member number %u out of range 0 to %u" :
                "enum_type: member number %u is invalid for enumerated "
                "type with zero members",
		n, num_values() - 1));
}


/*
 *  Change the number of members in an enum_type.  If the number is larger
 *  than before, the member arrays are reallocated.
 */

void
enum_type::set_num_values (unsigned n)
{
    if (n <= nvals) {
	nvals = n;
	return;
    }

    const char **new_names = new const char*[n];
    int *new_vals = new int[n];

    /* copy the old values (as much as possible) */
    unsigned i;
    for (i = 0; (i < nvals) && (i < n); i++) {
	new_names[i] = names[i];
	new_vals[i] = vals[i];
    }

    /* initialize any new values */
    for (unsigned j = nvals; j < n; j++) {
	new_names[i] = NULL;
	new_vals[i] = 0;
    }

    nvals = n;
    delete[] names;
    delete[] vals;
    names = new_names;
    vals = new_vals;
}


/*
 *  Set the name of an enum_type.  Make sure it is entered in the lexicon.
 */

void
enum_type::set_name (const char *nm)
{
    my_name = nm ? lexicon->enter(nm)->sp : NULL;
}


/*
 *  Set the name of a particular member of an enum_type.  Make sure it is
 *  entered in the lexicon.  The member number must be valid or an error will
 *  occur.
 */

void
enum_type::set_member (unsigned n, const char *nm)
{
    check_range(n);
    names[n] = nm ? lexicon->enter(nm)->sp : NULL;
}


/*
 *  Set the value of a particular member of an enum_type.  The member number
 *  must be valid or an error will occur.
 */

void
enum_type::set_value (unsigned n, int v)
{
    check_range(n);
    assert_msg(is_signed() || (v >= 0),
	       ("enum_type: negative member number not allowed in unsigned "
		"enumeration"));
    assert_msg((is_signed() && (((v >= 0) && ((v >> (size() - 1)) == 0)) ||
			       ((v < 0) && (~v >> (size() - 1)) == 0))) ||
	       (!is_signed() && ((v >> size()) == 0)),
	       ("enum_type: member number doesn't fit in size allocated"));
    vals[n] = v;
}


/*
 *  Search for a particular member of an enumerated type by either the
 *  value or the name.  If found, the index of the member is returned;
 *  otherwise, "(unsigned)-1" is returned.
 */

unsigned
enum_type::find_member_by_value (int v)
{
    for (unsigned i = 0; i < num_values(); i++) {
	if (value(i) == v) return i;
    }

    return (unsigned)-1;
}


unsigned
enum_type::find_member_by_name (const char *nm)
{
    const char *nn = nm ? lexicon->enter(nm)->sp : NULL;

    for (unsigned i = 0; i < num_values(); i++) {
	if (nn == member(i)) return i;
    }

    return (unsigned)-1;
}


/*
 *  Write the enum_type fields onto an annotation.
 */

annote *
enum_type::cvt_to_annote ()
{
    annote *an = new annote(k_enum_type);
    cvt_to_annote_base(an);
    an->immeds()->append(immed(name()));
    an->immeds()->append(immed(size()));
    an->immeds()->append(immed((int)is_signed()));
    an->immeds()->append(immed(num_values()));

    for (unsigned n = 0; n < num_values(); n++) {
	an->immeds()->append(immed(member(n)));
	an->immeds()->append(immed(value(n)));
    }

    return an;
}


/*
 *  Read the enum_type fields from an annotation.
 */

void
enum_type::cvt_from_annote (annote *an, base_symtab *symtab)
{
    type_node::cvt_from_annote(an, symtab);
    my_name = an->immeds()->pop().string();
    set_size(an->immeds()->pop().integer());
    set_signed((boolean)(an->immeds()->pop().integer()));
    nvals = an->immeds()->pop().unsigned_int();

    names = new const char*[nvals];
    vals = new int[nvals];

    for (unsigned n = 0; n < nvals; n++) {
	names[n] = an->immeds()->pop().string();
	vals[n] = an->immeds()->pop().integer();
    }
}


type_node *
enum_type::clone_internal(void)
{
    return clone();
}


/*
 *  Help print an enum_type.  This method includes all of the enum_type
 *  members.
 */

void
enum_type::print_helper (FILE *f, int depth)
{
    fputs(name(), f);
    if (is_signed()) {
	fputs(" signed", f);
    } else {
	fputs(" unsigned", f);
    }
    fprintf(f, " %d { ", size());

    for (unsigned i = 0; i < num_values(); i++) {
	putc('\n', f);
	suif_indent(f, depth+1);
	fprintf(f, "%s = %d, ", member(i), value(i));
    }
    if (num_values() > 0) {
	putc('\n', f);
	suif_indent(f, depth);
    }
    putc('}', f);
}


/*
 *  Make a copy of an enum_type.
 */

type_node *
enum_type::copy ()
{
    enum_type *result =
	new enum_type(name(), size(), is_signed(), num_values());

    /* set the members and values */
    for (unsigned n = 0; n < num_values(); n++) {
	result->set_member(n, member(n));
	result->set_value(n, value(n));
    }

    return result;
}


enum_type *
enum_type::clone(void)
{
    enum_type *result =
            new enum_type(name(), size(), is_signed(), num_values());

    for (unsigned n = 0; n < num_values(); n++) {
	result->set_member(n, member(n));
	result->set_value(n, value(n));
    }

    copy_annotes(result);
    return result;
}


/*
 *  Check if this type is compatible with another type_node.  ENUM types
 *  are compatible with INTs and other ENUMs of the same size and "signed-
 *  ness".
 */

boolean
enum_type::compatible (type_node *t)
{
    t = t->unqual();
    if ((t->op() == TYPE_INT) || (t->op() == TYPE_ENUM)) {
	base_type *bt = (base_type *)t;
	if ((bt->is_signed() == is_signed()) &&
	    (bt->size() == size())) {
	    return TRUE;
	}
    }
    return is_same(t);
}


/*
 *  Compare enum_types (using name equivalence).
 */

boolean
enum_type::is_same (type_node *t)
{
    return (t == (type_node *)this);
}

