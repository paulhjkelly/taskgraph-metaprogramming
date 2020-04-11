/*  Immediate Constants Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef IMMED_H
#define IMMED_H

#pragma interface

RCS_HEADER(immed_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class type_node;
class instruction;
struct replacements;


enum immed_kinds {
    im_int = 'i',			/* integer */
    im_extended_int = 'l',		/* extended precision integer */
    im_string = 's',			/* string */
    im_float = 'f',			/* float */
    im_extended_float = 'g',		/* extended precision float */
    im_symbol = 'p',			/* symbolic address */
    im_type = 'h',			/* high-level type */
    im_op = 'o',			/* operand */
    im_instr = 'I',			/* instruction */
    im_undef = 'u'			/* undefined */
};


/*
 *  Immediate values are used in ldc (load constant) instructions and as
 *  elements of annotations.  Since they are small, there are no methods
 *  to change the value of an immed.  Instead, a variety of constructors
 *  are provided to make it easy to create new immeds.
 */

class immed_dataonly {
friend class immed;
protected:
    union {
	int ival;
	const char *sval;
	double fval;
	sym_addr_dataonly yval;
	type_node *hval;
	operand_dataonly opval;
	instruction *instrval;
    } v;
    immed_kinds knd;
};

class tree_node_list;

class immed : public immed_dataonly {
protected:
    /* access dataonly fields with type casts */
    sym_addr get_y() const		{ return *reinterpret_cast<const sym_addr*>(&v.yval);}
    void set_y(sym_addr s)		{ v.yval = *reinterpret_cast<sym_addr_dataonly*>(&s); }

    static void instr_list_number(tree_node_list *the_list);

public:
    immed(int i)			{ knd = im_int; v.ival = i; }
    immed(unsigned the_unsigned);
    immed(long the_long);
    immed(unsigned long the_unsigned_long);
    immed(const char *s)		{ knd = im_string;
                                          assert(s != NULL);
                                          v.sval = lexicon->enter(s)->sp; }
    immed(immed_kinds this_kind, const char *value)
      {
	assert((this_kind == im_extended_int) ||
	       (this_kind == im_extended_float));
	knd = this_kind;
	assert(value != NULL);
	v.sval = lexicon->enter(value)->sp;
      }
    immed(double f)			{ knd = im_float; v.fval = f; }
    immed(sym_node *s, int of=0);
    immed(sym_addr sa)			{ knd = im_symbol; set_y(sa); }
    immed(type_node *h)			{ knd = im_type; v.hval = h; }
    immed(operand op)			{ knd = im_op; v.opval = op; }
    immed(instruction *instr)		{ knd = im_instr; v.instrval = instr; }
    immed(const immed_dataonly &ic)	{ knd = ic.knd; v = ic.v; }
    immed()				{ knd = im_undef; }
    immed(in_stream *is, base_symtab *symtab);

    immed_kinds kind() const		{ return knd; }

    boolean is_int_const() const
      { return ((kind() == im_int) || (kind() == im_extended_int)); }
    boolean is_integer() const		{ return knd == im_int; }
    boolean is_unsigned_int() const;
    boolean is_long_int() const;
    boolean is_unsigned_long() const;
    boolean is_ext_integer() const	{ return knd == im_extended_int; }

    boolean is_string() const		{ return knd == im_string; }

    boolean is_float_const() const
      { return ((kind() == im_float) || (kind() == im_extended_float)); }
    boolean is_flt() const		{ return knd == im_float; }
    boolean is_ext_flt() const		{ return knd == im_extended_float; }

    boolean is_symbol() const		{ return knd == im_symbol; }
    boolean is_type() const		{ return knd == im_type; }
    boolean is_op() const		{ return knd == im_op; }
    boolean is_instr() const		{ return knd == im_instr; }
    boolean is_error() const		{ return knd == im_undef; }

    int integer() const;
    unsigned unsigned_int() const;
    long long_int() const;
    unsigned long unsigned_long() const;
    const char *ext_integer() const;
    const char *string() const;
    double flt() const;
    const char *ext_flt() const;
    double force_double() const;
    sym_node *symbol() const;
    int offset() const;
    sym_addr addr() const;
    type_node *type() const;
    operand op() const;
    instruction *instr() const;

    boolean operator==(const immed &r) const;
    boolean operator!=(const immed &r) const	{ return !(*this == r); }

    immed &operator=(const immed &r)  { knd = r.knd; v = r.v; return *this; }

    immed clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void print(FILE *fp);
    void rawprint(FILE *fp);

    void write(out_stream *os);
};


DECLARE_DLIST_CLASSES(immed_list_base, immed_list_e, immed_list_iter, immed);

class immed_list : public immed_list_base {
public:
    immed_list()  { }
    immed_list(immed i) : immed_list_base(i)  { }
    immed_list(immed i1, immed i2) : immed_list_base(i1, i2)  { }
    immed_list(immed i1, immed i2, immed i3) : immed_list_base(i1, i2, i3)  { }
    ~immed_list();

    immed_list *clone(base_symtab *dst_scope = NULL);
    immed_list *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);
};

#endif /* IMMED_H */
