/*  SUIF Operands */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef OPERAND_H
#define OPERAND_H

#pragma interface

RCS_HEADER(operand_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class var_sym;
class instruction;
class immed;
class tree_node;
class type_node;
struct replacements;


/*
 *  Operands represent the sources and destinations of SUIF instructions.
 *  In the simplest cases, operands may either be null or refer directly to
 *  variables.  A source operand may also point to another instruction.  This
 *  is used to implement expression trees and, at lower levels of the system,
 *  to refer to the results of other instructions in a flat list.  An
 *  instruction pointer in a destination operand refers to the instruction
 *  that uses the result value.
 *
 *  The "is_expr" method tests if a source operand is a subexpression that is
 *  not contained in a separate tree_instr (i.e. it's not in a flat list).
 *  This method should not be used for destination operands.
 *
 *  Before changing an operand from an instruction pointer to some other
 *  value, the instruction must be removed.  The "remove" method checks if the
 *  operand is an instruction, and if so, calls the "instruction::remove"
 *  method for it.
 */

enum operand_kinds {
    OPER_NULL,				/* null operand */
    OPER_SYM,				/* variable symbol */
    OPER_INSTR,				/* instruction */
    OPER_REG                            /* register operand */
};


class operand_dataonly {
friend class operand;

protected:
    union {
        operand_kinds k;
        type_node *typ;         /* type for register operand */
    } v;
    union {                     /* union of basic operands */
        var_sym *sym;                   /* symbol */
        instruction *i;                 /* instruction pointer */
        int r;                          
    } u;

public:
    operand_kinds kind() const;
    boolean is_null() const		{ return (v.k == OPER_NULL); }
    boolean is_symbol() const           { return (v.k == OPER_SYM); }
    boolean is_instr() const		{ return (v.k == OPER_INSTR); }
    boolean is_expr() const;		/* is source operand a sub-expr? */
    boolean is_reg() const              { return (v.k >= OPER_REG); }
    boolean is_immed() const;           /* is operand a ldc immediate? */

    boolean is_hard_reg() const
        { return ((v.k >= OPER_REG) ? (u.r >= 0) : FALSE); }
    boolean is_virtual_reg() const
        { return ((v.k >= OPER_REG) ? (u.r < 0) : FALSE); }

    var_sym *symbol() const;
    instruction *instr() const;
    int reg() const;
    immed immediate() const;

    boolean operator==(const operand_dataonly &r) const;
    boolean operator!=(const operand_dataonly &r) const
      { return !(*this == r); }
};


class operand : public operand_dataonly {
public:
    operand(const operand_dataonly &other)      { v = other.v; u = other.u; }
    operand(const operand &other)       { v = other.v; u = other.u; }
    operand(in_stream *is, base_symtab *syms, tree_node *t);
    operand(var_sym *s)			{ set_symbol(s); }
    operand(instruction *i)		{ set_instr(i); }
    operand(int r, type_node *t)        { set_reg(r,t); }
    operand()				{ u.sym = NULL; set_null(); }
    ~operand();
    
    void set_null()			{ v.k = OPER_NULL; }
    void set_symbol(var_sym *s)		{ v.k = OPER_SYM; u.sym = s; }
    void set_instr(instruction *i)	{ v.k = OPER_INSTR; u.i = i; }
    void set_reg(int r, type_node *t)   { v.typ = t; u.r = r; }

    void remove();			/* remove src operand from expr tree */
    type_node *type() const;            /* find the type of the operand */
    boolean is_const_int(int *c=NULL) const;
					/* is this a constant integer? */

    operand& operator=(const operand &r);
    operand clone(base_symtab *dst_scope = NULL);
    operand clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os) const;
    void print(instruction *i, FILE *f=stdout) const;
    void print(FILE *fp=stdout) const;
    void print_source(FILE *f=stdout) const	{ print(f); } /* obsolete */
};

DECLARE_DLIST_CLASS(operand_list, operand);

#endif /* OPERAND_H */
