/*  SUIF Instruction Classes */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#pragma interface

RCS_HEADER(instruction_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class type_node;
class label_sym;
class tree_node;
class tree_instr;
struct replacements;

extern const char *k_suif;

class instruction;

/*
 *  The src_map method calls functions of the following type on each
 *  source operand of an instruction.  The operand parameter is a pointer
 *  to a copy of the operand; if the function returns TRUE, the value in the
 *  copy will be assigned to the actual operand field within the instruction.
 *  The void* pointer is used to pass arbitrary data.
 */

typedef boolean (*src_map_f)(instruction *i, operand *r, void *x);

EXPORTED_BY_SUIF const char *k_suif;

/*
 *  Instruction Base Class.  This is an abstract base class and cannot be
 *  directly instantiated.  All instructions contain some common fields:
 *
 *  opcode	required; this also determines the instruction format
 *  type	high-level type for the result of the instruction
 *  number	instruction number; set automatically
 *  parent	the tree_instr containing the instruction; set automatically
 *  dst_op	destination operand; only used for some opcodes
 *
 *  The type field specifies the high-level type of the result.  It should
 *  be set to "type_void" if the instruction does not produce a result.
 *
 *  Each instruction has a number assigned to it that is unique within the
 *  current procedure.  This is useful for many types of analysis.  When a new
 *  instruction is created, its number is set to zero; the tree_proc
 *  number_instrs method must be called to assign unique numbers to newly
 *  created instructions.  This is done automatically before writing the
 *  procedure out to a file.
 *
 *  If the destination field is unused in a derived class, the dst_op method
 *  will return a null operand; trying to use set_dst will cause an error.
 *  For instructions in the in_rrr class that do not use the destination,
 *  the user is responsible for making sure that it is always a null operand.
 *
 *  When an instruction is used as a source operand, the destination should
 *  point back to the instruction that uses it.  This is handled automatically
 *  and users are not allowed to set the destination to be an instruction
 *  operand; trying to do so will cause an error.  This is done to ensure that
 *  the pointers are always consistent.  Before changing a source operand from
 *  an instruction to some other value, you must call the remove method to
 *  clear the instruction's destination and possibly update some of the parent
 *  pointers.
 *
 *  The clone method is provided to make copies of instructions and their
 *  expression trees.  If the instruction is a label, the copy will be given
 *  a new label symbol.
 */

class instruction : public suif_object {
    friend class operand;
    friend class tree_instr;
    friend class tree_proc;
    friend class immed;

private:
    if_ops op;
    type_node *typ;
    unsigned inum;
    tree_instr *par;

    void set_parent(tree_instr *p);

protected:
    unsigned nd;
    operand *dsts;

    instruction(if_ops o, in_stream *is, base_symtab *syms);
    instruction(if_ops o, type_node *t);

    void set_number(unsigned n)		{ inum = n; }

    void read_annotes(in_stream *is, base_symtab *syms);
    void write_annotes(out_stream *os);
    void write_src_expr(operand s, out_stream *os);
    void write_dsts(out_stream *os);
    void check_new_dst(operand old_op, operand new_op);
    void check_new_src(operand old_op, operand new_op);
    void no_dst_error();
    virtual operand *get_src_ptr(unsigned n);

    void clone_base(instruction *i, replacements *r, boolean no_copy);
    void find_base_refs(base_symtab *dst_scope, replacements *r);

public:
    instruction();
    virtual ~instruction();
    
    object_kinds object_kind()		{ return INSTR_OBJ; }

    virtual const char *architecture()	{ return k_suif; }
    unsigned number()			{ return inum; }
    virtual inst_format format()	{ return which_format(op); }
    virtual if_ops opcode()		{ return op; }
    virtual const char *op_string()	{ return if_ops_name(op); }
    virtual void set_opcode(if_ops o)	{ op = o; }
    type_node *result_type()		{ return typ; }
    void set_result_type(type_node *t)	{ typ = t; }

    virtual unsigned num_dsts()		{ return nd; }
    virtual operand dst_op(unsigned n=0);
    void dst_map(src_map_f f, void *x);
    virtual void set_num_dsts(unsigned n);
    virtual void set_dst(operand r)	{ set_dst(0, r); }
    virtual void set_dst(unsigned n, operand r);

    tree_instr *parent()		{ return par; }
    tree_node *owner();			/* like parent but skips dummy cpys */

    virtual unsigned num_srcs()		{ return 0; }
    operand src_op(unsigned n);
    void set_src_op(unsigned n, operand r);
    void src_map(src_map_f f, void *x);

    virtual boolean is_branch();

    instruction *clone(base_symtab *dst_scope = NULL);
    virtual instruction *clone_helper(replacements *r,boolean no_copy=FALSE)=0;
    virtual void find_exposed_refs(base_symtab *dst_scope, replacements *r)=0;

    void remove(unsigned dstnum = 0);	/* remove instr from expr tree */
    void clear_numbers();		/* remove instr numbers */

    static instruction *read(in_stream *is, base_symtab *syms, tree_node *t);
    virtual void write(out_stream *os);
    virtual void print(FILE *fp = stdout, int depth=0, int elab=0,
                       int *en = NULL);
};

DECLARE_DLIST_CLASS(instruction_list, instruction*);

/*
 *  Three operand instructions.  This class is used for instructions with
 *  up to three operand fields.  Not all of the operands have to be used.
 *  For example, io_nops don't use any of them.  Besides the methods to
 *  directly access the two source operands, other methods are provided to
 *  refer to these sources according to their uses in specific operations.
 *  Using these operation-specific methods with the wrong operations will
 *  cause errors.
 */

class in_rrr : public instruction {
private:
    operand src1, src2;

    operand *get_src_ptr(unsigned n);

public:
    in_rrr(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_rrr(if_ops o,			/* the opcode */
	   type_node *t = type_void,	/* result type */
	   operand d = operand(),	/* destination */
	   operand s1 = operand(),	/* source 1 */
	   operand s2 = operand());	/* source 2 */
    in_rrr();
    ~in_rrr();

    unsigned num_srcs()			{ return 2; }
    operand src1_op()			{ return src1; }
    operand src2_op()			{ return src2; }
    operand src_op();			/* single source operand */
    operand shift_cnt_op();		/* count for shift & rotate ops */
    operand dst_addr_op();		/* dest address for store & memcpy */
    operand src_addr_op();		/* source address for load & memcpy */

    void set_src1(operand r)		{ check_new_src(src1, r); src1 = r; }
    void set_src2(operand r)		{ check_new_src(src2, r); src2 = r; }
    void set_src(operand r);
    void set_shift_cnt_op(operand r);
    void set_dst_addr_op(operand r);
    void set_src_addr_op(operand r);

    boolean is_unary();			/* is the format "op dst = src1"? */
    boolean is_commutative();

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Conditional branch and jump instructions.
 */

class in_bj : public instruction {
private:
    label_sym *targ;
    operand src;

    operand *get_src_ptr(unsigned n);

public:
    in_bj(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_bj(if_ops o,			/* opcode */
	  label_sym *t,			/* target label */
	  operand r = operand());	/* optional source operand */
    in_bj();
    ~in_bj();

    unsigned num_srcs()			{ return 1; }
    label_sym *target()			{ return targ; }
    operand src_op()			{ return src; }

    void set_target(label_sym *t)	{ targ = t; }
    void set_src_op(operand r)		{ check_new_src(src, r); src = r; }

    unsigned num_dsts()			{ return 0; }
    operand dst_op(unsigned n=0)	{ return operand(); }
    void set_num_dsts(unsigned)		{ return; }	/* dummy method */
    void set_dst(operand)		{ no_dst_error(); }

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Load constant instructions.  Only "int", "float", and "symbol" immeds
 *  are allowed as immediate values.
 */

class in_ldc : public instruction {
private:
    immed val;

public:
    in_ldc(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_ldc(type_node *t,		/* result type */
	   operand d,			/* destination */
	   const immed &v);		/* immediate value */
    in_ldc();

    immed value()			{ return val; }

    void set_value(immed v)		{ val = v; }

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Call Instructions.  If the called routine does not return a value or
 *  that value is never used, the dst operand should be null.  The addr_op
 *  must be a pointer to a procedure of the appropriate type.  The number
 *  of arguments may be changed at any point; if necessary the argument
 *  operand array will be reallocated.
 */

class in_cal : public instruction {
private:
    operand addr;
    unsigned nargs;
    operand *args;

    void check_range(unsigned n);
    operand *get_src_ptr(unsigned n);

public:
    in_cal(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_cal(type_node *t,		/* return value type */
	   operand d,			/* destination operand */
	   operand s,			/* address of the called procedure */
	   unsigned n);			/* number of arguments */
    in_cal(type_node *t,		/* return value type */
	   operand d,			/* destination operand */
	   operand s,			/* address of the called procedure */
	   operand arg1 = operand(),	/* argument 1 */
	   operand arg2 = operand(),	/* argument 2 */
	   operand arg3 = operand(),	/* argument 3 */
	   operand arg4 = operand(),	/* argument 4 */
	   operand arg5 = operand());	/* argument 5 */
    in_cal();
    ~in_cal();

    unsigned num_srcs()			{ return num_args() + 1; }
    operand addr_op()			{ return addr; }
    unsigned num_args()			{ return nargs; }
    operand argument(unsigned n)	{ check_range(n); return args[n]; }

    void set_addr_op(operand r)		{ check_new_src(addr, r); addr = r; }
    void set_num_args(unsigned n);
    void set_argument(unsigned n, operand r);

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Array instructions.  Calculate the address of an array element with
 *  an optional offset to a structure field in the element.  Unlike the
 *  array_types, which retain all of the type information for the arrays,
 *  the array instructions contain only the information necessary to perform
 *  the address calculations: element size, number of elements in each
 *  dimension (aka "bounds"), and possibly an offset for FORTRAN arrays.
 *  Note that the values of the "bounds" only match the upper bounds in
 *  the array_type if the corresponding lower bounds are equal to one.
 *  The array instructions also allow arrays of arrays to be treated as
 *  multidimensional arrays, even though the type system does not support
 *  that.  The number of dimensions in an array instruction may be changed
 *  at any point; if necessary the operand arrays will be reallocated.
 */

class in_array : public instruction {
private:
    operand base;
    unsigned elemsz, dms;
    operand *indxs, *uppers;

    /* OPER_NULL, or for FORTRAN, how much to subtract so a(i) looks
       like a[i] and not a[i-1] or whatever. */
    operand offsetop;

    /* for an array of structs: offset within the array element */
    unsigned off; 

    void check_range(unsigned n);
    operand *get_src_ptr(unsigned n);

public:
    in_array(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_array(type_node *t,		/* result type */
	     operand d,			/* destination operand */
	     operand b,			/* base address */
	     unsigned sz,		/* element size */
	     unsigned dm,		/* number of dimensions */
	     unsigned f = 0,		/* optional offset for struct field */
	     operand fop = operand());	/* offset for FORTRAN arrays */
    in_array();
    ~in_array();

    unsigned num_srcs()			{ return 2 * dims() + 2; }
    operand base_op()			{ return base; }
    operand offset_op()			{ return offsetop; }
    unsigned offset()			{ return off; }
    unsigned elem_size()		{ return elemsz; }
    unsigned dims()			{ return dms; }
    operand index(unsigned n)		{ check_range(n); return indxs[n]; }
    operand bound(unsigned n)		{ check_range(n); return uppers[n]; }
    type_node *elem_type();

    void set_base_op(operand r)		{ check_new_src(base, r); base = r; }
    void set_offset_op(operand r)	{ check_new_src(offsetop, r);
					  offsetop = r; }
    void set_offset(unsigned f)		{ off = f; }
    void set_elem_size(unsigned n)	{ elemsz = n; }
    void set_dims(unsigned n);
    void set_index(unsigned n, operand r);
    void set_bound(unsigned n, operand r);

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Multi-way branch instructions.  Branch to the label selected by the
 *  source operand minus the lower bound.  If that value is out of range
 *  (negative or greater than or equal to the number of target labels)
 *  branch to the default label.  Note that the number of labels is not
 *  the same as the "range" in the old SUIF library.  The number of labels
 *  may be changed at any point; if necessary the label array will be
 *  reallocated.  The destination operand is unused and trying to set it
 *  will cause an error.
 */

class in_mbr : public instruction {
private:
    operand src;
    int low;
    unsigned nlabs;
    label_sym *dlab;
    label_sym **labs;

    void check_range(unsigned n);
    operand *get_src_ptr(unsigned n);

public:
    in_mbr(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_mbr(operand s,			/* source operand */
	   int l,			/* lower bound for source */
	   unsigned n,			/* number of target labels */
	   label_sym *def);		/* default label */
    in_mbr();
    ~in_mbr();

    unsigned num_srcs()			{ return 1; }
    operand src_op()			{ return src; }
    int lower()				{ return low; }
    unsigned num_labs()			{ return nlabs; }
    label_sym *default_lab()		{ return dlab; }
    label_sym *label(unsigned n)	{ check_range(n); return labs[n]; }

    void set_src(operand r)		{ check_new_src(src, r); src = r; }
    void set_lower(int l)		{ low = l; }
    void set_num_labs(unsigned n);
    void set_default_lab(label_sym *l)	{ dlab = l; }
    void set_label(unsigned n, label_sym *ls);

    unsigned num_dsts()			{ return 0; }
    operand dst_op(unsigned n=0)	{ return operand(); }
    void set_num_dsts(unsigned)		{ return; }	/* dummy method */
    void set_dst(operand)		{ no_dst_error(); }

    instruction *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    void print(FILE *fp = stdout, int depth=0, int elab=0, int *en = NULL);
};


/*
 *  Label pseudo-instructions.  The destination operand is unused and trying
 *  to set it will cause an error.
 */

class in_lab : public instruction {
private:
    label_sym *lab;

public:
    in_lab(if_ops o, in_stream *is, base_symtab *syms);
    in_lab(label_sym *s);
    in_lab();

    label_sym *label()			{ return lab; }
    void set_label(label_sym *l)	{ lab = l; }

    unsigned num_dsts()			{ return 0; }
    operand dst_op(unsigned n=0)	{ return operand(); }
    void set_num_dsts(unsigned)		{ return; }	/* dummy method */
    void set_dst(operand)		{ no_dst_error(); }

    virtual instruction *clone_helper(replacements *r, boolean no_copy=FALSE);
    virtual void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    virtual void print(FILE *fp=stdout, int depth=0, int elab=0, int *en=NULL);
};


/*
 *  Generic instructions.  This instruction class is only provided for
 *  non-standard extensions of SUIF; most SUIF passes will not handle
 *  generic instructions.  The array of source operands can be accessed
 *  using the base class methods.
 */

class in_gen : public instruction {
private:
    const char *opnm;
    unsigned ns;
    operand *srcs;

    void init(const char *o, operand d, unsigned n, operand s0,
              operand s1 = operand(), operand s2 = operand(),
              operand s3 = operand(), operand s4 = operand());
    operand *get_src_ptr(unsigned n);

public:
    in_gen(if_ops o, in_stream *is, base_symtab *syms, tree_node *t);
    in_gen(const char *o,		/* operation name (NOT the opcode) */
	   type_node *t,		/* result type */
	   operand d,			/* destination operand */
	   unsigned n);			/* number of source operands */

    in_gen(const char *o, type_node *t, operand d, unsigned n, operand s0,
	   operand s1 = operand(), operand s2 = operand(),
	   operand s3 = operand(), operand s4 = operand()) :
        instruction(io_gen, t), opnm(0), ns(0), srcs(0)
      { init(o, d, n, s0, s1, s2, s3, s4); }
    in_gen(const char *o, type_node *t, operand d, operand s0) :
        instruction(io_gen, t), opnm(0), ns(0), srcs(0)
      { init(o, d, 1, s0); }
    in_gen(const char *o, type_node *t, operand d, operand s0, operand s1) :
        instruction(io_gen, t), opnm(0), ns(0), srcs(0)
      { init(o, d, 2, s0, s1); }
    in_gen(const char *o, type_node *t, operand d, operand s0, operand s1,
           operand s2) : instruction(io_gen, t), opnm(0), ns(0), srcs(0)
      { init(o, d, 3, s0, s1, s2); }
    in_gen(const char *o, type_node *t, operand d, operand s0, operand s1,
           operand s2, operand s3) : instruction(io_gen, t), 
	  opnm(0), ns(0), srcs(0)
      { init(o, d, 4, s0, s1, s2, s3); }
    in_gen(const char *o, type_node *t, operand d, operand s0, operand s1,
           operand s2, operand s3, operand s4) : instruction(io_gen, t), 
	  opnm(0), ns(0), srcs(0)
      { init(o, d, 5, s0, s1, s2, s3, s4); }
    in_gen();
    ~in_gen();

    const char *name()			{ return opnm; }
    unsigned num_srcs()			{ return ns; }

    void set_name(const char *o);
    void set_num_srcs(unsigned n);

    virtual instruction *clone_helper(replacements *r, boolean no_copy=FALSE);
    virtual void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void write(out_stream *os);
    virtual void print(FILE *fp=stdout, int depth=0, int elab=0, int *en=NULL);
};

#endif /* INSTRUCTION_H */
