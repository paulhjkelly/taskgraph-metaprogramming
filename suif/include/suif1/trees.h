/*  SUIF Tree Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef TREES_H
#define TREES_H

#pragma interface

RCS_HEADER(trees_h,
    "$Id$")

struct replacements;


enum tree_kinds {
    TREE_INSTR,
    TREE_LOOP,
    TREE_FOR,
    TREE_IF,
    TREE_BLOCK
};


/*
 *  The tree_node_list map function applies a tree_map_f function to every
 *  tree_node in a tree_node_list, optionally including those in child lists
 *  (e.g. loop bodies).  Parameters for the tree_map_f function can be passed
 *  through the void* argument.
 */

class tree_node;
typedef void (*tree_map_f)(tree_node *t, void *x);


/*
 *  Tree_node_lists are doubly-linked lists of tree_nodes.  Each list contains
 *  a back pointer to its parent tree_node; these pointers are maintained
 *  automatically.  The "set_elem" method overrides the default list class
 *  method to automatically set the tree_node back pointers when a list
 *  element is added to the list.  Methods are provided here to convert
 *  back and forth between expression trees and flat lists of instructions.
 *  The is_op() method recognizes a special case when the list consists of
 *  a single copy instruction with a null destination.  The op() method
 *  returns the source operand of that copy instruction.  These functions
 *  are used to handle the operands in tree_fors.
 */

DECLARE_DLIST_CLASSES(tree_node_list_base, tree_node_list_e,
		      tree_node_list_iter, tree_node*);

class tree_node_list : public tree_node_list_base {
    friend class tree_proc;
    friend class tree_block;
    friend class tree_loop;
    friend class tree_for;
    friend class tree_if;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    tree_node_list(const tree_node_list &);
    tree_node_list &operator=(const tree_node_list &);

private:
    tree_node *par;

    void set_parent(tree_node *pr)	{ par = pr; }

public:
    tree_node_list(in_stream *is, tree_node *pr, block_symtab *syms);
    tree_node_list(tree_node *pr) : par(pr) { }
    tree_node_list() : par(0)		    { }
    ~tree_node_list();

    tree_node *parent()			{ return par; }
    base_symtab *scope();

    void map(tree_map_f f, void *x,
	     boolean preorder = TRUE,	/* visit a node before its children */
	     boolean descend = TRUE);	/* visit children of the nodes? */

    /* virtual function to automatically set the back pointers when an
       element is added to the list */
    void set_elem(tree_node_list_e *e);

    tree_node_list *clone(base_symtab *dst_scope = NULL);
    tree_node_list *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    /* methods for handling tree_for operands stored in lists */
    boolean is_op();
    operand op();
    void set_op(operand o);
    boolean op_is_intconst(int *retval);

    /* convert between expression trees and flat lists */
    void flatten();
    void cvt_to_trees();

    /* remove all instruction and tree_node numbers */
    void clear_numbers();

    void print(FILE *fp=stdout, int depth=0);
    void print_expr(FILE *fp=stdout, int depth=0);
    void write(out_stream *os);
};


/*
 *  Tree nodes represent the abstract syntax trees for SUIF programs.
 *  The base class is abstract and cannot be directly instantiated.
 *  Except for the tree_procs at the roots of the trees, each tree_node is
 *  held on a list and has a back pointer to its parent list.  To simplify
 *  using the list methods that operate on list elements, each tree_node
 *  also includes a back pointer to its list element.  Both of these back
 *  pointers are set automatically when a node is added to a tree_node_list.
 *
 *  The base tree_node class includes several methods to access objects that
 *  are frequently needed.  The "proc" method returns the symbol for the
 *  procedure containing a tree_node; this will not work if the tree_proc at
 *  the root is not yet attached to a proc_sym.  The "scope" method returns
 *  the symbol table for the closest enclosing scope at a tree_node.
 *
 *  The tree_node and tree_node_list classes include methods to clone
 *  trees.  These are often needed when performing transformations (e.g.
 *  loop unrolling) on the SUIF code.  When cloning a tree or part of a tree,
 *  all of the objects within the tree are duplicated as needed.  Besides the
 *  tree_nodes and their contents, this includes symbol tables, types, and
 *  symbols.  If the code being cloned contains labels, new label symbols are
 *  automatically created.  All of the known references (including those in
 *  annotations) to cloned objects are updated.  The "find_exposed_refs"
 *  and "clone_helper" methods provide more precise control over the cloning
 *  process; see the SUIF reference manual for more details.
 */

class tree_node : public suif_object {
    friend class tree_node_list;
    friend class tree_proc;

private:
    unsigned num;
    tree_node_list *par;		/* parent list */
    tree_node_list_e *le;		/* list element containing this node */

    void set_parent(tree_node_list *pr)	{ par = pr; }
    void set_list_e(tree_node_list_e *e) { le = e; }

protected:
    tree_node()	: num(0), par(0), le(0) { }

    void set_number(unsigned n)		{ num = n; }

public:
    virtual ~tree_node()		{ }

    object_kinds object_kind()		{ return TREE_OBJ; }

    virtual tree_kinds kind() = 0;
    unsigned number()			{ return num; }
    tree_node_list *parent()		{ return par; }
    tree_node_list_e *list_e()		{ return le; }
    virtual proc_sym *proc();
    virtual base_symtab *scope();

    virtual unsigned num_child_lists(void) const = 0;
    virtual tree_node_list *child_list_num(unsigned num) = 0;

    tree_node *clone(base_symtab *dst_scope = NULL);
    virtual tree_node *clone_helper(replacements *r,boolean no_copy = FALSE)=0;
    virtual void find_exposed_refs(base_symtab *dst_scope, replacements *r)=0;

    boolean is_instr()			{ return kind() == TREE_INSTR; }
    boolean is_loop()			{ return kind() == TREE_LOOP; }
    boolean is_for()			{ return kind() == TREE_FOR; }
    boolean is_if()			{ return kind() == TREE_IF; }
    boolean is_block()			{ return kind() == TREE_BLOCK; }
    virtual boolean is_proc()		{ return FALSE; }

    void map(tree_map_f f, void *x, boolean preorder = TRUE,
	     boolean descend = TRUE);

    virtual void clear_numbers()	{ set_number(0); }

    virtual void print(FILE *fp=stdout, int depth=0) = 0;
    virtual void write(out_stream *os) = 0;
};


/*
 *  The only effect of a tree_block is to introduce a new scope and an
 *  associated symbol table.  This is useful both for retaining scope
 *  information from the source program for debugging purposes and for
 *  transforming code (name conflicts are easily avoided by introducing
 *  a new scope).  Besides the symbol table, a tree_block just contains
 *  a list of the tree_nodes inside the block.
 */

class tree_block : public tree_node {
protected:
    block_symtab *st;
    tree_node_list *bod;

    tree_block() : st(0), bod(0)	{ }

public:
    tree_block(instruction *mrk, in_stream *is, block_symtab *syms);
    tree_block(tree_node_list *b,	/* contents of the block */
	       block_symtab *s);	/* local symbol table */
    ~tree_block();

    tree_kinds kind()			{ return TREE_BLOCK; }
    block_symtab *symtab()		{ return st; }
    tree_node_list *body()		{ return bod; }

    void set_symtab(block_symtab *s);
    void set_body(tree_node_list *b);

    unsigned num_child_lists(void) const  { return 1; }
    tree_node_list *child_list_num(unsigned num);

    tree_block *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_block *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void clear_numbers();
    void write(out_stream *os);
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  The tree_proc class is derived from tree_block to include some features
 *  specific to the top-level procedure nodes.  First of all, the top-level
 *  symbol table must be a proc_symtab instead of just an ordinary
 *  block_symtab.  Thus, the tree_proc constructor requires a proc_symtab
 *  and the "proc_syms" method is provided to cast the symtab pointer to be
 *  a proc_symtab.  A tree_proc node also records the symbol of the corres-
 *  ponding procedure; this is set automatically by the proc_sym.  This
 *  class is also responsible for assigning unique numbers to all of the
 *  instructions within a procedure.  The "number_instrs" method is called
 *  automatically before writing out a procedure, but it can also be called
 *  manually to assign numbers to newly created instructions.
 */

class tree_proc : public tree_block {
    friend class proc_sym;

private:
    proc_sym *psym;

    void set_proc(proc_sym *ps)		{ psym = ps; }

    static void number_instrs_tree_map(tree_node *t, void *x);
    static void number_instrs_instr_map(instruction *i, void *x);

public:
    tree_proc(in_stream *is, proc_sym *top);
    tree_proc(tree_node_list *b, proc_symtab *s)
	: tree_block(b, s), psym(0) { }

    proc_symtab *proc_syms()		{ return static_cast<proc_symtab *>(symtab()); }
    base_symtab *scope()		{ return proc()->file()->symtab(); }

    boolean is_proc()			{ return TRUE; }
    proc_sym *proc()			{ return psym; }

    tree_proc *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_proc *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    /* inherit the find_exposed_refs method from tree_block */

    void number_instrs();
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  A tree_loop represents a generic do-while loop.  It contains two lists
 *  of tree_nodes: the body and the test.  The body list may contain branches
 *  to the "continue" and "break" labels.  The test list typically contains
 *  code to evaluate the "while" expression and conditionally branch back to
 *  the "top-of-loop" label.  The tree_loop labels must be in the current
 *  scope, but the label positions are implicit rather than being marked by
 *  label instructions.  When the loop is expanded to lower level SUIF
 *  instructions, the "top-of-loop" label instruction is inserted at the
 *  beginning of the body list; the "continue" label instruction is inserted
 *  between the body and the test lists; and the "break" label instruction
 *  is appended to the end of the test list.
 */

class tree_loop : public tree_node {
private:
    tree_node_list *bod, *tst;
    label_sym *clab, *blab, *tol;

public:
    tree_loop(instruction *mrk, in_stream *is, block_symtab *syms);
    tree_loop(tree_node_list *b,	/* loop body */
	      tree_node_list *t,	/* test code */
	      label_sym *cl,		/* continue label */
	      label_sym *bl,		/* break label */
	      label_sym *tl);		/* top-of-loop label */
    ~tree_loop();

    tree_kinds kind()			{ return TREE_LOOP; }
    tree_node_list *body()		{ return bod; }
    tree_node_list *test()		{ return tst; }
    label_sym *contlab()		{ return clab; }
    label_sym *brklab()			{ return blab; }
    label_sym *toplab()			{ return tol; }

    void set_body(tree_node_list *b);
    void set_test(tree_node_list *t);
    void set_contlab(label_sym *s)	{ clab = s; }
    void set_brklab(label_sym *s)	{ blab = s; }
    void set_toplab(label_sym *s)	{ tol = s; }

    unsigned num_child_lists(void) const { return 2; }
    tree_node_list *child_list_num(unsigned num);

    tree_loop *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_loop *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void clear_numbers();
    void write(out_stream *os);
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  FOR loop tests: These are straightforward except for the GELE tests.
 *  In FORTRAN, it may not be possible to determine the direction of a loop
 *  at compile time.  For example, in "DO I=1,N" the sign of N is unknown.
 *  The GELE tests indicate that the loop test may be either GE or LE
 *  depending on the sign of the step value.  The GELE, EQ, and NEQ tests
 *  are eliminated by the expander's cleanup pass right after the front-end
 *  and should never be encountered after that point.
 */

enum tree_for_test {
    FOR_EQ, FOR_NEQ,
    FOR_SGELE, FOR_SGT, FOR_SGTE, FOR_SLT, FOR_SLTE, /* signed */
    FOR_UGELE, FOR_UGT, FOR_UGTE, FOR_ULT, FOR_ULTE /* unsigned */
};


/*
 *  Tree_fors represent nicely-structured FOR loops.  Other loops are
 *  treated as tree_loops.  The expander's cleanup pass is responsible for
 *  checking tree_fors produced by the front-end to make sure that they
 *  conform to the semantics described below.  See the expander documentation
 *  for details.
 *
 *  The lower bound, upper bound, and step operands are evaluated once at
 *  the beginning of the loop.  Next, the lower bound is compared to the upper
 *  bound using the specified comparison operator.  If the comparison fails,
 *  the loop does not execute and control passes to the "break" label.
 *  Otherwise, the lower bound is assigned to the index variable, and any
 *  code in the optional "landing pad" list is executed.  After this, the
 *  body is executed repeatedly, incrementing the index variable by the step
 *  value on every iteration until the comparison with the upper bound value
 *  fails.  The instructions to perform this "test" code are automatically
 *  created when the tree_for is expanded to lower-level SUIF code; they are
 *  not included as part of the body.  The body list may contain branches to
 *  the "break" and "continue" labels.  As with tree_loops, the position of
 *  these labels is implicit until the tree_for is expanded.
 *
 *  The lower bound, upper bound, and step are generally treated as operands;
 *  however, they are actually contained on tree_node_lists, and if necessary
 *  these lists can be accessed directly.  At all times, however, each list
 *  is required to contain a single expression tree (possibly in a flattened
 *  form with multiple tree_instrs) with a dummy copy instruction at the root.
 *  The destination of the dummy copy should be a null operand.
 */

class tree_for : public tree_node {
private:
    var_sym *ind;
    tree_for_test tst;
    label_sym *clab, *blab;
    tree_node_list *bod, *lpad, *lower, *upper, *step;

public:
    tree_for(instruction *mrk, in_stream *is, block_symtab *syms);

    tree_for(var_sym *i,		/* index variable */
	     tree_for_test t,		/* test */
	     label_sym *cl,		/* continue label */
	     label_sym *bl,		/* break label */
	     tree_node_list *b,		/* loop body */
	     operand lb,		/* lower bound */
	     operand ub,		/* upper bound */
	     operand stp,		/* step */
	     tree_node_list *lp);	/* landing pad */

    /* alternate constructor with lists instead of operands */
    tree_for(var_sym *i,
	     tree_for_test t,
	     label_sym *cl,
	     label_sym *bl,
	     tree_node_list *b,
	     tree_node_list *lb,
	     tree_node_list *ub,
	     tree_node_list *stp,
	     tree_node_list *lp);

    ~tree_for();

    tree_kinds kind()			{ return TREE_FOR; }

    var_sym *index()			{ return ind; }
    tree_for_test test()		{ return tst; }
    label_sym *contlab()		{ return clab; }
    label_sym *brklab()			{ return blab; }
    tree_node_list *body()		{ return bod; }
    tree_node_list *landing_pad()	{ return lpad; }
    operand lb_op()			{ return lower->op(); }
    operand ub_op()			{ return upper->op(); }
    operand step_op()			{ return step->op(); }
    tree_node_list *lb_list()		{ return lower; }
    tree_node_list *ub_list()		{ return upper; }
    tree_node_list *step_list()		{ return step; }

    void set_index(var_sym *vs)		{ ind = vs; }
    void set_test(tree_for_test t)	{ tst = t; }
    void set_contlab(label_sym *s)	{ clab = s; }
    void set_brklab(label_sym *s)	{ blab = s; }
    void set_body(tree_node_list *b);
    void set_landing_pad(tree_node_list *lp);
    void set_lb_op(operand o)		{ lower->set_op(o); }
    void set_ub_op(operand o)		{ upper->set_op(o); }
    void set_step_op(operand o)		{ step->set_op(o); }
    void set_lb_list(tree_node_list *lb);
    void set_ub_list(tree_node_list *ub);
    void set_step_list(tree_node_list *stp);

    boolean lb_is_constant(int *retval);
    boolean ub_is_constant(int *retval);
    boolean step_is_constant(int *retval);

    unsigned num_child_lists(void) const  { return 5; }
    tree_node_list *child_list_num(unsigned num);

    tree_for *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_for *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void clear_numbers();
    void write(out_stream *os);
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  The tree_if class is used to represent arbitrary "if-then-else" structures.
 *  A tree_if contains three lists of tree_nodes.  The "header" list contains
 *  code to evaluate the "if" condition and branch to the "jumpto" label if
 *  the condition is false.  Otherwise it falls through to the "then" part,
 *  which implicitly ends with a jump around the "else" part.  The "jumpto"
 *  label is implicitly located at the beginning of the "else" part.  Either
 *  the "then" or the "else" part may be empty.
 */

class tree_if : public tree_node {
private:
    label_sym *jmp;
    tree_node_list *hdr;
    tree_node_list *then;
    tree_node_list *els;

public:
    tree_if(instruction *mrk, in_stream *is, block_symtab *syms);
    tree_if(label_sym *j,
	    tree_node_list *h,		/* header */
	    tree_node_list *t,		/* then part */
	    tree_node_list *e);		/* else part */
    ~tree_if();

    tree_kinds kind()			{ return TREE_IF; }
    label_sym *jumpto()			{ return jmp; }
    tree_node_list *header()		{ return hdr; }
    tree_node_list *then_part()		{ return then; }
    tree_node_list *else_part()		{ return els; }

    void set_jumpto(label_sym *s)	{ jmp = s; }
    void set_header(tree_node_list *h);
    void set_then_part(tree_node_list *t);
    void set_else_part(tree_node_list *e);

    unsigned num_child_lists(void) const  { return 3; }
    tree_node_list *child_list_num(unsigned num);

    tree_if *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_if *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void clear_numbers();
    void write(out_stream *os);
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  Tree_instr nodes are at the leaves of the abstract syntax trees.  These
 *  nodes are just containers for SUIF instructions; each one holds a single
 *  instruction or expression tree.  Because the library automatically
 *  maintains back pointers from the SUIF instructions to the tree_instrs
 *  that contain them, the "remove_instr" method must be called whenever
 *  an instruction is moved out of a tree_instr.  (This is automatically
 *  done when removing an instruction from an expression tree with the
 *  instruction::remove method.)  The "instr_map" method applies a function
 *  to all of the instructions in an expression tree.
 *
 *  Unlike all of the other classes derived from the suif_object class, the
 *  current implementation does not fully support annotations on tree_instrs.
 *  The tree_instrs contain lists of annotations and these can be used if
 *  necessary, but the annotations will not be saved when the instructions
 *  are written out.  This is not really a problem, though, because it makes
 *  more sense to attach the annotations to the actual SUIF instructions.
 */

typedef void (*instr_map_f)(instruction *i, void *x);

class tree_instr : public tree_node {
private:
    instruction *ins;

public:
    tree_instr(instruction *i) : ins(0) { set_instr(i); }
    ~tree_instr();			/* delete the instr & its expr trees */

    tree_kinds kind()			{ return TREE_INSTR; }
    instruction *instr()		{ return ins; }
    void set_instr(instruction *i);
    void remove_instr(instruction *i);
    void instr_map(instr_map_f f, void *x, boolean preorder = TRUE);

    unsigned num_child_lists(void) const  { return 0; }
    tree_node_list *child_list_num(unsigned /* num */)
	{ assert(FALSE); return NULL; }

    tree_instr *clone(base_symtab *dst_scope = NULL)
	{ return static_cast<tree_instr *>(tree_node::clone(dst_scope)); }
    tree_node *clone_helper(replacements *r, boolean no_copy = FALSE);
    void find_exposed_refs(base_symtab *dst_scope, replacements *r);

    void clear_numbers();
    void write(out_stream *os);
    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  Structure to keep track of objects that have been replaced when
 *  cloning trees.  For each entry in one of the ``old'' lists, the
 *  corresponding entry in the ``new'' list is its replacement.
 */

struct replacements {

    sym_node_list oldsyms;
    sym_node_list newsyms;

    type_node_list oldtypes;
    type_node_list newtypes;

    var_def_list olddefs;
    var_def_list newdefs;

    instruction_list oldinstrs;
    instruction_list newinstrs;

    base_symtab_list oldtabs;
    base_symtab_list newtabs;

    /* methods used by the find_exposed_refs code */
    void add_sym_ref(sym_node *s, base_symtab *dst_scope,
		     boolean label_def = FALSE);
    void add_type_ref(type_node *t, base_symtab *dst_scope);
    void add_def_ref(var_def *d, base_symtab *dst_scope);
    void add_instr_ref(instruction *i);
    void add_tab_ref(base_symtab *t);

    void resolve_exposed_refs(base_symtab *dst_scope);

    replacements& operator=(replacements &r);
};

#endif /* TREES_H */
