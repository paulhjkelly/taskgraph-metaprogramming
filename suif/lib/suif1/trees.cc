/* Implementation of Trees */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "trees.h"

#define RCS_BASE_FILE trees_cc

#include "suif1.h"
#include "suif_internal.h"

RCS_BASE(
    "$Id$")


/*
 *  Recursively ascend the tree to find the symbol for the procedure
 *  containing this tree_node.  Eventually, this will reach the tree_proc
 *  at the root of the tree, and its "proc" method will override this
 *  virtual function to return the proc_sym.  If the search fails at any
 *  point, it returns NULL.
 */

proc_sym *
tree_node::proc ()
{
    tree_node_list *parent_list = parent();
    if (!parent_list) return NULL;

    tree_node *parent_node = parent_list->parent();
    if (!parent_node) return NULL;

    /* find the parent's proc; one of the ancestors should be a tree_proc */
    return parent_node->proc();
}


/*
 *  Recursively ascend the tree to find the symbol table for the closest
 *  enclosing scope.  As soon as this reaches a tree_block, the function
 *  will return the symtab pointer for that block.
 */

base_symtab *
tree_node::scope (void)
{
    tree_node_list *parent_list = parent();
    assert_msg(parent_list, ("tree_node::scope - no parent list"));
    return parent_list->scope();
}


/*
 *  Map a function over the nodes in a tree_node.
 */

void
tree_node::map (tree_map_f f, void *x, boolean preorder, boolean descend)
{
    unsigned num_children = num_child_lists();
    for (unsigned child_num = 0; child_num < num_children; ++child_num)
	child_list_num(child_num)->map(f, x, preorder, descend);
}


/*
 *  Clone a tree_node.  We must first find all of the exposed references
 *  to symbols and types in the subtree and create corresponding objects
 *  to be used in the cloned tree.
 */

tree_node *
tree_node::clone (base_symtab *dst_scope)
{
    replacements r;
    find_exposed_refs(dst_scope, &r);
    r.resolve_exposed_refs(dst_scope);
    return clone_helper(&r);
}


/*****************************************************************************/


/*
 *  Create a new tree_block.  The body and symtab must be provided.
 */

tree_block::tree_block (tree_node_list *b, block_symtab *s)
{
    set_symtab(s);
    set_body(b);
}


/*
 *  Destroy a tree_block and its contents.  The symbol table is removed from
 *  its parent before being deleted.
 */

tree_block::~tree_block ()
{
    if (body()) delete body();
    if (symtab()) {
        if (symtab()->parent() != NULL)
            symtab()->parent()->remove_child(symtab());
	delete symtab();
    }
}


/*
 *  Set the symbol table associated with a block.  Automatically updates
 *  the symtab's pointer to this block.
 */

void
tree_block::set_symtab (block_symtab *s)
{
    st = s;
    if (st) st->set_block(this);
}


/*
 *  Set the body of a block.  Automatically updates the body list's parent
 *  pointer.
 */

void
tree_block::set_body (tree_node_list *b)
{
    bod = b;
    if (bod) bod->set_parent(this);
}


/*
 *  Return the requested child list.
 */
tree_node_list *
tree_block::child_list_num(unsigned num)
{
    switch (num)
      {
        case 0:
            return body();
        default:
            assert(FALSE);
            return NULL;
      }
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this node.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_block::clear_numbers ()
{
    tree_node::clear_numbers();
    body()->clear_numbers();
}


/*
 *  Read a tree_block from an input stream.
 */

tree_block::tree_block (instruction *mrk, in_stream *is, block_symtab *syms)
{
    annote *an = mrk->annotes()->get_annote(k_block);
    const char *block_name = an->immeds()->pop().string();
    set_number(an->immeds()->pop().unsigned_int());
    delete an;

    mrk_to_annotes(mrk);
    delete mrk;

    st = new block_symtab(block_name);
    st->set_block(this);
    syms->add_child(st);
    st->read(is);

    bod = new tree_node_list(is, this, st);
}


/*
 *  Write a tree_block to an output stream.
 */

void
tree_block::write (out_stream *os)
{
    base_symtab *old_scope = (is_proc() ? NULL : scope());
    assert(write_scope == old_scope);
    write_scope = symtab();

    in_rrr *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_block);

    an->immeds()->append(immed(symtab()->name()));
    an->immeds()->append(immed(number()));
    mrk->annotes()->append(an);

    copy_annotes(mrk);
    mrk->write(os);
    delete mrk;
    
    symtab()->write(os);
    body()->write(os);

    assert(write_scope == symtab());
    write_scope = old_scope;
}


/*
 *  Print a tree_block.
 */

void
tree_block::print (FILE *fp, int depth)
{
    /* first print the number */
    if (number() != 0) {
	fprintf(fp, "%4u: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    fputs("BLOCK", fp);
    print_annotes(fp, depth);
    putc('\n', fp);
    symtab()->print(fp, depth+1);
    body()->print(fp, depth+1);
    suif_indent(fp, depth);
    fputs("BLOCK END\n", fp);
}


/*
 *  Clone a tree_block.  The associated symbol table and its contents are
 *  also cloned and the replacements list is updated so that references to
 *  local symbols and types will be changed to the entries in the cloned
 *  symtab.
 */

tree_node *
tree_block::clone_helper (replacements *r, boolean no_copy)
{
    tree_block *result = this;
    block_symtab *newsymtab = symtab()->clone_helper(r, no_copy);
    tree_node_list *newbody = body()->clone_helper(r, no_copy);
    if (!no_copy) {
	result = new tree_block(newbody, newsymtab);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Find the exposed references in a tree_block.
 */

void
tree_block::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    /* note: it is essential that we visit the body before the symtab;
       otherwise, the objects defined in the symtab will not be removed
       from the replacements lists */

    body()->find_exposed_refs(dst_scope, r);
    symtab()->find_exposed_refs(dst_scope, r);
    find_annote_refs(dst_scope, r);
}


/*****************************************************************************/


/*
 *  Print a tree_proc.
 */

void
tree_proc::print (FILE *fp, int depth)
{
    /* first print the number */
    if (number() != 0) {
	fprintf(fp, "%4u: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    fputs("PROC ", fp);
    proc()->print(fp);
    print_annotes(fp, depth);
    putc('\n', fp);
    symtab()->print(fp, depth+1);
    body()->print(fp, depth+1);
    suif_indent(fp, depth);
    fputs("PROC END\n", fp);
}


/*
 *  Read a tree_proc from an input stream.  Unlike the other input
 *  constructors, this one is not given the first instruction as a
 *  parameter.  Instead it must read the instruction itself.  Other
 *  than that and the fact that the symbol table is a proc_symtab,
 *  this is just like reading a tree_block.
 */

tree_proc::tree_proc (in_stream *is, proc_sym *top)
{
    instruction *mrk = instruction::read(is, top->parent(), NULL);
    annote *an = mrk->annotes()->get_annote(k_block);
    assert_msg((mrk->opcode() == io_mrk) && an,
	       ("tree_proc::tree_proc - missing proc begin mark"));
    const char *block_name = an->immeds()->pop().string();
    set_number(an->immeds()->pop().unsigned_int());
    delete an;

    mrk_to_annotes(mrk);
    delete mrk;

    psym = top;

    st = new proc_symtab(block_name);
    top->file()->symtab()->add_child(st);
    st->set_block(this);
    st->read(is);

    bod = new tree_node_list(is, this, st);
}


/*
 *  Clone a tree_proc.  This is just like cloning a tree_block except that
 *  the new symbol table is a proc_symtab instead of a block_symtab.
 */

tree_node *
tree_proc::clone_helper (replacements *r, boolean no_copy)
{
    tree_proc *result = this;
    proc_symtab *newsymtab = proc_syms()->clone_helper(r, no_copy);
    tree_node_list *newbody = body()->clone_helper(r, no_copy);
    if (!no_copy) {
	result = new tree_proc(newbody, newsymtab);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Assign unique id numbers to all of the instructions in the procedure
 *  that do not yet have numbers.  The proc_symtab keeps track of the next
 *  id number to be used.  This code just traverses the abstract syntax tree
 *  and the expression trees to find all of the instructions.
 */

void
tree_proc::number_instrs ()
{
    body()->map(tree_proc::number_instrs_tree_map, (void *)proc_syms());
}


void
tree_proc::number_instrs_tree_map (tree_node *t, void *x)
{
    if (t->is_instr()) {
	/* set the numbers for the instructions in this node */
	tree_instr *ti = (tree_instr *)t;
	ti->instr_map(tree_proc::number_instrs_instr_map, x);
    } else {
	if (t->number() == 0) {
	    proc_symtab *symtab = (proc_symtab *)x;
	    t->set_number(symtab->next_instr_num());
	}
    }
}


void
tree_proc::number_instrs_instr_map (instruction *i, void *x)
{
    if (i->number() == 0) {
	proc_symtab *symtab = (proc_symtab *)x;
	i->set_number(symtab->next_instr_num());
    }
}


/*****************************************************************************/


/*
 *  Create a new tree_loop.
 */

tree_loop::tree_loop (tree_node_list *b, tree_node_list *t, label_sym *cl,
		      label_sym *bl, label_sym *tl)
{
    set_body(b);
    set_test(t);
    clab = cl;
    blab = bl;
    tol = tl;
}


/*
 *  Destroy a tree_loop.
 */

tree_loop::~tree_loop ()
{
    if (body()) delete body();
    if (test()) delete test();
}


/*
 *  Set the body list for a tree_loop.  Automatically sets the parent
 *  pointer for the body list.
 */

void
tree_loop::set_body (tree_node_list *b)
{
    bod = b;
    if (bod) bod->set_parent(this);
}


/*
 *  Set the test list for a tree_loop.  Automatically sets the parent
 *  pointer for the test list.
 */

void
tree_loop::set_test (tree_node_list *t)
{
    tst = t;
    if (tst) tst->set_parent(this);
}


/*
 *  Return the requested child list.
 */
tree_node_list *
tree_loop::child_list_num(unsigned num)
{
    switch (num)
      {
        case 0:
            return body();
        case 1:
            return test();
        default:
            assert(FALSE);
            return NULL;
      }
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this node.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_loop::clear_numbers ()
{
    tree_node::clear_numbers();
    body()->clear_numbers();
    test()->clear_numbers();
}


/*
 *  Read a tree_loop from an input stream.
 */

tree_loop::tree_loop (instruction *mrk, in_stream *is, block_symtab *syms)
{
    annote *an = mrk->annotes()->get_annote(k_loop);
    clab = (label_sym *)an->immeds()->pop().symbol();
    blab = (label_sym *)an->immeds()->pop().symbol();
    tol = (label_sym *)an->immeds()->pop().symbol();
    set_number(an->immeds()->pop().unsigned_int());
    delete an;

    mrk_to_annotes(mrk);
    delete mrk;

    bod = new tree_node_list(is, this, syms);
    tst = new tree_node_list(is, this, syms);
}


/*
 *  Write a tree_loop to an output stream.
 */

void
tree_loop::write (out_stream *os)
{
    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_loop);

    an->immeds()->append(immed(contlab()));
    an->immeds()->append(immed(brklab()));
    an->immeds()->append(immed(toplab()));
    an->immeds()->append(immed(number()));
    mrk->annotes()->append(an);

    copy_annotes(mrk);
    mrk->write(os);
    delete mrk;
    
    body()->write(os);
    test()->write(os);
}


/*
 *  Print a tree_loop.
 */

void
tree_loop::print (FILE *fp, int depth)
{
    /* first print the number */
    if (number() != 0) {
	fprintf(fp, "%4u: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    fputs("LOOP (Top=", fp);
    toplab()->print(fp);
    fputs(" Cont=", fp);
    contlab()->print(fp);
    fputs(" Brk=", fp);
    brklab()->print(fp);
    putc(')', fp);
    print_annotes(fp, depth);
    putc('\n', fp);

    suif_indent(fp, depth);
    fputs("LOOP BODY\n", fp);
    body()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("LOOP TEST\n", fp);
    test()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("LOOP END\n", fp);
}


/*
 *  Clone a tree_loop.
 */

tree_node *
tree_loop::clone_helper (replacements *r, boolean no_copy)
{
    tree_loop *result = this;
    tree_node_list *newbody = body()->clone_helper(r, no_copy);
    tree_node_list *newtest = test()->clone_helper(r, no_copy);
    label_sym *cont = (label_sym *)contlab()->clone_helper(r);
    label_sym *brk = (label_sym *)brklab()->clone_helper(r);
    label_sym *top = (label_sym *)toplab()->clone_helper(r);
    if (!no_copy) {
	assert_msg((cont != contlab()) &&
		   (brk != brklab()) &&
		   (top != toplab()),
		   ("tree_loop::clone_helper - "
		    "label(s) have not been replaced"));
	result = new tree_loop(newbody, newtest, cont, brk, top);
    } else {
	set_contlab(cont);
	set_brklab(brk);
	set_toplab(top);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Find the exposed references in a tree_loop.
 */

void
tree_loop::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    /* record the labels that are defined in this loop */
    r->add_sym_ref(contlab(), dst_scope, TRUE);
    r->add_sym_ref(brklab(), dst_scope, TRUE);
    r->add_sym_ref(toplab(), dst_scope, TRUE);

    test()->find_exposed_refs(dst_scope, r);
    body()->find_exposed_refs(dst_scope, r);

    find_annote_refs(dst_scope, r);
}


/*****************************************************************************/


/*
 *  Create a new tree_for.
 */

tree_for::tree_for (var_sym *i, tree_for_test t, label_sym *cl, label_sym *bl,
		    tree_node_list *b, operand lb, operand ub, operand stp,
		    tree_node_list *lp)
{
    ind = i;
    tst = t;
    clab = cl;
    blab = bl;
    set_body(b);
    set_landing_pad(lp);

    lower = new tree_node_list(this);
    upper = new tree_node_list(this);
    step = new tree_node_list(this);

    lower->set_op(lb);
    upper->set_op(ub);
    step->set_op(stp);
}


tree_for::tree_for (var_sym *i, tree_for_test t, label_sym *cl, label_sym *bl,
		    tree_node_list *b, tree_node_list *lb, tree_node_list *ub,
		    tree_node_list *stp, tree_node_list *lp)
{
    ind = i;
    tst = t;
    clab = cl;
    blab = bl;
    set_body(b);
    set_lb_list(lb);
    set_ub_list(ub);
    set_step_list(stp);
    set_landing_pad(lp);
}


/*
 *  Destroy a tree_for.
 */

tree_for::~tree_for ()
{
    if (lb_list()) delete lb_list();
    if (ub_list()) delete ub_list();
    if (step_list()) delete step_list();
    if (body()) delete body();
    if (landing_pad()) delete landing_pad();
}


/*
 *  Set the body of a tree_for.  Automatically sets the parent pointer
 *  in the body list.
 */

void
tree_for::set_body (tree_node_list *b)
{
    bod = b;
    if (bod) bod->set_parent(this);
}


/*
 *  Set the landing pad of a tree_for.  Automatically sets the parent
 *  pointer in the landing pad list.
 */

void
tree_for::set_landing_pad (tree_node_list *lp)
{
    lpad = lp;
    if (lpad) lpad->set_parent(this);
}


/*
 *  Set the lower bound list of a tree_for.  Automatically sets the
 *  parent pointer in the lower bound list.
 */

void
tree_for::set_lb_list (tree_node_list *lb)
{
    lower = lb;
    if (lower) lower->set_parent(this);
}


/*
 *  Set the upper bound list of a tree_for.  Automatically sets the
 *  parent pointer in the upper bound list.
 */

void
tree_for::set_ub_list (tree_node_list *ub)
{
    upper = ub;
    if (upper) upper->set_parent(this);
}


/*
 *  Set the step list of a tree_for.  Automatically sets the
 *  parent pointer in the step list.
 */

void
tree_for::set_step_list (tree_node_list *stp)
{
    step = stp;
    if (step) step->set_parent(this);
}


/*
 *  See if the LB/UB/STEP of a FOR is a constant integer, and if so return
 *  the constant through the retval pointer.  If retval is NULL, the value
 *  is not returned.
 */

boolean
tree_for::lb_is_constant (int *retval)
{
    return lb_list()->op_is_intconst(retval);
}


boolean
tree_for::ub_is_constant (int *retval)
{
    return ub_list()->op_is_intconst(retval);
}


boolean
tree_for::step_is_constant (int *retval)
{
    return step_list()->op_is_intconst(retval);
}


/*
 *  Return the requested child list.
 */
tree_node_list *
tree_for::child_list_num(unsigned num)
{
    switch (num)
      {
        case 0:
            return body();
        case 1:
            return landing_pad();
        case 2:
            return lb_list();
        case 3:
            return ub_list();
        case 4:
            return step_list();
        default:
            assert(FALSE);
            return NULL;
      }
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this node.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_for::clear_numbers ()
{
    tree_node::clear_numbers();
    body()->clear_numbers();
    landing_pad()->clear_numbers();
    lb_list()->clear_numbers();
    ub_list()->clear_numbers();
    step_list()->clear_numbers();
}


/*
 *  Read a tree_for from an input stream.
 */

tree_for::tree_for (instruction *mrk, in_stream *is, block_symtab *syms)
{
    annote *an = mrk->annotes()->get_annote(k_for);
    ind = (var_sym *)an->immeds()->pop().symbol();
    tst = (tree_for_test)an->immeds()->pop().integer();
    clab = (label_sym *)an->immeds()->pop().symbol();
    blab = (label_sym *)an->immeds()->pop().symbol();
    set_number(an->immeds()->pop().unsigned_int());
    delete an;

    mrk_to_annotes(mrk);
    delete mrk;

    lower = new tree_node_list(is, this, syms);
    upper = new tree_node_list(is, this, syms);
    step = new tree_node_list(is, this, syms);
    bod = new tree_node_list(is, this, syms);
    lpad = new tree_node_list(is, this, syms);
}


/*
 *  Write a tree_for to an output stream.
 */

void
tree_for::write (out_stream *os)
{
    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_for);

    an->immeds()->append(immed(index()));
    an->immeds()->append(immed((int)test()));
    an->immeds()->append(immed(contlab()));
    an->immeds()->append(immed(brklab()));
    an->immeds()->append(immed(number()));
    mrk->annotes()->append(an);

    copy_annotes(mrk);
    mrk->write(os);
    delete mrk;

    lb_list()->write(os);
    ub_list()->write(os);
    step_list()->write(os);
    body()->write(os);
    landing_pad()->write(os);
}


/*
 *  Print a tree_for.
 */

void
tree_for::print (FILE *fp, int depth)
{
    /* first print the number */
    if (number() != 0) {
	fprintf(fp, "%4u: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    fputs("FOR (Index=", fp);
    index()->print(fp);
    fputs(" Test=", fp);
    switch (test()) {
	case FOR_EQ:	{ fputs("EQ", fp); break; }
	case FOR_NEQ:	{ fputs("NEQ", fp); break; }
	case FOR_SGELE:	{ fputs("SGELE", fp); break; }
	case FOR_SGT:	{ fputs("SGT", fp); break; }
	case FOR_SGTE:	{ fputs("SGTE", fp); break; }
	case FOR_SLT:	{ fputs("SLT", fp); break; }
	case FOR_SLTE:	{ fputs("SLTE", fp); break; }
	case FOR_UGELE:	{ fputs("UGELE", fp); break; }
	case FOR_UGT:	{ fputs("UGT", fp); break; }
	case FOR_UGTE:	{ fputs("UGTE", fp); break; }
	case FOR_ULT:	{ fputs("ULT", fp); break; }
	case FOR_ULTE:	{ fputs("ULTE", fp); break; }
    }
    fputs(" Cont=", fp);
    contlab()->print(fp);
    fputs(" Brk=", fp);
    brklab()->print(fp);
    putc(')', fp);
    print_annotes(fp, depth);
    putc('\n', fp);

    suif_indent(fp, depth);
    fputs("FOR LB\n", fp);
    lb_list()->print_expr(fp, depth+1);

    suif_indent(fp, depth);
    fputs("FOR UB\n", fp);
    ub_list()->print_expr(fp, depth+1);

    suif_indent(fp, depth);
    fputs("FOR STEP\n", fp);
    step_list()->print_expr(fp, depth+1);

    suif_indent(fp, depth);
    fputs("FOR LANDING PAD\n", fp);
    landing_pad()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("FOR BODY\n", fp);
    body()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("FOR END\n", fp);
}


/*
 *  Clone a tree_for.
 */

tree_node *
tree_for::clone_helper (replacements *r, boolean no_copy)
{
    tree_for *result = this;
    tree_node_list *newbody = body()->clone_helper(r, no_copy);
    tree_node_list *newlb = lb_list()->clone_helper(r, no_copy);
    tree_node_list *newub = ub_list()->clone_helper(r, no_copy);
    tree_node_list *newstp = step_list()->clone_helper(r, no_copy);
    tree_node_list *newlp = landing_pad()->clone_helper(r, no_copy);

    var_sym *indx = (var_sym *)index()->clone_helper(r);
    label_sym *cont = (label_sym *)contlab()->clone_helper(r);
    label_sym *brk = (label_sym *)brklab()->clone_helper(r);
    if (!no_copy) {
	assert_msg((cont != contlab()) &&
		   (brk != brklab()),
		   ("tree_for::clone_helper - "
		    "label(s) have not been replaced"));
	result = new tree_for(indx, test(), cont, brk, newbody,
			      newlb, newub, newstp, newlp);
    } else {
	set_index(indx);
	set_contlab(cont);
	set_brklab(brk);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Find the exposed references in a tree_for.
 */

void
tree_for::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    /* record the reference to the index variable */
    r->add_sym_ref(index(), dst_scope);

    /* record the labels that are defined in this for loop */
    r->add_sym_ref(contlab(), dst_scope, TRUE);
    r->add_sym_ref(brklab(), dst_scope, TRUE);

    body()->find_exposed_refs(dst_scope, r);
    landing_pad()->find_exposed_refs(dst_scope, r);
    lb_list()->find_exposed_refs(dst_scope, r);
    ub_list()->find_exposed_refs(dst_scope, r);
    step_list()->find_exposed_refs(dst_scope, r);

    find_annote_refs(dst_scope, r);
}


/*****************************************************************************/


/*
 *  Create a new tree_if.
 */

tree_if::tree_if (label_sym *j, tree_node_list *h, tree_node_list *t,
		  tree_node_list *e)
{
    jmp = j;
    set_header(h);
    set_then_part(t);
    set_else_part(e);
}


/*
 *  Destroy a tree_if.
 */

tree_if::~tree_if ()
{
    if (header()) delete header();
    if (then_part()) delete then_part();
    if (else_part()) delete else_part();
}


/*
 *  Set the header of a tree_if.  Automatically sets the parent pointer of
 *  the header list.
 */

void
tree_if::set_header (tree_node_list *h)
{
    hdr = h;
    if (hdr) hdr->set_parent(this);
}


/*
 *  Set the "then" part of a tree_if.  Automatically sets the parent pointer
 *  of the "then" list.
 */

void
tree_if::set_then_part (tree_node_list *t)
{
    then = t;
    if (then) then->set_parent(this);
}


/*
 *  Set the "else" part of a tree_if.  Automatically sets the parent pointer
 *  of the "else" list.
 */

void
tree_if::set_else_part (tree_node_list *e)
{
    els = e;
    if (els) els->set_parent(this);
}


/*
 *  Return the requested child list.
 */
tree_node_list *
tree_if::child_list_num(unsigned num)
{
    switch (num)
      {
        case 0:
            return header();
        case 1:
            return then_part();
        case 2:
            return else_part();
        default:
            assert(FALSE);
            return NULL;
      }
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this node.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_if::clear_numbers ()
{
    tree_node::clear_numbers();
    header()->clear_numbers();
    then_part()->clear_numbers();
    else_part()->clear_numbers();
}


/*
 *  Read a tree_if from an input stream.
 */

tree_if::tree_if (instruction *mrk, in_stream *is, block_symtab *syms)
{
    annote *an = mrk->annotes()->get_annote(k_if);
    jmp = (label_sym *)an->immeds()->pop().symbol();
    set_number(an->immeds()->pop().unsigned_int());
    delete an;

    mrk_to_annotes(mrk);
    delete mrk;

    hdr = new tree_node_list(is, this, syms);
    then = new tree_node_list(is, this, syms);
    els = new tree_node_list(is, this, syms);
}


/*
 *  Write a tree_if to an output stream.
 */

void
tree_if::write (out_stream *os)
{
    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_if);

    an->immeds()->append(immed(jumpto()));
    an->immeds()->append(immed(number()));
    mrk->annotes()->append(an);

    copy_annotes(mrk);
    mrk->write(os);
    delete mrk;

    header()->write(os);
    then_part()->write(os);
    else_part()->write(os);
}


/*
 *  Print a tree_if.
 */

void
tree_if::print (FILE *fp, int depth)
{
    /* first print the number */
    if (number() != 0) {
	fprintf(fp, "%4u: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    fputs("IF (Jumpto=", fp);
    jumpto()->print(fp);
    putc(')', fp);
    print_annotes(fp, depth);
    putc('\n', fp);

    suif_indent(fp, depth);
    fputs("IF HEADER\n", fp);
    header()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("IF THEN\n", fp);
    then_part()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("IF ELSE\n", fp);
    else_part()->print(fp, depth+1);

    suif_indent(fp, depth);
    fputs("IF END\n", fp);
}


/*
 *  Clone a tree_if.
 */

tree_node *
tree_if::clone_helper (replacements *r, boolean no_copy)
{
    tree_if *result = this;
    tree_node_list *newhead = header()->clone_helper(r, no_copy);
    tree_node_list *newthen = then_part()->clone_helper(r, no_copy);
    tree_node_list *newelse = else_part()->clone_helper(r, no_copy);
    label_sym *jmpto = (label_sym *)jumpto()->clone_helper(r);
    if (!no_copy) {
	assert_msg(jmpto != jumpto(),
		   ("tree_if::clone_helper - label has not been replaced"));

	result = new tree_if(jmpto, newhead, newthen, newelse);
    } else {
	set_jumpto(jmpto);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Find the exposed references in a tree_if.
 */

void
tree_if::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    /* record the labels that are defined in this if */
    r->add_sym_ref(jumpto(), dst_scope, TRUE);

    header()->find_exposed_refs(dst_scope, r);
    then_part()->find_exposed_refs(dst_scope, r);
    else_part()->find_exposed_refs(dst_scope, r);

    find_annote_refs(dst_scope, r);
}


/*****************************************************************************/


/*
 *  Destroy a tree_instr.
 */

tree_instr::~tree_instr ()
{
    /* delete the instruction and its expression trees */
    if (instr()) {
	instruction *save = instr();
	remove_instr(instr());
	delete save;
    }
}


/*
 *  Set the instruction in a tree_instr.  Automatically sets the instruction's
 *  parent pointer.
 */

void
tree_instr::set_instr (instruction *i)
{
    /* make sure that the previous contents (if any) was properly removed */
    assert_msg(!ins, ("tree_instr::set_instr - already have an instruction"));

    ins = i;
    if (i) i->set_parent(this);
}


/*
 *  Remove an instruction from a tree_instr.  The instruction's parent is
 *  set to NULL.  If the instruction is not in a subexpression tree (i.e.
 *  it is directly pointed to by the tree_instr), the tree_instr's pointer
 *  is also set to NULL.
 */

void
tree_instr::remove_instr (instruction *i)
{
    i->set_parent(NULL);
    if (ins == i) ins = NULL;
}


/*
 *  Map a function over all of the instructions in an expression tree.
 *  This is similar to the src_map function, but it recursively descends
 *  expression trees and includes the tree_instr root.  Instead of having
 *  a separate function to check the tree_instr root, only a single
 *  function is needed.
 */

static void instr_map_helper(instr_map_f f, instruction *i, void *x,
			     boolean preorder);

void
tree_instr::instr_map (instr_map_f f, void *x, boolean preorder)
{
    if (!instr()) return;
    instr_map_helper(f, instr(), x, preorder);
}


void
instr_map_helper (instr_map_f f, instruction *i, void *x, boolean preorder)
{
    push_clue(i);
    if (preorder) f(i, x);

    /* recursively handle the source expressions */
    for (unsigned n = 0; n < i->num_srcs(); n++) {
	operand r = i->src_op(n);
	if (r.is_expr()) {
	    instr_map_helper(f, r.instr(), x, preorder);
	}
    }

    if (!preorder) f(i, x);
    pop_clue(i);
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this node.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_instr::clear_numbers ()
{
    tree_node::clear_numbers();
    if (instr()) instr()->clear_numbers();
}


/*
 *  Write a tree_instr to an output stream.  Since the library does not
 *  support writing annotations on tree_instrs, we check to make sure that
 *  there are no annotations that are supposed to be written out.
 */

void
tree_instr::write (out_stream *os)
{
    /* make sure there are no writable annotations on the tree_instr */
    annote_list_iter iter(annotes());
    while (!iter.is_empty()) {
	annote *an = iter.step();
	annote_def *adef = lookup_annote(an->name());
	if (adef && adef->output()) {
	    /* print a non-fatal error message */
	    warning_line(this, "cannot write \"%s\" annotation on tree_instr",
			 an->name());
	}
    }

    if (instr()) instr()->write(os);
}


/*
 *  Print a tree_instr.
 */

void
tree_instr::print (FILE *fp, int depth)
{
    if (instr()) instr()->print(fp, depth);
}


/*
 *  Clone a tree_instr.
 */

tree_node *
tree_instr::clone_helper (replacements *r, boolean no_copy)
{
    tree_instr *result = this;
    instruction *i = instr();
    if (i) i = i->clone_helper(r, no_copy);
    if (!no_copy) {
	result = new tree_instr(i);
    }
    clone_annotes(result, r, no_copy);
    return result;
}


/*
 *  Find the exposed references in a tree_instr.
 */

void
tree_instr::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    if (instr()) instr()->find_exposed_refs(dst_scope, r);
    instruction_list_e *old_e = r->oldinstrs.lookup(instr());
    if (old_e) delete r->oldinstrs.remove(old_e);
    find_annote_refs(dst_scope, r);
}


/*****************************************************************************/

/* We make explicit copy constructor and assignment operator and
 * make them private to foil C++'s automatic default versions. */
tree_node_list::tree_node_list(const tree_node_list &)  { assert(FALSE); }
tree_node_list &tree_node_list::operator=(const tree_node_list &)  { assert(FALSE); return *this; }

/*
 *  Destroy a tree_node_list.
 */

tree_node_list::~tree_node_list ()
{
    cvt_to_trees();     /* prevent prior deletion of expr tree children */
    while (!is_empty()) {
	delete pop();
    }
}


/*
 *  Recursively ascend the tree to find the symbol table for the closest
 *  enclosing scope.  As soon as this reaches a tree_block, the function
 *  will return the symtab pointer for that block.
 */

base_symtab *
tree_node_list::scope (void)
{
    tree_node *parent_node = parent();
    assert_msg(parent_node, ("tree_node_list::scope - no parent node"));

    /* check if the parent is a block */
    if (parent_node->is_block()) {
	tree_block *blk = (tree_block *)parent_node;
	return blk->symtab();
    }

    return parent_node->scope();
}


/*
 *  Clone a tree_node_list.
 */

tree_node_list *
tree_node_list::clone (base_symtab *dst_scope)
{
    replacements r;
    find_exposed_refs(dst_scope, &r);
    r.resolve_exposed_refs(dst_scope);
    return clone_helper(&r);
}


tree_node_list *
tree_node_list::clone_helper (replacements *r, boolean no_copy)
{
    tree_node_list *result = this;
    if (!no_copy) result = new tree_node_list;

    tree_node_list_iter tnli(this);
    while (!tnli.is_empty()) {
	tree_node *t = tnli.step();
	tree_node *newnode = t->clone_helper(r, no_copy);
	if (!no_copy) {
	    result->append(newnode);
	}
    }

    return result;
}


/*
 *  Find the exposed references in a list.  In order to handle exposed
 *  references to instructions in flat lists, we have to traverse the list
 *  in reverse order.
 */

void
tree_node_list::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    tree_node_list_e *t = tail();
    while (t) {
	t->contents->find_exposed_refs(dst_scope, r);
	t = t->prev();
    }
}


/*
 *  Map a function over the nodes in a tree_node_list.
 */

void
tree_node_list::map (tree_map_f f, void *x, boolean preorder, boolean descend)
{
    tree_node_list_iter tnli(this);
    while (!tnli.is_empty()) {
	tree_node *t = tnli.step();

        push_clue(t);
	if (preorder) f(t, x);
	if (descend) t->map(f, x, preorder);
	if (!preorder) f(t, x);
        pop_clue(t);
    }
}


/*
 *  Set the back pointers when an element is added to the list.  This virtual
 *  function overrides the one in the base list class.
 */

void
tree_node_list::set_elem (tree_node_list_e *e)
{
    e->contents->set_parent(this);
    e->contents->set_list_e(e);
}


/*
 *  Read a tree_node_list from a SUIF file.
 */

tree_node_list::tree_node_list (in_stream *is, tree_node *pr,
				block_symtab *syms)
{
    tree_node *node = NULL;

    set_parent(pr);

    while (TRUE) {

	instruction *in = instruction::read(is, syms, node);
	node = NULL;

	if (in->opcode() == io_mrk) {

	    if (in->annotes()->peek_annote(k_block)) {
		node = new tree_block(in, is, syms);

	    } else if (in->annotes()->peek_annote(k_for)) {
		node = new tree_for(in, is, syms);

	    } else if (in->annotes()->peek_annote(k_loop)) {
		node = new tree_loop(in, is, syms);

	    } else if (in->annotes()->peek_annote(k_if)) {
		node = new tree_if(in, is, syms);

	    } else if (in->annotes()->peek_annote(k_list_end)) {
		delete in;
		break;
	    }
	}

	if (!node) {
	    in->convert_annotes();
	    node = new tree_instr(in);
	}

	append(node);
    }
}


/*
 *  Write a tree_node_list to an output stream.
 */

void
tree_node_list::write (out_stream *os)
{
    tree_node_list_iter tnli(this);
    while (!tnli.is_empty()) {
	tnli.step()->write(os);
    }

    /* write out an end-of-list marker */
    instruction *mrk = new in_rrr(io_mrk);
    mrk->annotes()->append(new annote(k_list_end));
    mrk->write(os);
    delete mrk;
}


/*
 *  Print a tree_node_list.
 */

void
tree_node_list::print (FILE *fp, int depth)
{
    tree_node_list_iter tnli(this);
    while (!tnli.is_empty())
	tnli.step()->print(fp, depth);
}


/*
 *  Print a tree_node_list used as an operand (e.g. in a tree_for).
 */

void
tree_node_list::print_expr (FILE *fp, int depth)
{
    if (!is_op()) {
	print(fp, depth);
    } else {
	if (op().is_instr()) {
	    op().instr()->print(fp, depth);
	} else {
	    suif_indent(fp, depth);
	    tree_instr *ti = (tree_instr *)tail()->contents;
	    op().print(ti->instr(), fp);
	    putc('\n', fp);
	}	    
    }
}


/*
 *  Check if a tree_node_list contains a single operand as used in tree_fors.
 *  The list must have a single element which is a copy instruction with a
 *  null destination operand.
 */

boolean
tree_node_list::is_op ()
{
    /* check if the list has a single cpy instruction */
    if (count() != 1) return FALSE;

    tree_node *t = head()->contents;
    if (!t->is_instr()) return FALSE;

    instruction *i = ((tree_instr *)t)->instr();
    assert_msg(i, ("tree_node_list::is_op - empty tree_instr"));
    return (i->opcode() == io_cpy) && i->dst_op().is_null();
}


/*
 *  Retrieve the operand stored in a tree_node_list.  The list contents must
 *  meet the conditions required by the "is_op" method, possibly after
 *  converting the list to an expression tree.  If not, an error will occur.
 */

operand
tree_node_list::op ()
{
    if (!is_op()) {
	/* try to convert the list to an operand */
	cvt_to_trees();
	assert_msg(is_op(),("tree_node_list::op - cannot convert to operand"));
    }

    tree_instr *ti = (tree_instr *)head()->contents;
    in_rrr *cpyi = (in_rrr *)ti->instr();
    return cpyi->src1_op();
}


/*
 *  Set the contents of a tree_node_list to be a single operand attached to
 *  a dummy copy instruction.  The list must either be empty or already
 *  contain the dummy copy; otherwise, an error will occur.
 */

void
tree_node_list::set_op (operand o)
{
    tree_instr *ti;
    in_rrr *cpyi;

    if (is_empty()) {

	/* create a new cpy instruction to hold the operand */
	cpyi = new in_rrr(io_cpy);
	ti = new tree_instr(cpyi);
	push(ti);

    } else {

	if (!is_op()) {
	    /* try to convert the list to an operand */
	    cvt_to_trees();
	    assert_msg(is_op(),("tree_node_list::set_op - cannot cvt to op"));
	}

	ti = (tree_instr *)head()->contents;
	cpyi = (in_rrr *)ti->instr();
    }

    cpyi->set_src1(o);
    cpyi->set_result_type(o.type()->unqual());
}


/*
 *  Check if the operand stored in a tree_node_list is a constant integer,
 *  and if so, return the value in the "retval" parameter.
 */


boolean
tree_node_list::op_is_intconst (int *retval)
{
    instruction *inst;

    if (!is_op()) {
	if (count() == 0)
	    return FALSE;
	inst = ((tree_instr*)tail())->instr();
    } else {
	if (!op().is_instr())
	    return FALSE;
	inst = op().instr();
    }
	
    if (inst->opcode() != io_ldc)
	return FALSE;

    immed v = ((in_ldc *)inst)->value();
    if (!v.is_integer())
	return FALSE;
    if (retval)
	*retval = v.integer();
    return TRUE;
}


/*
 *  Flattening expression trees.  The flatten function converts all of the
 *  expression trees in the list and its child lists to flat lists of
 *  tree_instrs.  Afterwards, each SUIF instruction will have its own
 *  tree_instr.
 */

static void flatten_tree_map(tree_node *t, void *x);
static boolean flatten_src_map(instruction *i, operand *r, void *x);


void
tree_node_list::flatten ()
{
    map(flatten_tree_map, NULL);
}


void
flatten_tree_map (tree_node *t, void *)
{
    if (t->is_instr()) {
	tree_instr *ti = (tree_instr *)t;
	ti->instr()->src_map(flatten_src_map, NULL);
    }
}


boolean
flatten_src_map (instruction *i, operand *r, void *)
{
    if (r->is_expr()) {
	tree_node_list *list = i->parent()->parent();

	/* insert the subtree before the current instruction */
	instruction *child = r->instr();
	child->parent()->remove_instr(child);
	tree_node_list_e *e = new tree_node_list_e(new tree_instr(child));
	list->insert_before(e, i->parent()->list_e());

	/* recursively flatten the child subtree */
	child->src_map(flatten_src_map, NULL);
    }
    return FALSE;
}


/*
 *  Convert a list of individual tree_instrs to a list of expression trees.
 *  This does not actually change the instructions, but merely collects the
 *  instructions within an expression under a single tree_instr.  The
 *  instructions must be ordered such that all of the instructions from an
 *  expression are contiguous.  If other instructions from outside the
 *  expression are intermixed, building the expression tree may change the
 *  behavior of the code.  Thus, if this occurs, the "promote_node"
 *  function is called to split up the expression trees so that the original
 *  semantics are preserved.
 */

static boolean ends_expr(tree_node *t);
static void cvt_tree_map(tree_node *t, void *x);
static boolean cvt_src_map(instruction *i, operand *r, void *x);
static void promote_node(tree_node_list_e *te, base_symtab *symtab);


void
tree_node_list::cvt_to_trees ()
{
    /* find the procedure-level symtab */
    tree_node *p = parent();
    proc_sym *psym = p ? p->proc() : NULL;
    tree_proc *tp = psym ? psym->block() : NULL;
    base_symtab *symtab = tp ? tp->symtab() : NULL;

    map(cvt_tree_map, symtab);
}


/*
 *  This function is mapped over all of the tree_nodes to find the
 *  instructions that can be combined with others as an expression tree.
 *  Nothing is done until reaching a node that ends an expression.  At that
 *  point, if the node is an tree_instr, all of the instructions in its
 *  expression tree are moved under that tree_instr.  Afterwards, if the
 *  instructions are properly ordered, there should be no instructions
 *  remaining between the previous expression and the current node.
 *  If that is not the case, the remaining instructions are "promoted" to
 *  be independent expressions.
 */

void
cvt_tree_map (tree_node *t, void *x)
{
    if (ends_expr(t)) {

	/* convert instruction sources to expressions */
	if (t->is_instr()) {
	    tree_instr *ti = (tree_instr *)t;
	    ti->instr()->src_map(cvt_src_map, NULL);
	}

	/* search backwards and promote remaining "nodes" to var_syms */
	tree_node_list_e *pos = t->list_e()->prev();
	while (pos && !ends_expr(pos->contents)) {
	    tree_instr *ti = (tree_instr *)pos->contents;
	    assert(ti->is_instr());
	    operand d = ti->instr()->dst_op();
	    if (d.is_instr()) {
		assert_msg(x,
			   ("cvt_to_trees - node used across expressions but no parent symtab"));
		fprintf(stderr, "Warning: cvt_to_trees - "
			"node %u used across expressions (promoted)\n",
			d.instr()->number());
		promote_node(pos, (base_symtab *)x);
	    }
	    pos = pos->prev();
	}
    }
}


boolean
cvt_src_map (instruction *, operand *r, void *)
{
    if (!r->is_instr() || r->is_expr()) return FALSE;

    instruction *ri = r->instr();
    tree_instr *ti = ri->parent();

    /* remove the instruction from the list */
    tree_node_list_e *pos = ti->list_e();
    ti->parent()->remove(pos);
    delete pos;

    assert(ti->instr() == ri);
    ti->remove_instr(ri);
    delete ti;

    /* recursively build the subtrees */
    ri->src_map(cvt_src_map, NULL);

    /* reassign the operand so that the parent pointers will be set */
    return TRUE;
}


void
promote_node (tree_node_list_e *te, base_symtab *symtab)
{
    assert(te->contents->is_instr());
    tree_instr *ti = (tree_instr *)te->contents;

    /* get the destination instruction */
    operand dst = ti->instr()->dst_op();
    assert(dst.is_instr());
    instruction *olddst = dst.instr();

    /* find the source operand where the node is used */
    boolean found = FALSE;
    unsigned n;
    for (n = 0; n < olddst->num_srcs(); n++) {
	operand r = olddst->src_op(n);
	if (r.is_instr() && (r.instr() == ti->instr())) {
	    found = TRUE;
	    break;
	}
    }
    assert_msg(found, ("promote_node - cannot find source operand"));

    /* create a new variable to replace the "node" */
    type_node *dtype = ti->instr()->result_type();
    var_sym *newdst = symtab->new_unique_var(dtype, "node");

    /* set the instruction destination to the new variable */
    ti->instr()->remove();
    ti->instr()->set_dst(operand(newdst));

    /* replace the use of the node */
    olddst->set_src_op(n, operand(newdst));

    /* build the expression tree for this instruction */
    ti->instr()->src_map(cvt_src_map, NULL);
}


/*
 *  Check if a tree_node ends an expression.
 */

boolean
ends_expr (tree_node *t)
{
    if (!t->is_instr()) return TRUE;

    instruction *i = ((tree_instr *)t)->instr();

    /* check for control flow instrs or instrs with unknown destinations */
    switch (i->opcode()) {
	case io_ret:
	case io_lab:
	case io_mbr:
	case io_btrue:
	case io_bfalse:
	case io_jmp:
	case io_str:
	case io_memcpy: {
	    return TRUE;
	}
	default: {
	    break;
	}
    }

    /* otherwise the expression ends if the dst is not an instruction */
    return !i->dst_op().is_instr();
}


/*
 *  Remove the instruction numbers from all the instrucions that are
 *  descendants of this list.  This is needed when moving code from one
 *  procedure to another so that numbers don't conflict.
 */

void
tree_node_list::clear_numbers ()
{
    tree_node_list_iter tnli(this);
    while (!tnli.is_empty()) {
	tnli.step()->clear_numbers();
    }
}


/*****************************************************************************/


/*
 *  Record references to sym_nodes that are not visible in the destination
 *  scope.  The "label_def" parameter is used to indicate that the symbol is
 *  a label that is being defined (e.g. in a label instruction).  This is
 *  needed because we have to enter label definitions on the replacements
 *  list even if they are visible in the destination scope.  If the symbol
 *  is added to the list, any references contained in the symbol are also
 *  checked.
 */

void
replacements::add_sym_ref (sym_node *s, base_symtab *dst_scope,
			   boolean label_def)
{
    /* check if the symbol is visible in the destination scope */
    if (!label_def &&
	((dst_scope == NULL) || dst_scope->is_ancestor(s->parent()))) {
	return;
    }

    /* check if the symbol is already in the oldsyms list */
    sym_node_list_iter snli(&oldsyms);
    while (!snli.is_empty()) {
	sym_node *sn = snli.step();
	if (sn == s) return;
    }

    /* add the new symbol to the list */
    oldsyms.append(s);

    if (dst_scope == NULL)
	return;

    /* now check for any references contained in the sym_node */
    if (s->is_var()) {
	var_sym *v = (var_sym *)s;
	add_type_ref(v->type(), dst_scope);
	if (v->parent_var() != NULL)
	    add_sym_ref(v->parent_var(), dst_scope, label_def);
	if (v->has_var_def())
	    add_def_ref(v->definition(), dst_scope);
    } else if (s->is_proc()) {
	proc_sym *p = (proc_sym *)s;
	add_type_ref(p->type(), dst_scope);
    }
    s->find_annote_refs(dst_scope, this);
}


/*
 *  Record references to type_nodes that are not visible in the destination
 *  scope.  This is basically the same as the "add_sym_ref" function above.
 */

void
replacements::add_type_ref (type_node *t, base_symtab *dst_scope)
{
    /* check if this type is visible in the destination scope */
    if ((dst_scope == NULL) || dst_scope->is_ancestor(t->parent())) return;

    /* check if the type is already in the oldtypes list */
    type_node_list_iter tnli(&oldtypes);
    while (!tnli.is_empty()) {
	type_node *tn = tnli.step();
	if (tn == t) return;
    }

    /* add the new type to the list */
    oldtypes.append(t);

    /* now check for any references contained in the type_node */
    switch (t->op()) {

	case TYPE_CONST:
	case TYPE_RESTRICT:
	case TYPE_VOLATILE:
	case TYPE_CALL_BY_REF:
	case TYPE_NULL: {
	    modifier_type *mt = (modifier_type *)t;
	    add_type_ref(mt->base(), dst_scope);
	    break;
	}

	case TYPE_PTR: {
	    ptr_type *pt = (ptr_type *)t;
	    add_type_ref(pt->ref_type(), dst_scope);
	    break;
	}

	case TYPE_ARRAY: {
	    array_type *at = (array_type *)t;
	    add_type_ref(at->elem_type(), dst_scope);
	    if (at->lower_bound().is_variable()) {
		add_sym_ref(at->lower_bound().variable(), dst_scope);
	    }
	    if (at->upper_bound().is_variable()) {
		add_sym_ref(at->upper_bound().variable(), dst_scope);
	    }
	    break;
	}

	case TYPE_FUNC: {
	    func_type *ft = (func_type *)t;
	    add_type_ref(ft->return_type(), dst_scope);
	    for (unsigned n = 0; n < ft->num_args(); n++) {
		add_type_ref(ft->arg_type(n), dst_scope);
	    }
	    break;
	}

	case TYPE_GROUP:
	case TYPE_STRUCT:
	case TYPE_UNION: {
	    struct_type *st = (struct_type *)t;
	    for (unsigned n = 0; n < st->num_fields(); n++) {
		add_type_ref(st->field_type(n), dst_scope);
	    }
	    break;
	}

	default: {
	    break;
	}
    }
    t->find_annote_refs(dst_scope, this);
}


void
replacements::add_def_ref (var_def *d, base_symtab *dst_scope)
{
    /* check if the definition is already in the olddefs list */
    var_def_list_iter vdli(&olddefs);
    while (!vdli.is_empty()) {
	var_def *def = vdli.step();
	if (def == d) return;
    }

    olddefs.append(d);

    /* check for any references contained in the var_def */
    add_sym_ref(d->variable(), dst_scope);
    d->find_annote_refs(dst_scope, this);
}


/*
 *  Record references from source operands to instructions in flat lists.
 *  This is just for error checking, as these references should all be
 *  resolved within the region being cloned.
 */

void
replacements::add_instr_ref (instruction *i)
{
    /* don't bother checking through the list -- each instruction may only
       be referenced once */

    /* add the new instruction to the list */
    oldinstrs.append(i);
}


/*
 *  Record references to parent symtabs.
 */

void
replacements::add_tab_ref (base_symtab *t)
{
    /* check if it's already in the list */
    base_symtab_list_iter bsli(&oldtabs);
    while (!bsli.is_empty()) {
	base_symtab *bs = bsli.step();
	if (bs == t) return;
    }

    /* add it to the list */
    oldtabs.append(t);
}


void
replacements::resolve_exposed_refs(base_symtab *dst_scope)
{
    if (dst_scope != NULL) {
	dst_scope->resolve_exposed_refs(this);
    } else {
	sym_node_list_e *first_exp_sym = oldsyms.head();
	int newsyms_length = newsyms.count();
	for (int n = 0; n < newsyms_length; n++) {
	    assert_msg(first_exp_sym,
		       ("resolve_exposed_refs - invalid symbol lists"));
	    first_exp_sym = first_exp_sym->next();
	}

	sym_node_list_iter snli;
	snli.set(first_exp_sym);
	while (!snli.is_empty()) {
	    sym_node *old_sn = snli.step();
	    sym_node *new_sn = old_sn->copy();

	    old_sn->parent()->add_sym(new_sn);
	    newsyms.append(new_sn);
	}

	assert(oldtypes.count() == newtypes.count());
	assert(olddefs.count() == newdefs.count());
	assert(oldinstrs.count() == newinstrs.count());
	assert(oldtabs.count() == newtabs.count());
    }
}


/*
 *  Copy a replacements structure.
 */

replacements&
replacements::operator= (replacements &r)
{
    oldsyms.copy(&r.oldsyms);
    newsyms.copy(&r.newsyms);
    oldtypes.copy(&r.oldtypes);
    newtypes.copy(&r.newtypes);
    oldtabs.copy(&r.oldtabs);
    newtabs.copy(&r.newtabs);
    oldinstrs.copy(&r.oldinstrs);
    newinstrs.copy(&r.newinstrs);

    return *this;
}

