/*  Call-By-Reference Conversion Functions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#define RCS_BASE_FILE callbyref_cc

#include "suif1.h"

RCS_BASE(
    "$Id$")


static void check_ref_ptrs_tree_map(tree_node *t, void *x);
static void illegal_ref_ptr(var_sym *v);
static void make_ref_params_tree_map(tree_node *t, void *x);
static operand make_ref_params_source(instruction *i, operand r);
static boolean check_add(in_rrr *i, operand r1, operand r2);

static boolean is_call_by_ref_var(operand r);
static type_node *unqual_cbr(type_node *t);


/*
 *  The make_ref_params function converts a procedure to use call-by-reference
 *  form, where parameters are accessed directly instead of through loads and
 *  stores.  The types of the call-by-ref parameters are changed from pointers
 *  to types with TYPE_CALL_BY_REF modifiers.  The call-by-ref parameters are
 *  identified by k_call_by_ref annotations on their ptr_types.  Every
 *  memory reference of a call-by-ref parameter is then changed to access the
 *  parameter directly, and other (non-memory) references are replaced by
 *  "ldc" instructions to get the address of the parameter.  This function
 *  must be called before converting to expression tree form.
 */

void
make_ref_params (proc_sym *ps)
{
    assert_msg(ps->is_in_memory(),
	       ("make_ref_params - procedure '%s' is not in memory",
		ps->name()));

    /* scan through the list of parameters */
    sym_node_list_iter param_iter(ps->block()->proc_syms()->params());
    while (!param_iter.is_empty()) {
	var_sym *param = (var_sym *)param_iter.step();
	assert_msg(param->is_var(), ("make_ref_params - parameter \"%s\" "
				     "is not a variable", param->name()));

	type_node *pt = param->type();
	assert_msg(!pt->is_call_by_ref(),
		   ("make_ref_params - parameter \"%s\" is already marked "
		    "call-by-ref", param->name()));

	/* check if the param type has a "call_by_ref" annotation */
	if (pt->are_annotations()) {
	    annote *an = pt->annotes()->peek_annote(k_call_by_ref);
	    if (an) {
		pt = pt->unqual();
		assert_msg(pt->is_ptr(),
			   ("make_ref_params - call-by-reference parameter "
			    "\"%s\" is not a pointer", param->name()));
		ptr_type *t = (ptr_type *)pt;

		/* make a new type that is marked call-by-ref */
		pt = new modifier_type(TYPE_CALL_BY_REF, t->ref_type());
		pt = t->ref_type()->parent()->install_type(pt);

		/* save the original parameter type */
		an = new annote(k_orig_type);
		an->immeds()->push(immed(param->type()));
		param->annotes()->push(an);

		/* replace the parameter type */
		param->set_type(pt);
	    }
	}
    }

    /* first check that the call-by-ref pointers are used properly */
    ps->block()->map(check_ref_ptrs_tree_map, NULL, FALSE);

    /* now convert the uses of call-by-ref pointers */
    ps->block()->map(make_ref_params_tree_map, NULL, FALSE);
}


/*
 *  Before changing the references to call-by-ref parameters, we have to
 *  check and make sure that they can be converted.  This means that the
 *  original call-by-ref pointers must neither be assigned nor addressed.
 */

void
check_ref_ptrs_tree_map (tree_node *t, void *)
{
    if (t->is_for()) {

	/* tree_for index variables are implicitly assigned to */
	tree_for *tf = (tree_for *)t;
	if (tf->index()->type()->is_call_by_ref()) {
	    illegal_ref_ptr(tf->index());
	}

    } else if (t->is_instr()) {

	tree_instr *ti = (tree_instr *)t;
	instruction *i = ti->instr();

	/* make sure that call-by-ref pointers are not addressed */
	if (i->opcode() == io_ldc) {
	    in_ldc *ldci = (in_ldc *)i;
	    if (ldci->value().is_symbol() &&
		(ldci->value().symbol()->is_var())) {
		var_sym *v = (var_sym *)ldci->value().symbol();
		if (v->type()->is_call_by_ref()) {
		    illegal_ref_ptr(v);
		}
	    }
	}

	/* make sure that call-by-ref pointers are not assigned to */
	if (is_call_by_ref_var(i->dst_op())) {
	    illegal_ref_ptr(i->dst_op().symbol());
	}
    }
}


/*
 *  If a call-by-reference pointer is used in such a way that it cannot be
 *  converted, this function is called to restore the original type of the
 *  parameter.
 */

void
illegal_ref_ptr (var_sym *v)
{
    /* print a warning message */
    warning_line(NULL, "illegal use of call-by-reference pointer \"%s\" "
		 "-- not converted", v->name());

    /* restore the original type of the variable */
    annote *an = v->annotes()->get_annote(k_orig_type);
    assert_msg(an, ("illegal_ref_ptr - missing original type annotation"));
    type_node *t = an->immeds()->pop().type();
    v->set_type(t);
    delete an;
}


/*
 *  Map function used to scan through the body of the procedure, converting
 *  all references to call-by-ref parameters.  Illegal references (e.g.
 *  tree_for indexes) have already been ruled out, so we need not check
 *  for them here.
 */

void
make_ref_params_tree_map (tree_node *t, void *)
{
    if (!t->is_instr()) return;
    tree_instr *ti = (tree_instr *)t;
    instruction *i = ti->instr();

    switch (i->opcode()) {

	case io_memcpy: {
	    in_rrr *mcpyi = (in_rrr *)i;
	    operand sr = mcpyi->src_addr_op();
	    operand dr = mcpyi->dst_addr_op();
	    if (is_call_by_ref_var(sr)) {
		if (is_call_by_ref_var(dr)) {

		    /* change the memcpy to a cpy */
		    mcpyi->set_opcode(io_cpy);
		    mcpyi->set_result_type(unqual_cbr(dr.type()));
		    mcpyi->set_dst(dr);
		    mcpyi->set_src1(sr);
		    mcpyi->set_src2(operand());

		} else {

		    /* change the memcpy to a store */
		    mcpyi->set_opcode(io_str);
		    mcpyi->set_dst_addr_op(make_ref_params_source(i, dr));
		    mcpyi->set_src(sr);
		}

	    } else if (is_call_by_ref_var(dr)) {

		/* change the memcpy to a load */
		sr.remove();
		dr.remove();
		mcpyi->set_src1(operand());
		mcpyi->set_src2(operand());
		mcpyi->set_opcode(io_lod);
		mcpyi->set_result_type(unqual_cbr(dr.type()));
		mcpyi->set_dst(dr);
		mcpyi->set_src_addr_op(make_ref_params_source(i, sr));
	    }
	    break;
	}

	case io_lod: {
	    in_rrr *lodi = (in_rrr *)i;
	    operand sr = lodi->src_addr_op();
	    if (is_call_by_ref_var(sr)) {

		/* change the lod to a cpy -- this may be optimized when
		   the result of the cpy is used (it's easier to do it
		   later when we know which source operand uses the cpy) */

		lodi->set_opcode(io_cpy);
	    } else {
		lodi->set_src_addr_op(make_ref_params_source(i, sr));
	    }
	    break;
	}

	case io_str: {
	    in_rrr *stri = (in_rrr *)i;
	    operand dr = stri->dst_addr_op();
	    operand sr = stri->src_op();
	    if (is_call_by_ref_var(dr)) {

		if (sr.is_instr() && within_expr(stri, sr.instr())) {

		    /* make source instr assign directly to the symbol */
		    sr.remove();
		    sr.instr()->set_dst(dr);

		    /* delete the store */
		    delete ti->parent()->remove(ti->list_e());
		    delete ti;

		} else {

		    /* change the store to a copy */
		    stri->set_opcode(io_cpy);
		    stri->set_result_type(unqual_cbr(dr.type()));
		    stri->set_dst(dr);
		    stri->set_src1(make_ref_params_source(i, sr));
		    stri->set_src2(operand());
		}
	    } else {

		stri->set_dst_addr_op(make_ref_params_source(i, dr));
		stri->set_src(make_ref_params_source(i, sr));
	    }
	    break;
	}

	case io_add: {
	    /* check for adds of call-by-ref ptrs and constant offsets */
	    in_rrr *addi = (in_rrr *)i;
	    if (!check_add(addi, addi->src1_op(), addi->src2_op()) &&
		!check_add(addi, addi->src2_op(), addi->src1_op())) {
		addi->set_src1(make_ref_params_source(i, addi->src1_op()));
		addi->set_src2(make_ref_params_source(i, addi->src2_op()));
	    }
	    break;
	}

	case io_cpy:
	case io_cvt: {
	    in_rrr *cvti = (in_rrr *)i;
	    operand sr = cvti->src_op();
	    if (is_call_by_ref_var(sr)) {

		/* check if there is a "fields" annotation on the cpy/cvt */
		annote *flds = NULL;
		if (cvti->are_annotations()) {
		    flds = cvti->annotes()->get_annote(k_fields);
		}

		/* insert an ldc instruction */
		in_ldc *ldci = new in_ldc(cvti->result_type(), operand(),
					  immed(sr.symbol()));
		sr.symbol()->set_addr_taken();
		tree_instr *ldcti = new tree_instr(ldci);
		ti->parent()->insert_before(ldcti, ti->list_e());

		/* move the "fields" annotation to the ldc */
		if (flds) ldci->annotes()->push(flds);

		/* leave a cpy instruction -- this may be optimized when
		   the result of the cpy is used (it's easier to do it
		   later when we know which source operand uses the cpy) */

		cvti->set_opcode(io_cpy);
		cvti->set_src(operand(ldci));
	    } else {
		cvti->set_src(make_ref_params_source(i, cvti->src_op()));
	    }
	    break;
	}

	default: {
	    for (unsigned n = 0; n < i->num_srcs(); n++) {
		operand r = i->src_op(n);
		i->set_src_op(n, make_ref_params_source(i, r));
	    }
	}
    }
}


/*
 *  Check if the operands of an add instruction are a call-by-ref pointer
 *  and an "ldc" of a constant integer.  If so, change the "ldc" to load
 *  the address of the parameter with the integer as an offset.
 *  Returns TRUE if this transformation is successfully applied.
 */

boolean
check_add (in_rrr *i, operand r1, operand r2)
{
    assert(i->opcode() == io_add);

    if (is_call_by_ref_var(r1)) {
	if (r2.is_instr() && (r2.instr()->opcode() == io_ldc)) {
	    in_ldc *ldci = (in_ldc *)r2.instr();
	    immed k = ldci->value();
	    if (k.is_integer()) {

		/* check if there is a "fields" annotation on the add */
		annote *flds = NULL;
		if (i->are_annotations()) {
		    flds = i->annotes()->get_annote(k_fields);
		}

		/* change the ldc to include the variable */
		ldci->set_value(immed(r1.symbol(), k.integer() * 8));
		ldci->set_result_type(i->result_type());

		/* move the "fields" annotation to the ldc */
		if (flds) ldci->annotes()->push(flds);

		/* change the add to a cpy -- this may be optimized when
		   the result of the cpy is used (it's easier to do it
		   later when we know which source operand uses the cpy) */

		i->set_opcode(io_cpy);
		i->set_src1(r2);
		i->set_src2(operand());

		return TRUE;
	    }
	}
    }
    return FALSE;
}


/*
 *  Translate non-memory references of call-by-ref variables to "ldc"
 *  instructions.  Remove useless "cpy" instructions that result from
 *  converting loads and adds of call-by-ref variables.
 */

operand
make_ref_params_source (instruction *i, operand r)
{
    tree_instr *ti = i->parent();

    if (is_call_by_ref_var(r)) {

	/* insert an ldc instruction */
	annote *an = r.symbol()->annotes()->peek_annote(k_orig_type);
	assert_msg(an, ("make_ref_params_source- missing "
			"original type annotation"));
	type_node *typ = (*an->immeds())[0].type();
	in_ldc *ldci = new in_ldc(typ, operand(), immed(r.symbol()));
	r.symbol()->set_addr_taken();
	tree_instr *ldcti = new tree_instr(ldci);
	ti->parent()->insert_before(ldcti, ti->list_e());

	return operand(ldci);

    } else if (r.is_instr() &&
	       (r.instr()->opcode() == io_cpy) &&
	       within_expr(i, r.instr())) {

	in_rrr *cpyi = (in_rrr *)r.instr();
	operand src = cpyi->src_op();

	/* check if the copy is the result of a call-by-ref conversion */
	boolean remove_cpy = FALSE;
	if (is_call_by_ref_var(src)) {
	    remove_cpy = TRUE;
	} else if (src.is_instr() && (src.instr()->opcode() == io_ldc)) {
	    in_ldc *ldci = (in_ldc *)src.instr();
	    immed k = ldci->value();
	    if (k.is_symbol() && k.symbol()->is_var()) {
		var_sym *v = (var_sym *)k.symbol();
		if (v->type()->is_call_by_ref()) {
		    remove_cpy = TRUE;
		}
	    }
	}

	if (remove_cpy) {
	    /* remove the cpy instruction */
	    cpyi->remove();
	    tree_instr *cpyti = cpyi->parent();
	    delete cpyti->parent()->remove(cpyti->list_e());
	    delete cpyti;
	    return src;
	}
    }
    return r;
}


/*****************************************************************************/


static void undo_ref_params_tree_map(tree_node *t, void *x);
/* @@@ static */ void undo_ref_params_instr(instruction *i, void *x);
static void undo_ref_params_default(instruction *i);
static void undo_ref_params_dst(instruction *i);
static operand undo_ref_params_source(instruction *i, operand r);
static operand cbr_immed_to_addr(immed cbr_immed, type_node *result_type,
                                 annote *field_annote);
static operand cbr_op_to_addr(operand the_op);


/*
 *  Convert the call-by-reference form back to the original code where
 *  the parameters are pointers.  First undo the transformations to the
 *  code and then restore the original types of the parameters.  Since
 *  the code may have either flat lists or expression trees, this code
 *  needs to handle both forms, and regardless of the input form, this
 *  function may create some expression trees.
 */

void
undo_ref_params (proc_sym *ps)
{
    sym_node_list_iter param_iter;

    assert_msg(ps->is_in_memory(),
	       ("undo_ref_params - procedure '%s' is not in memory",
		ps->name()));

    /* first check if there are any call-by-ref parameters */
    boolean need_cvt = FALSE;
    param_iter.reset(ps->block()->proc_syms()->params());
    while (!param_iter.is_empty()) {
	var_sym *param = (var_sym *)param_iter.step();
	/* also check that all parameters are variables */
	assert_msg(param->is_var(), ("undo_ref_params - parameter \"%s\" "
				     "is not a variable", param->name()));
	if (param->type()->is_call_by_ref()) need_cvt = TRUE;
    }

    /* return if we don't have to do any work */
    if (!need_cvt) return;

    /* fix up the body of the procedure */
    ps->block()->map(undo_ref_params_tree_map, NULL, FALSE);

    /* scan through the list of parameters */
    param_iter.reset(ps->block()->proc_syms()->params());
    while (!param_iter.is_empty()) {
	var_sym *param = (var_sym *)param_iter.step();

	if (param->type()->is_call_by_ref()) {

	    /* restore the parameter's original (pointer) type */
	    type_node *t;
	    annote *an = param->annotes()->get_annote(k_orig_type);
	    if (an) {
		t = an->immeds()->pop().type();
	    } else {
		t = new ptr_type(unqual_cbr(param->type()));
		t->append_annote(k_call_by_ref);
		t = ps->block()->symtab()->install_type(t);
	    }
	    param->set_type(t);
	    delete an;
	}
    }
}


/*
 *  Map function used to scan through the body of a procedure and convert
 *  references to call-by-ref parameters back to the original form where
 *  the parameters are pointers.  This undoes all of the transformations made
 *  when converting to call-by-ref form.
 */

void
undo_ref_params_tree_map (tree_node *t, void *)
{
    if (t->is_for()) {
	tree_for *tf = (tree_for *)t;

	/* FOR index variables cannot be call-by-reference parameters */
	assert_msg(!tf->index()->root_ancestor()->type()->is_call_by_ref(),
		   ("call-by-reference parameter \"%s\" used as FOR index",
		    tf->index()->name()));

    } else if (t->is_block()) {
	tree_block *tb = (tree_block *)t;

	/* array bounds for types in the symtab cannot be call-by-ref params */
	type_node_list_iter tnli(tb->symtab()->types());
	while (!tnli.is_empty()) {
	    array_type *at = (array_type *)tnli.step();
	    if (!at->is_array()) continue;
	    assert_msg(!at->lower_bound().is_variable() ||
		       !at->lower_bound().variable()->root_ancestor()->type()
			       ->is_call_by_ref(),
		       ("call-by-reference param \"%s\" used as array bound",
			at->lower_bound().variable()->name()));
	    assert_msg(!at->upper_bound().is_variable() ||
		       !at->upper_bound().variable()->root_ancestor()->type()
			       ->is_call_by_ref(),
		       ("call-by-reference param \"%s\" used as array bound",
			at->upper_bound().variable()->name()));
	}

    } else if (t->is_instr()) {
	tree_instr *ti = (tree_instr *)t;

	ti->instr_map(undo_ref_params_instr, NULL, FALSE);
    }
}


void
undo_ref_params_instr (instruction *i, void *)
{
    switch (i->opcode()) {

	case io_ldc: {
	    in_ldc *ldci = (in_ldc *)i;

	    /* ldcs used as operands will be handled later */
	    if (ldci->dst_op().is_instr()) break;

	    immed k = ldci->value();
	    if (k.is_symbol() &&
		k.symbol()->is_var() &&
		((var_sym *)k.symbol())->root_ancestor()->type()
			->is_call_by_ref()) {

		((var_sym *)k.symbol())->reset_addr_taken();

		/* remove the ldc instruction */
		tree_instr *ldcti = ldci->parent();
		assert(ldcti->instr() == ldci);
		ldcti->remove_instr(ldci);

		/* check if there is a "fields" annotation */
		annote *flds = NULL;
		if (ldci->are_annotations()) {
		    flds = ldci->annotes()->get_annote(k_fields);
		}

                operand new_op =
                        cbr_immed_to_addr(k, ldci->result_type(), flds);
                if (new_op.is_instr())
                  {
                    i = new_op.instr();
                  }
                else
                  {
                    i = new in_rrr(io_cpy, ldci->result_type(), operand(),
                                   new_op);
                  }
                i->set_dst(ldci->dst_op());
                delete ldci;

		ldcti->set_instr(i);
	    }

	    /* check the destination operand */
	    undo_ref_params_dst(i);

	    break;
	}

	case io_cpy: {
	    in_rrr *cpyi = (in_rrr *)i;
	    operand dr = cpyi->dst_op();
	    operand sr = cpyi->src_op();
	    if (is_call_by_ref_var(sr)) {

		/* check for dummy cpys in tree_fors */
		if (dr.is_null()) {

		    /* insert a lod instruction */
		    in_rrr *lodi = new in_rrr(io_lod, cpyi->result_type());
		    lodi->set_src_addr_op(cbr_op_to_addr(sr));
		    cpyi->set_src(operand(lodi));

		} else if (is_call_by_ref_var(dr)) {

		    /* convert the cpy to a memcpy */
		    cpyi->set_opcode(io_memcpy);
		    cpyi->set_result_type(type_void);
		    cpyi->set_dst(operand());
		    cpyi->set_dst_addr_op(cbr_op_to_addr(dr));
		    cpyi->set_src_addr_op(cbr_op_to_addr(sr));

		} else {

		    /* convert the cpy to a lod */
		    cpyi->set_opcode(io_lod);
		    cpyi->set_src_addr_op(cbr_op_to_addr(sr));
		}

	    } else if (is_call_by_ref_var(dr)) {

		/* convert the cpy to a str */
		sr.remove();
		cpyi->set_opcode(io_str);
		cpyi->set_result_type(type_void);
		cpyi->set_dst(operand());
		cpyi->set_dst_addr_op(cbr_op_to_addr(dr));
		cpyi->set_src(undo_ref_params_source(i, sr));

	    } else {
		undo_ref_params_default(i);
	    }
	    break;
	}

	case io_lod: {
	    in_rrr *lodi = (in_rrr *)i;
	    operand dr = lodi->dst_op();
	    operand sr = lodi->src_addr_op();
	    if (is_call_by_ref_var(dr)) {

		/* change the lod to a memcpy */
		sr.remove();
		lodi->set_opcode(io_memcpy);
		lodi->set_result_type(type_void);
		lodi->set_dst(operand());
		lodi->set_dst_addr_op(cbr_op_to_addr(dr));
		lodi->set_src_addr_op(undo_ref_params_source(i, sr));

	    } else {
		undo_ref_params_default(i);
	    }
	    break;
	}

	case io_str: {
	    in_rrr *stri = (in_rrr *)i;
	    operand dr = stri->dst_addr_op();
	    operand sr = stri->src_op();
	    if (is_call_by_ref_var(sr)) {

		/* change the str to a memcpy */
		dr.remove();
		stri->set_opcode(io_memcpy);
		stri->set_dst_addr_op(undo_ref_params_source(i, dr));
		stri->set_src_addr_op(cbr_op_to_addr(sr));

	    } else {
		undo_ref_params_default(i);
	    }
	    break;
	}

	default: {
	    undo_ref_params_default(i);
	}
    }
}


/*
 *  Except for "ldc", "cpy", "lod", and "str" opcodes, this function is used
 *  to convert instructions from call-by-ref form.  If the "dst" operand
 *  is a call-by-ref parameter, a store instruction is inserted.  The
 *  "undo_ref_params_source" function is then called to convert each source
 *  operand.
 */

void
undo_ref_params_default (instruction *i)
{
    /* check the destination operand */
    undo_ref_params_dst(i);

    /* convert the source operands */
    for (unsigned n = 0; n < i->num_srcs(); n++) {
	operand r = i->src_op(n);
	r.remove();
	i->set_src_op(n, undo_ref_params_source(i, r));
    }
}


void
undo_ref_params_dst (instruction *i)
{
    /* convert the destination operand */
    operand dr = i->dst_op();
    if (is_call_by_ref_var(dr)) {

	/* remove this instruction from its parent tree_instr */
	tree_instr *ti = i->parent();
	assert(ti->instr() == i);
	ti->remove_instr(i);

	/* create a new store instruction */
	in_rrr *stri = new in_rrr(io_str);
	stri->set_dst_addr_op(cbr_op_to_addr(dr));
	stri->set_src(i);

	/* put the store in the current tree_instr */
	ti->set_instr(stri);
    }
}


/*
 *  Convert a source operand.  If it is a call-by-ref parameter, insert a
 *  load instruction.  Otherwise, if it is an "ldc" instruction returning
 *  the address of a call-by-ref parameter, either remove the "ldc" or
 *  replace it with an "add", depending on the value of the offset.
 */

operand
undo_ref_params_source (instruction *, operand r)
{
    if (is_call_by_ref_var(r)) {

	/* create a lod instruction */
	in_rrr *lodi = new in_rrr(io_lod, unqual_cbr(r.type()));
	lodi->set_src_addr_op(cbr_op_to_addr(r));
	return operand(lodi);
    }

    if (r.is_instr() && (r.instr()->opcode() == io_ldc)) {
	in_ldc *ldci = (in_ldc *)r.instr();

	immed k = ldci->value();
	if (k.is_symbol() &&
	    k.symbol()->is_var() &&
	    ((var_sym *)k.symbol())->root_ancestor()->type()
		    ->is_call_by_ref()) {

	    ((var_sym *)k.symbol())->reset_addr_taken();

	    /* remove the ldc's tree_instr if it's in a flat list */
	    tree_instr *ldcti = ldci->parent();
	    if (ldcti) {
		ldcti->remove_instr(ldci);
		delete ldcti->parent()->remove(ldcti->list_e());
		delete ldcti;
	    }

	    /* check if there is a "fields" annotation */
	    annote *flds = NULL;
	    if (ldci->are_annotations()) {
		flds = ldci->annotes()->get_annote(k_fields);
	    }

	    type_node *result_type = ldci->result_type();

	    delete ldci;
	    return cbr_immed_to_addr(k, result_type, flds);
	}
    }
    return r;
}


static operand cbr_immed_to_addr(immed cbr_immed, type_node *result_type,
                                 annote *field_annote)
{
    assert(cbr_immed.is_symbol());
    sym_node *cbr_sym = cbr_immed.symbol();
    assert(cbr_sym->is_var());
    var_sym *cbr_var = (var_sym *)cbr_sym;
    var_sym *root_var = cbr_var->root_ancestor();
    assert(root_var->type()->is_call_by_ref());
    int offset = cbr_immed.offset() + cbr_var->root_offset();
    if (offset == 0)
      {
        if ((unqual_cbr(root_var->type())->ptr_to() == result_type) &&
            (field_annote == NULL))
          {
            return operand(root_var);
          }
        in_rrr *new_cvt =
                new in_rrr(io_cvt, result_type, operand(), operand(root_var));
        if (field_annote != NULL)
            new_cvt->annotes()->append(field_annote);
        return operand(new_cvt);
      }
    else
      {
        /* convert from bits to bytes */
        assert((offset % target.addressable_size) == 0);
        offset /= target.addressable_size;

        in_ldc *new_ldc = new in_ldc(type_ptr_diff, operand(), immed(offset));
        in_rrr *new_add =
                new in_rrr(io_add, result_type, operand(), operand(root_var),
                           new_ldc);
        if (field_annote != NULL)
            new_add->annotes()->append(field_annote);
        return operand(new_add);
      }
}


static operand cbr_op_to_addr(operand the_op)
{
    assert(the_op.is_symbol());
    return cbr_immed_to_addr(immed(the_op.symbol()),
                             unqual_cbr(the_op.type())->ptr_to(), NULL);
}


/*
 *  Check if an operand is a call-by-ref variable.
 */

boolean
is_call_by_ref_var (operand r)
{
    if (r.is_symbol()) {
	if (r.symbol()->root_ancestor()->type()->is_call_by_ref()) return TRUE;
    }
    return FALSE;
}


/*
 *  Remove the "call_by_ref" modifier from a type and return the result.
 */

type_node *
unqual_cbr (type_node *t)
{
    if (!t->is_modifier()) return t;
    modifier_type *mt = (modifier_type *)t;

    if (t->op() == TYPE_CALL_BY_REF) return mt->base();

    type_node *base = unqual_cbr(mt->base());
    type_node *tnew = new modifier_type(mt->op(), base);
    tnew = mt->parent()->install_type(tnew);
    return tnew;
}


