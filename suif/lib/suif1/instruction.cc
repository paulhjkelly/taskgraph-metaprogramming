/*  SUIF Instruction Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "instruction.h"

#define RCS_BASE_FILE instruction_cc

#include "suif1.h"

RCS_BASE(
    "$Id$")

const char *k_suif;

/*
 *  Writing Instructions to a File
 */

/*
 *  Write the fields shared by all instructions.  This is called by the
 *  write methods in the derived instruction classes.
 */

void
instruction::write (out_stream *os)
{
    os->write_byte((char)op);
    assert_msg(result_type() != NULL, 
	       ("attempt to write NULL type"));
    result_type()->write(os);
    os->write_int(number());
}


/*
 *  Writing instruction annotations.  Before writing the actual annotations,
 *  count the ones that will be output and write out the number.
 */

void
instruction::write_annotes (out_stream *os)
{
    /* first output the number of annotations that will be written */
    int n = num_output_annotes();
    os->write_int(n);

    /* now write them out */
    if (n != 0) {
	annote_list_iter ali(annotes());
	while (!ali.is_empty()) {
	    annote *an = ali.step();
	    an->write(os);
	}
    }
}


/*
 *  If a source operand is an expression tree (i.e. an instruction that does
 *  not have its own tree_instr parent), that expression tree must be written
 *  out before the current instruction.
 */

void
instruction::write_src_expr (operand s, out_stream *os)
{
    /* write the source operand instruction if it is an expression tree */
    if (s.is_expr()) {
	s.instr()->write(os);
    }
}


/*
 *  Instruction pointers as destination operands can be set automatically and
 *  do not need to be written out.  Moreover, when reading the instruction
 *  back the destination instruction will not yet exist.  Thus, if the
 *  destination is an instruction it is simply written as a null operand.
 */

void
instruction::write_dsts (out_stream *os)
{
    if (nd == 0)
      {
        operand opr;
        opr.write(os);
        append_annote(k_no_destinations, new immed_list);
      }
    else if (nd == 1)
      {
        if (dst_op(0).is_instr())
          {
            operand opr;
            opr.write(os);
          }
        else
          {
            dst_op(0).write(os);
          }
      }
    else
      {
        if (dst_op(0).is_instr())
          {
            operand opr;
            opr.write(os);
          }
        else
          {
            dst_op(0).write(os);
          }
        immed_list *new_data = new immed_list;
        for (unsigned extra_count = 1; extra_count < nd; ++extra_count)
            new_data->append(dst_op(extra_count));
        append_annote(k_extra_destinations, new_data);
      }
}


void
in_rrr::write (out_stream *os)
{
    write_src_expr(src1_op(), os);
    write_src_expr(src2_op(), os);
    instruction::write(os);
    write_dsts(os);
    src1_op().write(os);
    src2_op().write(os);
    write_annotes(os);
}


void
in_bj::write (out_stream *os)
{
    write_src_expr(src_op(), os);
    instruction::write(os);
    assert_msg(target(), ("target field is empty"));
    target()->write(os);
    src_op().write(os);
    write_annotes(os);
}


void
in_ldc::write (out_stream *os)
{
    instruction::write(os);
    write_dsts(os);
    value().write(os);
    write_annotes(os);
}


void
in_cal::write (out_stream *os)
{
    unsigned j;

    write_src_expr(addr_op(), os);
    for (j = 0; j < num_args(); j++) {
	write_src_expr(argument(j), os);
    }

    instruction::write(os);
    write_dsts(os);
    addr_op().write(os);

    os->write_int(num_args());
    for (j = 0; j < num_args(); j++) {
	argument(j).write(os);
    }
    write_annotes(os);
}


void
in_array::write (out_stream *os)
{
    unsigned j;

    write_src_expr(base_op(), os);
    write_src_expr(offset_op(), os);
    for (j = 0; j < dims(); j++) {
	write_src_expr(index(j), os);
	write_src_expr(bound(j), os);
    }
    
    instruction::write(os);
    write_dsts(os);
    base_op().write(os);
    offset_op().write(os);
    os->write_int(offset());
    os->write_int(elem_size());
    os->write_int(dims());

    for (j = 0; j < dims(); j++) {
	index(j).write(os);
	bound(j).write(os);
    }
    write_annotes(os);
}


void
in_mbr::write (out_stream *os)
{
    write_src_expr(src_op(), os);
    instruction::write(os);
    src_op().write(os);
    os->write_int(lower());
    os->write_int(num_labs());
    default_lab()->write(os);

    for (unsigned j = 0; j < num_labs(); j++) {
	assert_msg(label(j), ("mbr label field is empty"));
	label(j)->write(os);
    }
    write_annotes(os);
}


void
in_lab::write (out_stream *os)
{
    instruction::write(os);
    assert_msg(label(), ("label field is empty"));
    label()->write(os);
    write_annotes(os);
}


void
in_gen::write (out_stream *os)
{
    unsigned j;
    for (j = 0; j < num_srcs(); j++) {
	write_src_expr(src_op(j), os);
    }

    instruction::write(os);
    write_dsts(os);
    os->write_cached(name());
    os->write_int(num_srcs());

    for (j = 0; j < num_srcs(); j++) {
	src_op(j).write(os);
    }
    write_annotes(os);
}


/*****************************************************************************/


/*
 *  Reading instructions from a file.  Read the opcode and then call the
 *  appropriate constructor.  This is a static member function instead of
 *  a constructor because the instruction's storage cannot be allocated
 *  until the format is known.  Returns NULL when the end-of-stream marker
 *  is read.
 */

instruction *
instruction::read (in_stream *is, base_symtab *syms, tree_node *t)
{
    instruction *result = NULL;
    if_ops next_op = (if_ops)is->read_byte();

    switch (which_format(next_op)) {

	case inf_rrr: {
	    result = new in_rrr(next_op, is, syms, t);
	    break;
	}

	case inf_bj: {
	    result = new in_bj(next_op, is, syms, t);
	    break;
	}

	case inf_ldc: {
	    result = new in_ldc(next_op, is, syms, t);
	    break;
	}

	case inf_cal: {
	    result = new in_cal(next_op, is, syms, t);
	    break;
	}

	case inf_array: {
	    result = new in_array(next_op, is, syms, t);
	    break;
	}

	case inf_mbr: {
	    result = new in_mbr(next_op, is, syms, t);
	    break;
	}

	case inf_lab: {
	    result = new in_lab(next_op, is, syms);
	    break;
	}

	case inf_gen: {
	    result = new in_gen(next_op, is, syms, t);
	    break;
	}

	case inf_none: {
	    /* signal the end-of-stream */
	    assert(next_op == io_eos);
	    result = NULL;
	    break;
	}

	default: {
	    assert_msg(FALSE, ("unknown instruction format"));
	}
    }

    if (result)
      {
        result->read_annotes(is, syms);
        immed_list *extra_dest_immeds =
                (immed_list *) result->get_annote(k_extra_destinations);
        if (extra_dest_immeds != NULL)
          {
            result->nd = 1 + extra_dest_immeds->count();
            operand *new_dsts = new operand[result->nd];
            new_dsts[0] = result->dsts[0];
            delete[] result->dsts;
            result->dsts = new_dsts;
            for (unsigned dest_num = 1; dest_num < result->nd; ++dest_num)
              {
                immed this_immed = (*extra_dest_immeds)[dest_num - 1];
                assert(this_immed.is_op());
                result->dsts[dest_num] = this_immed.op();
              }
            delete extra_dest_immeds;
          }
        else
          {
            immed_list *no_dest_immeds =
              (immed_list *) result->get_annote(k_no_destinations);
            if (no_dest_immeds != NULL)
              {
                delete no_dest_immeds;
                result->nd = 0;
                delete[] result->dsts;
                result->dsts = new operand[0];
              }
          }
      }
    return result;
}


void
instruction::read_annotes (in_stream *is, base_symtab *syms)
{
    int num_annotes = is->read_int();
    for (int n = 0; n < num_annotes; n++) {
	annotes()->append(new annote(is, syms));
    }
}


/*
 *  Base class constructor.  This is called by the derived class constructors
 *  to read and initialize the fields shared by all instructions.
 */

instruction::instruction (if_ops o, in_stream *is, base_symtab *syms)
    : op(o), inum(0),
      par(0), nd(0), dsts(0)
{
    op = o;
    unsigned typnum = is->read_int();
    inum = is->read_int();

    typ = syms->lookup_type_id(typnum);
    if (!typ) {
	error_line(1, NULL,
		   "type (%u) for instruction %u does not exist",
		   typnum, inum);
    }
    par = NULL;
    nd = 0;                     /* temporarily */
    dsts = NULL;
}


in_rrr::in_rrr (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), src1(), src2()
{
    nd = 1;
    dsts = new operand[1];
    dsts[0] = operand(is, syms, t);
    src1 = operand(is, syms, t);
    src2 = operand(is, syms, t);
    check_new_src(operand(), src1);
    check_new_src(operand(), src2);
}


in_bj::in_bj (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms),
      targ(0), src()
{
    targ = (label_sym *)syms->lookup_sym_id(is->read_int());
    if (!targ->is_label() && !_suif_raw_syms)
	error_line(1, NULL, "in_bj target is not a label");
    src = operand(is, syms, t);
    check_new_src(operand(), src);
}


in_ldc::in_ldc (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), val(immed((int)0))
{
    nd = 1;
    dsts = new operand[1];
    dsts[0] = operand(is, syms, t);
    val = immed(is, syms);
}


in_cal::in_cal (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), addr(),
      nargs(0), args(0)

{
    unsigned j;
    nd = 1;
    dsts = new operand[1];
    dsts[0] = operand(is, syms, t);
    addr = operand(is, syms, t);
    check_new_src(operand(), addr);

    nargs = is->read_int();
    args = nargs ? new operand[nargs] : NULL;

    for (j = 0; j < nargs; j++) {
	args[j] = operand(is, syms, t);
	check_new_src(operand(), args[j]);
    }
}


in_array::in_array (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), base(), elemsz(0), dms(0),
      indxs(0), uppers(0), offsetop(), off(0)
{
    unsigned j;
    nd = 1;
    dsts = new operand[1];
    dsts[0] = operand(is, syms, t);
    base = operand(is, syms, t);
    offsetop = operand(is, syms, t);
    off = is->read_int();
    elemsz = is->read_int();
    dms = is->read_int();

    check_new_src(operand(), base);
    check_new_src(operand(), offsetop);

    indxs = dms ? new operand[dms] : NULL;
    uppers = dms ? new operand[dms] : NULL;
    for (j = 0; j < dms; j++) {
	indxs[j] = operand(is, syms, t);
	uppers[j] = operand(is, syms, t);
	check_new_src(operand(), indxs[j]);
	check_new_src(operand(), uppers[j]);
    }
}


in_mbr::in_mbr (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), src(), low(0), nlabs(0),
      dlab(0), labs(0)
{
    src = operand(is, syms, t);
    low = is->read_int();
    nlabs = is->read_int();
    dlab = (label_sym *)syms->lookup_sym_id(is->read_int());
    if (!dlab->is_label() && !_suif_raw_syms)
	error_line(1, NULL, "mbr default symbol is not a label");

    check_new_src(operand(), src);

    labs = nlabs ? new label_sym*[nlabs] : NULL;
    for (unsigned j = 0; j < nlabs; j++) {
	labs[j] = (label_sym *)syms->lookup_sym_id(is->read_int());
	if (!labs[j]->is_label() && !_suif_raw_syms)
	    error_line(1, NULL, "mbr symbol is not a label");
    }
}


in_lab::in_lab (if_ops o, in_stream *is, base_symtab *syms)
    : instruction(o, is, syms), lab(0)
{
    lab = (label_sym *)syms->lookup_sym_id(is->read_int());
    if (!lab->is_label() && !_suif_raw_syms)
	error_line(1, NULL, "in_lab symbol is not a label");
}


in_gen::in_gen (if_ops o, in_stream *is, base_symtab *syms, tree_node *t)
    : instruction(o, is, syms), opnm(0), ns(0), srcs(0)
{
    unsigned j;
    nd = 1;
    dsts = new operand[1];
    dsts[0] = operand(is, syms, t);
    opnm = is->read_cached();
    ns = is->read_int();

    srcs = ns ? new operand[ns] : NULL;
    for (j = 0; j < ns; j++) {
	srcs[j] = operand(is, syms, t);
	check_new_src(operand(), srcs[j]);
    }
}


/*****************************************************************************/


/*
 *  Cloning Instructions
 */

/*
 *  Clone an instruction.  We must first find all of the exposed references
 *  to symbols and types in the instruction and create corresponding objects
 *  to be used in the clone.
 */

instruction *
instruction::clone (base_symtab *dst_scope)
{
    replacements r;
    find_exposed_refs(dst_scope, &r);
    r.resolve_exposed_refs(dst_scope);
    return clone_helper(&r);
}


/*
 *  Set the common fields in the newly created clone instruction.  This is
 *  called by the clone_helper methods in the derived instruction classes.
 *  If the destination operand is an instruction that is not in the same
 *  expression tree (e.g. the instructions are in a flat list), the clone
 *  must be recorded as a replacement for this instruction, so that when
 *  the destination is cloned its source operand will be set to the new clone
 *  instead of this instruction.
 */

void
instruction::clone_base (instruction *i, replacements *r, boolean no_copy)
{
    i->instruction::set_opcode(instruction::opcode());
    i->set_result_type(result_type()->clone_helper(r));

    i->set_num_dsts(num_dsts());        /* requires dummy method in
                                         * classes that have no dst */
    for (unsigned j = 0; j < num_dsts(); j++) {
	if (!no_copy && dst_op(j).is_instr()) {

	    /* the destination will automatically be set later */
	    i->set_dst(j, operand());

	    /* record destinations used in other tree_instrs */
	    if (!dst_op(j).is_expr()) {
	    /* update the lists of replacements */
	    r->oldinstrs.append(this);
	    r->newinstrs.append(i);
	}

	} else if (dst_op(j).is_symbol()) {

	    /* set the destination to the symbol's clone */
	    var_sym *vdst = (var_sym *)dst_op(j).symbol()->clone_helper(r);
	    i->set_dst(j, operand(vdst));

	} else if (dst_op(j).is_reg()) {

	    /* set the destination to the register's clone */
	    i->set_dst(j, operand(dst_op(j).reg(), 
		dst_op(j).type()->clone_helper(r)));
	}
    }

    clone_annotes(i, r, no_copy);
}


instruction *
in_rrr::clone_helper (replacements *r, boolean no_copy)
{
    in_rrr *result = this;
    if (!no_copy) result = new in_rrr;
    clone_base(result, r, no_copy);
    result->set_src1(src1_op().clone_helper(r, no_copy));
    result->set_src2(src2_op().clone_helper(r, no_copy));
    return result;
}


instruction *
in_bj::clone_helper (replacements *r, boolean no_copy)
{
    in_bj *result = this;
    if (!no_copy) result = new in_bj;
    clone_base(result, r, no_copy);
    result->set_src_op(src_op().clone_helper(r, no_copy));
    result->set_target((label_sym *)target()->clone_helper(r));
    return result;
}


instruction *
in_ldc::clone_helper (replacements *r, boolean no_copy)
{
    in_ldc *result = this;
    if (!no_copy) result = new in_ldc;
    clone_base(result, r, no_copy);
    result->set_value(value().clone_helper(r, no_copy));
    return result;
}


instruction *
in_cal::clone_helper (replacements *r, boolean no_copy)
{
    in_cal *result = this;
    if (!no_copy) result = new in_cal;
    clone_base(result, r, no_copy);
    result->set_addr_op(addr_op().clone_helper(r, no_copy));
    result->set_num_args(num_args());
    for (unsigned j = 0; j < num_args(); j++) {
	result->set_argument(j, argument(j).clone_helper(r, no_copy));
    }
    return result;
}


instruction *
in_array::clone_helper (replacements *r, boolean no_copy)
{
    in_array *result = this;
    if (!no_copy) result = new in_array;
    clone_base(result, r, no_copy);
    result->set_base_op(base_op().clone_helper(r, no_copy));
    result->set_elem_size(elem_size());
    result->set_dims(dims());
    result->set_offset(offset());
    result->set_offset_op(offset_op().clone_helper(r, no_copy));
    for (unsigned j = 0; j < dims(); j++) {
	result->set_bound(j, bound(j).clone_helper(r, no_copy));
	result->set_index(j, index(j).clone_helper(r, no_copy));
    }
    return result;
}


instruction *
in_mbr::clone_helper (replacements *r, boolean no_copy)
{
    in_mbr *result = this;
    if (!no_copy) result = new in_mbr;
    clone_base(result, r, no_copy);
    result->set_src(src_op().clone_helper(r, no_copy));
    result->set_lower(lower());
    result->set_num_labs(num_labs());
    result->set_default_lab((label_sym *)default_lab()->clone_helper(r));
    for (unsigned j = 0; j < num_labs(); j++) {
	result->set_label(j,(label_sym *)label(j)->clone_helper(r));
    }
    return result;
}


instruction *
in_lab::clone_helper (replacements *r, boolean no_copy)
{
    in_lab *result = this;
    if (!no_copy) result = new in_lab;
    clone_base(result, r, no_copy);
    result->set_label((label_sym *)label()->clone_helper(r));
    if (!no_copy) {
	/* make sure that it is not a duplicate label */
	assert_msg(result->label() != label(),
		   ("in_lab::clone_helper -- label \"%s\" has not been "
		    "replaced", label()->name()));
    }
    return result;
}


instruction *
in_gen::clone_helper (replacements *r, boolean no_copy)
{
    in_gen *result = this;
    if (!no_copy) result = new in_gen;
    clone_base(result, r, no_copy);
    result->set_name(name());
    result->set_num_srcs(num_srcs());
    for (unsigned j = 0; j < num_srcs(); j++) {
	result->set_src_op(j, src_op(j).clone_helper(r, no_copy));
    }
    return result;
}


/*****************************************************************************/


/*
 *  Check for exposed references in the common instructions fields.  This is
 *  called by the find_exposed_refs methods in the derived instruction classes.
 */

void
instruction::find_base_refs (base_symtab *dst_scope, replacements *r)
{
    /* only deal with destination operands that refer to variables */
    for (unsigned j = 0; j < num_dsts(); j++) {
	if (dst_op(j).is_symbol()) {
	    r->add_sym_ref(dst_op(j).symbol(), dst_scope);
	}
    }

    /* check the result_type */
    r->add_type_ref(result_type(), dst_scope);

    /* check for references in the annotations */
    find_annote_refs(dst_scope, r);
}


void
in_rrr::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    src1_op().find_exposed_refs(dst_scope, r);
    src2_op().find_exposed_refs(dst_scope, r);
}


void
in_bj::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    src_op().find_exposed_refs(dst_scope, r);
    r->add_sym_ref(target(), dst_scope);
}


void
in_ldc::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    value().find_exposed_refs(dst_scope, r);
}


void
in_cal::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    addr_op().find_exposed_refs(dst_scope, r);
    for (unsigned n = 0; n < num_args(); n++) {
	argument(n).find_exposed_refs(dst_scope, r);
    }
}


void
in_array::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    base_op().find_exposed_refs(dst_scope, r);
    offset_op().find_exposed_refs(dst_scope, r);
    for (unsigned n = 0; n < dims(); n++) {
	index(n).find_exposed_refs(dst_scope, r);
	bound(n).find_exposed_refs(dst_scope, r);
    }
}


void
in_mbr::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    src_op().find_exposed_refs(dst_scope, r);
    r->add_sym_ref(default_lab(), dst_scope);
    for (unsigned n = 0; n < num_labs(); n++) {
	r->add_sym_ref(label(n), dst_scope);
    }
}


void
in_lab::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    r->add_sym_ref(label(), dst_scope, TRUE);
}


void
in_gen::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    find_base_refs(dst_scope, r);
    for (unsigned n = 0; n < num_srcs(); n++) {
	src_op(n).find_exposed_refs(dst_scope, r);
    }
}


/*****************************************************************************/


/*
 *  Retrieve a source operand specified by number.
 */

operand
instruction::src_op (unsigned n)
{
    assert_msg(n < num_srcs(),
	       ("instruction::src_op - operand number %u "
		"out of range 0 to %u", n, num_srcs() - 1));
    return *get_src_ptr(n);
}


/*
 *  Set a source operand specified by number.
 */

void
instruction::set_src_op (unsigned n, operand r)
{
    assert_msg(n < num_srcs(),
	       ("instruction::set_src_op - operand number %u "
		"out of range 0 to %u", n, num_srcs() - 1));

    operand *p = get_src_ptr(n);
    check_new_src(*p, r);
    *p = r;
}


/*
 *  Source Operand Iterator.  Call the user's function on each source
 *  operand in the instruction.
 */

void
instruction::src_map (src_map_f f, void *x)
{
    for (unsigned n = 0; n < num_srcs(); n++) {
	operand new_op = src_op(n);
	if (f(this, &new_op, x)) {
	    set_src_op(n, new_op);
	}
    }
}


/*
 *  The following functions are used to associate numbers with source
 *  operands.  Given a number, the functions return the address of the
 *  corresponding operand.  These are not public functions.
 */

operand *
instruction::get_src_ptr (unsigned)
{
    assert_msg(FALSE, ("instruction::get_src_ptr - %s instruction "
		       "has no source operands",
		       op_string()));
    return NULL;
}


operand *
in_rrr::get_src_ptr (unsigned n)
{
    if (n == 0) {
	return &src1;
    } else if (n == 1) {
	return &src2;
    } else {
	assert(FALSE);
    }
    return NULL;
}


operand *
in_bj::get_src_ptr (unsigned n)
{
    if (n == 0) {
	return &src;
    }
    return NULL;
}


operand *
in_cal::get_src_ptr (unsigned n)
{
    if (n == 0) {
	return &addr;
    } else {
	assert(n <= num_args());
	return &args[n - 1];
    }
}


operand *
in_array::get_src_ptr (unsigned n)
{
    if (n == 0) {
	return &base;
    } else if (n == 1) {
	return &offsetop;
    } else if (n < dims() + 2) {
	return &indxs[n - 2];
    } else {
	assert(n < 2 * dims() + 2);
	return &uppers[n - 2 - dims()];
    }
}


operand *
in_mbr::get_src_ptr (unsigned n)
{
    assert(n == 0);
    return &src;
}


operand *
in_gen::get_src_ptr (unsigned n)
{
    if (n < num_srcs()) {
	return &srcs[n];
    }
    return NULL;
}


/*****************************************************************************/


/*
 *  Print Routines.  The main complexity in these routines involves handling
 *  expression trees.  If a source operand is an expression tree, it is
 *  assigned a number N and printed as "eN".  The "en" parameter is used to
 *  keep track of the next expression number to be used.  After printing an
 *  instruction, all of its subexpressions are printed.  Each of these is
 *  indented and prefixed with "eN:" where N is its expression number
 *  (given in the "elab" parameter).
 */

/*
 *  Print instruction fields shared by all instructions.  This is called by
 *  the print methods in the derived classes.
 */

void
instruction::print (FILE *fp, int depth, int elab, int * /* en */)
{
    /* first print the instruction number */
    if (number() != 0) {
	fprintf(fp, "%4d: ", number());
	if (depth > 3) suif_indent(fp, depth - 3);
    } else {
	suif_indent(fp, depth);
    }

    /* print the expression label if this is a subexpression */
    if (elab) fprintf(fp, "e%d: ", elab);

    /* print the opcode */
    fprintf(fp, "%s ", instruction::op_string());
}


void
in_rrr::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;
    
    instruction::print(fp, depth, elab, ien);

    switch (opcode()) {

	case io_str:
	case io_memcpy: {
	    putc(' ', fp);
	    if (src1_op().is_expr()) {
		fprintf(fp, "e%d", (*ien)++);
	    } else {
		src1_op().print(this, fp);
	    }
	    fprintf(fp, " = ");
	    if (src2_op().is_expr()) {
		fprintf(fp, "e%d", (*ien)++);
	    } else {
		src2_op().print(this, fp);
	    }
	    print_annotes(fp, depth+1);
	    putc('\n', fp);

	    /* print the expression trees */
	    if (src1_op().is_expr()) {
		src1_op().instr()->print(fp, depth+1, starten++, ien);
	    }
	    if (src2_op().is_expr()) {
		src2_op().instr()->print(fp, depth+1, starten++, ien);
	    }
	    break;
	}

	case io_mrk:
	case io_nop: {
	    print_annotes(fp, depth+1);
	    putc('\n', fp);
	    break;
	}
	    
	case io_ret: {
	    putc(' ', fp);
	    if (src1_op().is_expr()) {
		fprintf(fp, "e%d", (*ien)++);
	    } else {
		src1_op().print(this, fp);
	    }
	    print_annotes(fp, depth+1);
	    putc('\n', fp);

	    /* print the expression tree */
	    if (src1.is_expr()) {
		src1.instr()->print(fp, depth+1, starten++, ien);
	    }
	    break;
	}

	default: {
	    if (!_suif_no_types) result_type()->print_abbrev(fp);
	    putc(' ', fp);
	    if (!parent() || parent()->instr() == this) {
		assert(num_dsts() == 1);	/* until otherwise */
		dst_op().print(this, fp);
		fprintf(fp, " = ");
	    }
	    if (src1_op().is_expr()) {
		fprintf(fp, "e%d", (*ien)++);
	    } else {
		src1_op().print(this, fp);
	    }
	    /* check if the second source operand is used */
	    if (!is_unary() && (opcode() != io_lod)) {
		fprintf(fp, ", ");
		if (src2_op().is_expr()) {
		    fprintf(fp, "e%d", (*ien)++);
		} else {
		    src2_op().print(this, fp);
		}
	    }
	    print_annotes(fp, depth+1);
	    putc('\n', fp);

	    /* print the expression tree(s) */
	    if (src1_op().is_expr()) {
		src1_op().instr()->print(fp, depth+1, starten++, ien);
	    }
	    if (!is_unary() && src2.is_expr()) {
		src2_op().instr()->print(fp, depth+1, starten++, ien);
	    }
	}
    }
}


void
in_bj::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;
    
    instruction::print(fp, depth, elab, ien);

    if (opcode() == io_jmp) {

	target()->print(fp);
	print_annotes(fp, depth+1);
	putc('\n', fp);

    } else {

	/* handle conditional branches */
	putc(' ', fp);
	if (src_op().is_expr()) {
	    fprintf(fp, "e%d", (*ien)++);
	} else {
	    src_op().print(this, fp);
	}
	fprintf(fp, ", ");
	target()->print(fp);
	print_annotes(fp, depth+1);
	putc('\n', fp);

	/* print the expression tree */
	if (src_op().is_expr()) {
	    src_op().instr()->print(fp, depth+1, starten++, ien);
	}
    }
}


void
in_ldc::print (FILE *fp, int depth, int elab, int *en)
{
    instruction::print(fp, depth, elab, en);
    if (!_suif_no_types) result_type()->print_abbrev(fp);
    putc(' ', fp);

    if (!parent() || parent()->instr() == this) {
	assert(num_dsts() == 1);	/* until otherwise */
	dst_op().print(this, fp);
	fprintf(fp, " = ");
    }

    value().print(fp);
    print_annotes(fp, depth+1);
    putc('\n', fp);
}


void
in_cal::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;
    
    instruction::print(fp, depth, elab, ien);
    if (!_suif_no_types) result_type()->print_abbrev(fp);
    putc(' ', fp);

    if (!parent() || parent()->instr() == this) {
	assert(num_dsts() == 1);	/* until otherwise */
	dst_op().print(this, fp);
	fprintf(fp, " = ");
    }

    /* print the procedure address */
    if (addr_op().is_expr()) {
	fprintf(fp, "e%d", (*ien)++);
    } else {
	addr_op().print(this, fp);
    }

    /* print the argument list */
    putc('(', fp);
    unsigned i;
    for (i = 0; i < num_args(); i++) {
	if (argument(i).is_expr()) {
	    fprintf(fp, "e%d", (*ien)++);
	} else {
	    argument(i).print(this, fp);
	}
	if (i < num_args()-1) fprintf(fp, ", ");
    }
    putc(')', fp);

    print_annotes(fp, depth+1);
    putc('\n', fp);

    /* print expression tree for the address */
    if (addr_op().is_expr()) {
	addr_op().instr()->print(fp, depth+1, starten++, ien);
    }

    /* print expression trees for the arguments */
    for (i = 0; i < num_args(); i++) {
	if (argument(i).is_expr()) {
	    argument(i).instr()->print(fp, depth+1, starten++, ien);
	}
    }
}


void
in_array::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;
    
    instruction::print(fp, depth, elab, ien);
    if (!_suif_no_types) result_type()->print_abbrev(fp);
    putc(' ', fp);

    if (!parent() || parent()->instr() == this) {
	assert(num_dsts() == 1);	/* until otherwise */
	dst_op().print(this, fp);
	fputs(" = ", fp);
    }

    /* print the base address plus the offset within the element */
    if (base_op().is_expr()) {
	fprintf(fp, "e%d", (*ien)++);
    } else {
	base_op().print(this, fp);
    }
    fprintf(fp, "+%u", offset());

    /* print the FORTRAN array offset */
    if (!offset_op().is_null()) {
	if (offset_op().is_expr()) {
	    fprintf(fp, "-e%d", (*ien)++);
	} else {
	    fputs("-", fp);
	    offset_op().print(this, fp);
	}
    }

    /* print the element size */
    fprintf(fp, ", size(%u", elem_size());

    /* print the indexes */
    fputs("), index(", fp);
    unsigned i;
    for (i = 0; i < dims(); i++) {
	if (index(i).is_expr()) {
	    fprintf(fp, "e%d", (*ien)++);
	} else {
	    index(i).print(this, fp);
	}
	if (i < dims() - 1) fputs(", ", fp);
    }

    /* print the bounds */
    fputs("), bound(", fp);
    for (i = 0; i < dims(); i++) {
	if (bound(i).is_expr()) {
	    fprintf(fp, "e%d", (*ien)++);
	} else {
	    bound(i).print(this, fp);
	}
	if (i < dims() - 1) fputs(", ", fp);
    }
    putc(')', fp);
    print_annotes(fp, depth+1);
    putc('\n', fp);
    
    /* print the expression trees */
    if (base_op().is_expr()) {
	base_op().instr()->print(fp, depth+1, starten++, ien);
    }
    if (offset_op().is_expr()) {
	offset_op().instr()->print(fp, depth+1, starten++, ien);
    }
    for (i = 0; i < dims(); i++) {
	if (index(i).is_expr()) {
	    index(i).instr()->print(fp, depth+1, starten++, ien);
	}
    }
    for (i = 0; i < dims(); i++) {
	if (bound(i).is_expr()) {
	    bound(i).instr()->print(fp, depth+1, starten++, ien);
	}
    }
}


void
in_mbr::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;
    
    instruction::print(fp, depth, elab, ien);

    if (src_op().is_expr()) {
	fprintf(fp, "e%d", (*ien)++);
    } else {
	src_op().print(this, fp);
    }

    fprintf(fp, ", low %d, ", lower());
    default_lab()->print(fp);

    /* print the array of target labels */
    fprintf(fp, ", (");
    for (unsigned i = 0; i < num_labs(); i++) {
	if (i > 0) fprintf(fp, ", ");
	label(i)->print(fp);
    }
    putc(')', fp);
    print_annotes(fp, depth+1);
    putc('\n', fp);

    /* print the source expression tree */
    if (src_op().is_expr()) {
	src_op().instr()->print(fp, depth+1, starten++, ien);
    }
}


void
in_lab::print (FILE *fp, int depth, int elab, int *en)
{
    instruction::print(fp, depth, elab, en);
    label()->print(fp);
    print_annotes(fp, depth+1);
    putc('\n', fp);
}


void
in_gen::print (FILE *fp, int depth, int elab, int *en)
{
    static int internal_en;

    int *ien;
    if (en == NULL)
      {
        internal_en = 1;
        ien = &internal_en;
      }
    else
      {
        ien = en;
      }

    int starten = *ien;

    instruction::print(fp, depth, elab, ien);
    if (!_suif_no_types) result_type()->print_abbrev(fp);
    putc(' ', fp);

    if (!parent() || parent()->instr() == this) {
        for (unsigned i = 0; i < num_dsts(); i++) {
            dst_op(i).print(this, fp);
            if (i < num_dsts() - 1) fprintf(fp, ", ");
        }
	fprintf(fp, " = ");
    }

    /* print the generic operation name */
    fprintf(fp, "%s (", name());

    /* print the source operands */
    unsigned i;
    for (i = 0; i < num_srcs(); i++) {
	if (src_op(i).is_expr()) {
	    fprintf(fp, "e%d", (*ien)++);
	} else {
	    src_op(i).print(this, fp);
	}
	if (i < num_srcs() - 1) fprintf(fp, ", ");
    }

    putc(')', fp);
    print_annotes(fp, depth+1);
    putc('\n', fp);

    /* print the expression trees */
    for (i = 0; i < num_srcs(); i++) {
	if (src_op(i).is_expr()) {
	    src_op(i).instr()->print(fp, depth+1, starten++, ien);
	}
    }
}


/*****************************************************************************/


/*
 *  Other Stuff
 */

instruction::instruction ()
    : op(io_nop), typ(type_void), inum(0),
      par(0), nd(1), dsts(new operand[1])
{
    //nd = 1;                     /* implicit assumption in SUIF */
}


instruction::instruction (if_ops o, type_node *t)
    : op(o), typ(t), inum(0),
      par(0), nd(1), dsts(new operand[1])
{
    // nd = 1;                     /* implicit assumption in SUIF */
}


/*
 *  Instruction destructor.  It would be nice to just call src_map(free_expr,
 *  NULL) here but C++ refuses to handle virtual functions within destructors.
 *  To get around this, each derived class with source operands must have its
 *  own destructor to call the src_map function.
 */

instruction::~instruction ()
{
    /* make sure that the instruction has been removed from its tree_instr
       parent before this point */
    assert_msg(!parent(),
	       ("instruction::~instruction - instruction not yet removed"));

    /* if the destination operand is an instruction, it must be removed from
       the corresponding source operand in that instruction */
     for (unsigned j = 0; j < num_dsts(); j++) {
        if (dst_op(j).is_instr()) remove(j);
    }
    if (nd) delete[] dsts;
}


/*
 *  Delete an expression tree in a source operand.  This function is used in
 *  the destructors for the derived classes.  The "x" parameter is unused.
 */

static boolean
free_expr (instruction *, operand *r, void *)
{
    if (r->is_instr()) {

	/* remove the expression tree from this instruction */
	r->remove();

	/* delete the src instr if it isn't attached to another tree_instr */
	if (!r->instr()->parent()) delete r->instr();
    }

    /* no need to update the operand; remove already set it to null */
    return FALSE;
}


/*
 *  Retrieve a destination operand specified by a number.
 */

operand
instruction::dst_op (unsigned n)
{
    assert_msg(n < num_dsts(),
               ("instruction::dst_op - operand number %u "
                "out of range 0 to %u", n, num_dsts() - 1));
    return dsts[n];
}


/*
 *  Destination Operand Iterator.  Call the user's function on each
 *  destination operand in the instruction.
 */
void
instruction::dst_map (src_map_f f, void *x)
{
    for (unsigned n = 0; n < num_dsts(); n++) {
        operand new_op = dst_op(n);
        if (f(this, &new_op, x)) {
            set_dst(n, new_op);
        }
    }
}


/*
 *  Change the number of destinations.  If the number is increased, the array
 *  is reallocated, and the existing values (if any) are copied to the new
 *  array.
 */

void
instruction::set_num_dsts (unsigned n)
{
    if (n <= nd) {
        nd = n;
        return;
    }

    operand *new_dsts = new operand[n];

    /* copy the old array (as much as possible) */
    unsigned i;
    for (i = 0; (i < nd) && (i < n); i++) {
        new_dsts[i] = dsts[i];
    }

    /* initialize any new destination operands */
    for (; i < n; i++) {
        new_dsts[i] = operand();
    }

    nd = n;
    delete[] dsts;
    dsts = new_dsts;
}


/*
 *  Set a destination operand specified by a number.
 */

void
instruction::set_dst (unsigned n, operand r)
{
    assert_msg(n < num_dsts(),
               ("instruction::set_dst - operand number %u "
                "out of range 0 to % u", n, num_dsts() - 1));
    check_new_dst(dsts[n], r);
    dsts[n] = r;
}


/*
 *  Some derived classes do not use the destination operand.  This method
 *  is provided for them to call if the user tries to set the destination.
 */

void
instruction::no_dst_error ()
{
    assert_msg(FALSE, ("%s instruction does not have a destination",
                       op_string()));
}


/*
 *  If the destination operand needs to be an instruction pointer, the library
 *  will handle it automatically.  To avoid inconsistent pointers, users are
 *  not allowed to explicitly set the destination to be an instruction.  This
 *  method is called whenever the destination is set.
 */

void
instruction::check_new_dst (operand old_op, operand new_op)
{
    assert_msg(!old_op.is_instr() || new_op.is_null(),
	       ("check_new_dst - instruction operand not yet removed"));
    assert_msg(!new_op.is_instr(),
	       ("check_new_dst - cannot set dst operand to an instruction"));
}


/*
 *  When a source operand is an instruction, the destination of that
 *  instruction should point back to this one.  This is handled automatically
 *  by calling this method whenever a source operand is set.  If the source
 *  operand instruction does not have a parent (i.e. it is not already attached
 *  to a tree_instr), make it have the same parent as this instruction.
 */

void
instruction::check_new_src (operand old_op, operand new_op)
{
    /* check if changing an instruction operand without first removing it */
    assert_msg(!old_op.is_instr() || new_op.is_null() || (new_op == old_op),
	       ("check_new_src - instruction operand not yet removed"));

    if (new_op.is_instr()) {
        /* instruction pointers MUST be primary (i.e. 0th) dst operand */
        /* Note: access dsts[] directly to bypass checks in check_new_dst() */

        /* disallow dags in expression-tree lists */
        assert_msg(!new_op.instr()->dsts[0].is_instr() || (new_op == old_op),
            ("check_new_src - new_op still attached to another expr. tree"));

        new_op.instr()->dsts[0] = operand(this);

	/* check if the operand needs a parent */
	if (parent() && !new_op.instr()->parent()) {
	    new_op.instr()->set_parent(parent());
	}
    }
}


/*
 *  In most cases, the owner is the same as the parent.  The exception is
 *  for instructions that are tree_for operands.  These operands are actually
 *  attached to dummy copy instructions on lists attached to the tree_for.
 *  This function checks if the parent is such a dummy copy and if so, returns
 *  the tree_for.  This only works if the tree_for operand list contains a
 *  single expression (not a flat list); otherwise, the actual tree_instr
 *  parent is returned.
 */

tree_node *
instruction::owner ()
{
    tree_node *ti = parent();
    if (ti == NULL)
	return NULL;

    tree_node_list *parent_list = ti->parent();
    if (parent_list == NULL)
	return NULL;

    tree_node *grandparent = parent_list->parent();
    if (grandparent == NULL)
	return NULL;

    /* check if the grandparent is a tree_for */
    if (!grandparent->is_for()) return ti;

    /* check if the instruction is part of one of the operands */
    tree_for *tf = (tree_for *)grandparent;
    if ((parent_list != tf->lb_list()) &&
	(parent_list != tf->ub_list()) &&
	(parent_list != tf->step_list())) {
	return ti;
    }

    /* check if the parent_list contains a single expression */
    if (!parent_list->is_op()) return ti;

    return grandparent;
}


/*
 *  Set the parent of this instruction and all of the instructions in its
 *  subexpressions that do not already have parents.  Trying to change the
 *  parent from one tree_instr to another without first removing the
 *  instruction from its original parent will cause an error.
 */

void
instruction::set_parent (tree_instr *p)
{
    /* Only allow the parent to be set if the instruction doesn't already
       have a parent.  Otherwise, the parent can only be set to NULL.
       (That is needed to implement the tree_instr::remove_instr method.) */

    assert_msg(!par || !p,
	       ("instruction::set_parent - already have a parent"));

    par = p;

    /* check if instruction operands need parents */
    for (unsigned n = 0; n < num_srcs(); n++) {
	operand r = src_op(n);
	if (r.is_expr()) {
	    r.instr()->set_parent(p);
	}
    }
}


/*
 *  When changing a source operand from an instruction pointer to some other
 *  value, this method must first be called to remove the instruction from
 *  the source operand.  Besides setting that source to a null operand, it
 *  also sets the instruction's destination to a null operand.  If the
 *  instruction does not have its own parent tree_instr, it is removed
 *  from the tree_instr where it was used.
 */

void
instruction::remove (unsigned dstnum)
{
    /* find the instruction containing this as an operand */
    assert_msg(dst_op(dstnum).is_instr(),
	       ("instruction::remove - dst must be an instruction"));
    instruction *dsti = dst_op(dstnum).instr();

    /* remove this instruction from its parent tree_instr */
    if (parent() && (parent() == dsti->parent())) {
	parent()->remove_instr(this);
    }

    /* search through the parent's sources for this operand */
    for (unsigned n = 0; n < dsti->num_srcs(); n++) {
	operand r = dsti->src_op(n);
	if (r.is_instr() && (r.instr() == this)) {
	    dsti->set_src_op(n, operand());
	}
    }

    /* set the destination to null */
    set_dst(dstnum, operand());
}


/*
 *  Remove the instruction numbers from this node and all its descendants.
 *  This is needed when moving code from one procedure to another so that
 *  numbers don't conflict.
 */

void
instruction::clear_numbers ()
{
    set_number(0);

    /* recursively handle source expressions */
    for (unsigned n = 0; n < num_srcs(); n++) {
	operand r = src_op(n);
	if (r.is_expr()) r.instr()->clear_numbers();
    }
}


/*
 *  Check if an instruction is a branch of some sort.  This does not include
 *  calls and returns.
 */

boolean
instruction::is_branch ()
{
    switch (op) {
	case io_btrue:
	case io_bfalse:
	case io_jmp:
	case io_mbr: {
	    return TRUE;
	}
	default: {
	    break;
	}
    }
    return FALSE;
}


/*****************************************************************************/


in_rrr::in_rrr ()
    : instruction(), src1(), src2()
{
}


in_rrr::in_rrr (if_ops o, type_node *t, operand d, operand s1, operand s2)
    : instruction(o, t), src1(s1), src2(s2)
{
    check_new_dst(operand(), d);
    check_new_src(operand(), s1);
    check_new_src(operand(), s2);
    set_num_dsts(1);
    dsts[0] = d;
}


in_rrr::~in_rrr ()
{
    src_map(free_expr, NULL);
}


/*
 *  The src_op and set_src_op methods provide convenient access to the source
 *  operand in instructions with a single source.  This includes instructions
 *  where the other source operand is used for a special purpose (e.g. branch
 *  target address or shift count).  The check_src_op_ref function makes sure
 *  that the instruction has a single operand in the src1 field.
 */

static boolean
check_src_op_ref (in_rrr *i)
{
    if (i->is_unary()) return TRUE;

    switch (i->opcode()) {
	case io_asr:
	case io_lsl:
	case io_lsr:
	case io_rot:
	case io_ret:
	case io_lod: {
	    return TRUE;
	}

	case io_str: {
	    return FALSE;
	}

	default: {
	    assert_msg(FALSE, ("in_rrr::src_op - invalid opcode \"%s\"",
                               i->op_string()));
	}
    }
    return UNKNOWN;
}


operand
in_rrr::src_op ()
{
    if (check_src_op_ref(this)) return src1_op();
    return src2_op();
}


void
in_rrr::set_src (operand r)
{
    if (check_src_op_ref(this)) {
	set_src1(r);
    } else {
	set_src2(r);
    }
}


/*
 *  The shift_cnt_op methods provide access to the count operand for shift
 *  and rotate instructions.
 */

static void
check_shift_cnt_ref (in_rrr *i)
{
    switch (i->opcode()) {
	case io_asr:
	case io_lsl:
	case io_lsr:
	case io_rot: {
	    break;
	}
	default: {
	    assert_msg(FALSE, ("in_rrr::shift_cnt_op - invalid opcode \"%s\"",
                               i->op_string()));
	}
    }
}


operand
in_rrr::shift_cnt_op ()
{
    check_shift_cnt_ref(this);
    return src2_op();
}


void
in_rrr::set_shift_cnt_op (operand r)
{
    check_shift_cnt_ref(this);
    set_src2(r);
}


/*
 *  The dst_addr_op methods provide access to the destination address operand
 *  for store and memcpy instructions.
 */

static void
check_dst_addr_ref (in_rrr *i)
{
    switch (i->opcode()) {
	case io_memcpy:
	case io_str: {
	    break;
	}
	default: {
	    assert_msg(FALSE, ("in_rrr::dst_addr_op - invalid opcode \"%s\"",
                               i->op_string()));
	}
    }
}


operand
in_rrr::dst_addr_op ()
{
    check_dst_addr_ref(this);
    return src1_op();
}


void
in_rrr::set_dst_addr_op (operand r)
{
    check_dst_addr_ref(this);
    set_src1(r);
}

	    
/*
 *  The src_addr_op methods provide access to the source address operand for
 *  load and memcpy instructions.
 */

static boolean
check_src_addr_ref (in_rrr *i)
{
    switch (i->opcode()) {
	case io_lod: {
	    return TRUE;
	}
	case io_memcpy: {
	    return FALSE;
	}
	default: {
	    assert_msg(FALSE, ("in_rrr::src_addr_op - invalid opcode \"%s\"",
                               i->op_string()));
	}
    }
    return UNKNOWN;
}


operand
in_rrr::src_addr_op ()
{
    if (check_src_addr_ref(this)) return src1_op();
    return src2_op();
}


void
in_rrr::set_src_addr_op (operand r)
{
    if (check_src_addr_ref(this)) {
	set_src1(r);
    } else {
	set_src2(r);
    }
}


/*
 *  Check if an instruction performs a unary operation.
 */

boolean
in_rrr::is_unary ()
{
    switch (opcode()) {
	case io_cvt:
	case io_cpy:
	case io_neg:
	case io_not:
	case io_abs: {
	    return TRUE;
	}
	default: {
	    break;
	}
    }
    return FALSE;
}


/*
 *  Check if an instruction performs a commutative operation.
 */

boolean
in_rrr::is_commutative ()
{
    switch (opcode()) {
	case io_add:
	case io_mul:
	case io_min:
	case io_max:
	case io_and:
	case io_ior:
	case io_xor:
	case io_sne:
	case io_seq: {
	    return TRUE;
	}
	default: {
	    break;
	}
    }
    return FALSE;
}


/*****************************************************************************/


in_bj::in_bj ()
    : instruction(io_jmp, type_void), targ(0), src()
{
}


in_bj::in_bj (if_ops o, label_sym *t, operand r)
    : instruction(o, type_void), targ(t), src(r)
{
    check_new_src(operand(), r);
}


in_bj::~in_bj ()
{
    src_map(free_expr, NULL);
}


/*****************************************************************************/


in_ldc::in_ldc ()
    : instruction(io_ldc, type_void), val((int)0)
{
}


in_ldc::in_ldc (type_node *t, operand d, const immed &v)
    : instruction(io_ldc, t), val(v)
{
    check_new_dst(operand(), d);
    set_num_dsts(1);
    dsts[0] = d;
}


/*****************************************************************************/


in_cal::in_cal ()
    : instruction(io_cal, type_void), addr(), nargs(0), args(0)
{
}


in_cal::in_cal (type_node *t, operand d, operand s, unsigned n)
    : instruction(io_cal, t), addr(s), nargs(n), 
      args(n ? new operand[n] : 0)
{
    check_new_dst(operand(), d);
    check_new_src(operand(), s);
    set_num_dsts(1);
    dsts[0] = d;
}


in_cal::in_cal (type_node *t, operand d, operand s, operand arg1, operand arg2,
                operand arg3, operand arg4, operand arg5)
    : instruction(io_cal, t), addr(s), nargs(0), args(0)
{
    check_new_dst(operand(), d);
    check_new_src(operand(), s);
    set_num_dsts(1);
    dsts[0] = d;

    if (arg1.is_null()) {
	nargs = 0;
	assert(arg2.is_null() && arg3.is_null() && arg4.is_null() &&
	       arg5.is_null());
    } else if (arg2.is_null()) {
	nargs = 1;
	assert(arg3.is_null() && arg4.is_null() && arg5.is_null());
    } else if (arg3.is_null()) {
	nargs = 2;
	assert(arg4.is_null() && arg5.is_null());
    } else if (arg4.is_null()) {
	nargs = 3;
	assert(arg5.is_null());
    } else if (arg5.is_null()) {
	nargs = 4;
    } else {
	nargs = 5;
    }

    args = nargs ? new operand[nargs] : NULL;

    if (nargs == 0)
	return;
    set_argument(0, arg1);
    if (nargs == 1)
	return;
    set_argument(1, arg2);
    if (nargs == 2)
	return;
    set_argument(2, arg3);
    if (nargs == 3)
	return;
    set_argument(3, arg4);
    if (nargs == 4)
	return;
    set_argument(4, arg5);
}


in_cal::~in_cal ()
{
    src_map(free_expr, NULL);
    if (args) delete[] args;
}


/*
 *  Make sure that an argument index is valid.
 */

void
in_cal::check_range (unsigned n)
{
    assert_msg(n < num_args(),
	       ("in_cal: argument number %u out of range 0 to %u",
		n, num_args() - 1));
}


/*
 *  Change the number of arguments.  If the number is increased, the argument
 *  array is reallocated and the existing values (if any) are copied to the
 *  new array.
 */

void
in_cal::set_num_args (unsigned n)
{
    if (n <= nargs) {
	nargs = n;
	return;
    }

    operand *new_args = new operand[n];

    /* copy the old arguments (as much as possible) */
    unsigned i;
    for (i = 0; (i < nargs) && (i < n); i++) {
	new_args[i] = args[i];
    }

    /* initialize any new arg fields */
    for (unsigned j = nargs; j < n; j++) {
	new_args[j] = operand();
    }

    nargs = n;
    delete[] args;
    args = new_args;
}


void
in_cal::set_argument (unsigned n, operand r)
{
    check_range(n);
    check_new_src(args[n], r);
    args[n] = r;
}


/*****************************************************************************/


in_array::in_array ()
    : instruction(io_array, type_void), base(), elemsz(0), dms(0),
      indxs(0), uppers(0), offsetop(), off(0)
{
}


in_array::in_array (type_node *t, operand d, operand b, unsigned sz,
		    unsigned dm, unsigned f, operand fop)
    : instruction(io_array, t), base(b), elemsz(sz), dms(dm),
      indxs(dm ? new operand[dm] : NULL),
      uppers(dm ? new operand[dm] : NULL),
      offsetop(fop), off(f)
{
    check_new_dst(operand(), d);
    check_new_src(operand(), b);
    check_new_src(operand(), fop);
    set_num_dsts(1);
    dsts[0] = d;
}


in_array::~in_array ()
{
    src_map(free_expr, NULL);
    if (indxs) delete[] indxs;
    if (uppers) delete[] uppers;
}


/*
 *  Make sure that a dimension number is valid.
 */

void
in_array::check_range (unsigned n)
{
    assert_msg(n < dims(),
	       ("in_array: dimension %u out of range 0 to %u",
		n, dims() - 1));
}


/*
 *  Change the number of dimensions.  If the number is increased, the arrays
 *  are reallocated, and the existing values (if any) are copied to the new
 *  arrays.
 */

void
in_array::set_dims (unsigned n)
{
    if (n <= dms) {
	dms = n;
	return;
    }

    operand *new_indxs = new operand[n];
    operand *new_uppers = new operand[n];

    /* copy the old arrays (as much as possible) */
    unsigned i;
    for (i = 0; (i < dms) && (i < n); i++) {
	new_indxs[i] = indxs[i];
	new_uppers[i] = uppers[i];
    }

    /* initialize any new dimension fields */
    for (unsigned j = dms; j < n; j++) {
	new_indxs[j] = operand();
	new_uppers[j] = operand();
    }

    dms = n;
    delete[] indxs;
    delete[] uppers;
    indxs = new_indxs;
    uppers = new_uppers;
}


void
in_array::set_index (unsigned n, operand r)
{
    check_range(n);
    check_new_src(indxs[n], r);
    indxs[n] = r;
}


void
in_array::set_bound (unsigned n, operand r)
{
    check_range(n);
    check_new_src(uppers[n], r);
    uppers[n] = r;
}


/*
 *  Find the element type returned by an array instruction.  The result type
 *  will usually be a pointer to this or to a component of this type.
 */

type_node *
in_array::elem_type ()
{
    type_node *t = base_op().type()->unqual();
    assert_msg(t->is_ptr(),
	       ("in_array::elem_type - base operand not a pointer"));
    ptr_type *pt = (ptr_type *)t;
    t = pt->ref_type();

    /* find the element type for each dimension */
    for (unsigned d = 0; d < dims(); d++) {

	t = t->unqual();
	assert_msg(t->is_array(), ("in_array::elem_type - not an array type"));
	array_type *at = (array_type *)t;

	/* get the next element type */
	t = at->elem_type();
    }

    return t;
}


/*****************************************************************************/


in_mbr::in_mbr ()
    : instruction(io_mbr, type_void), src(), low(0), nlabs(0),
      dlab(0), labs(0)
{
    low = 0;
    nlabs = 0;
    dlab = NULL;
    labs = NULL;
}


in_mbr::in_mbr (operand s, int l, unsigned n, label_sym *def)
    : instruction(io_mbr, type_void), src(s), low(l), nlabs(n),
      dlab(def), labs(n ? new label_sym*[n] : 0)
{
    check_new_src(operand(), s);
}


in_mbr::~in_mbr ()
{
    if (labs) delete[] labs;
}


/*
 *  Make sure that a label index is valid.
 */

void
in_mbr::check_range (unsigned n)
{
    assert_msg(n < num_labs(),
	       ("in_mbr: label number %u out of range 0 to %u",
		n, num_labs() - 1));
}


/*
 *  Change the number of labels.  If the number is increased, the label array
 *  is reallocated, and the existing values (if any) are copied to the new
 *  array.
 */

void
in_mbr::set_num_labs (unsigned n)
{
    if (n <= nlabs) {
	nlabs = n;
	return;
    }

    label_sym **new_labs = new label_sym*[n];

    /* copy the old labels (as much as possible) */
    unsigned i;
    for (i = 0; (i < nlabs) && (i < n); i++) {
	new_labs[i] = labs[i];
    }

    /* initialize any new labels */
    for (unsigned j = nlabs; j < n; j++) {
	new_labs[j] = NULL;
    }

    nlabs = n;
    delete[] labs;
    labs = new_labs;
}


void
in_mbr::set_label (unsigned n, label_sym *ls)
{
    check_range(n);
    labs[n] = ls;
}


/*****************************************************************************/


in_lab::in_lab ()
    : instruction(io_lab, type_void), lab(0)
{
}


in_lab::in_lab (label_sym *s)
    : instruction(io_lab, type_void), lab(s)
{
}


/*****************************************************************************/


in_gen::in_gen ()
    : instruction(io_gen, type_void), opnm(0), ns(0), srcs(0)
{
}


in_gen::in_gen (const char *o, type_node *t, operand d, unsigned n)
    : instruction(io_gen, t), opnm(0), ns(0), srcs(0)
{
    check_new_dst(operand(), d);
    set_name(o);
    set_num_dsts(1);
    dsts[0] = d;
    ns = n;
    srcs = n ? new operand[n] : NULL;
}


void
in_gen::init (const char *o, operand d, unsigned n, operand s0, operand s1,
              operand s2, operand s3, operand s4)
{
    check_new_dst(operand(), d);
    set_name(o);
    set_num_dsts(1);
    dsts[0] = d;
    ns = n;
    srcs = n ? new operand[n] : NULL;

    switch (n)
      {
	default:
	case 5:
	    set_src_op(4, s4);
	case 4:
	    set_src_op(3, s3);
	case 3:
	    set_src_op(2, s2);
	case 2:
	    set_src_op(1, s1);
	case 1:
	    set_src_op(0, s0);
	case 0:
	    break;
      }
}


in_gen::~in_gen ()
{
    src_map(free_expr, NULL);
    if (srcs) delete[] srcs;
}


/*
 *  Set the name field.  This is really a sub-opcode field.  Since the
 *  names should always be entered in the lexicon, we make sure of that
 *  here.
 */

void
in_gen::set_name (const char *o)
{
    opnm = o ? lexicon->enter(o)->sp : NULL;
}


/*
 *  Change the number of sources.  If the number is increased, the array
 *  is reallocated, and the existing values (if any) are copied to the new
 *  array.
 */

void
in_gen::set_num_srcs (unsigned n)
{
    if (n <= ns) {
	ns = n;
	return;
    }

    operand *new_srcs = new operand[n];

    /* copy the old array (as much as possible) */
    unsigned i;
    for (i = 0; (i < ns) && (i < n); i++) {
	new_srcs[i] = srcs[i];
    }

    /* initialize any new source operands */
    for (; i < n; i++) {
	new_srcs[i] = operand();
    }

    ns = n;
    delete[] srcs;
    srcs = new_srcs;
}


