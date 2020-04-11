/*  Type-checking functions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libcheck.a"

#include "check.h"
#include "check_internal.h"

/*  local function declarations */
static void check_fields(tree_node *tn, instruction *i, type_node *tbase);
static void check_null_dst(instruction *i, tree_node *tn);
static void check_null_src(instruction *i, operand r, tree_node *tn);



void
check_type (tree_node *tn, void *)
{
    tree_node *old_node = current_node;
    current_node = tn;
    if (tn->is_instr()) {
	tree_instr *ti = (tree_instr *)tn;
	ti->instr_map(&check_type_instr, (void *)tn, FALSE);
    }
    current_node = old_node;
}


void
check_type_instr (instruction *i, void *x)
{
    tree_node *tn = (tree_node *)x;

    /* check if this is a dummy copy in a tree_for node */
    boolean is_dummy = FALSE;
    if ((tn != NULL) && (!tn->list_e()->next())) {
	assert(tn->is_instr());
	tree_instr *ti = (tree_instr *)tn;
	tree_node_list *parent_list = tn->parent();
	tree_node *grandparent = parent_list->parent();
	if (grandparent->is_for() && (ti->instr() == i)) {
	    tree_for *tf = (tree_for *)grandparent;
	    if ((parent_list == tf->lb_list()) ||
		(parent_list == tf->ub_list()) ||
		(parent_list == tf->step_list())) {
		if (i->opcode() == io_cpy) {
		    is_dummy = TRUE;
		} else {
		    problem("tree_for (%u) - missing dummy cpy for operand",
                            tf->number());
		}
	    }
	}
    }

    if (i->result_type() == 0) {
	problem_instr(tn, i, "destination has NULL type");
	return;
    }

    if (i->result_type()->is_modifier()) {
	problem_instr(tn, i, "destination has qualified type");
    }

    /* check the destination (note: if the destination is an instruction,
       the type will be checked when the result value is used) */

    operand dst = i->dst_op();
    if (is_dummy) {
	if (!dst.is_null()) {
	    problem_instr(tn, i, "destination for dummy cpy is not NULL");
	}
	if (i->result_type()->unqual()->op() == TYPE_VOID) {
	    problem_instr(tn, i, "dummy cpy has VOID type");
	}
    } else if (dst.is_symbol()) {
	if (!i->result_type()->compatible(dst.symbol()->type())) {
	    problem_instr(tn, i, "result type doesn't match destination");
	}
    }

    if (i->result_type()->op() == TYPE_VOID) {
	if (!dst.is_null()) {
	    problem_instr(tn, i,
                          "void return type with non-null destinaton operand");
	}
    }

    /* now check the source operands and other conditions associated with
       particular opcodes */

    // DLH - Check for types in each operand
    for (unsigned opn = 0; 
	 opn < i->num_srcs(); opn++) {
      if (i->src_op(opn).type() == 0) {
	problem_instr(tn, i,
		      "instruction with NULL source_op type");
	// Can't continue
	return;
      }
    }
    switch (i->opcode()) {

	case io_ret: {
	    in_rrr *ri = (in_rrr *)i;
	    check_null_dst(i, tn);
	    type_node *ret_type = ri->src_op().type()->unqual();
            if (tn != NULL) {
		proc_sym *ps = tn->proc();
		if (!ret_type->compatible(ps->type()->return_type())) {
		    problem_instr(tn, i, "return value type does not match "
                                         "function declaration");
		}
	    }
	    if ((ret_type->op() == TYPE_ARRAY) ||
		(ret_type->op() == TYPE_FUNC)) {
		problem_instr(tn, i, "invalid return type");
	    }
	    break;
	}

	case io_mbr: {
	    in_mbr *mbri = (in_mbr *)i;
	    check_null_dst(i, tn);
	    type_node *t = mbri->src_op().type()->unqual();
	    if ((t->op() != TYPE_INT) && (t->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "source operand must have type INT or "
                                     "ENUM");
	    }
	    break;
	}

	case io_lab:
	case io_nop:
	case io_mrk: {
	    check_null_dst(i, tn);
	    for (unsigned n = 0; n < i->num_srcs(); n++) {
		check_null_src(i, i->src_op(n), tn);
	    }
	    break;
	}

	case io_divfloor:
	case io_divceil:
	case io_rem:
	case io_mod: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "invalid result type");
	    } else {
		if (!t->compatible(ri->src1_op().type()) ||
		    !t->compatible(ri->src2_op().type())) {
		    problem_instr(tn, i, "source operand does not match "
                                         "result type");
		}
	    }
	    break;
	}

	case io_not:
	case io_and:
	case io_ior:
	case io_xor: {
	    in_rrr *ri = (in_rrr *)i;
	    boolean bad_type = FALSE;
	    type_node *t = i->result_type()->unqual();
	    if ((t->op() == TYPE_INT) || (t->op() == TYPE_ENUM)) {
		base_type *bt = (base_type *)t;
		if (bt->is_signed()) {
		    bad_type = TRUE;
		    problem_instr(tn, i, "result type is not unsigned");
		}
	    } else {
		bad_type = TRUE;
		problem_instr(tn, i, "invalid result type");
	    }

	    if (!bad_type) {
		boolean bad_source = FALSE;
		if (!t->compatible(ri->src1_op().type())) {
		    bad_source = TRUE;
		}
		if (i->opcode() == io_not) {
		    check_null_src(i, ri->src2_op(), tn);
		} else if (!t->compatible(ri->src2_op().type())) {
		    bad_source = TRUE;
		}
		if (bad_source) {
		    problem_instr(tn, i, "source operand does not match "
                                         "result type");
		}
	    }
	    break;
	}

	case io_add:
	case io_sub:
	case io_neg:
	case io_abs:
	case io_min:
	case io_max:
	case io_mul:
	case io_div: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    type_node *t1 = ri->src1_op().type()->unqual();
	    type_node *t2 = ri->src2_op().type()->unqual();

	    /* handle pointer addition separately */
	    if ((i->opcode() == io_add) && (t->op() == TYPE_PTR)) {
		type_node *toff;
		ptr_type *tptr;
		if (t1->is_ptr()) {
		    tptr = (ptr_type *)t1;
		    toff = t2;
		} else if (t2->is_ptr()) {
		    tptr = (ptr_type *)t2;
		    toff = t1;
		} else {
		    problem_instr(tn, i, "source operands do not match "
                                         "result type");
                    break;
		}
		if (((toff->op() != TYPE_INT) && (toff->op() != TYPE_ENUM)) ||
		    (toff->size() > target.size[C_ptr])) {
		    problem_instr(tn, i, "illegal pointer addition");
		}

		/* check the "fields" annotation (if there is one) */
		check_fields(tn, i, tptr->ref_type()->unqual());
		break;
	    }

	    /* check for pointer subtraction */
	    if ((i->opcode() == io_sub) && (t1->op() == TYPE_PTR)) {
		if (t2->op() == TYPE_PTR) {
		    if (((t->op() != TYPE_INT) && (t->op() != TYPE_ENUM)) ||
			(t->size() != target.size[target.ptr_diff_type])) {
			problem_instr(tn, i, "invalid result type for "
                                             "pointer subtraction");
		    }
		} else if (((t2->op() == TYPE_INT) ||
			    (t2->op() == TYPE_ENUM)) &&
			   (t2->size() <= target.size[C_ptr])) {
		    if (t->op() != TYPE_PTR) {
			problem_instr(tn, i, "invalid result type for pointer "
                                            "subtraction");
		    }
		} else {
		    problem_instr(tn, i, "invalid source operands");
		}
		break;
	    }

	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_FLOAT) &&
		(t->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "invalid result type");
	    } else {
		boolean bad_source = FALSE;
		if (!t->compatible(t1)) {
		    bad_source = TRUE;
		}
		if ((i->opcode() == io_neg) ||
		    (i->opcode() == io_abs)) {
		    check_null_src(i, ri->src2_op(), tn);
		} else if (!t->compatible(t2)) {
		    bad_source = TRUE;
		}
		if (bad_source) {
		    problem_instr(tn, i, "source operand does not match "
                                         "result type");
		}
	    }
	    break;
	}

	case io_cpy: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_ENUM) &&
		(t->op() != TYPE_FLOAT) &&
		(t->op() != TYPE_PTR) &&
		(t->op() != TYPE_ARRAY) &&
		(t->op() != TYPE_GROUP) &&
		(t->op() != TYPE_STRUCT) &&
		(t->op() != TYPE_UNION)) {
		problem_instr(tn, i, "invalid result type");
	    }
	    if (t->size() == 0) {
		problem_instr(tn, i, "invalid result type");
	    }
	    type_node *srct = ri->src_op().type()->unqual();
	    if (!t->compatible(srct)) {
		problem_instr(tn, i, "source operand does not match result "
                                     "type");
	    }
	    /* check the "fields" annotation (this should really only be
	       needed for io_cvt instructions but we might as well be safe) */
	    if (srct->is_ptr()) {
		check_fields(tn, i, ((ptr_type *)srct)->ref_type()->unqual());
	    }
	    check_null_src(i, ri->src2_op(), tn);
	    break;
	}

	case io_asr:
	case io_lsl:
	case io_lsr: {
	    in_rrr *ri = (in_rrr *)i;

	    /* must have "signed" type for asr, "unsigned" for lsr */
	    boolean bad_type = TRUE;
	    type_node *t = i->result_type()->unqual();
	    if ((t->op() == TYPE_INT) || (t->op() != TYPE_ENUM)) {
		base_type *bt = (base_type *)t;
		if (((i->opcode() != io_asr) || bt->is_signed()) &&
		    ((i->opcode() != io_lsr) || !bt->is_signed())) {
		    bad_type = FALSE;
		}
	    }
	    if (bad_type) {
		problem_instr(tn, i, "invalid result type");
	    } else if (!t->compatible(ri->src_op().type())) {
		problem_instr(tn, i, "source operand does not match result "
			      "type");
	    }

	    t = ri->shift_cnt_op().type()->unqual();
	    if ((t->op() == TYPE_INT) || (t->op() == TYPE_ENUM)) {
		base_type *bt = (base_type *)t;
		if (bt->is_signed()) {
		    problem_instr(tn, i, "shift count type is not unsigned ");
		}
	    } else {
		problem_instr(tn, i, "invalid type for shift count");
	    }

	    break;
	}

	case io_rot: {
	    in_rrr *ri = (in_rrr *)i;

	    type_node *t = i->result_type()->unqual();
	    if ((t->op() != TYPE_INT) && (t->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "invalid result type");
	    }
	    if (!t->compatible(ri->src_op().type())) {
		problem_instr(tn, i, "source operand does not match result "
                                     "type");
	    }

	    t = ri->shift_cnt_op().type()->unqual();
	    if ((t->op() == TYPE_INT) || (t->op() == TYPE_ENUM)) {
		base_type *bt = (base_type *)t;
		if (!bt->is_signed()) {
		    problem_instr(tn, i, "rotation amount type is not signed");
		}
	    } else {
		problem_instr(tn, i, "invalid type for rotation amount");
	    }
	    break;
	}

	case io_seq:
	case io_sne:
	case io_sl:
	case io_sle: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    if (!t->compatible(type_signed)) {
		problem_instr(tn, i, "result type of comparison is not "
                                     "signed INT");
	    }
	    t = ri->src1_op().type()->unqual();
	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_FLOAT) &&
		(t->op() != TYPE_PTR) &&
		(t->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "invalid source operand type");
	    } else if (!t->compatible(ri->src1_op().type())) {
		problem_instr(tn, i, "source operand types do not match");
	    }
	    break;
	}

	case io_btrue:
	case io_bfalse:
	case io_jmp: {
	    in_bj *bji = (in_bj *)i;
	    check_null_dst(i, tn);
	    if (i->opcode() == io_jmp) {
		check_null_src(i, bji->src_op(), tn);
	    } else if (!type_signed->compatible(bji->src_op().type())) {
		problem_instr(tn, i, "branch condition must be ``int''");
	    }
	    break;
	}

	case io_cvt: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_ENUM) &&
		(t->op() != TYPE_FLOAT) &&
		(t->op() != TYPE_PTR)) {
		problem_instr(tn, i, "invalid result type");
	    }
	    if (ri->src_op().type() == 0) {
	      problem_instr(tn, i, "invalid result type == 0");
	      break;
	    }
	    t = ri->src_op().type()->unqual();
	    if ((t->op() != TYPE_INT) &&
		(t->op() != TYPE_ENUM) &&
		(t->op() != TYPE_FLOAT) &&
		(t->op() != TYPE_PTR)) {
		problem_instr(tn, i, "invalid source operand type");
	    }
	    /* check the "fields" annotation (if there is one) */
	    if (t->is_ptr()) {
		check_fields(tn, i, ((ptr_type *)t)->ref_type()->unqual());
	    }
	    check_null_src(i, ri->src2_op(), tn);
	    break;
	}

	case io_ldc: {
	    in_ldc *ldci = (in_ldc *)i;
	    type_node *t = i->result_type()->unqual();
	    immed v = ldci->value();
	    boolean bad_type = FALSE;
	    switch (v.kind()) {
		case im_int:
		case im_extended_int: {
		    /* allow integers to be treated as pointers (e.g. NULL) */
		    if ((t->op() != TYPE_INT) &&
			(t->op() != TYPE_ENUM) &&
			(t->op() != TYPE_PTR)) {
			bad_type = TRUE;
		    }
		    break;
		}
		case im_float:
		case im_extended_float: {
		    if (t->op() != TYPE_FLOAT) {
			bad_type = TRUE;
		    }
		    break;
		}
		case im_symbol: {
		    if (t->op() != TYPE_PTR) {
			bad_type = TRUE;
		    }
		    break;
		}
		default: {
		    problem_instr(tn, i, "illegal constant value");
		}
	    }
	    if (bad_type) {
		problem_instr(tn, i, "value does not match result type");
	    }
	    break;
	}

	case io_lod: {
	    in_rrr *ri = (in_rrr *)i;
	    type_node *t = i->result_type()->unqual();
	    type_node *st = ri->src_addr_op().type()->unqual();
	    if (st->op() != TYPE_PTR) {
		problem_instr(tn, i, "source address is not a pointer");
	    } else {
		ptr_type *pt = (ptr_type *)st;
		if (!t->compatible(pt->ref_type())) {
		    problem_instr(tn, i, "incorrect return type");
		} else if ((t->op() == TYPE_VOID) ||
			   (t->op() == TYPE_FUNC) ||
			   (t->size() == 0)) {
		    problem_instr(tn, i, "illegal type");
		}
	    }
	    check_null_src(i, ri->src2_op(), tn);
	    break;
	}

	case io_str: {
	    in_rrr *ri = (in_rrr *)i;
	    check_null_dst(i, tn);
	    type_node *dt = ri->dst_addr_op().type()->unqual();
	    if (dt->op() != TYPE_PTR) {
		problem_instr(tn, i, "destination address is not a pointer");
	    } else {
		ptr_type *pt = (ptr_type *)dt;
		type_node *t = pt->ref_type()->unqual();
		if (!t->compatible(ri->src_op().type())) {
		    problem_instr(tn, i, "incorrect source type");
		} else if ((t->op() == TYPE_VOID) ||
			   (t->op() == TYPE_FUNC) ||
			   (t->op() == TYPE_ARRAY)) {
		    problem_instr(tn, i, "illegal type");
		}
	    }
	    break;
	}

	case io_memcpy: {
	    in_rrr *ri = (in_rrr *)i;
	    check_null_dst(i, tn);
	    type_node *st = ri->src_addr_op().type()->unqual();
	    type_node *dt = ri->dst_addr_op().type()->unqual();
	    boolean bad_ptr = FALSE;
	    if (dt->op() != TYPE_PTR) {
		problem_instr(tn, i, "destination address is not a pointer");
		bad_ptr = TRUE;
	    }
	    if (st->op() != TYPE_PTR) {
		problem_instr(tn, i, "source address is not a pointer");
		bad_ptr = TRUE;
	    }
	    if (!bad_ptr) {
		ptr_type *spt = (ptr_type *)st;
		ptr_type *dpt = (ptr_type *)dt;
		type_node *t = spt->ref_type()->unqual();

		if (!t->compatible(dpt->ref_type())) {
		    problem_instr(tn, i, "pointer types do not match");
		} else if ((t->op() == TYPE_VOID) ||
			   (t->op() == TYPE_FUNC) ||
			   (t->op() == TYPE_ARRAY)) {
		    problem_instr(tn, i, "illegal type");
		}
	    }
	    break;
	}

	case io_cal: {
	    in_cal *cali = (in_cal *)i;
	    type_node *t = cali->addr_op().type()->unqual();
	    func_type *ft = NULL;
	    if (t->op() == TYPE_PTR) {
		ptr_type *pt = (ptr_type *)t;
		type_node *rt = pt->ref_type()->unqual();
		if (rt->op() == TYPE_FUNC) {
		    ft = (func_type *)rt;
		}
	    }
	    if (!ft) {
		problem_instr(tn, i, "bad target address");
	    } else {
		if (!ft->return_type()->compatible(i->result_type())) {
		    problem_instr(tn, i, "result type does not match "
                                         "function declaration");
		}
		if (ft->return_type()->unqual()->is_array() ||
		    ft->return_type()->unqual()->is_func()) {
		    problem_instr(tn, i, "illegal return type for call");
		}
		if (ft->args_known()) {
		    unsigned numargs = ft->num_args();
		    if (cali->num_args() < numargs) {
			problem_instr(tn, i, "too few arguments");
			numargs = cali->num_args();
		    } else if ((cali->num_args() > numargs) &&
			       !ft->has_varargs()) {
			problem_instr(tn, i, "too many arguments");
		    }
		    for (unsigned n = 0; n < numargs; n++) {
			if (!ft->arg_type(n)->compatible
			    (cali->argument(n).type())) {
			    problem_instr(tn, i, "argument does not match "
                                                 "function declaration");
			}
		    }
		}
	    }
	    break;
	}

	case io_array: {
	    in_array *arri = (in_array *)i;

	    /* the result type may be a pointer of any sort */
	    if (arri->result_type()->unqual()->op() != TYPE_PTR) {
		problem_instr(tn, i, "result type is not a pointer");
	    }

	    /* check the type of the offset operand */
	    type_node *toff = arri->offset_op().type()->unqual();
	    if ((toff->op() != TYPE_VOID) &&
		(toff->op() != TYPE_INT) &&
		(toff->op() != TYPE_ENUM)) {
		problem_instr(tn, i, "invalid type for offset operand");
	    }

	    /* check the base operand type */
	    type_node *tbase = arri->base_op().type()->unqual();
	    if (tbase->op() == TYPE_PTR) {
		ptr_type *pt = (ptr_type *)tbase;
		tbase = pt->ref_type()->unqual();
	    } else {
		problem_instr(tn, i, "invalid type for base operand");
		tbase = NULL;
	    }

	    /* check each dimension */
	    for (unsigned d = 0; d < arri->dims(); d++) {
		array_type *at = (array_type *)tbase;
		if (at && (at->op() != TYPE_ARRAY)) {
		    problem_instr(tn, i, "array type does not match");
		    at = NULL;
		}
		/* check the index type */
		type_node *tind = arri->index(d).type()->unqual();
		if ((tind->op() != TYPE_INT) &&
		    (tind->op() != TYPE_ENUM)) {
		    problem_instr(tn, i, "incorrect type for index %u", d);
		}

		operand b = arri->bound(d);
		if (b.is_null()) {
		    if (d != 0) {
			problem_instr(tn, i, "missing bound %u", d);
		    }
		} else {
		    /* check the bound type */
		    type_node *tbnd = b.type()->unqual();
		    if ((tbnd->op() != TYPE_INT) &&
			(tbnd->op() != TYPE_ENUM)) {
			problem_instr(tn, i, "incorrect type for bound %u", d);
		    }
		    if (at) {
			/* compare values of bounds */
			if (at->lower_bound().is_constant() &&
			    at->upper_bound().is_constant()) {

			    int bndval = 1 + at->upper_bound().constant() -
				at->lower_bound().constant();

			    if (b.is_instr() &&
				(b.instr()->opcode() == io_ldc)) {
				in_ldc *ldci = (in_ldc *)b.instr();
				if (ldci->value().is_integer() &&
				    (ldci->value().integer() != bndval)) {
				    problem_instr(tn, i, "bound %u does not "
						  "match type", d);
				}
			    }
			}
		    }
		}

		/* get the next element type */
		if (at) {
		    tbase = at->elem_type()->unqual();
		} else {
		    tbase = NULL;
		}
	    }

	    /* give up now if we don't know the element type */
	    if (!tbase) break;

	    /* check that the element size is correct */
	    if (arri->elem_size() != (unsigned)tbase->size()) {
		problem_instr(tn, i, "element size does not match array type");
	    }

	    /* check the "fields" annotation (if there is one) */
	    check_fields(tn, i, tbase);

	    break;
	}

	default: {
	    problem_instr(tn, i, "unexpected opcode: %s",
                          if_ops_name(i->opcode()));
	}
    }
}


void
check_fields (tree_node *tn, instruction *i, type_node *tbase)
{
    /* try to figure out the field (if any) */
    annote *flds = NULL;
    if (i->are_annotations()) {
	flds = i->annotes()->peek_annote(k_fields);
    }
    if (flds) {
	immed_list_iter ili(flds->immeds());
	while (!ili.is_empty()) {
	    const char *fld = ili.step().string();
	    if (!tbase->is_struct()) {
		problem_instr(tn, i, "fields annotation doesn't match type");
		break;
	    }
	    struct_type *stbase = (struct_type *)tbase;
	    unsigned fldnum = stbase->find_field_by_name(fld);
	    if (fldnum >= stbase->num_fields()) {
		problem_instr(tn, i, "cannot find field '%s'", fld);
		break;
	    }
	    tbase = stbase->field_type(fldnum)->unqual();
	}
    }
}


void
check_null_dst (instruction *i, tree_node *tn)
{
    if (!i->dst_op().is_null()) {
	problem_instr(tn, i, "unexpected destination operand");
    }
    if (!i->result_type()->op() == TYPE_VOID) {
	problem_instr(tn, i, "result type should be void");
    }
}


void
check_null_src (instruction *i, operand r, tree_node *tn)
{
    if (!r.is_null()) {
	problem_instr(tn, i, "unused source operand is not null");
    }
    if (!i->result_type()->op() == TYPE_VOID) {
	problem_instr(tn, i, "non-void result type");
    }
}



