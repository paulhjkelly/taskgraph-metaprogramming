/*  SUIF Operand Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "operand.h"

#define RCS_BASE_FILE operand_cc

#include "suif1.h"

RCS_BASE(
    "$Id$")


operand::~operand ()
{
    /* Don't try to delete expression trees here -- things break all over
       the place.  It's very useful to use temporary operands when
       manipulating an expression tree, and when those temporaries go out
       of scope, any instructions in them will be deleted.  It would be
       extremely burdensome to require the users to set all of those
       operands to NULL.  Instead, the expression trees are deleted by
       the tree_instrs that contain them. */
}


operand_kinds
operand_dataonly::kind() const
{
    if (v.k >= OPER_REG) return OPER_REG;
    return v.k;
}


var_sym *
operand_dataonly::symbol () const
{
    assert_msg(is_symbol(), ("operand::sym - wrong kind"));
    return u.sym;
}


instruction *
operand_dataonly::instr () const
{
    assert_msg(is_instr(), ("operand::instr - wrong kind"));
    return u.i;
}


int
operand_dataonly::reg () const
{
    assert_msg(is_reg(), ("operand::reg - wrong kind"));
    return u.r;
}


immed
operand_dataonly::immediate () const
{
    assert_msg(is_immed(), ("operand::immed - wrong kind"));
    return ((in_ldc *)instr())->value();
}


/*
 *  Check if an operand is an instruction pointer and if that instruction
 *  is a subexpression that is not attached to a separate tree_instr.
 *  This should only be used with source operands.
 */

boolean
operand_dataonly::is_expr () const
{
    if (is_instr()) {

	/* consider "unattached" instructions to be expressions */
	if (!instr()->parent()) return TRUE;

	/* if the instruction is not part of an expression tree, it will
	   be the immediate child of its parent tree_instr */
	return (instr()->parent()->instr() != instr());
    }
    return FALSE;
}


/*
 * Check if operand is a simple immediate value.  These values are
 * kept as ldc instructions (to maintain type information).
 */

boolean
operand_dataonly::is_immed () const
{
    if (is_instr()) {
        if (instr()->opcode() == io_ldc) return TRUE;
    }
    return FALSE;
}


/*
 *  Write an operand to a file.  Symbols are written using their ID numbers
 *  and instructions are written using their instruction numbers.
 */

void
operand::write (out_stream *os) const
{
    os->write_byte((char)kind());
    if (is_symbol()) {
	symbol()->write(os);
    } else if (is_instr()) {
	assert_msg(instr()->number() != 0,
		   ("operand::write - instruction not numbered"));
	os->write_int(instr()->number());
    } else if (is_reg()) {
        os->write_int(reg());
        type()->write(os);
    } else {
	assert_msg(is_null(), ("operand::write - unknown operand kind"));
    }
}


/*
 *  Read an operand from a file.  For an instruction operand, we have to
 *  search back through the list to find the instruction number.  Note: The
 *  _suif_raw_syms flag is checked here to allow reading references to
 *  instructions without checking if they actually exist.  This is done by
 *  the "printsuif" program to assist with debugging the SUIF I/O functions.
 */

operand::operand (in_stream *is, base_symtab *syms, tree_node *t)
{
    v.k = (operand_kinds)is->read_byte();

    switch (v.k) {

	case OPER_NULL: {
	    u.sym = NULL; /* to avoid ``uninitialized mem read'' warnings */
	    break;
	}

	case OPER_SYM: {
	    u.sym = (var_sym *)sym_node::read(is, syms);
	    assert_msg(u.sym->is_var(),
		       ("operand::operand - symbol (`%s') is not a variable",
			u.sym->name()));
	    break;
	}

	case OPER_INSTR: {
	    unsigned inum = (unsigned)(is->read_int());

	    /* check for debugging mode */
	    if (_suif_raw_syms) {
		/* create a dummy instruction */
		instruction *i = new in_rrr(io_nop);
		i->set_number(inum);
		/* need to create a tree_instr to hold the dummy */
		tree_instr *ti = new tree_instr(i);
		u.i = ti->instr();
		break;
	    }

	    /* search backwards for the instruction number */
	    assert_msg(t && t->is_instr(),
		       ("operand::operand - no previous instructions"));

	    while (TRUE) {
		instruction *i = ((tree_instr *)t)->instr();
		if (i->number() == inum) {
		    u.i = i;
		    break;
		}
		tree_node_list_e *prev = t->list_e()->prev();
		assert_msg(prev && prev->contents->is_instr(),
			   ("operand::operand - cannot find node %u "
			    "definition", inum));
		t = prev->contents;
	    }
	    break;
	}

        case OPER_REG: {
            u.r = is->read_int();
            v.typ = type_node::read(is, syms);
            break;
        }

	default: {
	    assert_msg(FALSE, ("operand::operand - unexpected kind"));
	}
    }
}


/*
 *  Print an operand.  The instruction parameter is needed to tell whether
 *  the operand is used as a source or a destination.  This method is used
 *  within the library when printing instructions.
 */

void
operand::print (instruction *i, FILE *f) const
{
    switch (kind()) {
	case OPER_NULL: {
	    fprintf(f, "<nullop>");
	    break;
	}
	case OPER_SYM: {
	    symbol()->print(f);
	    break;
	}
	case OPER_INSTR: {
	    unsigned node;
	    if (i && (*this == i->dst_op())) {
		node = i->number();
	    } else {
		node = instr()->number();
	    }
	    fprintf(f, "nd#%u", node);
	    break;
	}
        case OPER_REG: {
            fprintf(f, "r%d", reg());
            break;
        }
	default: {
	    assert_msg(FALSE, ("operand::print - unknown kind %d", v.k));
	}
    }
}


/*
 *  Print a source operand.  This is currently intended primarily for
 *  debugging.  It may produce strange results if called on an operand
 *  that is used as a destination.
 */

void
operand::print (FILE *f) const
{
    if (is_expr()) {
	instr()->print(f);
    } else {
	print(NULL, f);
    }
}


boolean
operand_dataonly::operator== (const operand_dataonly &r) const
{
    if (is_null() && r.is_null()) return TRUE;
    if (is_symbol() && r.is_symbol()) return (symbol() == r.symbol());
    if (is_instr() && r.is_instr()) return (instr() == r.instr());
    if (is_reg() && r.is_reg()) return (reg() == r.reg());
    return FALSE;
}


/*
 *  If the operand is a symbol or an instruction, return its type.
 *  Return "type_void" for null operands.
 */

type_node *
operand::type () const
{
    if (is_symbol()) return symbol()->type()->unqual();

    if (is_instr()) {
	instruction *i = instr();
	return i->result_type();
    }
    if (is_reg()) return v.typ;

    if (is_reg()) return v.typ;

    assert(is_null());
    return type_void;
}


/*
 *  Check if an operand is a constant integer, and if so, return the
 *  value in the "c" parameter.  If the parameter is NULL, the value is
 *  not returned.
 */

boolean
operand::is_const_int (int *c) const
{
    if (!is_instr()) return FALSE;
    if (instr()->opcode() != io_ldc) return FALSE;

    immed v = ((in_ldc *)instr())->value();
    if (!v.is_integer()) return FALSE;

    if (c) *c = v.integer();
    return TRUE;
}


operand&
operand::operator=(const operand &r)
{
    v = r.v;
    u = r.u;
    return *this;
}


/*
 *  Remove an instruction operand.  Just a wrapper for convenience.
 */

void
operand::remove ()
{
    if (is_instr()) instr()->remove();
}


/*
 *  Clone a source operand.  References to variables are just copied.
 *  Operands that are instruction pointers may be cloned only if the
 *  instructions are in the same expression tree (not in a flat list).
 *  As a special case, the "dst_scope" parameter is allowed to be NULL
 *  here; this is equivalent to specifying that the clone will be in
 *  the same scope.  (There are some situations where the scope of the
 *  object being cloned is not yet known.)
 */

operand
operand::clone (base_symtab *dst_scope)
{
    replacements r;
    find_exposed_refs(dst_scope, &r);
    r.resolve_exposed_refs(dst_scope);
    if (is_instr()) {
	assert_msg(is_expr(), ("operand::clone - cannot clone an instruction "
			       "in a flat list"));
    }
    return clone_helper(&r);
}


/*
 *  Clone a source operand.  The only complication here is handling flat
 *  lists.  If the operand is an instruction that is contained in a separate
 *  tree_instr, then that instruction must have already been cloned and we
 *  need to use the replacement instead of the original.
 */

operand
operand::clone_helper (replacements *r, boolean no_copy)
{
    if (is_symbol()) {
	return operand((var_sym *)symbol()->clone_helper(r));

    } else if (is_expr()) {
	/* clone if it's in the same expression tree */
	return operand(instr()->clone_helper(r, no_copy));

    } else if (is_instr()) {

	/* find the replacement */
	instruction_list_iter old_inli(&r->oldinstrs);
	instruction_list_iter new_inli(&r->newinstrs);
	while (!old_inli.is_empty()) {
	    assert(!new_inli.is_empty());

	    instruction *old_in = old_inli.step();
	    instruction *new_in = new_inli.step();

	    if (old_in == instr()) {
		delete r->oldinstrs.remove(old_inli.cur_elem());
		delete r->newinstrs.remove(new_inli.cur_elem());
		return operand(new_in);
	    }
	}
	assert_msg(no_copy, ("operand::clone_helper - "
			     "cannot find instruction"));
    } else if (is_reg()) {
        return operand(reg(), type()->clone_helper(r));
    }

    return *this;
}


/*
 *  Find the exposed references in a source operand.  This will not work
 *  correctly for destination operands.  The code to check for references
 *  to instructions in a flat list requires that the tree_node_lists be
 *  traversed in reverse order.
 */

void
operand::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    if (is_symbol()) {
	r->add_sym_ref(symbol(), dst_scope);
    } else if (is_expr()) {
	instr()->find_exposed_refs(dst_scope, r);
    } else if (is_instr()) {
	r->add_instr_ref(instr());
    } else if (is_reg()) {
        r->add_type_ref(type(), dst_scope);
    }
}


