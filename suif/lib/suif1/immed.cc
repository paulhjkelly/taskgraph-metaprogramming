/*  Immediate Constants Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "immed.h"

#define RCS_BASE_FILE immed_cc

#include "suif1.h"
#include <climits>

RCS_BASE(
    "$Id$")


static const char *double_to_string(double the_double);
static const char *int_to_string(int the_int);
static const char *unsigned_to_string(unsigned the_unsigned);
static const char *long_to_string(long the_long);
static const char *unsigned_long_to_string(unsigned long the_unsigned_long);


/*
 *  The following constructors take various sizes of C++ integer and
 *  form SUIF immeds with the given integer value, using im_int or
 *  im_extended_int if necessary to represent the value.
 */

immed::immed(unsigned the_unsigned)
{
    if (the_unsigned <= (unsigned)INT_MAX)
      {
        knd = im_int;
        v.ival = (int)the_unsigned;
      }
    else
      {
        knd = im_extended_int;
        v.sval = unsigned_to_string(the_unsigned);
      }
}


immed::immed(long the_long)
{
    if ((the_long >= (long)INT_MIN) && (the_long <= (long)INT_MAX))
      {
        knd = im_int;
        v.ival = (int)the_long;
      }
    else
      {
        knd = im_extended_int;
        v.sval = long_to_string(the_long);
      }
}


immed::immed(unsigned long the_unsigned_long)
{
    if (the_unsigned_long <= (unsigned long)INT_MAX)
      {
        knd = im_int;
        v.ival = (int)the_unsigned_long;
      }
    else
      {
        knd = im_extended_int;
        v.sval = unsigned_long_to_string(the_unsigned_long);
      }
}


immed::immed(sym_node *s, int of)
{
    if (s == NULL)
      {
        knd = im_undef;
      }
    else
      {
        knd = im_symbol;
        set_y(sym_addr(s, of));
      }
}


/*
 *  The following three functions test to see if the immediate value
 *  is an integer that fits in an ``unsigned'', ``long'', or
 *  ``unsigned long'' C++ type on the machine on which SUIF is
 *  running.
 */

boolean
immed::is_unsigned_int(void) const
{
    switch (kind())
      {
        case im_int:
          {
            return (v.ival >= 0);
          }
        case im_extended_int:
          {
            errno = 0;
            unsigned long unsigned_long_value = strtoul(v.sval, NULL, 10);
            if (errno != 0)
                return FALSE;
            return (unsigned_long_value <= (unsigned long)UINT_MAX);
          }
        default:
            return FALSE;
      }
}


boolean
immed::is_long_int(void) const
{
    switch (kind())
      {
        case im_int:
          {
            return TRUE;
          }
        case im_extended_int:
          {
            errno = 0;
            (void)strtol(v.sval, NULL, 10);
            return (errno == 0);
          }
        default:
            return FALSE;
      }
}


boolean
immed::is_unsigned_long(void) const
{
    switch (kind())
      {
        case im_int:
          {
            return (v.ival >= 0);
          }
        case im_extended_int:
          {
            errno = 0;
            (void)strtoul(v.sval, NULL, 10);
            return (errno == 0);
          }
        default:
            return FALSE;
      }
}


/*
 *  Check if two immeds are equal.
 */

boolean
immed::operator== (const immed &r) const
{
    if (kind() != r.kind())
	return FALSE;

    switch (kind()) {
	case im_int:		{ return (integer() == r.integer()); }
	case im_extended_int:	{ return (ext_integer() == r.ext_integer()); }
	case im_string:		{ return (string() == r.string()); }
	case im_symbol:		{ return (addr() == r.addr()); }
	case im_float:		{ return (flt() == r.flt()); }
	case im_extended_float:	{ return (ext_flt() == r.ext_flt()); }
	case im_type:		{ return (type() == r.type()); }
	case im_op:		{ return (op() == r.op()); }
	case im_instr:		{ return (instr() == r.instr()); }
	case im_undef:		{ return FALSE; }
	default: {
	    assert_msg(FALSE, ("immed::operator== - unhandled case"));
	}
    }
    return FALSE;
}


/*
 *  Print an immed value.
 */

void
immed::print (FILE *fp)
{
    switch (kind()) {
	case im_int: {
	    fprintf(fp, "%d", integer());
	    break;
	}
	case im_extended_int: {
	    fprintf(fp, "%s", ext_integer());
	    break;
	}
	case im_string: {
	    putc('"', fp);
	    const char *p = string();
	    assert(p);

	    /* print backslashes before '\\' and '"' characters */
	    int c;
	    while ((c = *p++)) {
		if (c == '\\' || c == '"')
		    putc('\\', fp);
		putc(c, fp);
	    }

	    putc('"', fp);
	    break;
	}
	case im_float: {
	    fprintf(fp, "%.18e", flt());
	    break;
	}
	case im_extended_float: {
	    fprintf(fp, "%s", ext_flt());
	    break;
	}
	case im_symbol: {
	    addr().print(fp);
	    break;
	}
	case im_type: {
	    type()->print(fp);
	    break;
	}
	case im_op: {
	    op().print_source(fp);
	    break;
	}
	case im_instr: {
	    instr()->print(fp);
	    break;
	}
	case im_undef: {
	    fputs("undef", fp);
	    break;
	}
	default: {
	    assert_msg(FALSE, ("immed::print - unhandled case"));
	}
    }
}


/*
 *  Same as print except it doesn't escape " and \ in string printing.
 */

void
immed::rawprint (FILE *fp)
{
    if (kind() != im_string) {
	print(fp);
    } else {
	putc('"', fp);
	const char *p = string();
	assert(p);
	fputs(p, fp);
	putc('"', fp);
    }
}


/*
 *  Immed accessor methods.  Each one checks to make sure that the immed
 *  is the correct kind before returning the value.
 */

int
immed::integer () const
{
    assert_msg(kind() == im_int, ("immed::integer - type is %c", kind()));
    return v.ival;
}


unsigned
immed::unsigned_int () const
{
    switch (kind())
      {
        case im_int:
          {
            assert(v.ival >= 0);
            return (unsigned)(v.ival);
          }
        case im_extended_int:
          {
            errno = 0;
            unsigned long unsigned_long_value = strtoul(v.sval, NULL, 10);
            assert(errno == 0);
            assert(unsigned_long_value <= (unsigned long)UINT_MAX);
            return (unsigned)unsigned_long_value;
          }
        default:
            assert(FALSE);
            return 0u;
      }
}


long
immed::long_int () const
{
    switch (kind())
      {
        case im_int:
          {
            return (long)(v.ival);
          }
        case im_extended_int:
          {
            errno = 0;
            long long_value = strtol(v.sval, NULL, 10);
            assert(errno == 0);
            return long_value;
          }
        default:
            assert(FALSE);
            return 0l;
      }
}


unsigned long
immed::unsigned_long () const
{
    switch (kind())
      {
        case im_int:
          {
            assert(v.ival >= 0);
            return (unsigned long)(v.ival);
          }
        case im_extended_int:
          {
            errno = 0;
            unsigned long unsigned_long_value = strtoul(v.sval, NULL, 10);
            assert(errno == 0);
            return unsigned_long_value;
          }
        default:
            assert(FALSE);
            return 0ul;
      }
}


const char *
immed::ext_integer () const
{
    assert_msg(kind() == im_extended_int,
               ("immed::ext_integer - type is %c", kind()));
    return v.sval;
}


const char *
immed::string () const
{
    assert_msg(kind() == im_string, ("immed::string - type is %c", kind()));
    return v.sval;
}


double
immed::flt () const
{
    assert_msg(kind() == im_float, ("immed::flt - type is %c", kind()));
    return v.fval;
}


const char *
immed::ext_flt () const
{
    assert_msg(kind() == im_extended_float,
               ("immed::ext_flt - type is %c", kind()));
    return v.sval;
}


double
immed::force_double () const
{
    assert_msg((kind() == im_float) || (kind() == im_extended_float),
               ("immed::force_double - type is %c", kind()));
    if (kind() == im_float)
        return v.fval;
    else
        return atof(v.sval);
}


sym_node *
immed::symbol () const
{
    if (kind() == im_undef)
	return NULL;
    assert_msg(kind() == im_symbol, ("immed::symbol - type is %c", kind()));
    return get_y().symbol();
}


int
immed::offset () const
{
    assert_msg(kind() == im_symbol, ("immed::offset - type is %c", kind()));
    return get_y().offset();
}


sym_addr
immed::addr () const
{
    assert_msg(kind() == im_symbol, ("immed::addr - type is %c", kind()));
    return get_y();
}


type_node *
immed::type () const
{
    assert_msg(kind() == im_type, ("immed::type - type is %c", kind()));
    return v.hval;
}


operand
immed::op () const
{
    assert_msg(kind() == im_op, ("immed::op - type is %c", kind()));
    return *(operand *)&(v.opval);
}


instruction *
immed::instr () const
{
    assert_msg(kind() == im_instr, ("immed::instr - type is %c", kind()));
    return v.instrval;
}


/*
 *  Write an immed.
 */

void
immed::write (out_stream *os)
{
    /* write the immed kind */
    if (kind() == im_extended_float)
	os->write_byte((char)im_float);
    else
	os->write_byte((char)kind());

    switch (kind()) {
	case im_int: {
	    os->write_int(integer());
	    return;
	}
	case im_extended_int: {
	    os->write_cached(ext_integer());
	    return;
	}
	case im_string: {
	    os->write_cached(string());
	    return;
	}
	case im_float: {
	    os->write_cached(double_to_string(flt()));
	    return;
	}
	case im_extended_float: {
	    os->write_cached(ext_flt());
	    return;
	}
	case im_symbol: {
	    addr().write(os);
	    return;
	}
	case im_type: {
	    type()->write(os);
	    return;
	}
	case im_op: {
	    immed temp_immed;
#ifndef MONTANA_BRAIN_DAMAGE
	    switch (op().kind()) {
#else
	    operand the_op = op();
	    switch (the_op.kind()) {
#endif
		case OPER_NULL:
		    temp_immed = immed();
		    break;
		case OPER_SYM:
		    temp_immed = immed(op().symbol());
		    break;
		case OPER_INSTR:
		    temp_immed = immed(op().instr());
		    break;
		case OPER_REG:
		    temp_immed = immed(op().reg());
		    break;
	    }
	    temp_immed.write(os);
	    if (op().kind() == OPER_REG) op().type()->write(os);
	    return;
	}
	case im_instr: {
	    tree_node_list *temp_list = new tree_node_list;
	    tree_instr *temp_tree_instr = new tree_instr(instr());
	    temp_list->append(temp_tree_instr);
	    temp_list->flatten();
	    instr_list_number(temp_list);
	    temp_list->write(os);
	    temp_list->cvt_to_trees();
	    temp_tree_instr->remove_instr(instr());
	    delete temp_list;
	    return;
	}
	case im_undef: {
	    return;
	}
    }
    assert_msg(FALSE, ("immed::write - unknown immed kind"));
}


/*
 *  Read an immed.
 */

immed::immed (in_stream *is, base_symtab *symtab)
{
    knd = (immed_kinds)is->read_byte();

    switch (knd) {
	case im_int: {
	    boolean overflow = FALSE;
	    char *decimal_value;
	    int int_value = is->read_int(&overflow, &decimal_value);
	    if (overflow) {
		knd = im_extended_int;
		v.sval = lexicon->enter(decimal_value)->sp;
	    } else {
		knd = im_int;
		v.ival = int_value;
	    }
	    return;
	}
	case im_extended_int: {
	    const char *string = is->read_cached();
	    int int_value = atoi(string);
	    if (int_to_string(int_value) == string) {
		knd = im_int;
		v.ival = int_value;
	    } else {
		knd = im_extended_int;
		v.sval = string;
	    }
	    return;
	}
	case im_string: {
	    v.sval = is->read_cached();
	    return;
	}
	case im_float: {
	    const char *string = is->read_cached();
	    double double_value = atof(string);
	    if (double_to_string(double_value) == string) {
		knd = im_float;
		v.fval = double_value;
	    } else {
		knd = im_extended_float;
		v.sval = string;
	    }
	    return;
	}
	case im_extended_float: {
	    assert(FALSE);
	    return;
	}
	case im_symbol: {
	    set_y(sym_addr(is, symtab));
	    return;
	}
	case im_type: {
	    v.hval = type_node::read(is, symtab);
	    return;
	}
	case im_op: {
	    immed temp_immed = immed(is, symtab);
	    switch (temp_immed.kind()) {
		case im_undef:
		    v.opval = operand();
		    break;
		case im_symbol: {
		    sym_node *temp_sym = temp_immed.symbol();
		    assert(temp_sym->is_var());
		    v.opval = operand((var_sym *)temp_sym);
		    break;
		}
		case im_instr:
		    v.opval = operand(temp_immed.instr());
		    break;
		case im_int: {
		    type_node *temp_type = type_node::read(is, symtab);
		    v.opval = operand(temp_immed.integer(), temp_type);
		    break;
		}
		default:
		    assert_msg(FALSE, ("immed::immed - illegal kind in op"));
	    }
	    return;
	}
	case im_instr: {
	    tree_node_list *temp_list =
		    new tree_node_list(is, NULL, (block_symtab *)symtab);
	    temp_list->cvt_to_trees();
	    assert(temp_list->count() == 1);
	    tree_node *head_node = temp_list->head()->contents;
	    assert(head_node->is_instr());
	    tree_instr *temp_tree_instr = (tree_instr *)head_node;
	    v.instrval = temp_tree_instr->instr();
	    temp_tree_instr->remove_instr(instr());
	    delete temp_list;
	    return;
	}
	case im_undef: {
	    return;
	}
    }
}


/*
 *  Clone an immed value.  Symbols and types may need to be translated
 *  if they have been replaced.  Other kinds of immeds are simply copied.
 */

immed
immed::clone_helper (replacements *r, boolean no_copy)
{
    if (is_symbol()) {
	return immed(symbol()->clone_helper(r), offset());
    } else if (is_type()) {
	return immed(type()->clone_helper(r));
    } else if (is_op()) {
	return immed(op().clone_helper(r, no_copy));
    } else if (is_instr()) {
	return immed(instr()->clone_helper(r, no_copy));
    }

    return *this;
}


/*
 *  Check if an immed value is an exposed reference.  Symbols and types
 *  may not be visible in the destination scope.
 */

void
immed::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    if (is_symbol()) {
	r->add_sym_ref(symbol(), dst_scope);
    } else if (is_type()) {
	r->add_type_ref(type(), dst_scope);
    } else if (is_op()) {
	op().find_exposed_refs(dst_scope, r);
    } else if (is_instr()) {
	instr()->find_exposed_refs(dst_scope, r);
    }
}


void
immed::instr_list_number(tree_node_list *the_list)
{
    int number = 1;

    tree_node_list_iter the_iter(the_list);
    while (!the_iter.is_empty()) {
	tree_node *this_node = the_iter.step();
	assert(this_node->is_instr());
	tree_instr *this_tree_instr = (tree_instr *)this_node;
	this_tree_instr->instr()->set_number(number);
	++number;
    }
}


immed_list::~immed_list ()
{
    while (!is_empty()) {
	immed this_immed = pop();
	if (this_immed.is_op() && this_immed.op().is_expr())
	    delete this_immed.op().instr();
	else if (this_immed.is_instr())
	    delete this_immed.instr();
    }
}


immed_list *
immed_list::clone(base_symtab *dst_scope)
{
    replacements r;
    find_exposed_refs(dst_scope, &r);
    r.resolve_exposed_refs(dst_scope);
    return clone_helper(&r);
}


immed_list *
immed_list::clone_helper(replacements *r, boolean no_copy)
{
    immed_list *result;
    if (no_copy)
	result = this;
    else
	result = new immed_list;
    immed_list_e *follow_e = head();
    while (follow_e != NULL) {
	immed old_immed = follow_e->contents;
	immed new_immed = old_immed.clone_helper(r, no_copy);
	if (no_copy)
	    follow_e->contents = new_immed;
	else
	    result->append(new_immed);
	follow_e = follow_e->next();
    }
    return result;
}


void
immed_list::find_exposed_refs(base_symtab *dst_scope, replacements *r)
{
    immed_list_iter the_iter(this);
    while (!the_iter.is_empty()) {
	immed this_immed = the_iter.step();
	this_immed.find_exposed_refs(dst_scope, r);
    }
}


static const char *double_to_string(double the_double)
{
    char bfr[128];
    char fmt[16];
    sprintf(fmt, "%%.%de", (int)(sizeof(double) * 2.4));
    sprintf(bfr, fmt, the_double);
    return lexicon->enter(bfr)->sp;
}

static const char *int_to_string(int the_int)
{
    char bfr[CHAR_BIT * sizeof(int) + 3];
    sprintf(bfr, "%d", the_int);
    return lexicon->enter(bfr)->sp;
}

static const char *unsigned_to_string(unsigned the_unsigned)
{
    char bfr[CHAR_BIT * sizeof(unsigned) + 3];
    sprintf(bfr, "%u", the_unsigned);
    return lexicon->enter(bfr)->sp;
}

static const char *long_to_string(long the_long)
{
    char bfr[CHAR_BIT * sizeof(long) + 3];
    sprintf(bfr, "%ld", the_long);
    return lexicon->enter(bfr)->sp;
}

static const char *unsigned_long_to_string(unsigned long the_unsigned_long)
{
    char bfr[CHAR_BIT * sizeof(unsigned long) + 3];
    sprintf(bfr, "%lu", the_unsigned_long);
    return lexicon->enter(bfr)->sp;
}
