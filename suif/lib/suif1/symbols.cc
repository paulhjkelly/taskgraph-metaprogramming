/*  Symbol Class Implementations */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "symbols.h"

#define RCS_BASE_FILE symbols_cc

#include "suif1.h"
#include "suif_internal.h"

RCS_BASE(
    "$Id$")


sym_node::sym_node ()
{
    nm = NULL;
    table = NULL;
    id = 0;
    flags = 0;
    ext = TRUE;
}


void
sym_node::clear_sym_id ()
{
    if (parent() != NULL)
        parent()->register_sym_id_change(this, 0);
    set_sym_id(0);
}


boolean
sym_node::is_global ()
{
    if (parent() && parent()->is_global()) return TRUE;
    return FALSE;
}


boolean
sym_node::is_private ()
{
    if (parent() && parent()->is_file()) return TRUE;
    return FALSE;
}


void
sym_node::copy_flags (sym_node *sn)
{
    flags = sn->flags;
}


void
sym_node::set_name (const char *n)
{
    if (parent() != NULL)
        parent()->register_name_change(this, n);
    nm = n ? lexicon->enter(n)->sp : NULL;
}


void
sym_node::remove_from_table (void)
{
    parent()->remove_sym(this);
}


void
sym_node::add_to_table (base_symtab *new_table)
{
    new_table->add_sym(this);
}


/*
 *  Print the symbol name prefixed by the name of the parent symtab.  This
 *  virtual function is called directly for var_syms.  It is overridden in
 *  the label_sym and proc_sym classes to prepend "L:" or "P:", respectively.
 */

void
sym_node::print (FILE *f)
{
    const char *cn = parent()->chain_name();
    if (cn) {
	fprintf(f, "%s.%s", cn, name());
    } else {
	fprintf(f, "%s", name());
    }
}


/*
 *  Print out all the symbol information.  This is used for printing symtabs.
 *  This function handles the fields shared by all sym_nodes and the
 *  print_helper virtual function is used to print fields specific to the
 *  derived classes.
 */

void
sym_node::print_full (FILE *f, int depth)
{
    suif_indent(f, depth);

    /* print the ID number */
    fputs("s:", f);
    print_id_number(f, sym_id());

    fprintf(f, ": `%s' ", name());

    switch (kind()) {
	case SYM_VAR:		{ fputs("var_sym ", f); break; }
	case SYM_LABEL:		{ fputs("label_sym ", f); break; }
	case SYM_PROC:		{ fputs("proc_sym ", f); break; }
	default: {
	    assert_msg(FALSE, ("sym_node::print_full - invalid symbol kind"));
	}
    }

    /* print any fields specific to the symbol kind */
    print_helper(f);

    /* print the flags */
    if (is_extern()) fputs("extern ", f);
    if (is_userdef()) fputs("userdef ", f);

    print_annotes(f, depth+1);
    putc('\n', f);
}



sym_node *
sym_node::read (in_stream *is, base_symtab *symtab)
{
    unsigned i = is->read_int();
    sym_node *s = symtab->lookup_sym_id(i);
    assert_msg(s, ("sym_node::read - unknown sym_node id: %u", i));
    return s;
}


void
sym_node::write (out_stream *os)
{
    write_check();
    os->write_int(sym_id());
}


void
sym_node::write_check (void)
{
    assert(write_scope != NULL);
    assert_msg(write_scope->is_visible(this),
               ("attempt to write reference to symbol `%s' used outside its "
                "scope", name()));
    assert_msg(sym_id() != 0,
               ("attempt to write reference to symbol `%s' with no sym_id",
                name()));
}


/*
 *  Write the sym_node fields onto an annotation.  This function is called
 *  by the cvt_to_annote functions to append to the annotation the fields
 *  shared by all sym_nodes.
 */

void
sym_node::cvt_to_annote_base (annote *an)
{
    an->immeds()->append(immed(sym_id()));
    an->immeds()->append(immed(name()));
    an->immeds()->append(immed(flags));
}


/*
 *  Read a new sym_node from an annotation.  The annotation name determines
 *  the kind of symbol to create, and then the cvt_from_annote virtual
 *  function is used to read the sym_node fields.
 */

sym_node *
sym_node::scan_from_annote (annote *an, base_symtab *symtab)
{
    sym_node *result = NULL;

    if (an->name() == k_procsym) {
	assert_msg(symtab->is_global(),
		   ("Attempt to declare nested procedure"));
	result = new proc_sym();

    } else if (an->name() == k_varsym) {
	result = new var_sym();

    } else if (an->name() == k_labelsym) {
	assert_msg(!symtab->is_global(),
		   ("Attempt to declare label outside a procedure"));
	result = new label_sym();

    } else {
	assert_msg(result, ("scan_sym_from_annote: unknown symbol kind"));
    }

    assert_msg((*an->immeds())[0].is_unsigned_int(),
               ("scan_sym_from_annote: sym id too large for machine "
                "unsigned"));
    result->id = (*an->immeds())[0].unsigned_int();

    return result;
}


/*
 *  Read the common sym_node fields from an annotation.  This virtual function
 *  is only called by the derived cvt_from_annote functions.  The "symtab"
 *  parameter is not used here.
 */

void
sym_node::cvt_from_annote (annote *an, base_symtab * /* symtab */)
{
    /* pop the general sym_node fields from the annotation */
    id = an->immeds()->pop().unsigned_int();
    const char *new_name = an->immeds()->pop().string();
    if (parent() != NULL)
        parent()->register_name_change(this, new_name);
    nm = new_name;
    flags = an->immeds()->pop().integer();
}


/*
 *  Since symbols cannot be cloned by themselves, this is only called when
 *  cloning objects that refer to symbols.  If this symbol has been replaced,
 *  its replacement is returned.
 */

sym_node *
sym_node::clone_helper (replacements *r)
{
    /* check if this symbol has been replaced */
    sym_node_list_iter old_snli(&r->oldsyms);
    sym_node_list_iter new_snli(&r->newsyms);
    while (!old_snli.is_empty()) {
	assert(!new_snli.is_empty());

	sym_node *old_sn = old_snli.step();
	sym_node *new_sn = new_snli.step();

	/* return the replacement */
	if (old_sn == this) return new_sn;
    }

    return this;
}


/*****************************************************************************/


var_sym::var_sym (void)
{
    defd = FALSE;
    the_parent_var = NULL;
    the_offset = 0;
    children = NULL;
}


var_sym::var_sym (type_node *t, const char *n)
{
    set_name(n);
    typ = t;
    defd = FALSE;
    the_parent_var = NULL;
    the_offset = 0;
    children = NULL;
}


var_sym::~var_sym ()
{
    if (children != NULL)
        delete children;
}


sym_node *
var_sym::copy ()
{
    var_sym *result = new var_sym(type(), name());
    result->copy_flags(this);
    return result;
}


var_sym *
var_sym::root_ancestor (void)
{
    if (parent_var() == NULL)
	return this;
    else
	return parent_var()->root_ancestor();
}


int
var_sym::root_offset (void)
{
    if (parent_var() == NULL)
	return 0;
    else
	return offset() + parent_var()->root_offset();
}


boolean
var_sym::overlaps (var_sym *other_var)
{
    if (other_var == this)
        return TRUE;

    if (root_ancestor() != other_var->root_ancestor())
	return FALSE;

    int this_root_offset = root_offset();
    int that_root_offset = other_var->root_offset();
    if (this_root_offset <= that_root_offset)
      {
	return ((this_root_offset + type()->size()) > that_root_offset);
      }
    else
      {
	return ((that_root_offset + other_var->type()->size()) >
		this_root_offset);
      }
}


unsigned
var_sym::num_children (void)
{
    if (children == NULL)
	return 0u;
    else
	return (unsigned)(children->count());
}


var_sym *
var_sym::child_var (unsigned the_num)
{
    if (children == NULL)
	return NULL;
    else
	return (*children)[the_num];
}


var_sym *
var_sym::find_child (int child_offset, type_node *child_type)
{
    if (children == NULL)
        return NULL;

    var_sym_list_iter child_iter(children);
    while (!child_iter.is_empty())
      {
        var_sym *this_child = child_iter.step();
        if ((this_child->type() == child_type) &&
            (this_child->offset() == child_offset))
          {
            return this_child;
          }
      }

    return NULL;
}


var_sym *
var_sym::build_child (int child_offset, type_node *child_type,
                      const char *child_name)
{
    if (children == NULL)
        children = new var_sym_list;

    var_sym *new_var;
    if (parent() == NULL)
        new_var = new var_sym(child_type, child_name);
    else
        new_var = parent()->new_var(child_type, child_name);
    add_child(new_var, child_offset);
    return new_var;
}


boolean
var_sym::is_scalar ()
{
    return type()->is_scalar();
}


boolean
var_sym::is_static ()
{
    return (!is_reg()) && (!is_auto());
}


boolean
var_sym::is_spilled ()
{
    if (parent_var() != NULL)
        return parent_var()->is_spilled();
    else
        return is_addr_taken() || is_static() || !is_scalar();
}


boolean
var_sym::is_auto ()
{
    if (parent_var() != NULL)
        return parent_var()->is_auto();
    else
        return (!is_global()) && (!has_var_def());
}


boolean
var_sym::has_var_def ()
{
    return defd;
}


void
var_sym::set_param ()
{
    assert_msg(is_auto(), ("var_sym::set_param - variable '%s' is not auto",
			   name()));
    assert_msg(parent_var() == NULL,
               ("var_sym::set_param - sub-variable '%s' may not be a "
                "parameter", name()));
    flags |= VARSYM_ISPARAM;
}


void
var_sym::reset_param ()
{
    flags &= ~VARSYM_ISPARAM;
}


boolean
var_sym::is_addr_taken ()
{
    if (parent_var() != NULL)
        return parent_var()->is_addr_taken();
    else
        return (flags & VARSYM_ADDRTAKEN) ? TRUE : FALSE;
}


void
var_sym::set_addr_taken ()
{
    if (parent_var() != NULL)
      {
        parent_var()->set_addr_taken();
        return;
      }
    flags |= VARSYM_ADDRTAKEN;
}


void
var_sym::reset_addr_taken ()
{
    if (parent_var() != NULL)
      {
        parent_var()->reset_addr_taken();
        return;
      }
    flags &= ~VARSYM_ADDRTAKEN;
}


boolean
var_sym::is_reg ()
{
    if (parent_var() != NULL)
        return parent_var()->is_reg();
    else
        return (flags & VARSYM_ISREG) ? TRUE : FALSE;
}


void
var_sym::set_reg ()
{
    if (parent_var() != NULL)
      {
        parent_var()->set_reg();
        return;
      }
    flags |= VARSYM_ISREG;
}


void
var_sym::reset_reg ()
{
    if (parent_var() != NULL)
      {
        parent_var()->reset_reg();
        return;
      }
    if (!is_auto()) set_extern(TRUE);
    flags &= ~VARSYM_ISREG;
}


void
var_sym::add_child (var_sym *new_child, int child_offset)
{
    prepare_child_addition(new_child);
    new_child->the_offset = child_offset;
    if (children == NULL)
	children = new var_sym_list;
    children->append(new_child);
}


void
var_sym::remove_child (var_sym *old_child)
{
    prepare_child_removal(old_child);
    assert(children != NULL);
    var_sym_list_e *the_elem = children->lookup(old_child);
    assert(the_elem != NULL);
    children->remove(the_elem);
    delete the_elem;
}


void
var_sym::replace_child (unsigned child_num, var_sym *new_child)
{
    var_sym *old_child = child_var(child_num);
    int this_offset = old_child->offset();

    prepare_child_removal(old_child);
    prepare_child_addition(new_child);

    assert(children != NULL);
    var_sym_list_e *the_elem = children->lookup(old_child);
    assert(the_elem != NULL);
    the_elem->contents = new_child;
    new_child->the_offset = this_offset;
}


void
var_sym::remove_from_table (void)
{
    if ((parent_var() != NULL) && (parent_var()->parent() != NULL))
      {
        parent_var()->remove_from_table();
        return;
      }

    sym_node::remove_from_table();

    if (children != NULL)
      {
        var_sym_list_iter child_iter(children);
        while (!child_iter.is_empty())
          {
            var_sym *this_var = child_iter.step();
            this_var->remove_from_table();
          }
      }
}


void
var_sym::add_to_table (base_symtab *new_table)
{
    if ((parent_var() != NULL) && (parent_var()->parent() == NULL))
      {
        parent_var()->add_to_table(new_table);
        return;
      }

    sym_node::add_to_table(new_table);

    if (children != NULL)
      {
        var_sym_list_iter child_iter(children);
        while (!child_iter.is_empty())
          {
            var_sym *this_var = child_iter.step();
            this_var->add_to_table(new_table);
          }
      }
}


/*
 *  Print var_sym-specific fields.  This function is called by print_full
 *  to print the variable type and flags.
 */

void
var_sym::print_helper (FILE *f)
{
    fputs("with ", f);
    type()->print(f);
    if (parent_var() != NULL)
      {
	fprintf(f, " at offset %d in ", offset());
	parent_var()->print(f);
      }
    fputs("; ", f);

    if (has_var_def() && !is_global()) fputs("static ", f);
    if (is_param()) fputs("param ", f);
    if (is_addr_taken()) fputs("addr_taken ", f);
    if (is_reg()) fputs("reg ", f);
}


annote *
var_sym::cvt_to_annote ()
{
    annote *an = new annote(k_varsym);
    cvt_to_annote_base(an);
    if (write_scope != NULL)
	type()->write_check();
    an->immeds()->append(immed(type()->type_id()));
    unsigned child_count = num_children();
    if ((child_count > 0) || parent_var() != NULL)
        an->immeds()->append(immed(child_count));
    for (unsigned child_num = 0; child_num < child_count; ++child_num)
      {
        assert_msg(child_var(child_num)->parent() == parent(),
                   ("var_sym::cvt_to_annote - child '%s' and parent variable "
                    "'%s' are in different tables",
                    child_var(child_num)->name(), name()));
        an->immeds()->append(immed(child_var(child_num)->sym_id()));
      }
    if (parent_var() != NULL)
      {
        assert_msg(parent_var()->parent() == parent(),
                   ("var_sym::cvt_to_annote - child '%s' and parent variable "
                    "'%s' are in different tables", name(),
                    parent_var()->name()));
        an->immeds()->append(immed(parent_var()->sym_id()));
        an->immeds()->append(immed(offset()));
      }
    return an;
}


void
var_sym::cvt_from_annote (annote *an, base_symtab *symtab)
{
    sym_node::cvt_from_annote(an, symtab);
    unsigned type_id = an->immeds()->pop().unsigned_int();
    typ = symtab->lookup_type_id(type_id);
    if (typ == NULL) {
	error_line(1, NULL, "unknown type id %u for symbol %s", type_id,
		   name());
    }
    if (!an->immeds()->is_empty())
      {
        unsigned child_count = an->immeds()->pop().unsigned_int();
        if (child_count != 0)
          {
            children = new var_sym_list;
            for (unsigned child_num = 0; child_num < child_count; ++child_num)
              {
                unsigned child_id = an->immeds()->pop().unsigned_int();
                children->append((var_sym *)(symtab->lookup_sym_id(child_id)));
              }
          }
        if (!an->immeds()->is_empty())
          {
            unsigned parent_id = an->immeds()->pop().unsigned_int();
            the_parent_var = (var_sym *)(symtab->lookup_sym_id(parent_id));
            the_offset = an->immeds()->pop().integer();
          }
      }
    defd = FALSE;
}


void
var_sym::set_has_def (boolean d)
{
    assert_msg((!d) || (parent_var() == NULL),
               ("var_sym::set_has_def - sub-variable is not allowed to have "
                "its own var_def"));
    defd = d;
}


boolean
var_sym::is_extern ()
{
    if (parent_var() != NULL)
        return parent_var()->is_extern();
    else
        return sym_node::is_extern();
}


void
var_sym::set_extern (boolean d)
{
    if (parent_var() != NULL)
        parent_var()->set_extern(d);
    else
        sym_node::set_extern(d);
}


void
var_sym::prepare_child_removal(var_sym *old_child)
{
    assert(old_child->parent_var() == this);
    old_child->the_parent_var = NULL;
    old_child->the_offset = 0;

    old_child->set_extern(is_extern());

    if (is_addr_taken())
        old_child->set_addr_taken();
    else
        old_child->reset_addr_taken();

    if (is_reg())
        old_child->set_reg();
    else
        old_child->reset_reg();
}


void
var_sym::prepare_child_addition(var_sym *new_child)
{
    assert_msg(!new_child->has_var_def(),
               ("var_sym::add_child - child variable '%s' is not allowed to "
                "have its own var_def", new_child->name()));
    assert_msg(!new_child->is_param(),
               ("var_sym::add_child - child variable '%s' is not allowed to "
                "be a parameter", new_child->name()));
    new_child->the_parent_var = this;
}


/*
 *  Try to find a var_def definition for a variable.  This is trivial
 *  except for variables in the global symbol table.  In that case, we have
 *  to check through all of the file_symtabs to see which one has the
 *  definition.  This is rather inefficient but this method will probably
 *  not be used frequently, and the number of source files is usually small.
 */

var_def *
var_sym::definition ()
{
    assert_msg(has_var_def(),
	       ("var_sym::definition - variable '%s' not defined", name()));

    /* check if this variable is in the global symbol table */
    if (parent()->kind() == SYMTAB_GLOBAL) {
	/* search through all of the files */
	base_symtab_list_iter bsli(parent()->children());
	while (!bsli.is_empty()) {
	    base_symtab *symtab = bsli.step();
	    var_def *vd = symtab->lookup_var_def(this);
	    if (vd) return vd;
	}
	return NULL;
    }

    return parent()->lookup_var_def(this);
}


void
var_sym::print (FILE *f)
{
    if (parent_var() == NULL)
      {
	sym_node::print(f);
      }
    else
      {
	parent_var()->print(f);
        fprintf(f, "/%s", name());
      }
}


/*****************************************************************************/


label_sym::label_sym (const char *n)
{
    set_name(n);
    set_extern(FALSE);
}


sym_node *
label_sym::copy ()
{
    label_sym *result = new label_sym(name());
    result->copy_flags(this);
    return result;
}


void
label_sym::print (FILE *f)
{
    fputs("L:", f);
    sym_node::print(f);
}


annote *
label_sym::cvt_to_annote ()
{
    annote *an = new annote(k_labelsym);
    cvt_to_annote_base(an);
    return an;
}


void
label_sym::cvt_from_annote (annote *an, base_symtab *symtab)
{
    sym_node::cvt_from_annote(an, symtab);
    set_extern(FALSE);
}


/*****************************************************************************/


proc_sym::proc_sym (func_type *t, src_lang_type src, const char *n)
{
    set_name(n);
    fse = NULL;
    file_pos = -1;
    written = FALSE;
    typ = t;
    srclang = src;
    pr = NULL;
}


sym_node *
proc_sym::copy ()
{
    proc_sym *result = new proc_sym(type(), src_lang(), name());
    result->copy_flags(this);
    return result;
}


void
proc_sym::set_block (tree_proc *p)
{
    pr = p;
    /* automatically record the proc_sym in the tree_proc */
    if (pr) pr->set_proc(this);
}


void
proc_sym::print (FILE *f)
{
    fputs("P:", f);
    sym_node::print(f);
}


/*
 *  Print proc_sym-specific fields.  This function is called by print_full
 *  to print the procedure type and source language.
 */

void
proc_sym::print_helper (FILE *f)
{
    /* print the type */
    fputs("of ", f);
    type()->print(f);
    fputs("; ", f);

    switch (src_lang()) {
	case src_c:		{ fputs("C ", f); break; }
	case src_fortran:	{ fputs("Fortran ", f); break; }
	case src_verilog:	{ fputs("Verilog ", f); break; }
	default:		{ fputs("Unknown ", f); break; }
    }
}


annote *
proc_sym::cvt_to_annote ()
{
    annote *an = new annote(k_procsym);
    cvt_to_annote_base(an);
    if (write_scope != NULL)
	type()->write_check();
    an->immeds()->append(immed(type()->type_id()));
    an->immeds()->append(immed((int)src_lang()));
    return an;
}


void
proc_sym::cvt_from_annote (annote *an, base_symtab *symtab)
{
    sym_node::cvt_from_annote(an, symtab);
    fse = NULL;
    file_pos = -1;
    written = FALSE;
    unsigned type_id = an->immeds()->pop().unsigned_int();
    typ = (func_type *)symtab->lookup_type_id(type_id);
    if (typ == NULL) {
	error_line(1, NULL, "unknown type id %u for symbol %s", type_id,
		   name());
    }
    srclang = (src_lang_type)an->immeds()->pop().integer();
    pr = NULL;
}


/*
 *  Read a procedure into memory.  This is only valid if the procedure
 *  (1) exists in the input stream, (2) is not marked extern, and (3) has
 *  not already been written.  By default it also builds expression trees.
 */

extern void make_ref_params(proc_sym *ps);

void
proc_sym::read_proc (boolean exp_trees, boolean use_fortran_form)
{
    /* cannot re-read a proc after it has been written out */
    assert_msg(!is_written(), ("read_proc - procedure %s already "
			       "written out", name()));

    /* cannot read a proc that isn't in the input file */
    if (!file() || (file_pos == -1)) return;

    assert_msg(!is_in_memory(), ("read_proc - procedure %s already "
				 "in memory", name()));

    /* seek to the procedure's position in the input stream */
    file()->parent()->open_stream(file()->is, file());
    file()->is->setpos(file_pos);

    set_block(new tree_proc(file()->is, this));

    /* convert to call-by-reference format */
    if (use_fortran_form) {
	make_ref_params(this);

	/* assign numbers to any new types/symbols that were created */
	fileset->globals()->number_globals();
	file()->symtab()->number_globals();
	block()->proc_syms()->number_locals();
    }

    if (exp_trees) {
	block()->body()->cvt_to_trees();
    }
}


/*
 *  Write a procedure.  Each procedure can only be written once.  If the
 *  file_set_entry has no output stream, everything proceeds as usual
 *  except that nothing is actually written; no warning is given if this
 *  occurs.
 */

extern void undo_ref_params(proc_sym *ps);

void
proc_sym::write_proc (file_set_entry *f)
{
    assert_msg(f, ("proc_sym::write_proc - output file parameter is NULL"));
    fse = f;

    if (is_written()) {
	error_line(1, NULL, "Attempt to write proc %s more than once", name());
    }

    /* convert back from call-by-reference format */
    undo_ref_params(this);

    /* make sure that everything has a number */
    fileset->globals()->number_globals();
    file()->symtab()->number_globals();
    block()->number_instrs();

    written = TRUE;
    set_extern(FALSE);

    /* go ahead and write it out now */
    if (file()->os) {
	file()->parent()->open_stream(file()->os, file());
	file_pos = file()->os->getpos();
	file()->os->flush_cache();
	block()->write(file()->os);
    } else {
	assert_msg(FALSE,
		   ("attempt to write proc to fileset entry with no output "
		    "stream"));
    }

    /* put an annotation on the file where the procedure is written */
    annote *an = new annote(k_proc_in_file);
    an->immeds()->append(immed(this));
    an->immeds()->append(immed(file_pos));
    file()->annotes()->append(an);
}


/*
 *  Flush a procedure from memory.
 */

void
proc_sym::flush_proc ()
{
    delete block();
    set_block(NULL);
}
