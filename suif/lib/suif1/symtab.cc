/*  Implementation of Symbol Tables */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "symtab.h"

#define RCS_BASE_FILE symtab_cc

#include "suif1.h"
#include <cstring>

RCS_BASE(
    "$Id$")


/*
 *  The ID numbers (for both sym_nodes and type_nodes) are divided into
 *  three ranges.  The global_symtab entries are shared across all of the
 *  files and use the first range of ID numbers.  The file_symtab entries
 *  use the second range of numbers, and these are only unique within a
 *  particular file.  Finally, local symbols and types use the third range
 *  of IDs which are only unique within a procedure.  The "print_id_number"
 *  function prints ID numbers as described in the header file.
 */

const unsigned first_global_id = 0x00000001;
const unsigned first_file_id = 0x40000001;
const unsigned first_local_id = 0x60000001;

void
print_id_number (FILE *fp, unsigned id)
{
    if (id < first_file_id) {
	fprintf(fp, "g%u", id - first_global_id + 1);
    } else if (id < first_local_id) {
	fprintf(fp, "f%u", id - first_file_id + 1);
    } else {
	fprintf(fp, "p%u", id - first_local_id + 1);
    }
}


/*****************************************************************************/


base_symtab::base_symtab (const char *n)
{
    syms = new sym_node_list;
    typs = new type_node_list;
    defs = new var_def_list;

    par = NULL;
    childs = new base_symtab_list;

    nm = n ? lexicon->enter(n)->sp : NULL;

    next_sym_id = 0;
    next_type_id = 0;
    var_counter = 0;

    sym_name_index = new tree_string_index;
    def_index = new ts_ptr_index;
    sym_id_index = new ts_ptr_index;
    type_id_index = new ts_ptr_index;
}


base_symtab::~base_symtab ()
{
    /* destroy the child symtabs */
    while (!children()->is_empty()) {
	delete children()->pop();
    }
    delete children();

    /* destroy the variable definitions */
    while (!var_defs()->is_empty()) {
	var_def *this_def = var_defs()->head()->contents;
	remove_def_internal(this_def, TRUE);
	delete this_def;
    }
    delete var_defs();

    /* destroy the symbols */
    while (!symbols()->is_empty()) {
	sym_node *this_sym = symbols()->head()->contents;
	remove_sym(this_sym);
	delete this_sym;
    }
    delete symbols();

    /* destroy the types */
    while (!types()->is_empty()) {
	delete types()->pop();
    }
    delete types();

    delete sym_name_index;
    delete def_index;
    delete sym_id_index;
    delete type_id_index;
}


/*
 *  Set the symtab name.  Make sure it's entered in the lexicon.
 */

void
base_symtab::set_name (const char *n)
{
    nm = n ? lexicon->enter(n)->sp : NULL;
}


/*
 *  Write the "chain name" of this symtab into a static buffer and return it.
 *  The result is a string containing the chain name of the parent symtab and
 *  the name of this symtab separated by slashes (just like a Unix path).
 */

static char chain_name_bfr[1024];

const char *
base_symtab::chain_name ()
{
    *chain_name_bfr = '\0';
    chain_name_helper();
    return chain_name_bfr;
}


void
base_symtab::chain_name_helper ()
{
    if (parent() && !parent()->is_file() && !parent()->is_global()) {
	parent()->chain_name_helper();
	strcat(chain_name_bfr, "/");
    }
    if (!is_file() && !is_global()) {
	strcat(chain_name_bfr, name());
    }
}


/*
 *  Search through the list of child symtabs for one with the specified name.
 *  If not found, the function returns NULL.
 */

base_symtab *
base_symtab::lookup_child (const char *name)
{
    const char *n = name ? lexicon->enter(name)->sp : NULL;

    /* search through the child list */
    base_symtab_list_iter bsli(children());
    while (!bsli.is_empty()) {
	base_symtab *s = bsli.step();
	if (s->name() == n) return s;
    }
    return NULL;
}


/*
 *  Search through the list of variable definitions to try to find one for
 *  the specified variable symbol.
 */

var_def *
base_symtab::lookup_var_def (var_sym *v)
{
    return (var_def *)(def_index->lookup(v));
}


/*
 *  Lookup a type.  If the "up" parameter is FALSE only the current symtab
 *  is searched; otherwise, the ancestor symtabs are also searched.  See the
 *  "type_node::is_same" method for a description of when types are considered
 *  to be equal.
 */

type_node *
base_symtab::lookup_type (type_node *t, boolean up)
{
    /* search through the type list */
    type_node_list_iter tnli(types());
    while (!tnli.is_empty()) {
	type_node *type = tnli.step();
	if (type->is_same(t)) {
	    return type;
	}
    }

    /* optionally check the parent symtabs */
    if (up && parent()) return parent()->lookup_type(t, up);

    return NULL;
}


/*
 *  Lookup a symbol by name and symbol kind.  If the "up" parameter is FALSE
 *  only the current symtab is searched; otherwise, the ancestor symtabs are
 *  also searched.
 */

sym_node *
base_symtab::lookup_sym (const char *name, sym_kinds k, boolean up)
{
    assert(name != NULL);
    sym_node_list *entry = (sym_node_list *)(sym_name_index->lookup(name));
    if (entry != NULL)
      {
        /* search through the symbol list */
        sym_node_list_iter snli(entry);
        while (!snli.is_empty())
          {
            sym_node *s = snli.step();
            if (s->kind() == k)
                return s;
          }
      }

    /* optionally check the parent symtabs */
    if (up && parent()) return parent()->lookup_sym(name, k, up);

    return NULL;
}


/*
 *  Lookup a symbol or type by its ID number.  If not found in this symtab,
 *  the ancestor symtabs are automatically searched.  If the ID cannot be
 *  found, the functions return NULL.  Note: The "_suif_raw_syms" flag is
 *  checked here to allow reading binary SUIF files without parsing the
 *  symtab information.  This is done by the "printsuif" program to assist
 *  with debugging the SUIF I/O functions.  In this mode, when a symbol or
 *  type ID number is read from a file, these lookup functions create a
 *  dummy to represent the actual symbol or type.
 */

sym_node *
base_symtab::lookup_sym_id (unsigned i)
{
    sym_node *s = (sym_node *)(sym_id_index->lookup((void *)i));
    if (s != NULL)
	return s;
    if (parent()) return parent()->lookup_sym_id(i);

    /* if in "raw_syms" mode, create a dummy var_sym */
    if (_suif_raw_syms) {
	char buf[20];
	sprintf(buf, "sym(%u)", i);
	var_sym *v = new_var(install_type(new base_type(TYPE_VOID, 0)), buf);
	v->set_sym_id(i);
        if (i != 0)
	    sym_id_index->enter((void *)i, v);
	return v;
    }

    return NULL;
}


type_node *
base_symtab::lookup_type_id (unsigned i)
{
    type_node *t = (type_node *)(type_id_index->lookup((void *)i));
    if (t != NULL)
	return t;
    if (parent()) return parent()->lookup_type_id(i);

    /* if in "raw_syms" mode, create a dummy type_node */
    if (_suif_raw_syms) {
	type_node *t = new base_type(TYPE_VOID, 0);
	add_type(t);
	t->set_type_id(i);
        if (i != 0)
	    type_id_index->enter((void *)i, t);
	return t;
    }

    return NULL;
}


/*
 *  Create a new variable symbol and add it to this symbol table.
 *  The variable name should be unique, but this is not checked.
 */

var_sym *
base_symtab::new_var (type_node *t, const char *n)
{
    var_sym *v = new var_sym(t, n);
    add_sym(v);
    return v;
}


/*
 *  Create a new variable symbol with a unique name and add it to this
 *  symbol table.  The "base" is optional; if it is not provided, a default
 *  base is used for the name.
 */

var_sym *
base_symtab::new_unique_var (type_node *t, const char *base)
{
    char bfr[1024];

    /* provide a default base for the variable name */
    if (!base) base = "suif_tmp";

    strcpy(bfr, base);
    /* add a number to the base name and make sure it's unique */
    while (lookup_var(bfr, FALSE)) {
	sprintf(bfr, "%s%u", base, var_counter++);
    }

    return new_var(t, bfr);
}


/*
 *  Create a definition (var_def) for a variable and enter it in the
 *  symbol table.  The alignment (in bits) must be specified.
 */

var_def *
base_symtab::define_var (var_sym *v, int a)
{
    var_def *d = new var_def(v, a);
    add_def(d);
    return d;
}


/*
 *  Check if the given symtab is an ancestor of this one.  This actually
 *  assumes that a symtab is also its own ancestor, so this function makes
 *  it easy to check if a symbol or type is visible in the current scope
 *  (e.g. "boolean visible = symtab->is_ancestor(sym->parent());").
 */

boolean
base_symtab::is_ancestor (base_symtab *s)
{
    base_symtab *p = this;
    while (p) {
	if (p == s) return TRUE;
	p = p->parent();
    }
    return FALSE;
}


/*
 *  Make replacements for the symbols and types in the "replacements" lists.
 *  If possible, type_nodes are moved to a common scope instead of copying
 *  them.  Copies are always created for the sym_nodes.  Because the symbols
 *  and types may refer to one another, this is done is two passes.  First,
 *  all of the symbols and types are copied (or moved) and added to the
 *  "replacements" lists.  The replacement objects may still contain
 *  references to the old objects, so the second pass goes through all of
 *  the new symbols and types to update the references.
 *
 *  When called by users, the "replacements" parameter should contain lists
 *  of old symbols and types that are not visible in the current symtab.
 *  The corresponding lists of new symbols and types should be empty.
 *  However, this method is also used internally to clone symbol tables.
 *  In that case, the lists of old and new symbols and types may begin
 *  with the replacements that have already been made (e.g. the old lists
 *  may initially be longer than the new lists).
 */

void
base_symtab::resolve_exposed_refs (replacements *r)
{
    sym_node_list_iter snli;
    type_node_list_iter tnli;
    var_def_list_iter vdli;
    int n;

    /* provide a replacement for the parent symtab */
    if ((r->oldtabs.count() == 1) && (r->newtabs.count() == 0)) {
	r->newtabs.append(this);
    }

    assert_msg(r->oldtabs.count() == r->newtabs.count(),
	       ("resolve_exposed_refs - unable to handle symtabs"));
    assert_msg(r->oldinstrs.count() == r->newinstrs.count(),
	       ("resolve_exposed_refs - unable to handle instructions"));

    /* find the first unmatched entry on the oldsyms list */
    sym_node_list_e *first_exp_sym = r->oldsyms.head();
    int newsyms_length = r->newsyms.count();
    for (n = 0; n < newsyms_length; n++) {
	assert_msg(first_exp_sym,
		   ("resolve_exposed_refs - invalid symbol lists"));
	first_exp_sym = first_exp_sym->next();
    }

    /* find the first unmatched entry on the oldtypes list */
    type_node_list_e *first_exp_type = r->oldtypes.head();
    int newtypes_length = r->newtypes.count();
    for (n = 0; n < newtypes_length; n++) {
	assert_msg(first_exp_type,
		   ("resolve_exposed_refs - invalid type lists"));
	first_exp_type = first_exp_type->next();
    }

    /* find the first unmatched entry on the olddefs list */
    var_def_list_e *first_exp_def = r->olddefs.head();
    int newdefs_length = r->newdefs.count();
    for (n = 0; n < newdefs_length; n++) {
	assert_msg(first_exp_def,
		   ("resolve_exposed_refs - invalid def lists"));
	first_exp_def = first_exp_def->next();
    }

    /* make copies of the symbols in the new symtab */
    snli.set(first_exp_sym);
    while (!snli.is_empty()) {
	sym_node *old_sn = snli.step();
	sym_node *new_sn = old_sn->copy();

	add_sym(new_sn);

	/* update the list of replacements */
	r->newsyms.append(new_sn);
    }

    /* move or copy the types */
    tnli.set(first_exp_type);
    while (!tnli.is_empty()) {
	type_node *old_tn = tnli.step();
	type_node *new_tn;

	if (make_type_visible(old_tn, false)) {
	    new_tn = old_tn;
	} else {
	    new_tn = old_tn->copy();
	    add_type(new_tn);
	}

	/* update the list of replacements */
	r->newtypes.append(new_tn);
    }

    /* copy the variable definitions */
    vdli.set(first_exp_def);
    while (!vdli.is_empty()) {
	var_def *old_def = vdli.step();
	var_def *new_def =
	    new var_def(old_def->variable(), old_def->alignment());

	var_defs()->append(new_def);
	def_index->enter(old_def->variable(), new_def);
	new_def->set_parent(this);

	/* update the list of replacements */
	r->newdefs.append(new_def);
    }
 
    /*  Note: At this point, the new symbols, types, and var_defs may still
	contain references to objects that have been replaced.  We have to
	update all of the fields that refer to symbols and types to check
	for this.  The annotations on the symbols, types, and var_defs also
	need to be updated.  */

    snli.reset(symbols());
    while (!snli.is_empty()) {

	sym_node *new_sn = snli.step();

	/* find the corresponding symbol from the old list by swapping the
	   old and new replacements lists and calling clone_helper */
	sym_node_list tmpsyms;
	tmpsyms.grab_from(&r->oldsyms);
	r->oldsyms.grab_from(&r->newsyms);
	r->newsyms.grab_from(&tmpsyms);
	sym_node *old_sn = new_sn->clone_helper(r);
	/* swap back */
	tmpsyms.grab_from(&r->oldsyms);
	r->oldsyms.grab_from(&r->newsyms);
	r->newsyms.grab_from(&tmpsyms);

	old_sn->clone_annotes(new_sn, r, (boolean)(new_sn == old_sn));

	if (new_sn->is_var()) {
	    assert(old_sn->is_var());
	    var_sym *new_var = (var_sym *)new_sn;
	    var_sym *old_var = (var_sym *)old_sn;
	    new_var->set_type(new_var->type()->clone_helper(r));

	    if ((old_var->parent_var() != NULL) && (old_var != new_var)) {
		var_sym *old_parent = old_var->parent_var();
		sym_node *new_parent_sym = old_parent->clone_helper(r);
		assert((new_parent_sym != NULL) && new_parent_sym->is_var());
		var_sym *new_parent = (var_sym *)new_parent_sym;
		new_parent->add_child(new_var, old_var->offset());
	    }
	} else if (new_sn->is_proc()) {
	    proc_sym *new_proc = (proc_sym *)new_sn;
	    type_node *new_type = new_proc->type()->clone_helper(r);
	    assert(new_type->is_func());
	    new_proc->set_type((func_type *)new_type);
	}
    }

    tnli.reset(types());
    while (!tnli.is_empty()) {

	type_node *new_tn = tnli.step();

	/* find the corresponding type from the old list by swapping the
	   old and new replacements lists and calling clone_helper */
	type_node_list tmptypes;
	tmptypes.grab_from(&r->oldtypes);
	r->oldtypes.grab_from(&r->newtypes);
	r->newtypes.grab_from(&tmptypes);
	type_node *old_tn = new_tn->clone_helper(r);
	/* swap back */
	tmptypes.grab_from(&r->oldtypes);
	r->oldtypes.grab_from(&r->newtypes);
	r->newtypes.grab_from(&tmptypes);

	old_tn->clone_annotes(new_tn, r, (boolean)(new_tn == old_tn));

	switch (new_tn->op()) {

	    case TYPE_NULL:
	    case TYPE_CONST:
	    case TYPE_VOLATILE:
	    case TYPE_RESTRICT:
	    case TYPE_CALL_BY_REF: {
		modifier_type *new_mod = (modifier_type *)new_tn;
		new_mod->set_base(new_mod->base()->clone_helper(r));
		break;
	    }

	    case TYPE_PTR: {
		ptr_type *new_ptr = (ptr_type *)new_tn;
		new_ptr->set_ref_type(new_ptr->ref_type()->clone_helper(r));
		break;
	    }

	    case TYPE_ARRAY: {
		array_type *new_array = (array_type *)new_tn;
		new_array->set_elem_type(new_array->elem_type()->
					 clone_helper(r));
		if (new_array->lower_bound().is_variable()) {
		    var_sym *new_var = (var_sym *)new_array->
			lower_bound().variable()->clone_helper(r);
		    new_array->set_lower_bound(array_bound(new_var));
		}
		if (new_array->upper_bound().is_variable()) {
		    var_sym *new_var = (var_sym *)new_array->
			upper_bound().variable()->clone_helper(r);
		    new_array->set_upper_bound(array_bound(new_var));
		}
		break;
	    }

	    case TYPE_FUNC: {
		func_type *new_func = (func_type *)new_tn;
		new_func->set_return_type(new_func->return_type()->
					  clone_helper(r));
		for (unsigned n = 0; n < new_func->num_args(); n++) {
		    new_func->set_arg_type(n, new_func->arg_type(n)->
					   clone_helper(r));
		}
		break;
	    }

	    case TYPE_GROUP:
	    case TYPE_STRUCT:
	    case TYPE_UNION: {
		struct_type *new_struct = (struct_type *)new_tn;
		for (unsigned n = 0; n < new_struct->num_fields(); n++) {
		    new_struct->set_field_type(n, new_struct->field_type(n)->
					       clone_helper(r));
		}
		break;
	    }

	    default: {
		break;
	    }
	}
    }

    vdli.reset(var_defs());
    while (!vdli.is_empty()) {

	var_def *new_def = vdli.step();

	/* find the corresponding var_def from the old list by swapping the
	   old and new replacements lists and calling clone_helper */
	var_def_list tmpdefs;
	tmpdefs.grab_from(&r->olddefs);
	r->olddefs.grab_from(&r->newdefs);
	r->newdefs.grab_from(&tmpdefs);
	var_def *old_def = new_def->clone_helper(r);
	/* swap back */
	tmpdefs.grab_from(&r->olddefs);
	r->olddefs.grab_from(&r->newdefs);
	r->newdefs.grab_from(&tmpdefs);

	old_def->clone_annotes(new_def, r, (boolean)(new_def == old_def));
	sym_node *new_sym = new_def->variable()->clone_helper(r);
	if (new_sym != new_def->variable()) {
	    assert(new_sym->is_var());
	    var_sym *new_var = (var_sym *)new_sym;
	    assert_msg(!new_var->has_var_def(),
		       ("base_symtab::resolve_exposed_refs - variable '%s' "
			"already defined", new_def->variable()->name()));
	    new_def->set_variable(new_var);
	}
    }
}


base_symtab *
base_symtab::clone_helper (replacements *r, boolean no_copy)
{
    base_symtab *result = this;

    /* find the parent symbol table */
    base_symtab *new_parent = parent();

    /* check if the parent has been replaced by a clone */
    base_symtab_list_iter old_bsli(&r->oldtabs);
    base_symtab_list_iter new_bsli(&r->newtabs);
    while (!old_bsli.is_empty()) {

	base_symtab *oldtab = old_bsli.step();
	base_symtab *newtab = new_bsli.step();

	if (oldtab == new_parent) {
	    new_parent = newtab;
	    break;
	}
    }
	    
    if (!no_copy) {
	char bfr[1024];
	unsigned counter = 0;

	/* find a unique symtab name */
	while (TRUE) {
	    sprintf(bfr, "%s%u", name(), counter++);
	    if (!new_parent->lookup_child(bfr)) break;
	}

	/* make a new symbol table */
	result = this->new_peer(bfr);
	new_parent->add_child(result);

    } else if (new_parent != parent()) {
	parent()->remove_child(this);
	new_parent->add_child(this);
    }

    if (!no_copy) add_contents_to_replacements(r);
    result->resolve_exposed_refs(r);

    clone_annotes(result, r, no_copy);

    /* update the lists of replacements */
    if (!no_copy) {
	r->oldtabs.append(this);
	r->newtabs.append(result);
    }

    return result;
}


/*
 *  Try to move a type_node upwards in the symbol table hierarchy from its
 *  current location so that it will be visible from the current scope.
 *  The "check" parameter has a default value of FALSE.  If TRUE, the types
 *  are only checked to see if it is possible to move them; nothing is
 *  actually moved.  The function returns TRUE only if it succeeds.
 *  Otherwise, it moves as many of the components as possible and then
 *  returns FALSE.
 */

boolean
base_symtab::make_type_visible (type_node *t, boolean check)
{
    /* first check if the type is already visible */
    if (is_ancestor(t->parent())) return TRUE;

    boolean failed = FALSE;

    switch (t->op()) {

	case TYPE_CONST:
	case TYPE_VOLATILE:
	case TYPE_CALL_BY_REF:
	case TYPE_RESTRICT:
	case TYPE_NULL: {
	    modifier_type *mt = (modifier_type *)t;
	    if (!make_type_visible(mt->base(), check)) failed = TRUE;
	    break;
	}

	case TYPE_PTR: {
	    ptr_type *pt = (ptr_type *)t;
	    if (!make_type_visible(pt->ref_type(), check)) failed = TRUE;
	    break;
	}

	case TYPE_ARRAY: {
	    array_type *at = (array_type *)t;
	    if (!make_type_visible(at->elem_type(), check)) failed = TRUE;
	    if (at->lower_bound().is_variable() &&
		!is_ancestor(at->lower_bound().variable()->parent())) {
		failed = TRUE;
	    }
	    if (at->upper_bound().is_variable() &&
		!is_ancestor(at->upper_bound().variable()->parent())) {
		failed = TRUE;
	    }
	    break;
	}

	case TYPE_FUNC: {
	    func_type *ft = (func_type *)t;
	    if (!make_type_visible(ft->return_type(), check)) failed = TRUE;
	    for (unsigned n = 0; n < ft->num_args(); n++) {
		if (!make_type_visible(ft->arg_type(n), check)) failed = TRUE;
	    }
	    break;
	}

	case TYPE_GROUP:
	case TYPE_STRUCT:
	case TYPE_UNION: {
	    struct_type *st = (struct_type *)t;

	    /* temporarily move the type to this symtab -- this is needed
	       to keep recursive types from causing infinite loops */
	    base_symtab *orig_parent = t->parent();
	    orig_parent->remove_type(t);
	    add_type(t);

	    for (unsigned n = 0; n < st->num_fields(); n++) {
		/* do not actually move any types yet */
		if (!make_type_visible(st->field_type(n), TRUE)) failed = TRUE;
	    }

	    /* move the struct back to its original symtab */
	    remove_type(t);
	    orig_parent->add_type(t);

	    break;
	}

	default: {
	    break;
	}
    }

    /* check the annotations on the type_node */
    if (t->are_annotations()) {
	annote_list_iter anli(annotes());
	while (!anli.is_empty()) {

	    annote *an = anli.step();
	    immed_list *iml = an->immeds();

	    /* ignore unregistered annotations */
	    if (!iml) continue;

	    immed_list_iter imli(iml);
	    while (!imli.is_empty()) {
		immed im = imli.step();
		if (im.is_symbol()) {
		    if (!is_ancestor(im.symbol()->parent())) failed = TRUE;
		} else if (im.is_type()) {
		    if (!make_type_visible(im.type(), check)) failed = TRUE;
		}
	    }

	    if (an->is_structured()) {
		delete iml;
	    }
	}
    }

    /* check if types have a common scope, if not, set failure flag */    
    base_symtab *dst_scope = t->parent();
    while (dst_scope && !is_ancestor(dst_scope))
	    dst_scope = dst_scope->parent();

    failed |= (dst_scope == NULL);

    /* move the type if possible */
    if (!failed && !check) {

	/* find the closest shared scope */
	base_symtab *dst_scope = t->parent();
	while (dst_scope && !is_ancestor(dst_scope)) {
	    dst_scope = dst_scope->parent();
	}
	assert_msg(dst_scope,("make_type_visible - cannot find shared scope"));

	/* move the type */
	t->parent()->remove_type(t);
	dst_scope->add_type(t);

	if (t->is_struct()) {
	    struct_type *st = (struct_type *)t;

	    /* now it is safe to actually move the field types */
	    for (unsigned n = 0; n < st->num_fields(); n++) {
		assert(make_type_visible(st->field_type(n), FALSE));
	    }
	}
    }

    return !failed;
}


/*
 *  Check if a type has already been entered in this symtab or one of its
 *  ancestors.  If so, delete the new type and return the one in the symtab.
 *  If a type is not found, it is entered into this symtab and returned.
 *  All of the components of a type are installed before the type itself.
 *  This makes it easy to create new types without worrying about duplicate
 *  entries in the symtabs--you can just create the new type and let
 *  install_type take care of duplicates.
 */

type_node *
base_symtab::install_type (type_node *t)
{
    if (!t) return NULL;

    /* check if the type is already installed */
    if (t->parent()) {
	/* make sure the symtab is an ancestor of this one */
	assert_msg(is_ancestor(t->parent()),
		   ("base_symtab::install_type - "
		    "type is already installed but not reachable"));
	return t;
    }

    /* handle structs separately -- each one is unique even if the
       component types are equivalent */
    if (t->is_struct()) {
	if (lookup_type(t)) return t;
	add_type(t);
    }

    /* recursively install the component types */
    switch (t->op()) {

	case TYPE_CONST:
	case TYPE_VOLATILE:
	case TYPE_CALL_BY_REF:
	case TYPE_RESTRICT:
	case TYPE_NULL: {
	    modifier_type *mt = (modifier_type *)t;
	    mt->set_base(install_type(mt->base()));
	    assert_msg(is_ancestor(mt->base()->parent()),
		       ("base_symtab::install_type - base type not visible"));
	    break;
	}

	case TYPE_VOID:
	case TYPE_INT:
	case TYPE_FLOAT: {
	    break;
	}

	case TYPE_PTR: {
	    ptr_type *pt = (ptr_type *)t;
	    pt->set_ref_type(install_type(pt->ref_type()));
	    assert_msg(is_ancestor(pt->ref_type()->parent()),
		       ("base_symtab::install_type - ref type not visible"));
	    break;
	}

	case TYPE_ARRAY: {
	    array_type *at = (array_type *)t;
	    at->set_elem_type(install_type(at->elem_type()));
	    assert_msg(is_ancestor(at->elem_type()->parent()),
		       ("base_symtab::install_type - elem type not visible"));
	    if (at->lower_bound().is_variable()) {
		var_sym *vlb = at->lower_bound().variable();
		if (!vlb->parent()) {
		    add_sym(vlb);
		} else {
		    assert_msg(is_ancestor(vlb->parent()),
			       ("base_symtab::install_type - "
				"lower bound not visible"));
		}
	    }
	    if (at->upper_bound().is_variable()) {
		var_sym *vub = at->upper_bound().variable();
		if (!vub->parent()) {
		    add_sym(vub);
		} else {
		    assert_msg(is_ancestor(vub->parent()),
			       ("base_symtab::install_type - "
				"upper bound not visible"));
		}
	    }
	    break;
	}

	case TYPE_FUNC: {
	    func_type *ft = (func_type *)t;
	    ft->set_return_type(install_type(ft->return_type()));
	    assert_msg(is_ancestor(ft->return_type()->parent()),
		       ("base_symtab::install_type - "
			"return type not visible"));
	    for (unsigned n = 0; n < ft->num_args(); n++) {
		ft->set_arg_type(n, install_type(ft->arg_type(n)));
		assert_msg(is_ancestor(ft->arg_type(n)->parent()),
			   ("base_symtab::install_type - "
			    "arg type not visible"));
	    }
	    break;
	}

	case TYPE_GROUP:
	case TYPE_STRUCT:
	case TYPE_UNION: {
	    struct_type *st = (struct_type *)t;
	    for (unsigned n = 0; n < st->num_fields(); n++) {
		st->set_field_type(n, install_type(st->field_type(n)));
		assert_msg(is_ancestor(st->field_type(n)->parent()),
			   ("base_symtab::install_type - "
			    "field type not visible"));
	    }
	    /* just return -- the struct has already been added */
	    return t;
	}

	case TYPE_ENUM: {
	    /* no sub-types */
	    break;
	}
    }

    /* check if this type is already in the symtab (or its parents) */
    type_node *oldt = lookup_type(t);
    if (oldt) {
	assert(t != oldt);
	delete t;
	return oldt;
    }

    /* add the new type to the destination symtab */
    add_type(t);
    return t;
}


/*
 *  Add a child symtab.  The child should have a name that is different from
 *  all of the other children, but if not it will be automatically renamed
 *  before the table is written out.  The child's parent pointer is
 *  automatically set to point back to this symtab.
 */

void
base_symtab::add_child (base_symtab *c)
{
    /* add it to the list of children */
    children()->append(c);
    c->par = this;
}


/*
 *  Remove a child symtab.
 */

void
base_symtab::remove_child (base_symtab *c)
{
    base_symtab_list_e *list_e = children()->lookup(c);
    assert_msg(list_e, ("base_symtab::remove_child - child not in list"));
    children()->remove(list_e);
    delete list_e;
    c->par = NULL;
}


/*
 *  Add a symbol.  The symbol should have a name that is different from all
 *  of the other symbols in this table, but if not it will be automatically
 *  renamed before the table is written out.  The symbol's parent pointer
 *  is automatically set to point back to this symtab.
 */

void
base_symtab::add_sym (sym_node *s)
{
    symbols()->append(s);
    s->set_parent(this);
    add_name_entry(s, s->name());
    if (s->sym_id() != 0)
        sym_id_index->enter((void *)(s->sym_id()), s);
}


/*
 *  Remove a symbol.
 */

void
base_symtab::remove_sym (sym_node *s)
{
    sym_node_list_e *list_e = symbols()->lookup(s);
    assert_msg(list_e, ("base_symtab::remove_sym - symbol not in list"));
    symbols()->remove(list_e);
    delete list_e;
    s->set_parent(NULL);
    remove_name_entry(s, s->name());
    s->clear_sym_id();
}


/*
 *  Add a type.  The type's symtab parent is automatically set to point
 *  back to this symtab.
 */

void
base_symtab::add_type (type_node *t)
{
    types()->append(t);
    t->set_parent(this);
    if (t->type_id() != 0)
        type_id_index->enter((void *)(t->type_id()), t);
}


/*
 *  Remove a type.
 */

void
base_symtab::remove_type (type_node *t)
{
    type_node_list_e *list_e = types()->lookup(t);
    assert_msg(list_e, ("base_symtab::remove_type - type not in list"));
    types()->remove(list_e);
    delete list_e;
    si_entry *old_si_entry =
            type_id_index->lookup_entry((void *)(t->type_id()));
    if (old_si_entry != NULL)
        type_id_index->remove_entry(old_si_entry);
    t->set_parent(NULL);
    t->clear_type_id();
}


/*
 *  Add a variable definition.  Clear the "extern" flag on the variable and
 *  set its "has_var_def" flag to indicate that it has a definition.
 */

void
base_symtab::add_def (var_def *d)
{
    var_sym *v = d->variable();
    assert_msg(!v->has_var_def(), ("base_symtab::add_def - variable '%s' "
				   "already defined", v->name()));
    var_defs()->append(d);
    def_index->enter(v, d);
    v->set_has_def(TRUE);
    v->set_extern(FALSE);
    d->set_parent(this);
}


/*
 *  Remove a variable definition.  Set the "extern" flag on the variable and
 *  clear its "has_var_def" flag.
 */

void
base_symtab::remove_def (var_def *d)
{
    remove_def_internal(d, FALSE);
}


/*
 *  Print the contents of a symtab.  This function is called by the symtab
 *  print methods.
 */

void
base_symtab::print_contents (FILE *fp, int depth)
{
    /* print the symtab name if available */
    if (name()) {
	fprintf(fp, "`%s'\n", name());
    } else {
	fprintf(fp, "<None>\n");
    }

    /* print the types */
    suif_indent(fp, depth+1);
    fprintf(fp, "Types:\n");
    if (types()->is_empty()) {
	suif_indent(fp, depth+2);
	fprintf(fp, "<None>\n");
    } else {
	type_node_list_iter tmi(types());
	while (!tmi.is_empty()) {
	    type_node *t = tmi.step();
	    t->print_full(fp, depth+2);
	}
    }

    /* print the symbols */
    suif_indent(fp, depth+1);
    fprintf(fp, "Symbols:\n");
    if (symbols()->is_empty()) {
	suif_indent(fp, depth+2);
	fprintf(fp, "<None>\n");
    } else {
	sym_node_list_iter smi(symbols());
	while (!smi.is_empty()) {
	    smi.step()->print_full(fp, depth+2);
	}
    }

    /* print the variable definitions */
    suif_indent(fp, depth+1);
    fprintf(fp, "Definitions:\n");
    if (var_defs()->is_empty()) {
	suif_indent(fp, depth+2);
	fprintf(fp, "<None>\n");
    } else {
	var_def_list_iter vdli(var_defs());
	while (!vdli.is_empty()) {
	    vdli.step()->print(fp, depth+2);
	}
    }

    /* print the symtab annotations */
    suif_indent(fp, depth+1);
    fprintf(fp, "Annotations:");
    if (!are_annotations()) {
	putc('\n', fp);
	suif_indent(fp, depth+2);
	fprintf(fp, "<None>\n");
    } else {
	print_annotes(fp, depth+2);
	putc('\n', fp);
    }
}


void
base_symtab::add_contents_to_replacements (replacements *r)
{
    assert(r->oldsyms.count() == r->newsyms.count());
    assert(r->oldtypes.count() == r->newtypes.count());
    assert(r->olddefs.count() == r->newdefs.count());

    sym_node_list_iter snli(symbols());
    while (!snli.is_empty()) {
	sym_node *sn = snli.step();
	r->oldsyms.append(sn);
    }

    type_node_list_iter tnli(types());
    while (!tnli.is_empty()) {
	type_node *tn = tnli.step();
	r->oldtypes.append(tn);
    }

    var_def_list_iter vdli(var_defs());
    while (!vdli.is_empty()) {
	var_def *vd = vdli.step();
	r->olddefs.append(vd);
    }
}


void
base_symtab::register_name_change (sym_node *the_sym, const char *new_name)
{
    const char *old_name = the_sym->name();
    remove_name_entry(the_sym, old_name);
    add_name_entry(the_sym, new_name);
}


void
base_symtab::register_sym_id_change (sym_node *the_sym, unsigned new_id)
{
    unsigned old_id = the_sym->sym_id();
    if (old_id == new_id)
        return;
    if (old_id != 0)
      {
        si_entry *old_si_entry = sym_id_index->lookup_entry((void *)old_id);
        assert(old_si_entry != NULL);
        sym_id_index->remove_entry(old_si_entry);
      }
    if (new_id != 0)
        sym_id_index->enter((void *)new_id, the_sym);
}


void
base_symtab::register_type_id_change (type_node *the_type, unsigned new_id)
{
    unsigned old_id = the_type->type_id();
    if (old_id == new_id)
        return;
    if (old_id != 0)
      {
        si_entry *old_si_entry = type_id_index->lookup_entry((void *)old_id);
        assert(old_si_entry != NULL);
        type_id_index->remove_entry(old_si_entry);
      }
    if (new_id != 0)
        type_id_index->enter((void *)new_id, the_type);
}


void
base_symtab::add_name_entry(sym_node *the_sym, const char *new_name)
{
    if (new_name == NULL)
        return;

    sym_node_list *existing_entry =
            (sym_node_list *)(sym_name_index->lookup(new_name));
    if (existing_entry != NULL)
        existing_entry->append(the_sym);
    else
        sym_name_index->enter(new_name, new sym_node_list(the_sym));
}


void
base_symtab::remove_name_entry(sym_node *the_sym, const char *old_name)
{
    if (old_name == NULL)
        return;

    si_entry *old_entry = sym_name_index->lookup_entry(old_name);
    assert(old_entry != NULL);
    sym_node_list *old_list = (sym_node_list *)(old_entry->data_value());
    assert(old_list != NULL);
    assert(!old_list->is_empty());
    sym_node_list_e *follow = old_list->head();
    assert(follow != NULL);
    if (follow->next() == NULL)
      {
        assert(follow->contents == the_sym);
        delete old_list;
        sym_name_index->remove_entry(old_entry);
      }
    else
      {
        while (follow->contents != the_sym)
          {
            assert(follow->next() != NULL);
            follow = follow->next();
          }
        old_list->remove(follow);
      }
}


void
base_symtab::register_def_sym_change(var_def *the_def, var_sym *new_var)
{
    var_sym *old_var = the_def->variable();
    if (old_var == new_var)
        return;
    if (old_var != NULL)
      {
        si_entry *old_si_entry = def_index->lookup_entry(old_var);
        assert(old_si_entry != NULL);
        def_index->remove_entry(old_si_entry);
        old_var->set_has_def(FALSE);
        if (old_var->parent()->is_global())
            old_var->set_extern(TRUE);
      }
    if (new_var != NULL)
      {
        def_index->enter(new_var, the_def);
        new_var->set_has_def(TRUE);
        new_var->set_extern(FALSE);
      }
}


void
base_symtab::remove_def_internal(var_def *d, boolean in_destructor)
{
    var_def_list_e *list_e = var_defs()->lookup(d);
    assert_msg(list_e, ("base_symtab::remove_def - var_def not in list"));
    var_defs()->remove(list_e);
    delete list_e;

    si_entry *old_entry = def_index->lookup_entry(d->variable());
    assert(old_entry != NULL);
    def_index->remove_entry(old_entry);

    d->set_parent(NULL);

    var_sym *v = d->variable();
    v->set_has_def(FALSE);
    /*
     *  Note: in C++ it is illegal to call a method x within the
     *  destructor of class y if x is pure virtual with respect to y.
     *  The IRIX 5.3 compiler will in fact detect this condition and
     *  core dump.
     *
     *  That is relevant here because the base_symtab destructor needs
     *  to call this method to remove var_def's, but the
     *  var_sym::is_auto() method calls the is_global() method on its
     *  parent symtab.  base_symtab::is_global() is pure virtual, so
     *  if v is in the same symtab as the var_def, is_global() will be
     *  called indirectly from within the base_symtab destructor on
     *  the same object.
     *
     *  That's why we have all this nonsense with the in_destructor
     *  flag.  Within the base_symtab destructor, if we find a var_sym
     *  for the same table, there's no need to change the is_extern
     *  flag because the var_sym will just be destroyed later in the
     *  base_symtab destructor anyway, without the is_extern flag ever
     *  being checked again.
     */
    if ((!in_destructor) || (v->parent() != this))
      {
        if (!v->is_auto()) v->set_extern(TRUE);
      }
}


/*
 *  Check for symbols, types, and child symtabs with duplicate names and
 *  rename them so that everything is unique.  This is called automatically
 *  before writing out each symtab.
 */

static const char *unique_type_name(const char *base, base_symtab *syms);

void
base_symtab::rename_duplicates ()
{
    char bfr[1024];

    sym_node_list_iter sym_iter(symbols());
    while (!sym_iter.is_empty()) {
	sym_node_list_iter other_sym_iter;
	sym_node *sym = sym_iter.step();

	/* symbols in file_symtabs must also be checked against symbols
	   in the global_symtab parent */

	if (is_file() && parent()) {
	    other_sym_iter.reset(parent()->symbols());
	    while (!other_sym_iter.is_empty()) {
		sym_node *other_sym = other_sym_iter.step();

		if ((other_sym->kind() == sym->kind()) &&
		    (other_sym->name() == sym->name())) {

		    /* rename this symbol */
		    unsigned counter = 1;
		    while (TRUE) {
			sprintf(bfr, "%s_%u", sym->name(), counter++);
			if (!lookup_sym(bfr, sym->kind(), TRUE)) break;
		    }
		    sym->set_name(bfr);
		}
	    }
	}

	/* search through the rest of the list */
	other_sym_iter.set(sym_iter.cur_elem()->next());
	while (!other_sym_iter.is_empty()) {
	    sym_node *other_sym = other_sym_iter.step();

	    if ((other_sym->kind() == sym->kind()) &&
		(other_sym->name() == sym->name())) {

		/* rename the other symbol */
		unsigned counter = 1;
		while (TRUE) {
		    sprintf(bfr, "%s_%u", sym->name(), counter++);
		    if (!lookup_sym(bfr, sym->kind(), FALSE)) break;
		}
		other_sym->set_name(bfr);
	    }
	}
    }

    type_node_list_iter type_iter(types());
    while (!type_iter.is_empty()) {
	type_node *type = type_iter.step();

	const char *type_name;
	switch (type->op()) {
	    case TYPE_GROUP:
	    case TYPE_STRUCT:
	    case TYPE_UNION: {
		type_name = ((struct_type *)type)->name();
		break;
	    }
	    case TYPE_ENUM: {
		type_name = ((enum_type *)type)->name();
		break;
	    }
	    default: {
		/* ignore unnamed types */
		continue;
	    }
	}

	/* search through the rest of the list */
	type_node_list_iter other_type_iter;
	other_type_iter.set(type_iter.cur_elem()->next());
	while (!other_type_iter.is_empty()) {
	    type_node *other_type = other_type_iter.step();

	    switch (other_type->op()) {
		case TYPE_GROUP:
		case TYPE_STRUCT:
		case TYPE_UNION: {
		    struct_type *stype = (struct_type *)other_type;
		    if (stype->name() == type_name) {
			stype->set_name(unique_type_name(type_name,this));
		    }
		    break;
		}
		case TYPE_ENUM: {
		    enum_type *etype = (enum_type *)other_type;
		    if (etype->name() == type_name) {
			etype->set_name(unique_type_name(type_name,this));
		    }
		    break;
		}
		default: {
		    break;
		}
	    }
	}
    }

    base_symtab_list_iter child_iter(children());
    while (!child_iter.is_empty()) {
	base_symtab *child = child_iter.step();

	base_symtab_list_iter other_child_iter;
	other_child_iter.set(child_iter.cur_elem()->next());
	while (!other_child_iter.is_empty()) {
	    base_symtab *other_child = other_child_iter.step();

	    if (other_child->name() == child->name()) {

		/* rename the other symtab */
		unsigned counter = 1;
		while (TRUE) {
		    sprintf(bfr, "%s%u", child->name(), counter++);
		    if (!lookup_child(bfr)) break;
		}
		other_child->set_name(bfr);
	    }
	}
    }
}


/*
 *  Find a unique aggregate type name.
 */

const char *
unique_type_name (const char *base, base_symtab *syms)
{
    char bfr[1024];
    const char *type_name;
    unsigned counter = 1;

    while (TRUE) {
	sprintf(bfr, "%s%u", base, counter++);
	type_name = lexicon->enter(bfr)->sp;

	boolean found = FALSE;
	type_node_list_iter type_iter(syms->types());
	while (!type_iter.is_empty()) {
	    type_node *type = type_iter.step();

	    switch (type->op()) {
		case TYPE_GROUP:
		case TYPE_STRUCT:
		case TYPE_UNION: {
		    if (type_name == ((struct_type *)type)->name()) {
			found = TRUE;
		    }
		    break;
		}
		case TYPE_ENUM: {
		    if (type_name == ((enum_type *)type)->name()) {
			found = TRUE;
		    }
		    break;
		}
		default: {
		    /* ignore unnamed types */
		    break;
		}
	    }
	    if (found) break;
	}
	if (!found) break;
    }
    assert(type_name);
    return type_name;
}


/* This flag determines renaming of duplicate names for all the
 * instantiations of base_symtab and derived classes
 */
static boolean automatic_renaming_enabled = TRUE;

void enable_automatic_renaming(void)
{
    automatic_renaming_enabled = TRUE;
}

void disable_automatic_renaming(void)
{
    automatic_renaming_enabled = FALSE;
}


/*
 *  Write a base symtab to a SUIF file.  The types and symbols are
 *  converted to annotations that are then attached to mark instructions
 *  and written out.
 */

void
base_symtab::write (out_stream *os)
{
    if (automatic_renaming_enabled)
        rename_duplicates();

    instruction *mrk;

    /* write the types */
    if (!types()->is_empty()) {
	mrk = new in_rrr(io_mrk);
	mrk->annotes()->append(new annote(k_types));

	type_node_list_iter tli(types());
	while (!tli.is_empty()) {
	    annote *an = tli.step()->cvt_to_annote();
	    mrk->annotes()->append(an);
	}

	mrk->write(os);
	delete mrk;
    }

    /* write the symbols */
    if (!symbols()->is_empty()) {
	mrk = new in_rrr(io_mrk);
	mrk->annotes()->append(new annote(k_symbols));

	sym_node_list_iter sli(symbols());
	while (!sli.is_empty()) {
	    annote *an = sli.step()->cvt_to_annote();
	    mrk->annotes()->append(an);
	}

	mrk->write(os);
	delete mrk;
    }

    /* write the variable definitions */
    if (!var_defs()->is_empty()) {
	mrk = new in_rrr(io_mrk);
	mrk->annotes()->append(new annote(k_var_defs));

	var_def_list_iter vdli(var_defs());
	while (!vdli.is_empty()) {
	    annote *an = vdli.step()->cvt_to_annote();
	    mrk->annotes()->append(an);
	}

	mrk->write(os);
	delete mrk;
    }

    /* write the type annotations */
    type_node_list_iter tli(types());
    while (!tli.is_empty()) {
	type_node *t = tli.step();
	if (t->num_output_annotes() != 0) {
	    mrk = new in_rrr(io_mrk);
	    annote *an = new annote(k_type_annotes);
	    an->immeds()->append(immed(t));
	    mrk->annotes()->append(an);
	    t->copy_annotes(mrk);
	    mrk->write(os);
	    delete mrk;
	}
    }
	    
    /* write the symbol annotations */
    sym_node_list_iter sli(symbols());
    while (!sli.is_empty()) {
	sym_node *sn = sli.step();
	if (sn->num_output_annotes() != 0) {
	    mrk = new in_rrr(io_mrk);
	    annote *an = new annote(k_sym_annotes);
	    an->immeds()->append(immed(sn));
	    mrk->annotes()->append(an);
	    sn->copy_annotes(mrk);
	    mrk->write(os);
	    delete mrk;
	}
    }
	    
    /* write the var_def annotations */
    var_def_list_iter vdli(var_defs());
    while (!vdli.is_empty()) {
	var_def *vd = vdli.step();
	if (vd->num_output_annotes() != 0) {
	    mrk = new in_rrr(io_mrk);
	    annote *an = new annote(k_var_def_annotes);
	    an->immeds()->append(immed(vd->variable()));
	    mrk->annotes()->append(an);
	    vd->copy_annotes(mrk);
	    mrk->write(os);
	    delete mrk;
	}
    }

    /* write the next ID numbers and the var_counter */
    annote *an;
    mrk = new in_rrr(io_mrk);
    an = new annote(k_base_symtab);
    mrk->annotes()->append(an);

    an = new annote(k_next_sym_id);
    an->immeds()->push(immed(next_sym_id));
    mrk->annotes()->append(an);

    an = new annote(k_next_type_id);
    an->immeds()->push(immed(next_type_id));
    mrk->annotes()->append(an);

    an = new annote(k_var_counter);
    an->immeds()->push(immed(var_counter));
    mrk->annotes()->append(an);

    mrk->write(os);
    delete mrk;

    /* write the symtab annotations */
    mrk = new in_rrr(io_mrk);
    copy_annotes(mrk);
    mrk->write(os);
    delete mrk;
}


/*
 *  Read a base symtab from a SUIF file.  This is basically the reverse of
 *  writing a symtab, but reading types requires some extra work.  Because
 *  type_nodes may refer to other type_nodes and var_syms (in array bounds),
 *  we have to read the types in two passes.  The first pass just creates
 *  the types and enters them in the symtab; after reading the symbols, the
 *  second pass goes back and reads the type fields from the annotations.
 *  Everything else is pretty straightforward....
 */

void
base_symtab::read (in_stream *is)
{
    instruction *mrk, *tmrk, *smrk;
    annote *an;

    mrk = instruction::read(is, this, NULL);
    assert(mrk->opcode() == io_mrk);

    /* check if there are any new types */
    tmrk = NULL;
    an = mrk->annotes()->get_annote(k_types);
    if (an) {
	delete an;

	/* scan through the list of types and create all of the type_nodes */
	annote_list_iter iter(mrk->annotes());
	while (!iter.is_empty()) {
	    an = iter.step();
	    add_type(type_node::scan_from_annote(an));
	}

	/* read the next instruction */
	tmrk = mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    /* check if there are any new symbols */
    smrk = NULL;
    an = mrk->annotes()->get_annote(k_symbols);
    if (an) {
	delete an;

	/* scan through the list of symbols and create all the sym_nodes */
	annote_list_iter iter(mrk->annotes());
	while (!iter.is_empty()) {
	    an = iter.step();
	    sym_node *sn = sym_node::scan_from_annote(an, this);
	    add_sym(sn);
	}

	/* read the next instruction */
	smrk = mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    if (tmrk) {
	/* now go back and read the type details */
	while (!tmrk->annotes()->is_empty()) {
	    an = tmrk->annotes()->pop();
	    unsigned tid = (*an->immeds())[0].unsigned_int();
	    type_node *t = lookup_type_id(tid);
	    assert_msg(t && (t->parent() == this),
		       ("base_symtab::read - cannot find type ID %u", tid));
	    t->cvt_from_annote(an, this);
	    delete an;
	}

	delete tmrk;
    }

    if (smrk) {
	/* now go back and read the symbol details */
	while (!smrk->annotes()->is_empty()) {
	    an = smrk->annotes()->pop();
	    unsigned sid = (*an->immeds())[0].unsigned_int();
	    sym_node *s = lookup_sym_id(sid);
	    assert_msg(s && (s->parent() == this),
		       ("base_symtab::read - cannot find sym ID %u", sid));
	    s->cvt_from_annote(an, this);
	    delete an;
	}

	delete smrk;
    }

    /* check if there are any variable definitions */
    an = mrk->annotes()->get_annote(k_var_defs);
    if (an) {
	delete an;

	/* read the list of var_defs */
	while (!mrk->annotes()->is_empty()) {
	    an = mrk->annotes()->pop();
	    var_def *vd = var_def::scan_from_annote(an, this);
	    add_def(vd);
	    delete an;
	}

	/* read the next instruction */
	delete mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    /* read the type annotations */
    while ((an = mrk->annotes()->get_annote(k_type_annotes))) {

	type_node *t = an->immeds()->pop().type();
	delete an;
	t->mrk_to_annotes(mrk);

	/* read the next instruction */
	delete mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    /* read the symbol annotations */
    while ((an = mrk->annotes()->get_annote(k_sym_annotes))) {

	sym_node *sn = an->immeds()->pop().symbol();
	delete an;
	sn->mrk_to_annotes(mrk);

	/* read the next instruction */
	delete mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    /* read the var_def annotations */
    while ((an = mrk->annotes()->get_annote(k_var_def_annotes))) {

	var_sym *v = (var_sym *)an->immeds()->pop().symbol();
	assert(v->is_var());
	delete an;
	var_def *vd = lookup_var_def(v);
	assert_msg(vd, ("base_symtab::read - cannot find var_def for '%s'",
			v->name()));
	vd->mrk_to_annotes(mrk);

	/* read the next instruction */
	delete mrk;
	mrk = instruction::read(is, this, NULL);
	assert(mrk->opcode() == io_mrk);
    }

    /* read the base_symtab mark (required) */
    an = mrk->annotes()->get_annote(k_base_symtab);
    assert_msg(an, ("base_symtab::read - missing base symtab mark"));
    delete an;

    an = mrk->annotes()->get_annote(k_next_sym_id);
    assert_msg(an, ("base_symtab::read - missing next sym_id annotation"));
    next_sym_id = an->immeds()->pop().unsigned_int();
    delete an;

    an = mrk->annotes()->get_annote(k_next_type_id);
    assert_msg(an, ("base_symtab::read - missing next type_id annotation"));
    next_type_id = an->immeds()->pop().unsigned_int();
    delete an;

    /* read the current value of the var_counter */
    an = mrk->annotes()->get_annote(k_var_counter);
    assert_msg(an, ("base_symtab::read - missing var counter annotation"));
    var_counter = (*an->immeds())[0].unsigned_int();
    delete an;

    delete mrk;

    /* read the symtab annotations */
    mrk = instruction::read(is, this, NULL);
    assert(mrk->opcode() == io_mrk);
    mrk_to_annotes(mrk);
    delete mrk;
}


/*****************************************************************************/


global_symtab::global_symtab (const char *n)
    : base_symtab(n)
{
    next_sym_id = first_global_id;
    next_type_id = first_global_id;
}


/*
 *  Create a new procedure symbol and add it to this symbol table.
 *  The procedure name should be unique, but this is not checked.
 */

proc_sym *
global_symtab::new_proc (func_type *r, src_lang_type src, const char *n)
{ 
    proc_sym *p = new proc_sym(r, src, n);
    add_sym(p);
    return p;
}



/*
 *  Add a symbol.  This function overrides the base class function to make
 *  sure that global variable symbols default to be "extern".
 */

void
global_symtab::add_sym (sym_node *s)
{
    if (s->is_var()) {
	var_sym *v = (var_sym *)s;
	if (!v->has_var_def()) set_sym_extern(v, TRUE);
    }

    base_symtab::add_sym(s);
}



/*
 *  Number global symbols and types that do not yet have ID numbers.
 */

void
global_symtab::number_globals ()
{
    sym_node_list_iter snli(symbols());
    while (!snli.is_empty()) {
	sym_node *sym = snli.step();
	if (sym->sym_id() == 0) {
	    assert_msg(!lookup_sym_id(next_sym_id),
		       ("global_symtab::number_globals - ID already used"));
	    set_sym_id(sym, next_sym_id++);
            if (sym->sym_id() != 0)
                sym_id_index->enter((void *)(sym->sym_id()), sym);
	}
    }

    type_node_list_iter tnli(types());
    while (!tnli.is_empty()) {
	type_node *typ = tnli.step();
	if (typ->type_id() == 0) {
	    typ->set_type_id(next_type_id++);
            if (typ->type_id() != 0)
                type_id_index->enter((void *)(typ->type_id()), typ);
	    assert_msg(!lookup_type_id(next_type_id),
		       ("global_symtab::number_globals - ID already used"));
	}
    }
}


/*
 *  Print a global_symtab.
 */

void
global_symtab::print (FILE *fp, int depth)
{
    if (_suif_no_symtabs) return;

    suif_indent(fp, depth);
    fprintf(fp, "Global symbol table: ");
    print_contents(fp, depth);
}


/*
 *  Write out a global symtab.  First assign ID numbers to symbols and types
 *  that are not yet numbered, then call the base class write method to write
 *  the contents.
 */

void
global_symtab::write (out_stream *os)
{
    /* assign ID numbers to new symbols and types */
    number_globals();

    /* write out the contents */
    base_symtab::write(os);
}


/*
 *  Predefine some basic types.  This is called for the root symbol table
 *  by the file_set constructor.
 */

void
global_symtab::predefine_types ()
{
    type_v0 = install_type(new base_type(TYPE_VOID, 0));
    type_s8 = install_type(new base_type(TYPE_INT, 8, TRUE));
    type_s16 = install_type(new base_type(TYPE_INT, 16, TRUE));
    type_s32 = install_type(new base_type(TYPE_INT, 32, TRUE));
    type_s64 = install_type(new base_type(TYPE_INT, 64, TRUE));
    type_u8 = install_type(new base_type(TYPE_INT, 8, FALSE));
    type_u16 = install_type(new base_type(TYPE_INT, 16, FALSE));
    type_u32 = install_type(new base_type(TYPE_INT, 32, FALSE));
    type_u64 = install_type(new base_type(TYPE_INT, 64, FALSE));
    type_f32 = install_type(new base_type(TYPE_FLOAT, 32));
    type_f64 = install_type(new base_type(TYPE_FLOAT, 64));
    type_f128 = install_type(new base_type(TYPE_FLOAT, 128));

    /* set the "portable" types */
    type_void = type_v0;
    type_ptr = install_type(new ptr_type(type_v0));
    type_char = install_type(new base_type(TYPE_INT, target.size[C_char],
					   target.char_is_signed));
    type_signed_char = install_type(new base_type(TYPE_INT,
						  target.size[C_char],
						  TRUE));
    type_unsigned_char = install_type(new base_type(TYPE_INT,
						    target.size[C_char],
						    FALSE));
    type_signed_short =
	install_type(new base_type(TYPE_INT, target.size[C_short], TRUE));
    type_unsigned_short =
	install_type(new base_type(TYPE_INT, target.size[C_short], FALSE));
    type_signed =
	install_type(new base_type(TYPE_INT, target.size[C_int], TRUE));
    type_unsigned =
	install_type(new base_type(TYPE_INT, target.size[C_int], FALSE));
    type_signed_long =
	install_type(new base_type(TYPE_INT, target.size[C_long], TRUE));
    type_unsigned_long =
	install_type(new base_type(TYPE_INT, target.size[C_long], FALSE));
    type_signed_longlong =
	install_type(new base_type(TYPE_INT, target.size[C_longlong], TRUE));
    type_unsigned_longlong =
	install_type(new base_type(TYPE_INT, target.size[C_longlong], FALSE));
    type_ptr_diff =
        install_type(new base_type(TYPE_INT, target.size[target.ptr_diff_type],
                                   TRUE));
    type_float = install_type(new base_type(TYPE_FLOAT, target.size[C_float]));  type_double =
	install_type(new base_type(TYPE_FLOAT, target.size[C_double]));
    type_longdouble =
	install_type(new base_type(TYPE_FLOAT, target.size[C_longdouble]));

    number_globals();
}


/*****************************************************************************/


file_symtab::file_symtab(const char *n, file_set_entry *fse)
    : global_symtab(n)
{
    fset = fse;

    /* overwrite the ID numbers set in the global_symtab constructor */
    next_sym_id = first_file_id;
    next_type_id = first_file_id;
}


/*
 *  Print a file symtab.
 */

void
file_symtab::print (FILE *fp, int depth)
{
    if (_suif_no_symtabs) return;

    suif_indent(fp, depth);
    fprintf(fp, "File symbol table: ");
    print_contents(fp, depth);
}


/*****************************************************************************/


block_symtab::block_symtab (const char *n)
    : base_symtab(n)
{
    /* the block pointer will be set by the block */
    blk = NULL;

    label_counter = 1;

    /* the next_sym_id and next_type_id counters are not used here */
}


/*
 *  Create a new block symtab with a unique name and add it as a child.
 *  The "base" name is optional; if not provided a default name is used.
 *  Since we do not anticipate large numbers of new blocks, the counter
 *  for this method is not saved--it restarts at zero each time.
 */

block_symtab *
block_symtab::new_unique_child (const char *base)
{
    char bfr[1024];
    unsigned counter = 0;

    /* provide a default base for the symtab name */
    if (!base) base = "block";

    while (TRUE) {
	sprintf(bfr, "%s%u", base, counter++);
	if (!lookup_child(bfr)) break;
    }

    block_symtab *s = new block_symtab(bfr);
    add_child(s);
    return s;
}


/*
 *  Create a new label symbol and add it to this symbol table.  The name
 *  should be unique, but if not it will be automatically renamed before
 *  the table is written out.
 */

label_sym *
block_symtab::new_label (const char *n)
{
    label_sym *l = new label_sym(n);
    add_sym(l);
    return l;
}


/*
 *  Create a new label symbol with a unique name and add it to this symtab.
 *  If the "base" name is not provided, a default name is used.
 */

label_sym *
block_symtab::new_unique_label (const char *base)
{
    char bfr[1024];
    
    /* provide a default base for the label name */
    if (!base) base = "L";

    /* add a number to the base name and make sure it's unique */
    while (TRUE) {
	sprintf(bfr, "%s%u", base, label_counter++);
	if (!lookup_label(bfr)) break;
    }

    return new_label(bfr);
}


/*
 *  Add a symbol.  This function overrides the base class function to make
 *  sure that auto variable symbols are not marked "extern".
 */

void
block_symtab::add_sym (sym_node *s)
{
    if (s->is_var()) {
	set_sym_extern(s, FALSE);
    }

    base_symtab::add_sym(s);
}



/*
 *  Print a block symtab.
 */

void
block_symtab::print (FILE *fp, int depth)
{
    if (_suif_no_symtabs) return;

    suif_indent(fp, depth);
    fprintf(fp, "Block symbol table: ");
    print_contents(fp, depth);
}


/*
 *  Write to a SUIF file.  The base class write method is called to write out
 *  the contents.  After this, a "block_symtab" mark is written to indicate
 *  that this is a block_symtab and to record the label counter.
 */

void
block_symtab::write (out_stream *os)
{
    /* write out the types and symbols */
    base_symtab::write(os);

    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_block_symtab);
    mrk->annotes()->append(an);
    
    /* write the label counter */
    an = new annote(k_label_counter);
    an->immeds()->push(immed(label_counter));
    mrk->annotes()->append(an);

    mrk->write(os);
    delete mrk;
}


/*
 *  Read a block symtab from a SUIF file.  After reading the contents of
 *  the symtab using the base class read method, the label counter is read
 *  from the "block_symtab" mark instruction.
 */

void
block_symtab::read (in_stream *is)
{
    /* read the types and symbols */
    base_symtab::read(is);

    instruction *mrk = instruction::read(is, this, NULL);
    assert(mrk->opcode() == io_mrk);

    annote *an = mrk->annotes()->get_annote(k_block_symtab);
    assert_msg(an, ("missing block symtab"));
    delete an;

    an = mrk->annotes()->get_annote(k_label_counter);
    label_counter = an->immeds()->pop().unsigned_int();
    delete an;

    delete mrk;
}


/*
 *  Find exposed references in a symbol table.
 */

void
block_symtab::find_exposed_refs (base_symtab *dst_scope, replacements *r)
{
    /* First add anything that is referenced within the symtab.  This is
       a really naive solution because it includes the symbols, types, and
       var_defs that are defined in the symtab, even though those objects
       will be immediately removed from the list.  */

    sym_node_list_iter snli(symbols());
    while (!snli.is_empty()) {
	sym_node *sn = snli.step();
	r->add_sym_ref(sn, dst_scope);
    }

    type_node_list_iter tnli(types());
    while (!tnli.is_empty()) {
	type_node *tn = tnli.step();
	r->add_type_ref(tn, dst_scope);
    }

    var_def_list_iter vdli(var_defs());
    while (!vdli.is_empty()) {
	var_def *vd = vdli.step();
	r->add_def_ref(vd, dst_scope);
    }

    find_annote_refs(dst_scope, r);

    /* now remove symbols, types, and var_defs that are defined here */

    snli.reset(&r->oldsyms);
    while (!snli.is_empty()) {
	sym_node *sn = snli.step();
	if (sn->parent() == this) {
	    delete r->oldsyms.remove(snli.cur_elem());
	}
    }

    tnli.reset(&r->oldtypes);
    while (!tnli.is_empty()) {
	type_node *tn = tnli.step();
	if (tn->parent() == this) {
	    delete r->oldtypes.remove(tnli.cur_elem());
	}
    }

    vdli.reset(&r->olddefs);
    while (!vdli.is_empty()) {
	var_def *vd = vdli.step();
	if (var_defs()->lookup(vd)) {
	    delete r->olddefs.remove(vdli.cur_elem());
	}
    }

    /* finally, update the exposed references to parent symbol tables */

    base_symtab_list_iter bsli(&r->oldtabs);
    while (!bsli.is_empty()) {
	base_symtab *bs = bsli.step();
	if (bs == this) {
	    delete r->oldtabs.remove(bsli.cur_elem());
	}
    }
    if (dst_scope != NULL)
        r->add_tab_ref(parent());
}


/*
 *  Clone a block symtab.  If the parent has already been cloned, the new
 *  symtab must be added as a child of the parent's replacement.  After
 *  cloning the contents of the symtab, the new symtab is added to the list
 *  of replacements.
 */

block_symtab *
block_symtab::clone_helper (replacements *r, boolean no_copy)
{
    return (block_symtab *)(base_symtab::clone_helper(r, no_copy));
}


/*
 *  Number the local symbols and types that do not yet have ID numbers
 *  in this symtab and its descendants.  Since the ID numbers need to be
 *  unique within the entire procedure, we don't use the next_*id counters
 *  in the nested symtabs.  Instead the counters from the proc_symtab are
 *  used for all of the blocks within the procedure.
 */

void
block_symtab::number_locals (unsigned *next_sym_idp, unsigned *next_type_idp)
{
    sym_node_list_iter snli(symbols());
    while (!snli.is_empty()) {
	sym_node *sym = snli.step();
	if (sym->sym_id() == 0) {
            assert_msg(!lookup_sym_id(*next_sym_idp),
		       ("block_symtab::number_locals - ID already used"));
            set_sym_id(sym, (*next_sym_idp)++);
            if (sym->sym_id() != 0)
                sym_id_index->enter((void *)(sym->sym_id()), sym);
        }
    }

    type_node_list_iter tnli(types());
    while (!tnli.is_empty()) {
	type_node *typ = tnli.step();
	if (typ->type_id() == 0) {
            assert_msg(!lookup_type_id(*next_type_idp),
		       ("block_symtab::number_locals - ID already used"));
            typ->set_type_id((*next_type_idp)++);
            if (typ->type_id() != 0)
                type_id_index->enter((void *)(typ->type_id()), typ);
        }
    }

    base_symtab_list_iter bsli(children());
    while (!bsli.is_empty()) {
	block_symtab *st = (block_symtab *)bsli.step();
	assert_msg(st->is_block(),
		   ("block_symtab::number_locals - child is not a block"));
	st->number_locals(next_sym_idp, next_type_idp);
    }
}


/*****************************************************************************/


proc_symtab::proc_symtab (const char *n)
    : block_symtab(n)
{
    next_inum = 1;
    next_sym_id = first_local_id;
    next_type_id = first_local_id;
    prms = new sym_node_list;
}


proc_symtab::~proc_symtab ()
{
    /*  The symbols on the parameter list are also on the syms list and will
	be deleted when they are removed from that list.  */
    delete params();
}


/*
 *  Number the symbols and types that do not yet have ID numbers in this
 *  symtab and its descendants.  The next_*id counters from the proc_symtab
 *  are used for all of the blocks within the procedure.
 */

void
proc_symtab::number_locals ()
{
    block_symtab::number_locals(&next_sym_id, &next_type_id);
}


/*
 *  Write to a SUIF file.  After assigning numbers to symbols and types that
 *  do not yet have ID numbers, the block_symtab write method is called to
 *  write out the contents.  After this, a "proc_symtab" mark is written to
 *  indicate that this is a proc_symtab and to record the next instruction
 *  number and the parameter list.
 */

void
proc_symtab::write (out_stream *os)
{
    /* number symbols and types */
    number_locals();

    /* write out the types and symbols */
    block_symtab::write(os);

    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_proc_symtab);
    mrk->annotes()->append(an);
    
    /* write the next instruction number */
    an = new annote(k_next_inum);
    an->immeds()->push(immed(next_inum));
    mrk->annotes()->append(an);

    /* write out the list of parameters */
    unsigned param_num = 0;
    sym_node_list_iter sli(params());
    while (!sli.is_empty()) {
	var_sym *s = (var_sym *)sli.step();
	assert(s->is_var());
	an = new annote(k_param);
	an->immeds()->append(immed(param_num++));
	an->immeds()->append(immed(s));
	mrk->annotes()->append(an);
    }

    mrk->write(os);
    delete mrk;
}


/*
 *  Read a procedure symtab from a SUIF file.  After reading the contents of
 *  the symtab using the block_symtab read method, the "proc_symtab" mark is
 *  read and the next instruction number and the parameter list are read from
 *  the mark.
 */

void
proc_symtab::read (in_stream *is)
{
    /* read the types and symbols */
    block_symtab::read(is);

    instruction *mrk = instruction::read(is, this, NULL);
    assert(mrk->opcode() == io_mrk);

    annote *an = mrk->annotes()->get_annote(k_proc_symtab);
    assert_msg(an, ("missing procedure symtab"));
    delete an;

    an = mrk->annotes()->get_annote(k_next_inum);
    next_inum = an->immeds()->pop().unsigned_int();
    delete an;

    /* read the parameter list */
    unsigned param_num = 0;
    while ((an = mrk->annotes()->get_annote(k_param))) {
	assert_msg(an->immeds()->pop().unsigned_int() == param_num++,
		   ("parameter out-of-order"));
	sym_node *v = an->immeds()->pop().symbol();
	prms->append(v);
	delete an;
    }

    delete mrk;
}


/*
 *  Print a procedure symbol table, including the list of parameters.
 */

void
proc_symtab::print (FILE *fp, int depth)
{
    if (_suif_no_symtabs) return;

    suif_indent(fp, depth);
    fprintf(fp, "Procedure symbol table: ");
    print_contents(fp, depth);

    /* print the parameter list */
    suif_indent(fp, depth+1);
    fprintf(fp, "Parameters:\n");
    if (params()->is_empty()) {
	suif_indent(fp, depth+2);
	fprintf(fp, "<None>\n");
    } else {
	sym_node_list_iter snli(params());
	while (!snli.is_empty()) {
	    suif_indent(fp, depth+2);
	    snli.step()->print(fp);
	    putc('\n', fp);
	}
    }
}


/*
 *  Clone a procedure symtab.  The new symtab needs a unique name; procedure
 *  symtabs usually have the same name as the procedure itself.  However, at
 *  this point, we can't know what the procedure name will be, so instead we
 *  just append a counter to the old name.  The user can then either use this
 *  name as the name of the procedure or change the name to something else.
 */

proc_symtab *
proc_symtab::clone_helper (replacements *r, boolean no_copy)
{
    proc_symtab *result =
	(proc_symtab *)(base_symtab::clone_helper(r, no_copy));

    /* clone the parameter list */
    sym_node_list_iter snli(params());
    while (!snli.is_empty()) {
	sym_node *sn = snli.step();
	sn = sn->clone_helper(r);
	if (no_copy) {
	    snli.cur_elem()->contents = sn;
	} else {
	    result->params()->append(sn);
	}
    }

    return result;
}


