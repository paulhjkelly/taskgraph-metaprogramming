/*  SUIF Object Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "suifobj.h"

#define RCS_BASE_FILE suifobj_cc

#include "suif1.h"

RCS_BASE(
    "$Id$")


// override stupid defaults
suif_object::suif_object(const suif_object &) : anl(0) { assert(FALSE); }
suif_object &suif_object::operator=(const suif_object &)  { assert(FALSE); return *this; }

suif_object::~suif_object ()
{
    delete anl;
}


/*
 *  Check if the annotation list exists and is non-empty.
 */

boolean
suif_object::are_annotations ()
{
    if (anl == NULL) return FALSE;
    if (anl->is_empty()) return FALSE;
    return TRUE;
}


/*
 *  Retrieve the annotation list.  If it does not yet exist, a new
 *  list is automatically created.  This just saves storage by allowing
 *  us to only allocate the lists when they are actually needed.
 */

annote_list *
suif_object::annotes ()
{
    if (!anl)
	anl = new annote_list;

    return anl;
}


/*
 *  Add new annotations.
 */

void
suif_object::append_annote (const char *name, void *data)
{
    annotes()->append(new annote(name, data));
}


void
suif_object::prepend_annote (const char *name, void *data)
{
    annotes()->push(new annote(name, data));
}


void
suif_object::set_annote (const char *name, void *data)
{
    assert_msg(name, ("Need annotation name: have you initialized?"));
    annote *new_an = new annote(name, data);

    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {
	annote *an = anli.step();
	if (an->name() == name) {
	    /* replace the existing annotation */
	    delete an;
	    anli.cur_elem()->contents = new_an;
	    return;
	}
    }
    
    annotes()->append(new_an);
}


/*
 *  Lookup annotation data
 */

void *
suif_object::get_annote (const char *name)
{
    void *result = NULL;

    if (are_annotations()) {
        assert(name != NULL);
	annote *an = annotes()->get_annote(name);
	if (an) {
	    result = an->data();
	    an->set_data(NULL);
	    an->set_name(NULL);
	    delete an;
	}
    }

    return result;
}


void *
suif_object::peek_annote (const char *name)
{
    if (are_annotations()) {
        assert(name != NULL);
	annote *an = annotes()->peek_annote(name);
	if (an)
	    return an->data();
	return NULL;
    }

    return NULL;
}


/*
 *  Print annotation list
 */

void
suif_object::print_annotes (FILE *fp, int depth)
{
    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {
	putc('\n', fp);
	suif_indent(fp, depth);
	anli.step()->print(fp);
    }
}


/*
 *  Count the number of annotations that will be written out.
 */

int
suif_object::num_output_annotes ()
{
    int num_real_annotes = 0;

    if (are_annotations()) {
	annote_list_iter ali(annotes());
	while (!ali.is_empty()) {
	    annote *an = ali.step();
	    annote_def *ad = lookup_annote(an->name());
	    if (ad && ad->output()) {
		num_real_annotes += 1;
	    }
	}
    }
    return num_real_annotes;
}


/*
 *  Move annotations to/from a mark instruction.  These methods are used in
 *  handling suif i/o and in the programs to convert to/from old SUIF.  There
 *  is no reason for users to care about them.
 */

void
suif_object::annotes_to_mrk (instruction *mrk)
{
    /* transfer the annotations to the mark */
    if (are_annotations()) {
	while (!annotes()->is_empty()) {
	    annote *an = annotes()->pop();
	    mrk->annotes()->append(an);
	}
    }
}


void
suif_object::mrk_to_annotes (instruction *mrk)
{
    /* tranfer the annotations from the mark to this object */
    if (mrk->are_annotations()) {
	while (!mrk->annotes()->is_empty()) {
	    annote *an = mrk->annotes()->pop();
	    annotes()->append(an);
	}
    }
    convert_annotes();
}


/*
 *  Convert annotations to their structured forms.  This method is called
 *  shortly after the annotations have been read from a file.
 */

void
suif_object::convert_annotes ()
{
    if (!are_annotations()) return;

    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {

	annote *an = anli.step();
	annote_def *adef = lookup_annote(an->name());

	/* ignore unregistered annotations */
	if (adef && adef->is_structured()) {
	    /* convert structured annotations */
	    cvt_from_imm_f fromf = ((struct_annote_def *)adef)->from();
	    assert_msg(fromf != NULL,
		       ("suif_object::convert_annotes - '%s' is a structured"
			" annotation and appears in the input file, but no "
			"function is provided to convert from immeds",
			an->name()));
	    immed_list *iml = (immed_list *)an->data();
	    an->set_data(fromf(an->name(), iml, this));
	    delete iml;
	}
    }
}


/*
 *  Copy the annotations from this object to another suif_object.
 *  Unregistered annotations are ignored.
 */

void
suif_object::copy_annotes (suif_object *target)
{
    if (!are_annotations()) return;

    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {

	annote *an = anli.step();
	immed_list *iml = an->immeds();

	/* ignore unregistered annotations */
	if (!iml) continue;

	/* copy the immed values */
	immed_list *new_iml = iml->clone();

	/* create a new annotation */
	annote *new_an = new annote(an->name(), (void *)new_iml);

	new_an->set_immeds(new_iml, this);

	if (an->is_structured()) {
	    delete iml;
	    delete new_iml;
	}

	target->annotes()->append(new_an);
    }
}


/*
 *  Clone the annotation list.  Because annotations may contain references
 *  to objects (e.g. local symbols) that have been replaced, they cannot be
 *  blindly copied.  Instead, structured annotations are converted to flat
 *  lists.  Then each immed value in the annotation can be converted.
 *  Unregistered annotations are not cloned.
 */

void
suif_object::clone_annotes (suif_object *target, replacements *r,
			    boolean no_copy)
{
    if (!are_annotations()) return;

    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {

	annote *an = anli.step();
	immed_list *iml = an->immeds();

	/* ignore unregistered annotations */
	if (!iml) continue;

	/* clone the immed values */
	immed_list *new_iml = iml->clone_helper(r, FALSE);

	/* create a new annotation */
	annote *new_an = new annote(an->name(), (void *)new_iml);

	new_an->set_immeds(new_iml, this);

	if (an->is_structured()) {
	    delete iml;
	    delete new_iml;
	}

	if (no_copy) {
	    anli.cur_elem()->contents = new_an;
	    delete an;
	} else {
	    target->annotes()->append(new_an);
	}
    }
}


/*
 *  Find the exposed references in the list of annotations on a suif object.
 */

void
suif_object::find_annote_refs (base_symtab *dst_scope, replacements *r)
{
    if (!are_annotations()) return;

    annote_list_iter anli(annotes());
    while (!anli.is_empty()) {

	annote *an = anli.step();
	immed_list *iml = an->immeds();

	/* ignore unregistered annotations */
	if (!iml) continue;

	/* check for exposed references in the immed values */
	iml->find_exposed_refs(dst_scope, r);

	if (an->is_structured()) {
	    delete iml;
	}
    }
}

