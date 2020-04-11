/*  Annotation Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "annote.h"

#define RCS_BASE_FILE annote_cc

#define DEFINE_ANNOTES

#include "suif1.h"

RCS_BASE(
    "$Id$")


// override stupid defaults
annote::annote(const annote &) : nm(0), dt(0) { assert(FALSE); }
annote &annote::operator=(const annote &)  { assert(FALSE);  return *this; }

/*
 *  Read an annotation from an input file.  All annotations are
 *  initially read as flat immed lists.  If the annotation name is
 *  not recognized, it is registered as a flat annotation.
 */

annote::annote (in_stream *is, base_symtab *syms)
{
    /* read the name */
    nm = is->read_cached();

    /* read the immed list */
    int num_immeds = is->read_int();
    immed_list *iml = new immed_list;
    for (int n = 0; n < num_immeds; n++) {
	iml->append(immed(is, syms));
    }
    dt = iml;

    annote_def *ad = lookup_annote(nm);
    if (!ad) {
	/* if the manager does not recognize the annotation name,
	   it is registered now as a flat annotation */
	register_annote(new annote_def(nm, TRUE));
    }
}


/*
 *  Annotation constructor.  The name must have already been entered
 *  in the lexicon.  If the initial data for a flat annotation is NULL,
 *  create an empty immed_list to be used instead.
 */

annote::annote (const char *n, void *d) : nm(n), dt(d)
{

    /* for flat annotations, convert NULL to an empty immed_list */
    if (!dt) {
	annote_def *ad = lookup_annote(nm);
	if (ad && !ad->is_structured()) {
	    dt = new immed_list;
	}
    }
}


/*
 *  Annotation destructor.  Flat annotations are easily handled by
 *  deleting the immed list.  The data contained in unregistered
 *  annotations and structured annotations without "free" functions
 *  cannot be deleted because its type cannot be determined; if
 *  necessary, this data must be deallocated by the user.
 */

annote::~annote ()
{
    /* presumably the name is in the lexicon -- don't delete it */

    annote_def *ad = lookup_annote(name());

    /* can't delete the contents if it's not registered */
    if (!ad) return;

    if (ad->is_structured()) {
	ann_free_f free_f = ((struct_annote_def *)ad)->free();
	if (free_f) free_f(data());
    } else {
	immed_list *immeds = (immed_list *)data();
	if (immeds != NULL)
	    delete immeds;
    }
}


/*
 *  Retrieve the data in an annotation as a list of immed values.
 *  The conversion functions in the manager are used for structured
 *  annotations.  The return value is NULL if the annotation is not
 *  registered.  Note that for structured annotations the result is
 *  a new list, whereas for flat annotations it is a pointer to the
 *  actual data in the annotation.
 */

immed_list *
annote::immeds ()
{
    immed_list *iml;

    annote_def *adef = lookup_annote(name());
    if (!adef) return NULL;

    if (adef->is_structured()) {

	/* convert the data to an immed_list */
	cvt_to_imm_f to_func = ((struct_annote_def *)adef)->to();
	if (to_func != NULL)
	    iml = to_func(name(), data());
	else
	    iml = NULL;

    } else {

	/* handle flat annotations */
	iml = (immed_list *)data();
    }

    return iml;
}


/*
 *  Set the contents of a registered annotation using a list of immed
 *  values.  The conversion functions in the manager are used for
 *  structured annotations.  Note that the existing data is _not_
 *  deallocated!
 */

void
annote::set_immeds (immed_list *iml, suif_object *obj)
{
    annote_def *adef = lookup_annote(name());
    assert_msg(adef, ("annote::set_immeds - '%s' is not registered", name()));

    if (adef->is_structured()) {

	/* convert the data to the structured form */
	cvt_from_imm_f from_func = ((struct_annote_def *)adef)->from();
	assert_msg(from_func != NULL,
		   ("annote::set_immeds - '%s' is registered but has no "
                    "conversion-from-immeds function", name()));
	set_data(from_func(name(), iml, obj));

    } else {

	/* handle flat annotations */
	set_data(iml);
    }
}


/*
 *  Check if an annotation is structured.  This is just a wrapper
 *  function for convenience.
 */

boolean
annote::is_structured ()
{
    annote_def *adef = lookup_annote(name());
    return (adef && adef->is_structured());
}


/*
 *  Writing an annotation to an output file.  Ignore unregistered
 *  annotations and annotations that the manager says are not supposed
 *  to be output.  The data is always written out as an immed list.
 */

void
annote::write (out_stream *os)
{
    /* return if not registered as an output annotation */
    annote_def *ad = lookup_annote(name());
    if (!ad || !ad->output()) return;

    immed_list *iml = immeds();
    assert(iml);

    /* write it out */
    os->write_cached(name());
    os->write_int(iml->count());

    immed_list_iter ili(iml);
    while (!ili.is_empty()) {
	ili.step().write(os);
    }

    if (ad->is_structured()) delete iml;
}


/*
 *  Printing an annotation.  If the annotation is registered with
 *  a print function, that function is used to print it.  Otherwise,
 *  if it is flat or can be converted to an immed_list, it is printed
 *  as an immed_list.  As a last resort, the annotation data field is
 *  is printed directly as a hexadecimal value.
 */

void
annote::print (FILE *fp)
{
    /* first try to use the print function */
    annote_def *ad = lookup_annote(name());
    if (ad && ad->is_structured()) {
	ann_print_f print_func = ((struct_annote_def *)ad)->print();
	if (print_func && !_suif_flat_annotes) {
	    print_func(fp, name(), data());
	    return;
	}
    }

    /* next try to print the annotation as an immed_list */
    immed_list *iml = immeds();
    if (iml) {
	fprintf(fp, "[\"%s\":", name());
	immed_list_iter imli(iml);
	while (!imli.is_empty()) {
	    putc(' ', fp);
	    imli.step().print(fp);
	}
	fprintf(fp, "]");

	if (ad && ad->is_structured()) delete iml;
	return;
    }

    /* just print the data field directly */
    fprintf(fp, "[\"%s\": %p]", name(), data());
}


/*****************************************************************************/


annote_list::~annote_list ()
{
    while (!is_empty()) {
	delete pop();
    }
}


/*
 *  Search through the annotation list to find the first annotation
 *  with the given name.  If found, it is removed from the list and
 *  returned; otherwise, the return value is NULL.  The "name" parameter
 *  must be a string that is entered in the lexicon.
 */

annote *
annote_list::get_annote (const char *name)
{
    assert_msg(name, ("Need annotation name: have you initialized?")); 
    annote_list_iter anli(this);
    while (!anli.is_empty()) {
	annote *an = anli.step();
	if (an->name() == name) {
	    delete remove(anli.cur_elem());
	    return an;
	}
    }

    return NULL;
}


/*
 *  Non-destructively lookup an annotation.  This is just like get_annote
 *  except that if the annotation is found, it is not removed from the list.
 */

annote *
annote_list::peek_annote (const char *name)
{
    assert_msg(name, ("Need annotation name: have you initialized?")); 
    annote_list_iter anli(this);
    while (!anli.is_empty()) {
	annote *an = anli.step();
	if (an->name() == name)
	    return an;
    }

    return NULL;
}


/*****************************************************************************/


/*
 *  Register all of the library's predefined annotations.
 */

void
init_suif_annotes ()
{
    /* annotations visible to users */
    ANNOTE(k_fill, "fill", TRUE);
    ANNOTE(k_repeat_init, "repeat_init", TRUE);
    ANNOTE(k_multi_init, "multi_init", TRUE);
    ANNOTE(k_enable_exceptions, "enable_exceptions", TRUE);
    ANNOTE(k_line, "line", TRUE);
    ANNOTE(k_reg_num, "reg_num", TRUE);
    ANNOTE(k_fields, "fields", TRUE);
    ANNOTE(k_call_by_ref, "call_by_ref", TRUE);
    ANNOTE(k_orig_type, "orig_type", FALSE);
    ANNOTE(k_common_block, "common_block", TRUE);
    ANNOTE(k_common_field, "common_field", FALSE);
    ANNOTE(k_common_shape, "common_shape", TRUE);
    ANNOTE(k_history, "history", TRUE);
    ANNOTE(k_version_history, "version_history", TRUE);
    ANNOTE(k_source_file_name, "source_file_name", TRUE);

    /* annotations used internally */
    ANNOTE(k_int_type, "!int_type!", TRUE);
    ANNOTE(k_float_type, "!float_type!", TRUE);
    ANNOTE(k_void_type, "!void_type!", TRUE);
    ANNOTE(k_ptr_type, "!ptr_type!", TRUE);
    ANNOTE(k_array_type, "!array_type!", TRUE);
    ANNOTE(k_func_type, "!func_type!", TRUE);
    ANNOTE(k_group_type, "!group_type!", TRUE);
    ANNOTE(k_struct_type, "!struct_type!", TRUE);
    ANNOTE(k_union_type, "!union_type!", TRUE);
    ANNOTE(k_enum_type, "!enum_type!", TRUE);
    ANNOTE(k_null_type, "!null_type!", TRUE);
    ANNOTE(k_const_type, "!const_type!", TRUE);
    ANNOTE(k_restrict_type, "!restrict_type!", TRUE);
    ANNOTE(k_volatile_type, "!volatile_type!", TRUE);
    ANNOTE(k_call_by_ref_type, "!call_by_ref_type!", TRUE);
    ANNOTE(k_is_big_endian, "!k_is_big_endian!", TRUE);
    ANNOTE(k_char_is_signed, "!k_char_is_signed!", TRUE);
    ANNOTE(k_addressable_size, "!k_addressable_size!", TRUE);
    ANNOTE(k_C_type_size, "!k_C_type_size!", TRUE);
    ANNOTE(k_C_type_align, "!k_C_type_align!", TRUE);
    ANNOTE(k_array_align, "!k_array_align!", TRUE);
    ANNOTE(k_struct_align, "!k_struct_align!", TRUE);
    ANNOTE(k_ptr_diff_type, "!k_ptr_diff_type!", TRUE);
    ANNOTE(k_base_symtab, "!base_symtab!", TRUE);
    ANNOTE(k_block_symtab, "!block_symtab!", TRUE);
    ANNOTE(k_proc_symtab, "!proc_symtab!", TRUE);
    ANNOTE(k_var_counter, "!var_counter!", TRUE);
    ANNOTE(k_label_counter, "!label_counter!", TRUE);
    ANNOTE(k_next_inum, "!next_inum!", TRUE);
    ANNOTE(k_next_sym_id, "!next_sym_id!", TRUE);
    ANNOTE(k_next_type_id, "!next_type_id!", TRUE);
    ANNOTE(k_param, "!param!", TRUE);
    ANNOTE(k_symbols, "!symbols!", TRUE);
    ANNOTE(k_types, "!types!", TRUE);
    ANNOTE(k_var_defs, "!var_defs!", TRUE);
    ANNOTE(k_sym_annotes, "!sym_annotes!", TRUE);
    ANNOTE(k_type_annotes, "!type_annotes!", TRUE);
    ANNOTE(k_var_def_annotes, "!var_def_annotes!", TRUE);
    ANNOTE(k_labelsym, "!labelsym!", TRUE);
    ANNOTE(k_procsym, "!procsym!", TRUE);
    ANNOTE(k_varsym, "!varsym!", TRUE);
    ANNOTE(k_var_def, "!var_def!", TRUE);
    ANNOTE(k_block, "!block!", TRUE);
    ANNOTE(k_for, "!for!", TRUE);
    ANNOTE(k_loop, "!loop!", TRUE);
    ANNOTE(k_if, "!if!", TRUE);
    ANNOTE(k_list_end, "!list_end!", TRUE);
    ANNOTE(k_file_symtab_ptr, "!file_symtab_ptr!", TRUE);
    ANNOTE(k_proc_in_file, "!proc_in_file!", TRUE);
    ANNOTE(k_file_num, "!file_num!", TRUE);
    ANNOTE(k_clone, "!clone!", TRUE);
    ANNOTE(k_extra_destinations, "!extra_destinations!", TRUE);
    ANNOTE(k_no_destinations, "!no_destinations!", TRUE);
}

