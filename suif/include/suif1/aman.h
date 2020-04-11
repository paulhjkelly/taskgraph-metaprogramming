/*  Annotation Manager Interface */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef AMAN_H
#define AMAN_H

#pragma interface

RCS_HEADER(aman_h,
    "$Id$")

class immed_list;
class suif_object;


/*
 *  The annotation manager is implemented as a list of annote_def objects.
 *  The annote_def base class is used to represent flat annotations and the
 *  struct_annote_def derived class represents structured annotations.
 */


/*  types for functions to handle structured annotations */
typedef immed_list* (*cvt_to_imm_f)(const char *name, void *data);
typedef void* (*cvt_from_imm_f)(const char *name, immed_list *il, suif_object *obj);
typedef void (*ann_print_f)(FILE *out, const char *name, void *data);
typedef void (*ann_free_f)(void *data);


class annote_def {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    annote_def(const annote_def &);
    annote_def &operator=(const annote_def &);

protected:
    const char *nm;
    boolean out;

public:
    annote_def(const char *n,		/* annotation name */
	       boolean b);		/* should these be written out? */
    virtual ~annote_def() { }
    virtual boolean is_structured()	{ return FALSE; }

    const char *name()			{ return nm; }
    boolean output()			{ return out; }

    void set_output(boolean b)		{ out = b; }
};


class struct_annote_def : public annote_def {
private:
    cvt_from_imm_f from_f;
    cvt_to_imm_f to_f;
    ann_free_f free_f;
    ann_print_f pr_f;

public:
    struct_annote_def(const char *n,	/* annotation name */
		      boolean b,	/* should these be written out? */
		      cvt_from_imm_f f,	/* "from" conversion function */
		      cvt_to_imm_f t,	/* "to" conversion function */
		      ann_free_f d,	/* function to free annote storage */
		      ann_print_f p);	/* function to print annotes */

    virtual ~struct_annote_def() { }
    boolean is_structured()		{ return TRUE; }

    cvt_from_imm_f from()		{ return from_f; }
    cvt_to_imm_f to()			{ return to_f; }
    ann_free_f free()			{ return free_f; }
    ann_print_f print()			{ return pr_f; }
};


/*
 *  Macros to register annotations.  These should generally be used instead
 *  of calling register_annote directly.  The VAR parameter should be a global
 *  variable with a name beginning with "k_" followed by the annotation name;
 *  this variable can then be used to refer to the annotation name throughout
 *  a program.  The NAME parameter is a string containing the annotation name.
 *  The OUT parameter should be either TRUE or FALSE to indicate whether the
 *  annotation should be written out.
 */

#define ANNOTE(VAR, NAME, OUT) { \
    VAR = NAME ? lexicon->enter(NAME)->sp : NULL; \
    annote_def *def = new annote_def(VAR, OUT); \
    register_annote(def); }

#define STRUCT_ANNOTE(VAR, NAME, OUT, CFROM, CTO, FREE, PRT) { \
    VAR = NAME ? lexicon->enter(NAME)->sp : NULL; \
    annote_def *def = new struct_annote_def(VAR, OUT, CFROM, CTO, FREE, PRT); \
    register_annote(def); }


/*
 *  Global functions to access the manager.  The initialization function is
 *  called by the library's init_suif function.  The register_annote function
 *  is used in the macros above to enter new annote_defs.  The lookup_annote
 *  function searches through the list for the specified name (which must be
 *  an entry in the lexicon); it returns NULL if the name is not found.  The
 *  free_aman function may be called at the end of a program to deallocate
 *  the manager's data structures.
 */

void init_aman();
void register_annote(annote_def *def);
annote_def *lookup_annote(const char *name);
void free_aman();

#endif /* AMAN_H */
