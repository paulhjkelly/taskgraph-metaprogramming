/*  Annotation Manager Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "aman.h"

#define RCS_BASE_FILE aman_cc

#include "misc.h"
#include "glist.h"
#include "stringtable.h"
#include "string_index.h"
#include "tree_string_index.h"
#include "aman.h"
#include "dlist.h"

RCS_BASE(
    "$Id$")

DECLARE_DLIST_CLASS(annote_def_list, annote_def *);

static string_index *registered_annotes;
static annote_def_list *registration_list;

// override default copy construct and assignment op
annote_def::annote_def(const annote_def &) { assert(FALSE); }
annote_def &annote_def::operator=(const annote_def &)  { assert(FALSE); return *this; }

/*
 *  Initialize the annotation manager.  This function must be called before
 *  the manager is first used.
 */

void
init_aman ()
{
    /* create a list to hold the registered annotations */
    registered_annotes = new tree_string_index;
    registration_list = new annote_def_list;
}


/*
 *  Deallocate the annotation manager's data structures.
 */

void
free_aman ()
{
    delete registered_annotes;
    while (!registration_list->is_empty())
      {
        annote_def *this_def = registration_list->pop();
        delete this_def;
      }
    delete registration_list;
}


/*
 *  Register an annotation with the manager.  The name in the annote_def must
 *  be an entry in the lexicon and it must not have already been registered
 *  as an annotation name; otherwise, an error will occur.
 */

void
register_annote (annote_def *def)
{
    if (registered_annotes->exists(def->name()))
      {
        error_line(1, NULL, "Attempt to register annote name '%s' twice",
                   def->name());
      }
    registered_annotes->enter(def->name(), def);
    registration_list->append(def);
}


/*
 *  Check if an annotation name is registered.  The name parameter must be
 *  an entry in the lexicon.  If the name is not found in the manager, the
 *  return value is NULL.
 */

annote_def *
lookup_annote (const char *name)
{
    if (name == NULL)
        return NULL;
    return (annote_def *)(registered_annotes->lookup(name));
}


/*****************************************************************************/



annote_def::annote_def (const char *n, boolean b)
{
    nm = n ? lexicon->enter(n)->sp : NULL;
    out = b;
}


struct_annote_def::struct_annote_def (const char *n, boolean b,
				      cvt_from_imm_f f, cvt_to_imm_f t,
				      ann_free_f d, ann_print_f p)
    : annote_def(n, b), from_f(f), to_f(t), free_f(d), pr_f(p)
{
    assert((t != NULL) || (!b));
}
