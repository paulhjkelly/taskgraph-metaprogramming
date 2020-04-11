/* file "linkinfo.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of external linkage information routines
 * for the SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE linkinfo_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


const char *k_outlink_set = NULL;
const char *k_outlink_not_out = NULL;


static const char *k_useful_outlink_all_here = NULL;


extern boolean unreferenced_outside_fileset(sym_node *the_sym)
  {
    base_symtab *parent = the_sym->parent();
    if (parent->kind() != SYMTAB_GLOBAL)
        return TRUE;

    if (the_sym->peek_annote(k_outlink_not_out) == NULL)
        return FALSE;

    file_set_entry_list_e *top_e = fileset->file_list()->head();
    if (top_e == NULL)
        return FALSE;
    file_set_entry *top_fse = top_e->contents;

    if (top_fse->peek_annote(k_useful_outlink_all_here) != NULL)
        return TRUE;

    annote *set_annote = top_fse->annotes()->peek_annote(k_outlink_set);
    if (set_annote == NULL)
        return FALSE;
    immed_list_iter num_iter(set_annote->immeds());
    while (!num_iter.is_empty())
      {
        immed this_immed = num_iter.step();
        if (!this_immed.is_int_const())
            error_line(1, NULL, "bad format for \"%s\" annote", k_outlink_set);
        if (!this_immed.is_integer())
            return FALSE;
        if (fileset->find_by_num(this_immed.integer()) == NULL)
            return FALSE;
      }
    top_fse->append_annote(k_useful_outlink_all_here);
    return TRUE;
  }

extern void init_linkinfo(void)
  {
    ANNOTE(k_outlink_set,             "outlink set",             TRUE);
    ANNOTE(k_outlink_not_out,         "outlink not out",         TRUE);
    ANNOTE(k_useful_outlink_all_here, "useful outlink all here", FALSE);
  }
