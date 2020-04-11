/*  file "ts_ptr_index.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "ptr_index.h"
#pragma implementation "ts_ptr_index.h"

#define RCS_BASE_FILE ts_ptr_index_cc

#include "suif1.h"
#include <cstring>

RCS_BASE(
    "$Id$")

// override default copy constructors and assignment ops
ptr_index::ptr_index(const ptr_index &)  { assert(FALSE); }
ptr_index &ptr_index::operator=(const ptr_index &)  { assert(FALSE);return *this; }

ts_ptr_index::ts_ptr_index(const ts_ptr_index &)  { assert(FALSE); }
ts_ptr_index &ts_ptr_index::operator=(const ts_ptr_index &)  { assert(FALSE);return *this; }

ts_ptr_index::ts_ptr_index(void)
  {
    the_string_index = new tree_string_index('0', '9');
    buffer = new char[100];
  }

si_entry *ts_ptr_index::enter(void *the_ptr, void *the_data)
  {
    sprintf(buffer, "%p", the_ptr);
    return the_string_index->enter(buffer, the_data);
  }

si_entry *ts_ptr_index::lookup_entry(void *the_ptr)
  {
    sprintf(buffer, "%p", the_ptr);
    return the_string_index->lookup_entry(buffer);
  }

boolean ts_ptr_index::exists(void *the_ptr)
  {
    sprintf(buffer, "%p", the_ptr);
    return the_string_index->exists(buffer);
  }

void *ts_ptr_index::lookup(void *the_ptr)
  {
    sprintf(buffer, "%p", the_ptr);
    return the_string_index->lookup(buffer);
  }

void ts_ptr_index::remove_entry(si_entry *the_entry)
  {
    the_string_index->remove_entry(the_entry);
  }

void ts_ptr_index::development_dump(FILE *fp,
                              void (*node_func)(void *data, FILE *fp))
  {
    ((tree_string_index *)the_string_index)->development_dump(fp, node_func);
  }

void ts_ptr_index::development_internals_dump(FILE *fp)
  {
    ((tree_string_index *)the_string_index)->development_internals_dump(fp);
  }

void ts_ptr_index::development_stats_dump(FILE *fp)
  {
    ((tree_string_index *)the_string_index)->development_stats_dump(fp);
  }
