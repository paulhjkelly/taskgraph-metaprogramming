/* file "marker.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of temporary marking routines for the
 * SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE marker_cc

#include "useful_internal.h"
#include <climits>

RCS_BASE(
    "$Id$")


unsigned long temp_marker::next_id_num = 0;

temp_marker::temp_marker(void)
  {
    static char buffer[16 + sizeof(unsigned long) * 3];

    if (next_id_num == ULONG_MAX)
        error_line(1, NULL, "out of temporary id numbers");
    this_id = next_id_num;
    ++next_id_num;
    sprintf(buffer, "useful marker %lu", this_id);
    /* Note: Do not register this annotation name, since it will not
     * point to an immed_list. */
    annote_name = lexicon->enter(buffer)->sp;
  }

temp_marker::~temp_marker(void)
  {
    clear_marks();
  }

void temp_marker::mark(suif_object *the_object)
  {
    if (is_marked(the_object))
        return;
    suif_object_list_e *new_e =
            marked_list.append(new suif_object_list_e(the_object));
    the_object->append_annote(annote_name, new_e);
  }

void temp_marker::unmark(suif_object *the_object)
  {
    suif_object_list_e *old_e =
            (suif_object_list_e *)(the_object->get_annote(annote_name));
    if (old_e != NULL)
      {
        marked_list.remove(old_e);
        delete old_e;
      }
  }

boolean temp_marker::is_marked(suif_object *the_object)
  {
    return (the_object->peek_annote(annote_name) != NULL);
  }

void temp_marker::clear_marks(void)
  {
    while (!marked_list.is_empty())
      {
        suif_object *this_object = marked_list.pop();
        this_object->get_annote(annote_name);
      }
  }
