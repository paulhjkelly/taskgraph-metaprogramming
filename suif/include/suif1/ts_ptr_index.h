/* file "ts_ptr_index.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef TS_PTR_INDEX_H
#define TS_PTR_INDEX_H

#pragma interface

RCS_HEADER(ts_ptr_index_h,
    "$Id$")


class tspi_node;

class ts_ptr_index : public ptr_index
  {
    friend class tspi_node;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    ts_ptr_index(const ts_ptr_index &);
    ts_ptr_index &operator=(const ts_ptr_index &);

private:
    string_index *the_string_index;
    char *buffer;

public:
    ts_ptr_index(void);
    ~ts_ptr_index(void)  { delete the_string_index; delete[] buffer; }

    si_entry *enter(void *the_ptr, void *the_data = NULL);
    si_entry *lookup_entry(void *the_ptr);
    boolean exists(void *the_ptr);
    void *lookup(void *the_ptr);
    void remove_entry(si_entry *the_entry);

    void development_dump(FILE *fp = stderr,
                          void (*node_func)(void *data, FILE *fp) = NULL);
    void development_internals_dump(FILE *fp = stderr);
    void development_stats_dump(FILE *fp = stderr);
  };

#endif /* TS_PTR_INDEX_H */
