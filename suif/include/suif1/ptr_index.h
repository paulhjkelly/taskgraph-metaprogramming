/* file "ptr_index.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef PTR_INDEX_H
#define PTR_INDEX_H

#pragma interface

RCS_HEADER(ptr_index_h,
    "$Id$")


class ptr_index
  {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    ptr_index(const ptr_index &);
    ptr_index& operator=(const ptr_index &);

protected:
    ptr_index(void)  { }

    si_entry *create_entry(void *init_data)
      { return new si_entry(init_data); }
    void destroy_entry(si_entry *old_entry)  { delete old_entry; }
    void *get_back_pointer(si_entry *the_entry)
      { return the_entry->back_pointer; }
    void set_back_pointer(si_entry *the_entry, void *new_value)
      { the_entry->back_pointer = new_value; }

public:
    virtual ~ptr_index(void)  { }

    virtual si_entry *enter(void *the_ptr, void *the_data = NULL) = 0;
    virtual si_entry *lookup_entry(void *the_ptr) = 0;
    virtual boolean exists(void *the_ptr) = 0;
    virtual void *lookup(void *the_ptr) = 0;
    virtual void remove_entry(si_entry *the_entry) = 0;
  };

#endif /* PTR_INDEX_H */
