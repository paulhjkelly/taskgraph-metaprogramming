/* file "string_index.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef STRING_INDEX_H
#define STRING_INDEX_H

#pragma interface

RCS_HEADER(string_index_h,
    "$Id$")


class si_entry
  {
    friend class string_index;
    friend class ptr_index;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    si_entry(const si_entry &);
    si_entry &operator=(const si_entry &);

    void *the_data;
    void *back_pointer;

    si_entry(void *init_data) : the_data(init_data), back_pointer(0)
	{ }
    ~si_entry(void)  { }

public:
    void *data_value(void)  { return the_data; }
  };

class string_index
  {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    string_index(const string_index &);
    string_index &operator=(const string_index &);

protected:
    string_index(void)  { }

    si_entry *create_entry(void *init_data)
      { return new si_entry(init_data); }
    void destroy_entry(si_entry *old_entry)  { delete old_entry; }
    void *get_back_pointer(si_entry *the_entry)
      { return the_entry->back_pointer; }
    void set_back_pointer(si_entry *the_entry, void *new_value)
      { the_entry->back_pointer = new_value; }

public:
    virtual ~string_index(void)  { }

    virtual si_entry *enter(const char *the_string, void *the_data = NULL) = 0;
    virtual si_entry *lookup_entry(const char *the_string) = 0;
    virtual boolean exists(const char *the_string) = 0;
    virtual void *lookup(const char *the_string) = 0;
    virtual void remove_entry(si_entry *the_entry) = 0;
  };

#endif /* STRING_INDEX_H */
