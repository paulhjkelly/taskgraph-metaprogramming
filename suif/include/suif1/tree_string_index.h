/* file "tree_string_index.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef TREE_STRING_INDEX_H
#define TREE_STRING_INDEX_H

#pragma interface

RCS_HEADER(tree_string_index_h,
    "$Id$")


class tsi_node;

class tree_string_index : public string_index
  {
    friend class tsi_node;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    tree_string_index(const tree_string_index &);
    tree_string_index &operator=(const tree_string_index &);

private:
    class tsi_node *root_node;
    unsigned long the_list_size;
    char the_expected_lower;
    char the_expected_upper;

    tsi_node *follow_prefix_match(const char *string, unsigned long *prefix_length);
    void remove_entries(tsi_node *base_node);
    void internal_development_dump(char **buffer, unsigned long *buf_size,
                                   unsigned long position, tsi_node *base_node,
                                   FILE *fp,
                                   void (*node_func)(void *data, FILE *fp),
                                   boolean show_inernals);
    void get_development_stats(tsi_node *base_node, unsigned long *entry_count,
                               unsigned long *substring_node_count,
                               unsigned long *char_list_node_count,
                               unsigned long *char_table_node_count,
                               unsigned long *leaf_node_count,
                               unsigned long *substring_char_count,
                               unsigned long *char_list_place_count,
                               unsigned long *char_list_used_count,
                               unsigned long *char_table_place_count,
                               unsigned long *char_table_used_count);

public:
    tree_string_index(char expected_lower = ' ', char expected_upper = '~',
                      unsigned long list_size = 4);
    ~tree_string_index(void);

    si_entry *enter(const char *the_string, void *the_data = NULL);
    si_entry *lookup_entry(const char *the_string);
    boolean exists(const char *the_string);
    void *lookup(const char *the_string);
    void remove_entry(si_entry *the_entry);

    void development_dump(FILE *fp = stderr,
                          void (*node_func)(void *data, FILE *fp) = NULL);
    void development_internals_dump(FILE *fp = stderr);
    void development_stats_dump(FILE *fp = stderr);
  };

#endif /* TREE_STRING_INDEX_H */
