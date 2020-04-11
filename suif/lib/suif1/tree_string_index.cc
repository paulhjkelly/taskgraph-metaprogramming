/*  file "tree_string_index.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "string_index.h"
#pragma implementation "tree_string_index.h"

#define RCS_BASE_FILE tree_string_index_cc

#include "suif1.h"
#include <cstring>

RCS_BASE(
    "$Id$")

enum tsi_node_kind { TSI_SUBSTRING, TSI_CHAR_LIST, TSI_CHAR_TABLE, TSI_LEAF };


class tsi_node
  {
    friend class tsi_substring;
    friend class tsi_char_list;
    friend class tsi_char_table;
    friend class tsi_leaf;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    tsi_node(const tsi_node &)  { assert(FALSE); }
    void operator=(const tsi_node &)  { assert(FALSE); }

    si_entry *the_entry;

protected:
    tsi_node *parent_node;
    char parent_index;

    tsi_node(void) : the_entry(0), parent_node(0), parent_index(0)
	{ }

public:
    virtual ~tsi_node(void)  { }

    virtual tsi_node_kind kind(void) = 0;

    void replace(tsi_node *replacement);
    void remove(void);

    si_entry *base_entry(void)  { return the_entry; }
    void replace_base_entry(si_entry *new_entry, tree_string_index *the_tsi);
    tsi_node *find_branchpoint(void);
  };

class tsi_substring : public tsi_node
  {
private:
    char *the_chars;
    tsi_node *the_child;

public:
    tsi_substring(unsigned long char_count, const char *init_chars,
                  tsi_node *init_child);
    ~tsi_substring(void);

    tsi_node_kind kind(void)  { return TSI_SUBSTRING; }

    char *chars(void)  { return the_chars; }
    tsi_node *child(void)  { return the_child; }

    void replace_child(tsi_node *new_child);
  };

class tsi_char_list : public tsi_node
  {
private:
    unsigned long table_size;
    unsigned long num_entries;
    tsi_node **the_children;
    char *the_chars;

public:
    tsi_char_list(unsigned long init_table_size);
    ~tsi_char_list(void);

    tsi_node_kind kind(void)  { return TSI_CHAR_LIST; }

    unsigned long count(void)  { return num_entries; }
    tsi_node *child(unsigned long child_num)
      { assert(child_num < num_entries); return the_children[child_num]; }
    char which_char(unsigned long child_num)
      { assert(child_num < num_entries); return the_chars[child_num]; }

    void add_child(unsigned long child_num, tsi_node *new_child,
                   char this_char);
    void remove_child(unsigned long child_num);
    void replace_child(unsigned long child_num, tsi_node *new_child);
  };

class tsi_char_table : public tsi_node
  {
private:
    tsi_node **the_children;
    char first_char;
    char last_char;

public:
    tsi_char_table(char init_lower, char init_upper);
    ~tsi_char_table(void);

    tsi_node_kind kind(void)  { return TSI_CHAR_TABLE; }

    tsi_node *child(char which_char)
      {
        if ((which_char >= first_char) && (which_char <= last_char))
            return the_children[which_char - (long)first_char];
        else
            return NULL;
      }
    char lower_char(void)  { return first_char; }
    char upper_char(void)  { return last_char; }

    void replace_child(char which_char, tsi_node *new_child);
  };

class tsi_leaf : public tsi_node
  {
public:
    tsi_leaf(si_entry *init_entry, tree_string_index *the_tsi)
      { replace_base_entry(init_entry, the_tsi); }
    ~tsi_leaf(void)  { }

    tsi_node_kind kind(void)  { return TSI_LEAF; }
  };


void tsi_node::replace(tsi_node *replacement)
  {
    assert(parent_node != NULL);
    switch (parent_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *par_substring = (tsi_substring *)parent_node;
            assert(par_substring->child() == this);
            par_substring->replace_child(replacement);
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *par_char_list = (tsi_char_list *)parent_node;
            unsigned long count = par_char_list->count();
            for (unsigned long child_num = 0; child_num < count; ++child_num)
              {
                if (par_char_list->child(child_num) == this)
                  {
                    par_char_list->replace_child(child_num, replacement);
                    return;
                  }
              }
            assert(FALSE);
            break;
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *par_char_table = (tsi_char_table *)parent_node;
            assert(par_char_table->child(parent_index) == this);
            par_char_table->replace_child(parent_index, replacement);
            break;
          }
        case TSI_LEAF:
            assert(FALSE);
        default:
            assert(FALSE);
      }
  }

void tsi_node::remove(void)
  {
    assert(parent_node != NULL);
    switch (parent_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *par_substring = (tsi_substring *)parent_node;
            assert(par_substring->child() == this);
            par_substring->replace_child(NULL);
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *par_char_list = (tsi_char_list *)parent_node;
            unsigned long count = par_char_list->count();
            for (unsigned long child_num = 0; child_num < count; ++child_num)
              {
                if (par_char_list->child(child_num) == this)
                  {
                    par_char_list->remove_child(child_num);
                    return;
                  }
              }
            assert(FALSE);
            break;
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *par_char_table = (tsi_char_table *)parent_node;
            assert(par_char_table->child(parent_index) == this);
            par_char_table->replace_child(parent_index, NULL);
            break;
          }
        case TSI_LEAF:
            assert(FALSE);
        default:
            assert(FALSE);
      }
  }

void tsi_node::replace_base_entry(si_entry *new_entry,
                                  tree_string_index *the_tsi)
  {
    if (the_entry != NULL)
      {
        assert(the_tsi->get_back_pointer(the_entry) == this);
        the_tsi->set_back_pointer(the_entry, NULL);
      }
    the_entry = new_entry;
    if (new_entry != NULL)
      {
        assert(the_tsi->get_back_pointer(new_entry) == NULL);
        the_tsi->set_back_pointer(new_entry, this);
      }
  }

tsi_node *tsi_node::find_branchpoint(void)
  {
    if (parent_node == NULL)
        return this;
    if (parent_node->kind() != TSI_SUBSTRING)
        return this;
    tsi_substring *parent_substring = (tsi_substring *)parent_node;
    if (parent_substring->base_entry() != NULL)
        return parent_substring;
    return parent_substring->find_branchpoint();
  }


tsi_substring::tsi_substring(unsigned long char_count, const char *init_chars,
                             tsi_node *init_child)
    : the_chars(new char[char_count + 1]), the_child(init_child)
  {
    assert((init_child == NULL) || (init_child->parent_node == NULL));
    memcpy(the_chars, init_chars, char_count);
    the_chars[char_count] = 0;
    if (init_child != NULL)
        init_child->parent_node = this;
  }

tsi_substring::~tsi_substring(void)
  {
    delete[] the_chars;
    if (the_child != NULL)
        delete the_child;
  }

void tsi_substring::replace_child(tsi_node *new_child)
  {
    assert((new_child == NULL) || (new_child->parent_node == NULL));
    if (the_child != NULL)
        the_child->parent_node = NULL;
    the_child = new_child;
    if (new_child != NULL)
        new_child->parent_node = this;
  }


tsi_char_list::tsi_char_list(unsigned long init_table_size)
    : table_size(init_table_size), num_entries(0),
      the_children(new tsi_node *[init_table_size]),
      the_chars(new char[init_table_size])
  {
    table_size = init_table_size;
    num_entries = 0;
  }

tsi_char_list::~tsi_char_list(void)
  {
    for (unsigned long entry_num = 0; entry_num < num_entries; ++entry_num)
      {
        tsi_node *this_child = the_children[entry_num];
        if (this_child != NULL)
            delete this_child;
      }
    delete[] the_children;
    delete[] the_chars;
  }

void tsi_char_list::add_child(unsigned long child_num, tsi_node *new_child,
                              char this_char)
  {
    assert((new_child == NULL) || (new_child->parent_node == NULL));
    assert(child_num <= num_entries);
    if (num_entries == table_size)
      {
        tsi_node **new_child_list = new tsi_node *[table_size * 2];
        char *new_char_list = new char[table_size * 2];
        memcpy(new_child_list, the_children, table_size * sizeof(tsi_node *));
        memcpy(new_char_list, the_chars, table_size * sizeof(char));
        delete[] the_children;
        delete[] the_chars;
        the_children = new_child_list;
        the_chars = new_char_list;
        table_size *= 2;
      }
    if (child_num < num_entries)
      {
        memmove(&(the_children[child_num + 1]), &(the_children[child_num]),
                (num_entries - child_num) * sizeof(tsi_node *));
        memmove(&(the_chars[child_num + 1]), &(the_chars[child_num]),
                (num_entries - child_num) * sizeof(char));
      }
    the_children[child_num] = new_child;
    the_chars[child_num] = this_char;
    ++num_entries;
    if (new_child != NULL)
        new_child->parent_node = this;
  }

void tsi_char_list::remove_child(unsigned long child_num)
  {
    assert(child_num < num_entries);
    if (the_children[child_num] != NULL)
        the_children[child_num]->parent_node = NULL;
    --num_entries;
    if (child_num < num_entries)
      {
        memmove(&(the_children[child_num]), &(the_children[child_num + 1]),
                (num_entries - child_num) * sizeof(tsi_node *));
        memmove(&(the_chars[child_num]), &(the_chars[child_num + 1]),
                (num_entries - child_num) * sizeof(char));
      }
  }

void tsi_char_list::replace_child(unsigned long child_num, tsi_node *new_child)
  {
    assert((new_child == NULL) || (new_child->parent_node == NULL));
    assert(child_num < num_entries);
    if (the_children[child_num] != NULL)
        the_children[child_num]->parent_node = NULL;
    the_children[child_num] = new_child;
    if (new_child != NULL)
        new_child->parent_node = this;
  }


tsi_char_table::tsi_char_table(char init_lower, char init_upper)
    : the_children(0), first_char(init_lower), last_char(init_upper)
  {
    assert(init_lower <= init_upper);
    unsigned long init_size =
            (unsigned long)(unsigned char)(init_upper - init_lower) + 1;
    the_children = new tsi_node *[init_size];
    for (char test_char = init_lower; test_char < init_upper; ++test_char)
        the_children[test_char - (long)init_lower] = NULL;
    the_children[init_upper - (long)init_lower] = NULL;
  }

tsi_char_table::~tsi_char_table(void)
  {
    for (char test_char = first_char; test_char < last_char; ++test_char)
      {
        tsi_node *this_child = the_children[test_char - (long)first_char];
        if (this_child != NULL)
            delete this_child;
      }
    tsi_node *this_child = the_children[last_char - (long)first_char];
    if (this_child != NULL)
        delete this_child;
    delete[] the_children;
  }

void tsi_char_table::replace_child(char which_char, tsi_node *new_child)
  {
    assert((new_child == NULL) || (new_child->parent_node == NULL));
    if (which_char < first_char)
      {
        unsigned long init_size =
                (unsigned long)(unsigned char)(last_char - which_char) + 1;
        tsi_node **new_child_list = new tsi_node *[init_size];
        unsigned long extra =
                (unsigned long)(unsigned char)(first_char - which_char) + 1;
        unsigned long orig_size =
                (unsigned long)(unsigned char)(last_char - first_char) + 1;
        memcpy(&(new_child_list[extra]), the_children,
               orig_size * sizeof(tsi_node *));
        delete[] the_children;
        the_children = new_child_list;
        for (char test_char = which_char; test_char < first_char; ++test_char)
            the_children[test_char - (long)which_char] = NULL;
        first_char = which_char;
      }
    else if (which_char > last_char)
      {
        unsigned long init_size =
                (unsigned long)(unsigned char)(which_char - first_char) + 1;
        tsi_node **new_child_list = new tsi_node *[init_size];
        unsigned long orig_size =
                (unsigned long)(unsigned char)(last_char - first_char) + 1;
        memcpy(new_child_list, the_children, orig_size * sizeof(tsi_node *));
        delete[] the_children;
        the_children = new_child_list;
        for (char test_char = which_char; test_char > last_char; --test_char)
            the_children[test_char - (long)first_char] = NULL;
        last_char = which_char;
      }
    if (the_children[which_char - (long)first_char] != NULL)
        the_children[which_char - (long)first_char]->parent_node = NULL;
    the_children[which_char - (long)first_char] = new_child;
    if (new_child != NULL)
      {
        new_child->parent_node = this;
        new_child->parent_index = which_char;
      }
  }


tree_string_index::tree_string_index(char expected_lower, char expected_upper,
                                     unsigned long list_size)
    : root_node(0), the_list_size(list_size),
      the_expected_lower(expected_lower),
      the_expected_upper(expected_upper)
  {
    assert(list_size >= 2);
  }

tree_string_index::~tree_string_index(void)
  {
    if (root_node != NULL)
      {
        remove_entries(root_node);
        delete root_node;
      }
  }

tsi_node *tree_string_index::follow_prefix_match(const char *string,
                                                 unsigned long *prefix_length)
  {
    if (root_node == NULL)
        return NULL;
    unsigned long position = 0;
    tsi_node *current_node = root_node;
    while (string[position] != 0)
      {
        boolean done = FALSE;
        switch (current_node->kind())
          {
            case TSI_SUBSTRING:
              {
                tsi_substring *the_tsi_substring =
                        (tsi_substring *)current_node;
                char *the_chars = the_tsi_substring->chars();
                unsigned long num_chars = strlen(the_chars);
                if (strncmp(&(string[position]), the_chars, num_chars) == 0)
                  {
                    position += num_chars;
                    current_node = the_tsi_substring->child();
                  }
                else
                  {
                    done = TRUE;
                  }
                break;
              }
            case TSI_CHAR_LIST:
              {
                tsi_char_list *the_tsi_char_list =
                        (tsi_char_list *)current_node;
                unsigned long count = the_tsi_char_list->count();
                char this_char = string[position];
                unsigned long child_num;
                for (child_num = 0; child_num < count; ++child_num)
                  {
                    if (the_tsi_char_list->which_char(child_num) >= this_char)
                        break;
                  }
                if ((child_num < count) &&
                    (the_tsi_char_list->which_char(child_num) == this_char))
                  {
                    ++position;
                    current_node = the_tsi_char_list->child(child_num);
                  }
                else
                  {
                    done = TRUE;
                  }
                break;
              }
            case TSI_CHAR_TABLE:
              {
                tsi_char_table *the_tsi_char_table =
                        (tsi_char_table *)current_node;
                tsi_node *this_child =
                        the_tsi_char_table->child(string[position]);
                if (this_child != NULL)
                  {
                    ++position;
                    current_node = this_child;
                  }
                else
                  {
                    done = TRUE;
                  }
                break;
              }
            case TSI_LEAF:
              {
                done = TRUE;
                break;
              }
            default:
                assert(FALSE);
          }
        if (done)
            break;
      }
    *prefix_length = position;
    return current_node;
  }

void tree_string_index::remove_entries(tsi_node *base_node)
  {
    if (base_node == NULL)
        return;

    si_entry *base_entry = base_node->base_entry();
    if (base_entry != NULL)
      {
        base_node->replace_base_entry(NULL, this);
        destroy_entry(base_entry);
      }

    switch (base_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *the_tsi_substring = (tsi_substring *)base_node;
            remove_entries(the_tsi_substring->child());
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *the_tsi_char_list = (tsi_char_list *)base_node;
            unsigned long count = the_tsi_char_list->count();
            for (unsigned long child_num = 0; child_num < count; ++child_num)
                remove_entries(the_tsi_char_list->child(child_num));
            break;
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *the_tsi_char_table = (tsi_char_table *)base_node;
            char lower_char = the_tsi_char_table->lower_char();
            char upper_char = the_tsi_char_table->upper_char();
            for (char which = lower_char; which < upper_char; ++which)
                remove_entries(the_tsi_char_table->child(which));
            remove_entries(the_tsi_char_table->child(upper_char));
            break;
          }
        case TSI_LEAF:
            break;
        default:
           assert(FALSE);
      }
  }

void tree_string_index::internal_development_dump(char **buffer,
        unsigned long *buf_size, unsigned long position, tsi_node *base_node,
        FILE *fp, void (*node_func)(void *data, FILE *fp),
        boolean show_internals)
  {
    if (base_node == NULL)
        return;

    if (position >= *buf_size)
      {
        char *new_buffer = new char[*buf_size * 2];
        memcpy(new_buffer, *buffer, *buf_size);
        delete[] *buffer;
        *buffer = new_buffer;
        *buf_size *= 2;
      }

    if (show_internals)
      {
        fprintf(fp, "%*s", (int)(8 + 2 * position), "");
        switch (base_node->kind())
          {
            case TSI_SUBSTRING:
              {
                tsi_substring *the_substring = (tsi_substring *)base_node;
                fprintf(fp, "sub <\"%s\">", the_substring->chars());
                break;
              }
            case TSI_CHAR_LIST:
              {
                tsi_char_list *the_char_list = (tsi_char_list *)base_node;
                fprintf(fp, "list[%lu]", the_char_list->count());
                break;
              }
            case TSI_CHAR_TABLE:
              {
                tsi_char_table *the_char_table = (tsi_char_table *)base_node;
                fprintf(fp, "table['%c'..'%c']", the_char_table->lower_char(),
                        the_char_table->upper_char());
                break;
              }
            case TSI_LEAF:
                fprintf(fp, "leaf");
                break;
            default:
                assert(FALSE);
          }
        fprintf(fp, "\n");
      }

    si_entry *base_entry = base_node->base_entry();
    if (base_entry != NULL)
      {
        (*buffer)[position] = 0;
        fprintf(fp, "    (%p) \"%s\":", static_cast<void*>(base_entry), *buffer);
        if (node_func != NULL)
            (*node_func)(base_entry->data_value(), fp);
        else
            fprintf(fp, " [%p]", base_entry->data_value());
        fprintf(fp, "\n");
      }

    switch (base_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *the_tsi_substring = (tsi_substring *)base_node;
            char *chars = the_tsi_substring->chars();
            unsigned long string_size = strlen(chars);
            if (position + string_size + 1 >= *buf_size)
              {
                char *new_buffer =
                        new char[*buf_size + position + string_size + 1];
                memcpy(new_buffer, *buffer, *buf_size);
                delete[] *buffer;
                *buffer = new_buffer;
                *buf_size += position + string_size + 1;
              }
            strcpy(&((*buffer)[position]), chars);
            internal_development_dump(buffer, buf_size, position + string_size,
                                      the_tsi_substring->child(), fp,
                                      node_func, show_internals);
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *the_tsi_char_list = (tsi_char_list *)base_node;
            unsigned long count = the_tsi_char_list->count();
            for (unsigned long child_num = 0; child_num < count; ++child_num)
              {
                (*buffer)[position] = the_tsi_char_list->which_char(child_num);
                internal_development_dump(buffer, buf_size, position + 1,
                                          the_tsi_char_list->child(child_num),
                                          fp, node_func, show_internals);
              }
            break;
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *the_tsi_char_table = (tsi_char_table *)base_node;
            char lower_char = the_tsi_char_table->lower_char();
            char upper_char = the_tsi_char_table->upper_char();
            for (char which = lower_char; which < upper_char; ++which)
              {
                (*buffer)[position] = which;
                internal_development_dump(buffer, buf_size, position + 1,
                                          the_tsi_char_table->child(which), fp,
                                          node_func, show_internals);
              }
            (*buffer)[position] = upper_char;
            internal_development_dump(buffer, buf_size, position + 1,
                                      the_tsi_char_table->child(upper_char),
                                      fp, node_func, show_internals);
            break;
          }
        case TSI_LEAF:
            break;
        default:
           assert(FALSE);
      }
  }

void tree_string_index::get_development_stats(tsi_node *base_node,
        unsigned long *entry_count, unsigned long *substring_node_count,
        unsigned long *char_list_node_count,
        unsigned long *char_table_node_count, unsigned long *leaf_node_count,
        unsigned long *substring_char_count,
        unsigned long *char_list_place_count,
        unsigned long *char_list_used_count,
        unsigned long *char_table_place_count,
        unsigned long *char_table_used_count)
  {
    assert(base_node != NULL);
    if (base_node->base_entry() != NULL)
        ++*entry_count;

    switch (base_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *the_tsi_substring = (tsi_substring *)base_node;
            ++*substring_node_count;
            *substring_char_count += strlen(the_tsi_substring->chars());
            get_development_stats(the_tsi_substring->child(), entry_count,
                                  substring_node_count, char_list_node_count,
                                  char_table_node_count, leaf_node_count,
                                  substring_char_count, char_list_place_count,
                                  char_list_used_count, char_table_place_count,
                                  char_table_used_count);
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *the_tsi_char_list = (tsi_char_list *)base_node;
            ++*char_list_node_count;
            unsigned long count = the_tsi_char_list->count();
            *char_list_place_count += the_list_size;
            *char_list_used_count += count;
            for (unsigned long child_num = 0; child_num < count; ++child_num)
              {
                get_development_stats(the_tsi_char_list->child(child_num),
                                      entry_count, substring_node_count,
                                      char_list_node_count,
                                      char_table_node_count, leaf_node_count,
                                      substring_char_count,
                                      char_list_place_count,
                                      char_list_used_count,
                                      char_table_place_count,
                                      char_table_used_count);
              }
            break;
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *the_tsi_char_table = (tsi_char_table *)base_node;
            ++*char_table_node_count;
            char lower_char = the_tsi_char_table->lower_char();
            char upper_char = the_tsi_char_table->upper_char();
            *char_table_place_count +=
                    ((unsigned long)(upper_char - lower_char)) + 1;
            for (char which = lower_char; which < upper_char; ++which)
              {
                tsi_node *this_child = the_tsi_char_table->child(which);
                if (this_child != NULL)
                  {
                    get_development_stats(this_child, entry_count,
                                          substring_node_count,
                                          char_list_node_count,
                                          char_table_node_count,
                                          leaf_node_count,
                                          substring_char_count,
                                          char_list_place_count,
                                          char_list_used_count,
                                          char_table_place_count,
                                          char_table_used_count);
                    ++*char_table_used_count;
                  }
              }
            tsi_node *this_child = the_tsi_char_table->child(upper_char);
            if (this_child != NULL)
              {
                get_development_stats(this_child, entry_count,
                                      substring_node_count,
                                      char_list_node_count,
                                      char_table_node_count, leaf_node_count,
                                      substring_char_count,
                                      char_list_place_count,
                                      char_list_used_count,
                                      char_table_place_count,
                                      char_table_used_count);
                ++*char_table_used_count;
              }
            break;
          }
        case TSI_LEAF:
            ++*leaf_node_count;
            break;
        default:
           assert(FALSE);
      }
  }

si_entry *tree_string_index::enter(const char *the_string, void *the_data)
  {
    assert(the_string != NULL);

    if (root_node == NULL)
      {
        si_entry *result = create_entry(the_data);
        tsi_leaf *new_leaf = new tsi_leaf(result, this);
        root_node =
                new tsi_substring(strlen(the_string), the_string, new_leaf);
        return result;
      }

    unsigned long prefix_length;
    tsi_node *the_node = follow_prefix_match(the_string, &prefix_length);
    assert(the_node != NULL);
    if (the_string[prefix_length] == 0)
      {
        if (the_node->base_entry() != NULL)
            return NULL;
        si_entry *result = create_entry(the_data);
        the_node->replace_base_entry(result, this);
        return result;
      }
    si_entry *result = create_entry(the_data);
    si_entry *old_base = the_node->base_entry();
    tsi_node *new_node;
    switch (the_node->kind())
      {
        case TSI_SUBSTRING:
          {
            tsi_substring *the_tsi_substring = (tsi_substring *)the_node;
            char *old_chars = the_tsi_substring->chars();
            unsigned long match_length = 0;
            while ((the_string[prefix_length + match_length] ==
                    old_chars[match_length]))
              {
                assert(old_chars[match_length] != 0);
                ++match_length;
              }
            tsi_node *old_child = the_tsi_substring->child();
            the_tsi_substring->replace_child(NULL);
            unsigned long old_count = strlen(old_chars);
            assert(match_length < old_count);
            if (the_string[prefix_length + match_length] == 0)
              {
                tsi_substring *new_substring1 =
                        new tsi_substring(old_count - match_length,
                                          &(old_chars[match_length]),
                                          old_child);
                new_substring1->replace_base_entry(result, this);
                new_node =
                        new tsi_substring(match_length, old_chars,
                                          new_substring1);
                break;
              }
            if (match_length < (old_count - 1))
              {
                tsi_substring *new_substring =
                        new tsi_substring((old_count - 1) - match_length,
                                          &(old_chars[match_length + 1]),
                                          old_child);
                old_child = new_substring;
              }
            tsi_char_list *new_char_list = new tsi_char_list(the_list_size);
            tsi_node *new_child = new tsi_leaf(result, this);
            const char *suffix = &(the_string[prefix_length + match_length + 1]);
            if (suffix[0] != 0)
              {
                tsi_substring *new_substring =
                        new tsi_substring(strlen(suffix), suffix, new_child);
                new_child = new_substring;
              }
            char old_char = old_chars[match_length];
            char new_char = the_string[prefix_length + match_length];
            if (old_char > new_char)
              {
                char temp_char = old_char;
                old_char = new_char;
                new_char = temp_char;
                tsi_node *temp_child = old_child;
                old_child = new_child;
                new_child = temp_child;
              }
            new_char_list->add_child(0, old_child, old_char);
            new_char_list->add_child(1, new_child, new_char);
            new_node = new_char_list;
            if (match_length > 0)
              {
                const char *prefix = &(the_string[prefix_length]);
                new_node = new tsi_substring(match_length, prefix, new_node);
              }
            break;
          }
        case TSI_CHAR_LIST:
          {
            tsi_char_list *the_tsi_char_list = (tsi_char_list *)the_node;
            unsigned long count = the_tsi_char_list->count();
            tsi_node *new_child = new tsi_leaf(result, this);
            const char *suffix = &(the_string[prefix_length + 1]);
            if (*suffix != 0)
              {
                new_child =
                        new tsi_substring(strlen(suffix), suffix, new_child);
              }
            if (count == the_list_size)
              {
                tsi_char_table *new_table =
                        new tsi_char_table(the_expected_lower,
                                           the_expected_upper);
                for (unsigned child_ind = 0; child_ind < count; ++child_ind)
                  {
                    unsigned long child_num = (count - child_ind) - 1;
                    tsi_node *this_child = the_tsi_char_list->child(child_num);
                    char which_char = the_tsi_char_list->which_char(child_num);
                    the_tsi_char_list->remove_child(child_num);
                    new_table->replace_child(which_char, this_child);
                  }
                assert(the_tsi_char_list->count() == 0);
                new_table->replace_child(the_string[prefix_length], new_child);
                new_node = new_table;
                break;
              }
            else
              {
                char this_char = the_string[prefix_length];
                unsigned child_num;
                for (child_num = 0; child_num < count; ++child_num)
                  {
                    if (the_tsi_char_list->which_char(child_num) > this_char)
                        break;
                  }
                the_tsi_char_list->add_child(child_num, new_child, this_char);
                return result;
              }
          }
        case TSI_CHAR_TABLE:
          {
            tsi_char_table *the_tsi_char_table = (tsi_char_table *)the_node;
            assert(the_tsi_char_table->child(the_string[prefix_length]) ==
                   NULL);
            tsi_node *new_child = new tsi_leaf(result, this);
            const char *suffix = &(the_string[prefix_length + 1]);
            if (*suffix != 0)
              {
                new_child =
                        new tsi_substring(strlen(suffix), suffix, new_child);
              }
            the_tsi_char_table->replace_child(the_string[prefix_length],
                                              new_child);
            return result;
          }
        case TSI_LEAF:
          {
            const char *suffix = &(the_string[prefix_length]);
            tsi_leaf *new_leaf = new tsi_leaf(result, this);
            new_node = new tsi_substring(strlen(suffix), suffix, new_leaf);
            break;
          }
        default:
            assert(FALSE);
            return NULL;
      }
    if (the_node == root_node)
        root_node = new_node;
    else
        the_node->replace(new_node);
    the_node->replace_base_entry(NULL, this);
    new_node->replace_base_entry(old_base, this);
    delete the_node;
    return result;
  }

si_entry *tree_string_index::lookup_entry(const char *the_string)
  {
    assert(the_string != NULL);
    unsigned long prefix_length;
    tsi_node *the_node = follow_prefix_match(the_string, &prefix_length);
    if ((the_node != NULL) && (the_string[prefix_length] == 0))
        return the_node->base_entry();
    else
        return NULL;
  }

boolean tree_string_index::exists(const char *the_string)
  {
    return (lookup_entry(the_string) != NULL);
  }

void *tree_string_index::lookup(const char *the_string)
  {
    si_entry *the_entry = lookup_entry(the_string);
    if (the_entry != NULL)
        return the_entry->data_value();
    else
        return NULL;
  }

void tree_string_index::remove_entry(si_entry *the_entry)
  {
    assert(the_entry != NULL);
    tsi_node *back_pointer = (tsi_node *)(get_back_pointer(the_entry));
    assert(back_pointer != NULL);
    assert(back_pointer->base_entry() == the_entry);
    back_pointer->replace_base_entry(NULL, this);
    if (back_pointer->kind() == TSI_LEAF)
      {
        tsi_node *branchpoint = back_pointer->find_branchpoint();
        si_entry *branchpoint_entry = branchpoint->base_entry();
        if ((branchpoint_entry != NULL) && (branchpoint_entry != the_entry))
          {
            branchpoint->replace_base_entry(NULL, this);
            tsi_leaf *new_leaf = new tsi_leaf(branchpoint_entry, this);
            if (branchpoint == root_node)
                root_node = new_leaf;
            else
                branchpoint->replace(new_leaf);
          }
        else
          {
            if (branchpoint == root_node)
                root_node = NULL;
            else
                branchpoint->remove();
          }
        delete branchpoint;
      }
    destroy_entry(the_entry);
  }

void tree_string_index::development_dump(FILE *fp,
                                         void (*node_func)(void *data,
                                                           FILE *fp))
  {
    if (root_node == NULL)
      {
        fprintf(fp, "  <empty>\n");
      }
    else
      {
        unsigned long buf_size = 20;
        char *buffer = new char[buf_size];
        internal_development_dump(&buffer, &buf_size, 0, root_node, fp,
                                  node_func, FALSE);
        delete[] buffer;
      }
  }

void tree_string_index::development_internals_dump(FILE *fp)
  {
    if (root_node == NULL)
      {
        fprintf(fp, "  <empty>\n");
      }
    else
      {
        unsigned long buf_size = 20;
        char *buffer = new char[buf_size];
        internal_development_dump(&buffer, &buf_size, 0, root_node, fp, NULL,
                                  TRUE);
        delete[] buffer;
      }
  }

void tree_string_index::development_stats_dump(FILE *fp)
  {
    unsigned long entry_count = 0;

    unsigned long substring_node_count = 0;
    unsigned long char_list_node_count = 0;
    unsigned long char_table_node_count = 0;
    unsigned long leaf_node_count = 0;

    unsigned long substring_char_count = 0;
    unsigned long char_list_place_count = 0;
    unsigned long char_list_used_count = 0;
    unsigned long char_table_place_count = 0;
    unsigned long char_table_used_count = 0;

    if (root_node != NULL)
      {
        get_development_stats(root_node, &entry_count, &substring_node_count,
                              &char_list_node_count, &char_table_node_count,
                              &leaf_node_count, &substring_char_count,
                              &char_list_place_count, &char_list_used_count,
                              &char_table_place_count, &char_table_used_count);
      }

    unsigned long total_node_count =
            substring_node_count + char_list_node_count +
            char_table_node_count + leaf_node_count;
    unsigned long total_memory = sizeof(tree_string_index) +
            entry_count * sizeof(si_entry) +
            substring_node_count * sizeof(tsi_substring) +
              (substring_char_count + substring_node_count) * sizeof(char) +
            char_list_node_count * sizeof(tsi_char_list) +
              char_list_place_count * (sizeof(tsi_node *) + sizeof(char)) +
            char_table_node_count * sizeof(tsi_char_table) +
              char_table_place_count * sizeof(tsi_node *) +
            leaf_node_count * sizeof(tsi_leaf);

    fprintf(fp,
"    Substring nodes:                                   %20lu\n"
"        Chars in substrings:          %20lu\n"
"    Char list nodes:                                   %20lu\n"
"        Char list spaces:             %20lu\n"
"        Char list spaces occupied:    %20lu (%5.1f%%)\n"
"    Char table nodes:                                  %20lu\n"
"        Char table spaces:            %20lu\n"
"        Char table spaces occupied:   %20lu (%5.1f%%)\n"
"    Leaf nodes:                                        %20lu\n"
"                                                       --------------------\n"
"  Total nodes:                                         %20lu\n"
"\n"
"  Total entries:                                       %20lu\n"
"\n"
"  Total direct memory requirement:                     %20lu\n",
        substring_node_count, substring_char_count, char_list_node_count,
        char_list_place_count, char_list_used_count,
        (char_list_place_count == 0) ? 0.0 :
        ((((double)char_list_used_count) / ((double)char_list_place_count)) *
        100.0), char_table_node_count, char_table_place_count,
        char_table_used_count,
        (char_table_place_count == 0) ? 0.0 :
        ((((double)char_table_used_count) / ((double)char_table_place_count)) *
        100.0), leaf_node_count, total_node_count, entry_count, total_memory);
  }

// override default copy constructors and assignment ops
si_entry::si_entry(const si_entry &) { assert(FALSE); }
si_entry &si_entry::operator=(const si_entry &) { assert(FALSE); return *this; }

string_index::string_index(const string_index &)  { assert(FALSE); }
string_index &string_index::operator=(const string_index &)  { assert(FALSE); return *this; }

tree_string_index::tree_string_index(const tree_string_index &)  { assert(FALSE); }
tree_string_index &tree_string_index::operator=(const tree_string_index &)  { assert(FALSE); return *this; }

