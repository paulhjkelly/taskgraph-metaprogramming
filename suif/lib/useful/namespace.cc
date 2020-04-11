/* file "namespace.cc" */

/*  Copyright (c) 1996 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the implementation of routines to map the interfile global
 *  namespace to a subset of that namespace that conventional linkers
 *  accept, for the SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_namespace_cc

#include "useful_internal.h"
#include <cstring>
#include <cctype>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static boolean identifier_ok(const char *identifier);
static boolean symbol_name_exists(char *name, global_symtab *the_symtab);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void map_namespace_to_alphanumeric(global_symtab *the_symtab)
  {
    assert(!the_symtab->is_file());
    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_sym = sym_iter.step();
        if (identifier_ok(this_sym->name()))
            continue;
        char *new_name =
                new char[strlen(this_sym->name())*3 + 20 + sizeof(void *) * 3];
        strcpy(new_name, "__tR_");
        char *new_position = &(new_name[strlen(new_name)]);
        const char *old_position = this_sym->name();
        while (*old_position != 0)
          {
            if (isalnum(*old_position))
              {
                *new_position = *old_position;
                ++new_position;
              }
            else if (*old_position == '_')
              {
                *new_position = '_';
                ++new_position;
                *new_position = '_';
                ++new_position;
              }
            else
              {
                sprintf(new_position, "_%.2x", (unsigned int)(*old_position));
                new_position += 3;
              }
            ++old_position;
          }
        *new_position = 0;
        if (symbol_name_exists(new_name, the_symtab))
          {
            i_integer count = 1;
            do
              {
                count.write(new_position);
                ++count;
              } while (symbol_name_exists(new_name, the_symtab));
          }
        this_sym->set_name(new_name);
        delete[] new_name;
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static boolean identifier_ok(const char *identifier)
  {
    const char *follow = identifier;
    if ((!isalpha(*follow)) && (*follow != '_'))
        return FALSE;
    ++follow;
    while (*follow != 0)
      {
        if ((!isalnum(*follow)) && (*follow != '_'))
            return FALSE;
        ++follow;
      }
    return TRUE;
  }

static boolean symbol_name_exists(char *name, global_symtab *the_symtab)
  {
    return ((the_symtab->lookup_var(name) != NULL) ||
            (the_symtab->lookup_proc(name) != NULL));
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
