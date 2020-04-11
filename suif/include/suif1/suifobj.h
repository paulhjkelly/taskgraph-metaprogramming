/*  SUIF Object Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef SUIFOBJ_H
#define SUIFOBJ_H

#pragma interface

RCS_HEADER(suifobj_h,
    "$Id$")

class annote_list;
class instruction;
class base_symtab;
struct replacements;

enum object_kinds {
    FILE_OBJ,				/* file_set_entry */
    TREE_OBJ,				/* tree_node */
    INSTR_OBJ,				/* instruction */
    SYMTAB_OBJ,				/* base_symtab */
    SYM_OBJ,				/* sym_node */
    DEF_OBJ,				/* var_def */
    TYPE_OBJ				/* type_node */
};


/*
 *  A SUIF OBJECT is the base class for many classes in SUIF.  It
 *  provides routines that are common to all objects, such as the
 *  ability to hang annotations off of an object.
 */

class suif_object {
    friend class tree_node_list;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    suif_object(const suif_object &);
    suif_object &operator=(const suif_object &);

    annote_list *anl;

protected:
    void mrk_to_annotes(instruction *mrk);
    void annotes_to_mrk(instruction *mrk);
    void convert_annotes();

public:
    suif_object() : anl(0) { }
    virtual ~suif_object();

    virtual object_kinds object_kind() = 0;

    boolean is_file_obj()		{ return (object_kind() == FILE_OBJ); }
    boolean is_tree_obj()		{ return (object_kind() == TREE_OBJ); }
    boolean is_instr_obj()		{ return (object_kind() == INSTR_OBJ);}
    boolean is_symtab_obj()		{ return (object_kind() ==SYMTAB_OBJ);}
    boolean is_sym_obj()		{ return (object_kind() == SYM_OBJ); }
    boolean is_def_obj()		{ return (object_kind() == DEF_OBJ); }
    boolean is_type_obj()		{ return (object_kind() == TYPE_OBJ); }

    void append_annote(const char *name, void *data = NULL);
    void prepend_annote(const char *name, void *data = NULL);
    void set_annote(const char *name, void *data);
    
    annote_list *annotes();
    void *get_annote(const char *name);	/* return data, not whole annote */
    void *peek_annote(const char *name);/* return data, not whole annote */
    boolean are_annotations();
    int num_output_annotes();

    void copy_annotes(suif_object *target);
    void clone_annotes(suif_object *target, replacements *r,
		       boolean no_copy = FALSE);
    void find_annote_refs(base_symtab *dst_scope, replacements *r);

    void print_annotes(FILE *fp=stdout, int depth=0);
};

#endif /* SUIFOBJ_H */
