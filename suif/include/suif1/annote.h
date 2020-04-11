/*  Annotation Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef ANNOTE_H
#define ANNOTE_H

#pragma interface

RCS_HEADER(annote_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;


/*
 *  Annotations are used to record arbitrary data associated with
 *  instructions and other suif_objects.  An annotation consists of
 *  a name and a void* value.  The annotation name must be an entry
 *  in the lexicon and is generally registered with the annotation
 *  manager.  There are two kinds of registered annotations: flat and
 *  structured.  For flat annotations the data field is a pointer to
 *  a list of immed values, whereas the data for structured annotations
 *  may be of any type.  The "immeds" and "set_immeds" methods only
 *  work with registered annotations.
 */

class annote {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    annote(const annote &);
    annote &operator=(const annote &);

    const char *nm;
    void *dt;

public:
    annote(in_stream *is, base_symtab *syms);
    annote(const char *n, void *d = NULL);
    ~annote();
    
    const char *name()			{ return nm; }
    void *data()			{ return dt; }
    immed_list *immeds();

    void set_name(const char *n)	{ nm = n; }
    void set_data(void *d)		{ dt = d; }
    void set_immeds(immed_list *iml, suif_object *obj);

    boolean is_structured();

    void write(out_stream *os);
    void print(FILE *fp=stdout);
};


/*
 *  Annotation lists have added methods to retrieve annotations by
 *  name and to delete the contents of lists when they are destroyed.
 */

DECLARE_DLIST_CLASSES(annote_list_base, annote_list_e, annote_list_iter,
		      annote*);

class annote_list : public annote_list_base {
public:
    ~annote_list();

    annote *get_annote(const char *name);
    annote *peek_annote(const char *name);
};
    

/*
 *  Predefined annotations.  Most of these are only for the library's
 *  internal use, but a few of them are visible to users.
 */

void init_suif_annotes();

#ifdef DEFINE_ANNOTES
#define extern
#endif

/* annotations visible to users: */

/* initial data annotations */
EXPORTED_BY_SUIF const char *k_fill;
EXPORTED_BY_SUIF const char *k_repeat_init;
EXPORTED_BY_SUIF const char *k_multi_init;

/* miscellaneous */
EXPORTED_BY_SUIF const char *k_enable_exceptions;
EXPORTED_BY_SUIF const char *k_line;
EXPORTED_BY_SUIF const char *k_reg_num;
EXPORTED_BY_SUIF const char *k_fields;
EXPORTED_BY_SUIF const char *k_call_by_ref;
EXPORTED_BY_SUIF const char *k_orig_type;
EXPORTED_BY_SUIF const char *k_common_block;
EXPORTED_BY_SUIF const char *k_history;
EXPORTED_BY_SUIF const char *k_version_history;
EXPORTED_BY_SUIF const char *k_source_file_name;

/* OBSOLETE!! The following are obsolete.  They are still provided for
   backward compatibility, but they will eventually be removed. */
EXPORTED_BY_SUIF const char *k_common_field;
EXPORTED_BY_SUIF const char *k_common_shape;


/* annotations for internal use: */

/* types */
EXPORTED_BY_SUIF const char *k_int_type;
EXPORTED_BY_SUIF const char *k_float_type;
EXPORTED_BY_SUIF const char *k_void_type;
EXPORTED_BY_SUIF const char *k_ptr_type;
EXPORTED_BY_SUIF const char *k_array_type;
EXPORTED_BY_SUIF const char *k_func_type;
EXPORTED_BY_SUIF const char *k_group_type;
EXPORTED_BY_SUIF const char *k_struct_type;
EXPORTED_BY_SUIF const char *k_union_type;
EXPORTED_BY_SUIF const char *k_enum_type;
EXPORTED_BY_SUIF const char *k_const_type;
EXPORTED_BY_SUIF const char *k_restrict_type;
EXPORTED_BY_SUIF const char *k_volatile_type;
EXPORTED_BY_SUIF const char *k_call_by_ref_type;
EXPORTED_BY_SUIF const char *k_null_type;

/* target machine parameters */
EXPORTED_BY_SUIF const char *k_is_big_endian;
EXPORTED_BY_SUIF const char *k_char_is_signed;
EXPORTED_BY_SUIF const char *k_addressable_size;
EXPORTED_BY_SUIF const char *k_C_type_size;
EXPORTED_BY_SUIF const char *k_C_type_align;
EXPORTED_BY_SUIF const char *k_array_align;
EXPORTED_BY_SUIF const char *k_struct_align;
EXPORTED_BY_SUIF const char *k_ptr_diff_type;

/* symtabs */
EXPORTED_BY_SUIF const char *k_base_symtab;
EXPORTED_BY_SUIF const char *k_block_symtab;
EXPORTED_BY_SUIF const char *k_proc_symtab;
EXPORTED_BY_SUIF const char *k_var_counter;
EXPORTED_BY_SUIF const char *k_label_counter;
EXPORTED_BY_SUIF const char *k_next_inum;
EXPORTED_BY_SUIF const char *k_next_sym_id;
EXPORTED_BY_SUIF const char *k_next_type_id;
EXPORTED_BY_SUIF const char *k_param;
EXPORTED_BY_SUIF const char *k_symbols;
EXPORTED_BY_SUIF const char *k_types;
EXPORTED_BY_SUIF const char *k_var_defs;
EXPORTED_BY_SUIF const char *k_sym_annotes;
EXPORTED_BY_SUIF const char *k_type_annotes;
EXPORTED_BY_SUIF const char *k_var_def_annotes;

/* symbols */
EXPORTED_BY_SUIF const char *k_labelsym;
EXPORTED_BY_SUIF const char *k_procsym;
EXPORTED_BY_SUIF const char *k_varsym;

/* variable definitions */
EXPORTED_BY_SUIF const char *k_var_def;

/* tree nodes */
EXPORTED_BY_SUIF const char *k_block;
EXPORTED_BY_SUIF const char *k_for;
EXPORTED_BY_SUIF const char *k_loop;
EXPORTED_BY_SUIF const char *k_if;
EXPORTED_BY_SUIF const char *k_list_end;

/* miscellaneous */
EXPORTED_BY_SUIF const char *k_file_symtab_ptr;
EXPORTED_BY_SUIF const char *k_proc_in_file;
EXPORTED_BY_SUIF const char *k_file_num;
EXPORTED_BY_SUIF const char *k_clone;
EXPORTED_BY_SUIF const char *k_extra_destinations;
EXPORTED_BY_SUIF const char *k_no_destinations;

#ifdef DEFINE_ANNOTES
#undef extern
#endif

#endif /* ANNOTE_H */
