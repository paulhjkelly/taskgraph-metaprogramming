/*  Symbol Classes */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef SYMBOLS_H
#define SYMBOLS_H

#pragma interface

RCS_HEADER(symbols_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class type_node;
class func_type;

class annote;
class file_set_entry;
class tree_proc;
struct replacements;
class var_def;

enum sym_kinds {
    SYM_PROC,				/* procedure */
    SYM_LABEL,				/* label */
    SYM_VAR				/* variable */
};

/*  Symbol flags */
#define SYM_USERDEF	0x0001		/* user defined, not compiler tmp */
#define VARSYM_ISPARAM	0x0004		/* variable is a parameter */
#define VARSYM_ADDRTAKEN 0x0008		/* variable's address is taken */
#define VARSYM_ISREG	0x0010		/* variable is a machine register */


/*
 *  A sym_node represents a symbol for a procedure, label, or variable
 *  within a particular symbol table.  Procedure symbols may only be entered
 *  in global and file symtabs and labels may only be in procedure and block
 *  symtabs; variables may used anywhere.  Each sym_node has a back pointer to
 *  the symtab which contains it.  All of the symbols of the same kind within
 *  a symtab should have unique names; if not they will be automatically
 *  renamed before being written out.  Each symbol also has an ID number.
 *  The ID numbers for symbols in the global_symtab are unique, but the ID
 *  numbers for symbols in a file_symtab are only unique within the file.
 *  Local symbols have unique ID numbers within procedures (i.e. variables
 *  in separate procedures may have the same ID but variables in different
 *  scopes of the same procedure will always have different numbers).  The
 *  base sym_node class is abstract and cannot be directly instantiated.
 */

class sym_node : public suif_object {
    friend class base_symtab;

private:
    const char *nm;			/* the symbol name */
    base_symtab *table;			/* symtab containing this sym_node */
    unsigned id;			/* identifier used for i/o */
    boolean ext;			/* are global symbols external? */
    
    void set_parent(base_symtab *st)	{ table = st; }

protected:
    int flags;				/* userdef, etc. */

    sym_node();

    void set_sym_id(unsigned i)		{ id = i; }
    virtual void set_extern(boolean b)	{ ext = b; }
    virtual void print_helper(FILE *)	{ }

    static sym_node *scan_from_annote(annote *an, base_symtab *symtab);
    virtual void cvt_from_annote(annote *an, base_symtab *symtab);
    virtual annote *cvt_to_annote() = 0;
    void cvt_to_annote_base(annote *an);

public:
    virtual ~sym_node()			{ }
    
    object_kinds object_kind()		{ return SYM_OBJ; }

    virtual sym_kinds kind() = 0;
    base_symtab *parent()		{ return table; }
    unsigned sym_id()			{ return id; }

    const char *name()			{ return nm; }
    void set_name(const char *n);
    void clear_sym_id();

    boolean is_proc()			{ return kind() == SYM_PROC; }
    boolean is_label()			{ return kind() == SYM_LABEL; }
    boolean is_var()			{ return kind() == SYM_VAR; }

    boolean is_global();
    boolean is_private();
    virtual boolean is_extern()		{ return ext; }

    boolean is_userdef()		{ return flags & SYM_USERDEF; }
    void set_userdef()			{ flags |= SYM_USERDEF; }
    void reset_userdef()		{ flags &= ~SYM_USERDEF; }

    virtual void remove_from_table(void);
    virtual void add_to_table(base_symtab *new_table);

    virtual sym_node *copy() = 0;
    void copy_flags(sym_node *sn);

    sym_node *clone_helper(replacements *r);

    virtual void print(FILE *f = stdout);
    void print_full(FILE *f = stdout, int depth = 0);
    
    static sym_node *read(in_stream *is, base_symtab *symtab);
    void write(out_stream *os);
    void write_check(void);
};

DECLARE_DLIST_CLASS(sym_node_list, sym_node*);
DECLARE_DLIST_CLASS(var_sym_list, var_sym*);


/*
 *  A var_sym is a local or global variable (either in a register or in
 *  memory).  The type of the variable must always be specified.
 */

class var_sym : public sym_node {
    friend sym_node *sym_node::scan_from_annote(annote *an,
                                                base_symtab *symtab);
    friend class base_symtab;

private:
    type_node *typ;
    boolean defd;

    var_sym *the_parent_var;
    int the_offset;
    var_sym_list *children;

protected:
    var_sym();

    void print_helper(FILE *f = stdout);
    void cvt_from_annote(annote *an, base_symtab *symtab);
    annote *cvt_to_annote();

    void set_has_def(boolean d);
    void set_extern(boolean b);

    void prepare_child_removal(var_sym *old_child);
    void prepare_child_addition(var_sym *new_child);

public:
    var_sym(type_node *t,		/* variable's high-level type */
	    const char *n);		/* variable name */
    virtual ~var_sym();

    sym_kinds kind()			{ return SYM_VAR; }
    type_node *type()		 	{ return typ; }

    var_sym *parent_var(void)		{ return the_parent_var; }
    int offset(void)			{ return the_offset; }
    var_sym *root_ancestor(void);
    int root_offset(void);
    boolean overlaps(var_sym *other_var);
    unsigned num_children(void);
    var_sym *child_var(unsigned the_num);

    var_sym *find_child(int child_offset, type_node *child_type);
    var_sym *build_child(int child_offset, type_node *child_type,
                         const char *child_name);

    void set_type(type_node *t)		{ typ = t; }

    boolean is_scalar();
    boolean is_static();
    boolean is_spilled();
    boolean is_auto();
    boolean has_var_def();

    boolean is_param()			{ return flags & VARSYM_ISPARAM; }
    boolean is_addr_taken();
    boolean is_reg();

    void set_param();
    void set_addr_taken();
    void set_reg();

    void reset_param();
    void reset_addr_taken();
    void reset_reg();

    boolean is_extern();

    var_def *definition();		/* find the corresponding var_def */

    void add_child(var_sym *new_child, int child_offset);
    void remove_child(var_sym *old_child);
    void replace_child(unsigned child_num, var_sym *new_child);

    void remove_from_table(void);		/* also remove sub-variables */
    void add_to_table(base_symtab *new_table);	/* also add sub-variables */

    sym_node *copy();
    void print(FILE *f = stdout);

    /* OBSOLETE!!  The following is obsolete.  It is still provided
       for backward compatibility, but it will eventually be removed. */

    boolean is_common_blk_var()		{ return FALSE; }
};


/*
 *  A label_sym is an intra-procedure label.  By convention, label_syms may
 *  only be entered into proc_symtabs and block_symtabs.  The location of a
 *  label within a procedure is indicated by an io_lab instruction.
 */

class label_sym : public sym_node {
    friend sym_node *sym_node::scan_from_annote(annote *an,
                                                base_symtab *symtab);

protected:
    label_sym()				{ }

    /* use base class print_helper() function */
    void cvt_from_annote(annote *an, base_symtab *symtab);
    annote *cvt_to_annote();

public:
    label_sym(const char *n);

    sym_kinds kind()			{ return SYM_LABEL; }

    sym_node *copy();
    void print(FILE *f = stdout);
};


/*
 *  A proc_sym is a procedure declaration.  SUIF does not support nested
 *  procedures, so proc_syms should only be entered in global and file symtabs.
 *  Along with the symbol, we record the type of the function, the source
 *  language, and a pointer to the procedure body.  If the body of the
 *  procedure exists in one of the input files, a pointer to the file_set_entry
 *  is also available, and this can be given to the "read_proc" method to
 *  read the body.  The "write_proc" method may be used to write the body
 *  out to a particular file, and the "flush_proc" method deallocates the
 *  memory used by the procedure body.  Once a procedure has been written
 *  to a file it can no longer be re-read or re-written, and the proc_sym
 *  contains a flag to record if the procedure has been written.
 */

enum src_lang_type {
    src_unknown = 0,
    src_c = 1,
    src_fortran = 2,
    src_verilog = 3
};


class proc_sym : public sym_node {
    friend sym_node *sym_node::scan_from_annote(annote *an,
                                                base_symtab *symtab);
    friend class file_set_entry;
    
private:
    file_set_entry *fse;		/* file containing this procedure */
    long file_pos;			/* position in the file's i/o stream */
    boolean written;			/* has the procedure been written? */
    func_type *typ;			/* function type (not just ret type) */
    src_lang_type srclang;		/* source language */
    tree_proc *pr;			/* the procedure body */

protected:
    proc_sym() : fse(0), file_pos(0), written(FALSE), typ(0),
	srclang(src_unknown), pr(0) { }

    void print_helper(FILE *f = stdout);
    void cvt_from_annote(annote *an, base_symtab *symtab);
    annote *cvt_to_annote();

public:
    proc_sym(func_type *t,		/* the procedure's type */
	     src_lang_type src,		/* source language for the procedure */
	     const char *n);		/* procedure name */

    sym_kinds kind()			{ return SYM_PROC; }
    func_type *type()			{ return typ; }
    src_lang_type src_lang()		{ return srclang; }
    tree_proc *block()			{ return pr; }
    file_set_entry *file()		{ return fse; }

    void set_type(func_type *t)		{ typ = t; }
    void set_src_lang(src_lang_type st)	{ srclang = st; }
    void set_block(tree_proc *p);
    void set_fse(file_set_entry *f)	{ fse = f; }

    boolean is_written()		{ return written; }
    boolean is_readable()		{ return !written && (file_pos != -1);}
    boolean is_in_memory()		{ return (block() != NULL); }

    void read_proc(boolean exp_trees = TRUE, boolean use_fortran_form = FALSE);
    void write_proc(file_set_entry *f);
    void flush_proc();

    sym_node *copy();
    void print(FILE *f = stdout);
};

#endif /* SYMBOLS_H */
