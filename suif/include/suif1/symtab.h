/*  Symbol Table Classes */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef SYMTAB_H
#define SYMTAB_H

#pragma interface

RCS_HEADER(symtab_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab_list;
class file_set_entry;
class tree_block;
struct replacements;


enum symtab_kinds {
    SYMTAB_GLOBAL,
    SYMTAB_FILE,
    SYMTAB_PROC,
    SYMTAB_BLOCK
};

/* These functions affect how base_symtab::write() writes out the duplicate
 * symbol names. */

extern void enable_automatic_renaming(void);
extern void disable_automatic_renaming(void);

/*
 *  The ID numbers used for sym_nodes and type_nodes are divided into
 *  three ranges.  To make it easier to read an ID number, this function
 *  prints it as a single character to identify the range ("g" = global,
 *  "f" = file, "p" = procedure) combined with the offset of the number
 *  within that range.
 */

extern void print_id_number(FILE *fp, unsigned id);


/*
 *  A symtab contains lists of symbols and types that may be used within the
 *  scope associated with the symtab.  The base_symtab class is abstract and
 *  contains variables and methods shared by all of the other symbol tables.
 *  The symtabs form a strict hierarchy starting with the global symtab,
 *  followed by one symtab per file, followed by symtabs for the individual
 *  procedures and blocks.  Each symtab has a name and all of the children of
 *  a particular symtab should have unique names.  Symtabs are automatically
 *  removed and destroyed along with the corresponding "scopes", e.g.
 *  deleting a tree_block also deletes its block_symtab.
 */

class base_symtab : public suif_object {
    friend class sym_node;
    friend class type_node;
    friend class var_def;

private:
    void chain_name_helper();

protected:
    sym_node_list *syms;		/* list of symbols */
    var_def_list *defs;			/* variable definitions */
    type_node_list *typs;		/* list of types */
    base_symtab *par;			/* parent symtab */
    base_symtab_list *childs;		/* list of child symtabs */
    const char *nm;			/* name of this symtab */
    unsigned next_sym_id;		/* next unused symbol ID number */
    unsigned next_type_id;		/* next unused type ID number */
    unsigned var_counter;		/* for creating unique var_syms */
    string_index *sym_name_index;	/* for fast lookups by name */
    ptr_index *def_index;		/* for fast lookups by var_sym */
    ptr_index *sym_id_index;		/* for fast lookups by id # */
    ptr_index *type_id_index;		/* for fast lookups by id # */

    void print_contents(FILE *fp, int depth);

    void add_contents_to_replacements(replacements *r);
    virtual base_symtab *new_peer(const char *n) = 0;
					/* create a new symtab of the same
					   sub-class as this one */

    void register_name_change(sym_node *the_sym, const char *new_name);
    void register_sym_id_change(sym_node *the_sym, unsigned new_id);
    void register_type_id_change(type_node *the_type, unsigned new_id);
    void add_name_entry(sym_node *the_sym, const char *new_name);
    void remove_name_entry(sym_node *the_sym, const char *old_name);
    void register_def_sym_change(var_def *the_def, var_sym *new_var);

    void set_sym_extern(sym_node *the_sym, boolean new_value)
      { the_sym->set_extern(new_value); }
    void set_sym_id(sym_node *the_sym, unsigned new_id)
      { the_sym->set_sym_id(new_id); }

    void remove_def_internal(var_def *d, boolean in_destructor);

public:
    base_symtab(const char *n);		/* the symtab name */
    virtual ~base_symtab();

    object_kinds object_kind()		{ return SYMTAB_OBJ; }

    virtual symtab_kinds kind() = 0;

    boolean is_global()			{ return ((kind() == SYMTAB_GLOBAL) ||
						  (kind() == SYMTAB_FILE)); }
    boolean is_file()			{ return (kind() == SYMTAB_FILE); }
    boolean is_block()			{ return ((kind() == SYMTAB_BLOCK) ||
						  (kind() == SYMTAB_PROC)); }
    boolean is_proc()			{ return kind() == SYMTAB_PROC; }

    base_symtab *parent()		{ return par; }
    sym_node_list *symbols()		{ return syms; }
    var_def_list *var_defs()		{ return defs; }
    base_symtab_list *children()	{ return childs; }
    type_node_list *types()		{ return typs; }
    const char *name()			{ return nm; }
    const char *chain_name();		/* writes to static buffer! */

    void set_name(const char *n);
    void rename_duplicates();		/* give unique names to everything */

    base_symtab *lookup_child(const char *name);
    var_def *lookup_var_def(var_sym *v);
    type_node *lookup_type(type_node *t, boolean up=TRUE);
    sym_node *lookup_sym(const char *name, sym_kinds k, boolean up=TRUE);
    var_sym *lookup_var(const char *name, boolean up=TRUE)
	{ return static_cast<var_sym *>(lookup_sym(name, SYM_VAR, up)); }

    boolean is_visible(type_node *the_type)
	{ return is_ancestor(the_type->parent()); }
    boolean is_visible(sym_node *the_sym)
	{ return is_ancestor(the_sym->parent()); }

    sym_node *lookup_sym_id(unsigned i);
    type_node *lookup_type_id(unsigned i);

    var_sym *new_var(type_node *t, const char *n);
    var_sym *new_unique_var(type_node *t, const char *base = NULL);
    var_def *define_var(var_sym *v, int a);

    boolean is_ancestor(base_symtab *s);

    /* add a type and its component types if they do not already exist in this
       or a parent symtab; deletes type_nodes that are replaced */
    type_node *install_type(type_node *t);

    /* attempt to move a type so that it is visible in this scope */
    boolean make_type_visible(type_node *t, boolean check = FALSE);

    void resolve_exposed_refs(replacements *r);
    base_symtab *clone_helper(replacements *r, boolean no_copy = FALSE);

    void add_child(base_symtab *c);	/* sets c->parent */
    virtual void add_sym(sym_node *s);	/* sets s->table */
    void add_type(type_node *t);	/* sets t->table */
    void add_def(var_def *d);		/* sets d->table */

    void remove_child(base_symtab *c);	/* doesn't delete c */
    void remove_sym(sym_node *s);	/* doesn't delete s */
    void remove_type(type_node *t);	/* doesn't delete t */
    void remove_def(var_def *d);	/* doesn't delete d */

    virtual void print(FILE *fp=stdout, int depth=0) = 0;
    virtual void write(out_stream *os);
    virtual void read(in_stream *is);
};

DECLARE_DLIST_CLASS(base_symtab_list, base_symtab*);


/*
 *  The global_symtab at the root of the symbol table hierarchy is used to
 *  hold symbols and types shared across files (i.e. those with external
 *  linkage).  This class is also reused as the base class for file_symtabs.
 *  Since only global and file symtabs may contain procedure symbols, this
 *  class includes methods to create new proc_syms and to lookup existing
 *  ones.  The number_globals function is automatically called just before
 *  writing the symtab to assign ID numbers to new symbol and type nodes.
 */

class global_symtab : public base_symtab {
    friend class file_set;

protected:
    virtual base_symtab *new_peer(const char *n) { return new global_symtab(n); }

public:
    global_symtab(const char *n);		/* name (usually "global") */

    symtab_kinds kind()			{ return SYMTAB_GLOBAL; }

    proc_sym *new_proc(func_type *r, src_lang_type src, const char *n);
    proc_sym *lookup_proc(const char *s, boolean up=TRUE)
	{ return static_cast<proc_sym *>(lookup_sym(s, SYM_PROC, up)); }

    void predefine_types();		/* create common types */
    void number_globals();

    void add_sym(sym_node *s);
    void print(FILE *fp=stdout, int depth=0);
    void write(out_stream *os);
};


/*
 *  A file symtab is associated with a particular file_set_entry to hold
 *  symbols and types declared in the scope of the file (e.g. it contains
 *  the global variables declared with "static" linkage).  This class is
 *  identical to the global_symtab class except for the file_set_entry
 *  pointer.
 */

class file_symtab : public global_symtab {
    friend class suif_linker;

private:
    file_set_entry *fset;		/* corresponding file_set_entry */

    void set_fse(file_set_entry *new_fse) { fset = new_fse; }

protected:
    virtual base_symtab *new_peer(const char *n)
      { return new file_symtab(n, fse()); }

public:
    file_symtab(const char *n,		/* name (usually the input filename) */
		file_set_entry *fse);	/* corresponding file_set_entry */

    symtab_kinds kind()			{ return SYMTAB_FILE; }
    file_set_entry *fse()		{ return fset; }

    void print(FILE *fp=stdout, int depth=0);
};


/*
 *  The block_symtab class is used for nested block symbol tables and as the
 *  base class for procedure symbol tables.  Each one is associated with a
 *  particular tree_block (or tree_proc).  The "new_unique_child" method can
 *  be used to create a new child block symtab with a unique name.  Since
 *  label symbols may only be entered in symtabs within procedures, this
 *  class provides methods to create new labels and lookup existing ones.
 */

class block_symtab : public base_symtab {
    friend class tree_block;
    friend class tree_proc;

private:
    unsigned label_counter;		/* for creating unique label_syms */

protected:
    tree_block *blk;			/* corresponding block */

    void set_block(tree_block *b)	{ blk = b; }
    void number_locals(unsigned *next_sym_idp, unsigned *next_type_idp);

    virtual base_symtab *new_peer(const char *n) { return new block_symtab(n); }

public:
    block_symtab(const char *n);		/* block name */

    symtab_kinds kind()			{ return SYMTAB_BLOCK; }
    tree_block *block()			{ return blk; }

    block_symtab *new_unique_child(const char *base = NULL);

    label_sym *new_label(const char *n);
    label_sym *new_unique_label(const char *base = NULL);

    label_sym *lookup_label(const char *s, boolean up=TRUE)
	{ return static_cast<label_sym *>(lookup_sym(s, SYM_LABEL, up)); }

    void find_exposed_refs(base_symtab *dst_scope, replacements *r);
    block_symtab *clone_helper(replacements *r, boolean no_copy = FALSE);

    void add_sym(sym_node *s);
    void print(FILE *fp=stdout, int depth=0);
    void write(out_stream *os);
    void read(in_stream *is);
};


/*
 *  A proc_symtab is used as the top-level symbol table for a procedure.
 *  This class is basically the same as the block_symtab class with a few
 *  extensions.  The proc_symtab also contains the list of parameters for
 *  the procedure.  This is an ordered list of pointers to var_syms in the
 *  symtab; the flags for these var_syms must also be set to indicate that
 *  they are parameters.  All of the instructions within the procedure are
 *  assigned unique numbers, and the proc_symtab keeps track of the next
 *  instruction number to be used.  (The "tree_proc::number_instrs" function
 *  is used to assign these instruction numbers.)  Finally, the "number_locals"
 *  function is called automatically just before writing the symtab to
 *  assign ID numbers to the symbols and types in this symtab and all of its
 *  descendants.
 */

class proc_symtab : public block_symtab {
    friend class block_symtab;
    friend class tree_proc;

private:
    sym_node_list *prms;		/* parameter list (in order) */
    unsigned next_inum;			/* next unused instruction number */

protected:
    virtual base_symtab *new_peer(const char *n) { return new proc_symtab(n); }

public:
    proc_symtab(const char *n);		/* name (usually the procedure name) */
    ~proc_symtab();

    symtab_kinds kind()			{ return SYMTAB_PROC; }
    sym_node_list *params()		{ return prms; }

    unsigned instr_num()		{ return next_inum; }
    unsigned next_instr_num()		{ return next_inum++; }

    void number_locals();

    proc_symtab *clone_helper(replacements *r, boolean no_copy = FALSE);

    void print(FILE *fp=stdout, int depth=0);
    void write(out_stream *os);
    void read(in_stream *is);
};

#endif /* SYMTAB_H */
