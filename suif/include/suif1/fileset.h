/*  File Set Declarations */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef FILESET_H
#define FILESET_H

#pragma interface

RCS_HEADER(fileset_h,
    "$Id$")

class in_stream;
class out_stream;
class file_set_entry;


DECLARE_DLIST_CLASS(file_set_entry_list, file_set_entry*);


/*
 *  The file_set is basically just a list of file_set_entries.  Its main
 *  function is to handle opening streams to avoid exceeding the OS limit
 *  on the number of open files.  If there are too many open streams, the
 *  file_set will automatically find one to close.  All of this is invisible
 *  to the user.
 */

class file_set {
    friend class file_set_entry;
    friend class proc_sym;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    file_set(const file_set &);
    file_set &operator=(const file_set &);

    file_set_entry_list *files;		/* list of files */
    file_set_entry_list_iter *iter;	/* file iterator */
    global_symtab *globs;		/* global symbol table */
    long open_max;			/* maximum number of open files */
    int open_cnt;			/* current number of open files */
    boolean read_a_file;		/* have we read an input file yet? */

    void open_stream(base_stream *s, file_set_entry *fse);
    void close_stream(base_stream *s);
    void close_other_file(file_set_entry *fse);

public:
    file_set();
    ~file_set();

    global_symtab *globals()		{ return globs; }
    file_set_entry_list *file_list()	{ return files; }

    file_set_entry *add_file(char *in, char *out); /* either may be NULL */
    file_set_entry *delete_file( file_set_entry *fse );

    /* built-in file iterator */
    void reset_iter()			{ iter->reset(files); }
    file_set_entry *next_file()		{ return iter->is_empty() ?
					      NULL : iter->step(); }

    file_set_entry *find_by_num(int file_id);

    long get_max_open_files(void)	{ return open_max; }
    void set_max_open_files(long new_max);
};

EXPORTED_BY_SUIF file_set *fileset;


/*
 *  A file_set_entry keeps track of the input and output streams for a
 *  single source file.  It also includes the file_symtab.  The
 *  reset_proc_iter and next_proc methods provide a convenient means of
 *  visiting all of the procedures that are defined in the input file.
 */

class file_set_entry : public suif_object {
    friend class file_set;
    friend class proc_sym;
    friend class suif_linker;

private:
    file_set *par;			/* parent file_set */
    in_stream *is;			/* input stream */
    out_stream *os;			/* output stream */
    file_symtab *table;			/* corresponding file symtab */
    long symtab_ptr_loc;		/* offset in file for symtab info */
    sym_node_list_iter *global_proc_iter;
    sym_node_list_iter *static_proc_iter;
    int the_id;

    void set_file_id(int new_id)	{ the_id = new_id; }

protected:
    void fill_table();
    void write_table();
    void check_global_symtab(global_symtab *newglobs);
    void get_contents();
    boolean get_machine_param(int *flag, const char *annote_name, boolean is_array,
			      boolean read_a_file);
    void save_machine_param(int val, const char *annote_name, int ndx);

public:
    file_set_entry(file_set *p, char *inn, char *outn);
    ~file_set_entry();
    
    object_kinds object_kind()		{ return FILE_OBJ; }

    file_set *parent()			{ return par; }
    file_symtab *symtab()		{ return table; }
    const char *name();			/* name of the input (or output) file*/
    const char *in_name()		{ return (is ? is->name() : NULL); }
    const char *out_name()		{ return (os ? os->name() : NULL); }
    int file_id()			{ return the_id; }

    void set_symtab(file_symtab *new_table) { table = new_table; }
    void add_outfile(char *outn);

    void reset_proc_iter();
    proc_sym *next_proc();		/* find the next procedure */
};

/*
 *  This variable is normally set to FALSE.  It is only set to TRUE if
 *  the ``-no-glob-check'' flag is given as a command-line argument or
 *  if the user program manually changes it to TRUE.  That is, it only
 *  becomes TRUE if the user explicitly sets it to TRUE, whether that
 *  is the run-time user with the command-line or the link-time user
 *  writing the program that uses the SUIF library.
 *
 *  When set to TRUE, the automatic checking that global symbol tables
 *  match when there is more than one file in a file set is disabled.
 *  It is important to note that the requirement that global symbol
 *  tables match is still a requirement, it's just not explicitly
 *  checked.  That means that if the global symbol tables don't match,
 *  the library will probably fail in strange ways instead of giving a
 *  nice error message that the symbol tables don't match.
 *
 *  This is provided strictly for performance reasons.  The checks of
 *  global symbol tables can take a while, so if you're certain they
 *  will be right, you can save that time by not doing the check.
 *
 *  The linksuif pass is normally used to make sure the global symbol
 *  tables match initially.
 */
EXPORTED_BY_SUIF boolean disable_global_symtab_check;

#endif /* FILESET_H */
