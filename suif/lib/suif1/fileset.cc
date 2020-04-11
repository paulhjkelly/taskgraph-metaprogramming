/*  File Set Implementation */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "fileset.h"

#define RCS_BASE_FILE fileset_cc

#include <machine_dependent.h>
#include "suif1.h"
#include "suif_internal.h"
#include <cstdio>
#include <fcntl.h>

RCS_BASE(
    "$Id$")

static void set_default_target();

file_set *fileset = NULL;
boolean disable_global_symtab_check = FALSE;

base_symtab *write_scope = NULL;

/* We make explicit copy constructor and assignment operator and
 * make them private to foil C++'s automatic default versions. */
file_set::file_set(const file_set &)  { assert(FALSE); }
file_set &file_set::operator=(const file_set &)  { assert(FALSE); return *this; }

/*
 *  File_set constructor.  This is called when the fileset is created in
 *  the init_suif routine.
 */

file_set::file_set ()
{
    assert_msg(!fileset,
	       ("file_set::file_set - only one file_set is allowed"));

    files = new file_set_entry_list;
    iter = new file_set_entry_list_iter(files);
    globs = new global_symtab("global");
    set_default_target();
    globs->predefine_types();
    read_a_file = FALSE;

    /* keep track of number of open files */
    open_max = SUIF_FOPEN_MAX;
    /* check for non-SUIF files that are already open */
    for (int f = 0; f < SUIF_FOPEN_MAX; f++) {
        if (fcntl(f, F_GETFD) != -1) {
            open_max -= 1;
        }
    }

    if (open_max < 6)
      {
        /* In this case, we were unable to get the OS to guarantee a
         * reasonable number of files we can have open.  Usually, this
         * is because the OS is being too conservative in what it
         * tells us through the channels above.  For example, under
         * Solaris on a SPARC, FOPEN_MAX is 20, even though there are
         * really 64 positions available, at least under some
         * circumstances.  Running a program under gdb within emacs
         * and then re-running the program ends up eating up 20 open
         * files in all (but no more).  This leaves us with zero
         * additional files that the OS guarantees us, even though
         * there are 44 that can really be open (as experiments
         * verified).
         *
         * To address this problem, if the OS won't guarantee us
         * sufficient files, we just try ourselves to see if we can
         * keep a few "dev/null" handles open.
         */
        FILE *fps[6];
        int null_count;
        for (null_count = 0; null_count < 6; ++null_count)
          {
            fps[null_count] = fopen("/dev/null", "r");
            if (fps[null_count] == NULL)
                break;
          }
        for (int null_num = 0; null_num < null_count; ++null_num)
            fclose(fps[null_num]);
        if (null_count > open_max)
            open_max = null_count;
      }
    assert_msg(open_max > 0,
	       ("file_set::file_set - cannot open any files"));

    open_cnt = 0;
}


/*
 *  File_set destructor.  This destroys the file_set_entries, which causes
 *  them to write out the global symtabs and close the associated i/o streams.
 */

file_set::~file_set ()
{
    /* make sure that the global symtab contains no variable definitions */
    assert_msg(globs->var_defs()->is_empty(),
	       ("file_set destructor - "
		"variables defined in the global symtab"));

    /* write out and delete each of the files */
    while (!file_list()->is_empty()) {
	delete file_list()->pop();
    }

    if( files != NULL ) {
    delete files;
      files = NULL;
    }
    if( iter != NULL ) {
    delete iter;
      iter = NULL;
    }
    if( globs != NULL ) {
    delete globs;
      globs = NULL;
    }
}


/*
 *  Add a file to the set.
 */

file_set_entry *
file_set::add_file (char *in, char *out)
{
    file_set_entry *fse = new file_set_entry(this, in, out);
    file_list()->append( fse );
    return fse;
}

file_set_entry *
file_set::delete_file( file_set_entry *fse ) {
  return file_list()->remove( fse );
}

file_set_entry *
file_set::find_by_num (int file_id)
{
    file_set_entry_list_iter fse_iter(files);
    while (!fse_iter.is_empty())
      {
        file_set_entry *this_fse = fse_iter.step();
        if (this_fse->file_id() == file_id)
            return this_fse;
      }
    return NULL;
}


/*
 *  Set the maximum number of files the file set is allowed to keep
 *  open at one time.  By default this is set to the OS limit, but if
 *  a user opens other files outside of those controlled by this file
 *  set, the user must give the file set a lower limit.
 *
 *  The user can set this temporarily to zero to force all files used
 *  by the file set to be closed, but it must be set back to a higher
 *  limit before any operations that might need to use an open file
 *  are performed.
 */
void
file_set::set_max_open_files(long new_max)
{
    assert_msg(new_max >= 0,
	       ("file_set::set_max_open_files - maximum must be "
		"non-negative"));
    while (open_cnt > new_max)
	close_other_file(NULL);
    open_max = new_max;
}


/*
 *  Open a stream on behalf of a file_set_entry.  If too many files are open,
 *  search through the file list for an open stream to close.  Besides not
 *  closing a stream associated with the current file_set_entry, the function
 *  doesn't do anything fancy--it just closes the first open stream that it
 *  finds.
 */

void
file_set::open_stream (base_stream *s, file_set_entry *fse)
{
    /* do nothing if the stream is already open */
    if (s->is_open()) return;

    assert_msg(open_max > 0,
	       ("file_set::open_stream - max open files is zero"));

    /* check if there are too many open files */
    if (open_cnt == open_max)
	close_other_file(fse);

    s->open();
    open_cnt += 1;
}


/*
 *  Close a stream on behalf of a file_set_entry.  The file_set does
 *  all opening and closing of streams so that it can keep an accurate
 *  count of the number of open streams at an given time.
 */
void
file_set::close_stream (base_stream *s)
{
    if (s->is_open()) {
	s->close();
	assert(open_cnt > 0);
	open_cnt -= 1;
    }
}


/*
 *  Close any one stream that is not associated with fse; if fse is
 *  NULL, or if the maximum number of open streams allowed is one,
 *  close any stream at all.  This is called internally to reduce the
 *  number of currently open files.
 */
void
file_set::close_other_file(file_set_entry *fse)
{
    /* find a stream to close */
    file_set_entry_list_iter file_iter(file_list());
    while (!file_iter.is_empty()) {
	file_set_entry *other_fse = file_iter.step();

	/* don't mess with the current fse */
	if ((other_fse == fse) && (open_max > 1)) continue;

	/* try to close the input stream */
	in_stream *is = other_fse->is;
	if (is && is->is_open()) {
	    is->close();
	    open_cnt -= 1;
	    return;
	}

	/* try to close the output stream */
	out_stream *os = other_fse->os;
	if (os && os->is_open()) {
	    os->close();
	    open_cnt -= 1;
	    return;
	}
    }
    assert(FALSE);
}


/*
 *  Set the default target parameters.  These should usually be overridden
 *  by the annotations on the file_set_entries.
 */

void
set_default_target ()
{
    target.is_big_endian = FALSE;
    target.addressable_size = 8;
    target.char_is_signed = TRUE;

    target.size[C_char] = 8;
    target.size[C_short] = 16;
    target.size[C_int] = 32;
    target.size[C_long] = 32;
    target.size[C_longlong] = 64;
    target.size[C_float] = 32;
    target.size[C_double] = 64;
    target.size[C_longdouble] = 128;
    target.size[C_ptr] = 32;

    target.align[C_char] = 8;
    target.align[C_short] = 16;
    target.align[C_int] = 32;
    target.align[C_long] = 32;
    target.align[C_longlong] = 64;
    target.align[C_float] = 32;
    target.align[C_double] = 64;
    target.align[C_longdouble] = 128;
    target.align[C_ptr] = 32;

    target.array_align = 32;
    target.struct_align = 32;
    target.ptr_diff_type = C_int;
}


/*****************************************************************************/


/*
 *  File_set_entry constructor.  If there is an input file, open it and
 *  read the global symbol tables.  If there is an output file, write the
 *  low-level file header.
 */

file_set_entry::file_set_entry (file_set *p, char *inn, char *outn)
{
    /* record the parent file_set */
    par = p;

    /* create the i/o streams */
    is = inn ? new in_stream(lexicon->enter(inn)->sp) : NULL;
    os = NULL;

    table = new file_symtab(inn ? inn : (outn ? outn : (char*)"file"), this);
    par->globals()->add_child(table);
    if (is) {
	par->open_stream(is, this);

	/* read in the global and file symtabs */
	fill_table();

	/* read the annotations for the file_set_entry */
	instruction *mrk = instruction::read(is, symtab(), NULL);
	mrk_to_annotes(mrk);
	delete mrk;

	/* read extra info from annotations on the file_set_entry */
	get_contents();
    }

    if (outn != NULL)
	add_outfile(outn);

    {
	immed_list *file_num_immeds = (immed_list *)(get_annote(k_file_num));
	if (file_num_immeds == NULL) {
	    the_id = 0;
	} else {
	    assert(file_num_immeds->count() == 1);
	    assert(file_num_immeds->head()->contents.is_integer());
	    the_id = file_num_immeds->head()->contents.integer();
	    delete file_num_immeds;
	}
    }

    /* record the current pass name on a "history" annotation */
    annote *an = new annote(k_history);
    an->immeds()->append(immed(_suif_command_line));
    annotes()->append(an);

    an = new annote(k_version_history);
    an->immeds()->append(immed(_suif_prog_base_name));
    an->immeds()->append(immed(prog_ver_string));
    an->immeds()->append(immed("suif distribution"));
    an->immeds()->append(immed(libsuif1_suif_string));

    library_list_iter library_iter(suif_libraries);
    while (!library_iter.is_empty())
      {
        suif_library *this_library = library_iter.step();
        an->immeds()->append(immed(this_library->name));
        an->immeds()->append(immed(this_library->version));
      }

    annotes()->append(an);

    global_proc_iter = new sym_node_list_iter;
    static_proc_iter = new sym_node_list_iter;
    reset_proc_iter();
}


/*
 *  Read the global and file symbol tables.  Reading the global symtab is
 *  a little tricky.  If this is the first input file, the new symtab replaces
 *  the existing (empty) global symtab.  Otherwise, if the global symtab has
 *  already been read from another file, we just check to make sure that the
 *  new table is the same.  If disable_global_symtab_check has been
 *  set to TRUE, the user is asserting that the check will be
 *  successful and can be omitted to save time.  If it turns out the
 *  tables are not the same and the check is disabled, other things
 *  will probably fail in strange ways.
 */

void
file_set_entry::fill_table ()
{
    /* read the symtab position from the beginning of the file */
    instruction *mrk = instruction::read(is, parent()->globals(), NULL);
    annote *an = mrk->annotes()->get_annote(k_file_symtab_ptr);
    if ((mrk->opcode() != io_mrk) || !an || (an->immeds()->count() != 1)) {
	error_line(1, NULL, "File '%s' does not contain a symtab pointer",
		   is->name());
    }
    immed position_immed = an->immeds()->pop();
    if (!position_immed.is_int_const()) {
        error_line(1, NULL, "File '%s' has non-integer symtab pointer",
                   is->name());
    }
    if (!position_immed.is_long_int()) {
        error_line(1, NULL,
                   "File '%s' is too big -- symtab pointer out of range of "
                   "file indexing on this machine", is->name());
    }
    long pos = position_immed.long_int();
    delete an;
    delete mrk;

    if (pos == -1) {
        error_line(1, NULL,
                   "File '%s' has no symtab pointer -- probably the result "
                   "of a SUIF file which was never completely written out",
                   is->name());
    }

    is->setpos(pos);

    /* read the global symtab (temporarily make the new global symtab
       a child of the existing one so that it can access the predefined
       types while it is being read in) */

    global_symtab *tmp_globals = new global_symtab("globals");
    parent()->globals()->add_child(tmp_globals);
    tmp_globals->read(is);
    parent()->globals()->remove_child(tmp_globals);

    if (parent()->read_a_file) {

	/* make sure the new symbol table matches and delete it */
	if (!disable_global_symtab_check)
	    check_global_symtab(tmp_globals);
	delete tmp_globals;

    } else {

	/* move the file_symtab to be a child of tmp_globals */
	parent()->globs->remove_child(symtab());
	tmp_globals->add_child(symtab());

	/* make sure the existing symbol table is empty (except possibly for
	   some predefined types) and replace it */

	assert_msg(parent()->globs->symbols()->is_empty() &&
		   parent()->globs->children()->is_empty(),
		   ("global symbol table not empty"));
	delete parent()->globs;
	parent()->globs = tmp_globals;
    }

    /* read the file symtab */
    symtab()->read(is);

    /* no need to restore the stream position */
}


/*
 *  Make sure that a new global symtab matches the existing global symtab.
 *  (The files in a file_set must be "linked" together by a separate SUIF
 *  pass to match up the global symtab entries.)
 */

void
file_set_entry::check_global_symtab (global_symtab *newglobs)
{
    global_symtab *globals = parent()->globals();

    type_node_list_iter type_iter(newglobs->types());
    while (!type_iter.is_empty()) {
	type_node *newtyp = type_iter.step();
	type_node *oldtyp = globals->lookup_type_id(newtyp->type_id());

	/* make sure they're equivalent */
	if (!oldtyp ||
	    (oldtyp->op() != newtyp->op()) ||
	    (oldtyp->size() != newtyp->size())) {
	    error_line(-1, NULL, "type in global symbol table does not match");
	}
    }

    sym_node_list_iter sym_iter(newglobs->symbols());
    while (!sym_iter.is_empty()) {
	sym_node *newsym = sym_iter.step();
	sym_node *oldsym = globals->lookup_sym_id(newsym->sym_id());

	/* make sure they're equivalent */
	if (!oldsym ||
	    (oldsym->kind() != newsym->kind()) ||
	    (oldsym->name() != newsym->name())) {
	    error_line(-1, NULL, "symbol in global symtab does not match");
	}
    }
}


/*
 *  Several kinds of information are included as annotations on the
 *  file_set_entries.  These annotations are stripped off when the file
 *  is first opened.  First, the target machine parameters are read.  If
 *  this is the first input file to be read, the global "target" structure
 *  is updated.  Otherwise, the parameters are checked against the existing
 *  ones.  Second, the file_set_entry contains annotations to identify
 *  procedure symbols whose bodies are defined within the file.  This
 *  information is read and used to set the "file" pointers in the proc_syms.
 */

void
file_set_entry::get_contents ()
{
    /* read the target machine parameters */
    get_machine_param((int*)&target.is_big_endian, k_is_big_endian,
		      FALSE, parent()->read_a_file);
    get_machine_param(&target.addressable_size, k_addressable_size,
		      FALSE, parent()->read_a_file);
    get_machine_param((int*)&target.char_is_signed, k_char_is_signed,
		      FALSE, parent()->read_a_file);
    while (get_machine_param(&target.size[0], k_C_type_size,
			     TRUE, parent()->read_a_file)) {
    }
    while (get_machine_param(&target.align[0], k_C_type_align,
			     TRUE, parent()->read_a_file)) {
    }
    get_machine_param(&target.array_align, k_array_align,
		      FALSE, parent()->read_a_file);
    get_machine_param(&target.struct_align, k_struct_align,
		      FALSE, parent()->read_a_file);

    int tmp_pdiff_type = target.ptr_diff_type;
    get_machine_param(&tmp_pdiff_type, k_ptr_diff_type,
		      FALSE, parent()->read_a_file);
    target.ptr_diff_type = (C_types)tmp_pdiff_type;

    /* check if this is the first input file */
    if (!parent()->read_a_file) {
	parent()->read_a_file = TRUE;

	/* reset the predefined type pointers to use the new parameters */
	parent()->globals()->predefine_types();
    }

    /* find out which procedure bodies are contained in this file */
    immed_list *iml;
    while ((iml = (immed_list *)get_annote(k_proc_in_file))) {
	proc_sym *psym = (proc_sym *)iml->pop().symbol();
	assert(psym->is_proc());
	psym->set_extern(FALSE);
	psym->fse = this;
	psym->file_pos = iml->pop().long_int();
	delete iml;
    }
}


/*
 *  Read a machine parameter annotation from the file_set_entry.  If this
 *  is the first entry in the file_set, the parameter overrides the default
 *  value.  Otherwise, this just checks that all of the parameters match
 *  the values specified in the first file_set_entry.
 */

boolean
file_set_entry::get_machine_param (int *flag, const char *annote_name,
				   boolean is_array, boolean read_a_file)
{
    immed_list *iml = (immed_list *)get_annote(annote_name);
    if (iml) {

	int val = iml->pop().integer();
	if (is_array) {
	    flag += val;
	    val = iml->pop().integer();
	}
	delete iml;

	if (read_a_file) {
	    if (*flag != val) {
		error_line(1, NULL, "machine parameter annotation '%s' "
			   "does not match", annote_name);
	    }
	} else {
	    *flag = val;
	}

	return TRUE;
    }
    return FALSE;
}


/*
 *  Write out the global and file symbol tables.
 */

void
file_set_entry::write_table ()
{
    /* get the file position where the symtab will be written */
    long current_pos = os->getpos();

    /* create a mark to record the symtab position */
    instruction *symtab_ptr = new in_rrr(io_mrk);
    annote *an = new annote(k_file_symtab_ptr);
    an->immeds()->append(immed(current_pos));
    symtab_ptr->annotes()->append(an);

    /* write the mark at the beginning of the file */
    os->flush_cache();
    os->setpos(symtab_ptr_loc);
    symtab_ptr->write(os);
    delete symtab_ptr;

    /* go back and write out the symtab */
    os->setpos(current_pos);
    os->flush_cache();

    parent()->globals()->write(os);

    symtab()->write(os);
}


/*
 *  File_set_entry destructor.  If necessary, write out the file_symtab
 *  and file_set_entry annotations.  Close the i/o streams.
 */

file_set_entry::~file_set_entry ()
{
  delete global_proc_iter; global_proc_iter = NULL;
  delete static_proc_iter; static_proc_iter = NULL;

    /* close the i/o streams */
    if (is) {
	parent()->close_stream(is);
	delete is; is = NULL;
    }
    if (os) {
	parent()->open_stream(os, this);

        assert(write_scope == NULL);
        write_scope = symtab();

	/* write the file symtab */
	write_table();

	/* save the target machine parameters on the file_set_entry */
	save_machine_param(target.is_big_endian, k_is_big_endian, -1);
	save_machine_param(target.addressable_size, k_addressable_size, -1);
	save_machine_param(target.char_is_signed, k_char_is_signed, -1);
	for (int i = 0; i < num_C_types; i++) {
	    save_machine_param(target.size[i], k_C_type_size, i);
	    save_machine_param(target.align[i], k_C_type_align, i);
	}
	save_machine_param(target.array_align, k_array_align, -1);
	save_machine_param(target.struct_align, k_struct_align, -1);
	save_machine_param(target.ptr_diff_type, k_ptr_diff_type, -1);

	if (file_id() != 0) {
	    immed_list *file_num_immeds = new immed_list;
	    file_num_immeds->append(immed(file_id()));
	    append_annote(k_file_num, file_num_immeds);
	}

	/* write the file_set_entry annotations */
	instruction *mrk = new in_rrr(io_mrk);
	copy_annotes(mrk);
	mrk->write(os);
	delete mrk;
	mrk = NULL;

	/* write the end-of-stream marker */
	os->write_int((int)io_eos);

        assert(write_scope == symtab());
        write_scope = NULL;

	parent()->close_stream(os);
	delete os;
	os = NULL;
    }

    /* remove the symtab from its parent's child list */
    if (symtab()) {
	assert_msg(symtab()->parent(), ("symbol table has no parent!"));
 	symtab()->parent()->remove_child(symtab());
 	delete symtab();
    }
}


/*
 *  Save one of the target machine parameters as an annotation on the
 *  file_set_entry.  If the "ndx" parameter is non-negative, store it
 *  as the first immed value on the annotation.
 */

void
file_set_entry::save_machine_param (int val, const char *annote_name, int ndx)
{
    annote *an = new annote(annote_name);
    if (ndx >= 0) {
	an->immeds()->append(immed(ndx));
    }
    an->immeds()->append(immed(val));

    annotes()->append(an);
}


/*
 *  Find the name of the file.  If there is no input stream, use the
 *  name from the output stream.
 */

const char *
file_set_entry::name ()
{
    if (is) return is->name();
    return os->name();
}


void
file_set_entry::add_outfile (char *outn)
{
    assert(os == NULL);
    assert(outn != NULL);

    os = new out_stream(lexicon->enter(outn)->sp);

    /* write the header for the output stream */
    par->open_stream(os, this);

    assert(write_scope == NULL);
    write_scope = table;

    /* write out a mark to record the position of the file symtab */
    instruction *mrk = new in_rrr(io_mrk);
    annote *an = new annote(k_file_symtab_ptr);
    an->immeds()->append(immed(-1));
    mrk->annotes()->append(an);
    symtab_ptr_loc = os->getpos();
    mrk->write(os);
    delete mrk;

    assert(write_scope == table);
    write_scope = NULL;
}


/*
 *  Reset the procedure iterators.
 */

void
file_set_entry::reset_proc_iter ()
{
    global_proc_iter->reset(parent()->globals()->symbols());
    static_proc_iter->reset(symtab()->symbols());
}


/*
 *  Find the next proc_sym that is defined (not extern).  Because
 *  proc_syms may appear in either the global or file symtabs, we
 *  actually need two iterators here.
 */

proc_sym *
file_set_entry::next_proc ()
{
    /* first check the file symtab */
    while (!static_proc_iter->is_empty()) {
	sym_node *sn = static_proc_iter->step();
	if (sn->is_proc()) return (proc_sym *)sn;
    }

    /* next check the global symtab */
    while (!global_proc_iter->is_empty()) {
	sym_node *sn = global_proc_iter->step();
	if (sn->is_proc()) {
	    proc_sym *psym = (proc_sym *)sn;
	    if (psym->file() == this) return psym;
	}
    }

    return NULL;
}


