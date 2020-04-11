/*  Library Initialization */

/*  Copyright (c) 1994, 1997, 1998 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "initsuif.h"

#define RCS_BASE_FILE initsuif_cc

#include "suif1.h"
#include "suif_internal.h"
#include <cstring>

RCS_BASE(
    "$Id$")


/*
 *  There are three logical states the initialization system can be in:
 *
 *    (1) suif_libraries is NULL.
 *    (2) suif_libraries is non-NULL and initializations_done is FALSE.
 *    (3) suif_libraries is non-NULL and initailizations_done is TRUE.
 *
 *  The system starts in state 1.  In state 1, a call to
 *  register_library() will change to state 2, a call to init_suif()
 *  will change to state 3, and a call to exit_suif1() will not change
 *  the system from state 1 (and will have no other effect either).
 *
 *  In state 2, a call to register_library() will keep the system in
 *  state 2, a call to init_suif() will change to state 3, and a call
 *  to exit_suif1() will change to state 1.
 *
 *  In state 3, calls to register_library() are illegal, a call to
 *  init_suif() will be ignored (the system will remain in state 3),
 *  and a call to exit_suif1() will change to state 1.
 */
boolean initializations_done = FALSE;
library_list *suif_libraries = NULL;

boolean warn_only_once = FALSE;
boolean suif_shared = FALSE;

/*
 *  Options to be recognized by the command-line parser.
 */

static boolean version_only;
static cmd_line_option libsuif_options[] = {
    { CLO_NOARG, "-version",        "", &version_only },
    { CLO_NOARG, "-err-context",    "", &show_error_context },
    { CLO_NOARG, "-no-warn",        "", &ignore_warnings },
    { CLO_NOARG, "-warn-only-once", "", &warn_only_once },
    { CLO_NOARG, "-no-glob-check",  "", &disable_global_symtab_check }
};


/*  local function prototypes */
static void suif_new_handler();
static void init_suif_internal(int& argc, char *argv[]);
static void exit_suif_internal();


/*
 *  Add a library to the list of registered libraries.  This list records
 *  the library name, version, and compilation information along with
 *  pointers to the initialization and finalization functions for the library.
 */

void
register_library (const char *nm, const char *ver, const char *info,
		  lib_init_f init_f, lib_exit_f exit_f)
{
	// HACK
	if ( suif_shared ) {
		return;
	}
    if ((suif_libraries != NULL) && initializations_done) {
        assert_msg(FALSE,
                   ("attempt to register a library after init_suif() and "
                    "before exit_suif1()"));
    }
    suif_library *new_lib = new suif_library;
    new_lib->name = nm;
    new_lib->version = ver;
    new_lib->compile_info = info;
    new_lib->initializer = init_f;
    new_lib->finalizer = exit_f;

    if (suif_libraries == NULL) {
        suif_libraries = new library_list;
        initializations_done = FALSE;
    }
    suif_libraries->append(new_lib);
}


/*
 *  Initialize the SUIF library and any libraries that have been registered.
 */

void
init_suif (int& argc, char * argv[])
{
    int i;

    if ((suif_libraries != NULL) && initializations_done)
        return;

    if (suif_libraries == NULL)
        suif_libraries = new library_list;
    initializations_done = TRUE;

    /* record the entire command line */
    int cmd_line_len = 1;
    for (i = 0; i < argc; i++) {
	cmd_line_len += strlen(argv[i]) + 1;
    }
    _suif_command_line = new char[cmd_line_len];
    _suif_command_line[0] = '\0';
    if (argc > 0) {
	for (i = 0; i < argc - 1; i++) {
	    strcat(_suif_command_line, argv[i]);
	    strcat(_suif_command_line, " ");
	}
	strcat(_suif_command_line, argv[argc - 1]);
    }

    _suif_program_name = argv ? argv[0] : NULL;
    if (_suif_program_name != NULL) {
        _suif_prog_base_name = strrchr(_suif_program_name, '/');
        if (_suif_prog_base_name == NULL)
            _suif_prog_base_name = _suif_program_name;
        else
            ++_suif_prog_base_name;
    } else {
        _suif_prog_base_name = NULL;
    }

    // set_new_handler(suif_new_handler);

    lexicon = new string_table;
    k_suif = lexicon->enter("suif")->sp;

    /* register the SUIF library itself */
    suif_library *new_lib = new suif_library;
    new_lib->name = "suif1";
    new_lib->version = libsuif1_ver_string;
    new_lib->compile_info = libsuif1_who_string;
    new_lib->initializer = init_suif_internal;
    new_lib->finalizer = exit_suif_internal;
    suif_libraries->push(new_lib);

    parse_cmd_line(argc, argv, libsuif_options,
		   sizeof(libsuif_options) / sizeof(cmd_line_option));

    if (version_only) {
	fprintf(stderr, "%s %s %s\n", _suif_prog_base_name, prog_ver_string,
		prog_who_string);
	fprintf(stderr, "    Based on SUIF distribution %s\n",
                libsuif1_suif_string);
	fprintf(stderr, "    Linked with:\n");
	library_list_iter libiter(suif_libraries);
	while (!libiter.is_empty()) {
	    suif_library *lib = libiter.step();
            fprintf(stderr, "  lib%s %s %s\n",
		    lib->name, lib->version, lib->compile_info);
	}
        exit(0);
    }

    /* call the initializers for the registered libraries */
    library_list_iter libiter(suif_libraries);
    while (!libiter.is_empty()) {
	suif_library *lib = libiter.step();
	if (lib->initializer) lib->initializer(argc, argv);
    }
}



//HACK

void
init_suif_shared_suif ( statics *st ) {
    suif_libraries = st->suif_libraries;
    initializations_done = TRUE;

    _suif_command_line = st->_suif_command_line;
    _suif_program_name = st->_suif_program_name;

    lexicon = st->lexicon;
    fileset = st->fileset;

    type_v0 = st->type_v0;
    type_s8 = st->type_s8;
    type_s16 = st->type_s16;
    type_s32 = st->type_s32;
    type_s64 = st->type_s64;
    type_u8 = st->type_u8;
    type_u16 = st->type_u16;
    type_u32 = st->type_u32;
    type_u64 = st->type_u64;
    type_f32 = st->type_f32;
    type_f64 = st->type_f64;
    type_f128 = st->type_f128;
    type_void = st->type_void;
    type_ptr = st->type_ptr;
    type_char = st->type_char;
    type_signed_char = st->type_signed_char;
    type_unsigned_char = st->type_unsigned_char;
    type_signed_short = st->type_signed_short;
    type_unsigned_short = st->type_unsigned_short;
    type_signed = st->type_signed;
    type_unsigned = st->type_unsigned;
    type_signed_long = st->type_signed_long;
    type_unsigned_long = st->type_unsigned_long;
    type_signed_longlong = st->type_signed_longlong;
    type_unsigned_longlong = st->type_unsigned_longlong;
    type_ptr_diff = st->type_ptr_diff;
    type_float = st->type_float;
    type_double = st->type_double;
    type_longdouble = st->type_longdouble;
    suif_shared = TRUE;
}

//HACK
void
fill_statics( statics *st ) {
	st->lexicon = lexicon;
    st->_suif_command_line = _suif_command_line;
    st->_suif_program_name = _suif_program_name;
    st->suif_libraries = suif_libraries;
    st->fileset = fileset;

    st->type_v0 = type_v0;
    st->type_s8 = type_s8;
    st->type_s16 = type_s16;
    st->type_s32 = type_s32;
    st->type_s64 = type_s64;
    st->type_u8 = type_u8;
    st->type_u16 = type_u16;
    st->type_u32 = type_u32;
    st->type_u64 = type_u64;
    st->type_f32 = type_f32;
    st->type_f64 = type_f64;
    st->type_f128 = type_f128;
    st->type_void = type_void;
    st->type_ptr = type_ptr;
    st->type_char = type_char;
    st->type_signed_char = type_signed_char;
    st->type_unsigned_char = type_unsigned_char;
    st->type_signed_short = type_signed_short;
    st->type_unsigned_short = type_unsigned_short;
    st->type_signed = type_signed;
    st->type_unsigned = type_unsigned;
    st->type_signed_long = type_signed_long;
    st->type_unsigned_long = type_unsigned_long;
    st->type_signed_longlong = type_signed_longlong;
    st->type_unsigned_longlong = type_unsigned_longlong;
    st->type_ptr_diff = type_ptr_diff;
    st->type_float = type_float;
    st->type_double = type_double;
    st->type_longdouble = type_longdouble;
}


void
suif_new_handler ()
{
    error_line(1, NULL, "Out of Memory");
}


/*
 *  This is the initialization function given when "init_suif" registers
 *  the SUIF library itself.  It initializes the annotation manager, adds
 *  the predefined annotations, and creates the file set.
 */

void
init_suif_internal (int& /* argc */, char * /* argv */ [])
{
    init_aman();
    init_suif_annotes();
    init_error_handler();

    fileset = new file_set;
}


/*
 *  SUIF library cleanup function.  Call the finalization functions for
 *  the registered libraries.  (This is not really necessary but it makes
 *  it easier to read the output from Purify.)
 */

extern "C" void exit_suif1 (void)
{
	if (suif_shared) {
		suif_libraries = NULL;
		initializations_done = 0;
		return;
	}
    delete [] _suif_command_line;
    _suif_command_line = NULL;

    if (suif_libraries == NULL)
	return;

    /* call the finalization functions in reverse order */
    while (!suif_libraries->is_empty()) {
	library_list_e *lib_e = suif_libraries->tail();
	suif_library *lib = lib_e->contents;
	delete suif_libraries->remove(lib_e);
	if (initializations_done) {
	    if (lib->finalizer) lib->finalizer();
	}
	delete lib;
    }

    delete suif_libraries;
    suif_libraries = NULL;
    initializations_done = 0;
}


/*
 *  This is the finalization function given when "init_suif" registers
 *  the SUIF library itself.  It deallocate all of the global data
 *  structures in the library.
 */




void
exit_suif_internal ()
{
  if( fileset != NULL ) {
    delete fileset;
    fileset = NULL;
  }
    free_aman();
  if( lexicon != NULL ) {
    delete lexicon;
    lexicon = NULL;
  }
    assert(clue_stack_empty());
  exit_error_handler();
}

extern "C" void enter_suif1(int *argc, char *argv[])
  {
    init_suif(*argc, argv);
  }

extern "C" void register_suif1(void)
  {
    /* empty */
  }
