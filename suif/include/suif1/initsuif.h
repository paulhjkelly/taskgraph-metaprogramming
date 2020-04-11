/*  Library Initialization */

/*  Copyright (c) 1994, 1997, 1998 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef INITSUIF_H
#define INITSUIF_H

#pragma interface

RCS_HEADER(initsuif_h,
    "$Id$")

/*
 *  Library initialization.  For the SUIF library to be able to print the
 *  version numbers for all the libraries linked in a particular program,
 *  it needs to know which libraries are included.  Thus, the user must
 *  register each library (using the "register_library" function) before
 *  calling "init_suif".  The library records the information for the
 *  registered libraries in a list of "suif_library" structures.  The
 *  "start_suif" function declared here may be automatically generated
 *  by the makefiles to register the appropriate libraries.
 */

typedef void (*lib_init_f)(int& argc, char *argv[]);
typedef void (*lib_exit_f)();

struct suif_library {
    const char *name;			/* name of the library (e.g. "suif") */
    const char *version;		/* the library version */
    const char *compile_info;		/* who/when/where was it compiled */
    lib_init_f initializer;		/* function to initialize it */
    lib_exit_f finalizer;		/* function to deallocate storage */
};

DECLARE_DLIST_CLASS(library_list, suif_library*);
EXPORTED_BY_SUIF library_list *suif_libraries;	/* list of registered libraries */

void register_library(const char *nm, const char *ver, const char *info,
		      lib_init_f init_f=NULL,
		      lib_exit_f exit_f=NULL);
void start_suif(int& argc, char *argv[]);
void init_suif(int& argc, char *argv[]);
#define exit_suif exit_suif1
extern "C" void exit_suif1(void);

#define LIBRARY(name, init_f, exit_f) \
    EXPORTED_BY_SUIF const char *lib ## name ## _ver_string; \
    EXPORTED_BY_SUIF const char *lib ## name ## _who_string; \
    EXPORTED_BY_SUIF void init_f(int& argc, char *argv[]); \
    EXPORTED_BY_SUIF void exit_f(); \
    register_library(#name, lib ## name ## _ver_string, \
		     lib ## name ## _who_string, init_f, exit_f)

/* these variables are linked in by the standard Makefiles */
extern const char *prog_ver_string;	/* program version, e.g. "2.4" */
extern const char *prog_who_string;	/* who, when, where compiled/linked */
extern const char *prog_suif_string;	/* SUIF distribution version */
#define libsuif_ver_string libsuif1_ver_string
#define libsuif_who_string libsuif1_who_string
#define libsuif_suif_string libsuif1_suif_string
extern const char *libsuif1_ver_string;
extern const char *libsuif1_who_string;
extern const char *libsuif1_suif_string;

extern "C" void enter_suif1(int *argc, char *argv[]);
extern "C" void register_suif1(void);

//HACK
class type_node;
class file_set;

struct statics {
	library_list *suif_libraries;
	string_table *lexicon;
	char *_suif_command_line;
	char *_suif_program_name;
	file_set *fileset;
    type_node *type_v0;
    type_node *type_s8;
    type_node *type_s16;
    type_node *type_s32;
    type_node *type_s64;
    type_node *type_u8;
    type_node *type_u16;
    type_node *type_u32;
    type_node *type_u64;
    type_node *type_f32;
    type_node *type_f64;
    type_node *type_f128;

    type_node *type_void;
    type_node *type_ptr;
    type_node *type_char;
    type_node *type_signed_char;
    type_node *type_unsigned_char;
    type_node *type_signed_short;
    type_node *type_unsigned_short;
    type_node *type_signed;
    type_node *type_unsigned;
    type_node *type_signed_long;
    type_node *type_unsigned_long;
    type_node *type_signed_longlong;
    type_node *type_unsigned_longlong;
    type_node *type_ptr_diff;
    type_node *type_float;
    type_node *type_double;
    type_node *type_longdouble;
};

void init_suif_shared_suif ( statics *st );
void fill_statics( statics *st );

#endif /* INITSUIF_H */
