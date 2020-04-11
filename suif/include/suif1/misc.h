/*  Miscellaneous Declarations */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#ifndef MISC_H
#define MISC_H

/*
 *  Macros for including RCS information and copyrights in SUIF
 *  source files.
 */

/*  SUIF string concatenation macros, needed for RCS and copyright macros */
#define SUIF_CAT_INTERNAL(a, b)  a ## b
#define SUIF_CAT(a, b)  SUIF_CAT_INTERNAL(a, b)

/*  RCS information inclusion macros */
#ifdef RCS_BASE_FILE
#define RCS_BASE(version_string) \
    const char * SUIF_CAT(rcs_string_, \
                          SUIF_CAT(RCS_BASE_FILE, _base)) = version_string;
#define RCS_HEADER(header_id, version_string) \
    const char * SUIF_CAT(rcs_string_, \
                          SUIF_CAT(RCS_BASE_FILE, \
                            SUIF_CAT(_, header_id))) = version_string;
#else
#define RCS_BASE(version_string)
#define RCS_HEADER(header_id, version_string)
#endif

/*  SUIF copyright inclusion macros */
#define INCLUDE_SUIF_COPYRIGHT_NAMED(name) \
    const char * SUIF_CAT(SUIF_copyright_, name) = \
        "Copyright (c) 1994 Stanford University";
#define INCLUDE_SUIF_COPYRIGHT  INCLUDE_SUIF_COPYRIGHT_NAMED(base)

/*  RCS information for this file -- it has to come after the macro
    definitions */
RCS_HEADER(misc_h,
    "$Id$")

#include <suif_copyright.h>
#include <machine_dependent.h>

#include <cstdio>
#include <cstdarg>
#include <cstddef>

/*
 *  g++ version 2.7.0 on IRIX 5.3 defines NULL as ((void *)0) in some
 *  standard include file, but then the compiler complains if a ``void
 *  *'' is assigned to any other pointer type without a cast, so it
 *  complains at almost every use of NULL.  As a work-around, for now
 *  we'll just remove any previous definition of NULL and then always
 *  #define it here to be simply ``0'', which is always legal and
 *  never gets a compiler complaint.
 */
#ifdef NULL
#undef NULL
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef _MODULE_
#define _MODULE_ NULL
#endif

#ifdef TRUE
#undef TRUE
#endif

#ifdef FALSE
#undef FALSE
#endif

typedef int boolean;
const int FALSE=0;
const int TRUE=1;
const int UNKNOWN=-1;

/*  Byte size on the system running the compiler (not the target machine) */
const int suif_byte_size = 8;


/*
 *  Target machine parameters.  This structure records various parameters
 *  for the target architecture, whether that be a real machine or a back-end
 *  C compiler.  The front-end sets these parameters and they are then passed
 *  along in the SUIF files.
 */

enum C_types {
    C_char,
    C_short,
    C_int,
    C_long,
    C_longlong,
    C_float,
    C_double,
    C_longdouble,
    C_ptr,
    num_C_types
};

struct machine_params {
    boolean is_big_endian;
    int addressable_size;		/* usually 8 (byte-addressable) */
    boolean char_is_signed;		/* "char" types signed by default? */
    int size[num_C_types];
    int align[num_C_types];
    int array_align;			/* extra alignment for arrays */
    int struct_align;			/* extra alignment for structs */
    C_types ptr_diff_type;		/* type produced by ptr subtraction */

    void print(FILE *fp=stdout, int depth=0);
    boolean operator==(machine_params &other);
    boolean operator!=(machine_params &other)	{ return !(*this == other); }
};
EXPORTED_BY_SUIF machine_params target;


/*
 *  Miscellaneous utility functions.
 */

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))

extern int suif_round_to_int(int n);
extern boolean is_power_of_2(int n);
char *const_string(const char *s);    
void suif_indent(FILE *fp, int depth);

class instruction;
extern boolean within_expr(instruction *dst, instruction *src);


/*
 *  Error handling functions.
 */

class suif_object;
class annote;
extern unsigned source_line_num(suif_object *the_object);
extern const char *source_file_name(suif_object *the_node);

EXPORTED_BY_SUIF char *_suif_program_name;
EXPORTED_BY_SUIF char *_suif_prog_base_name;
EXPORTED_BY_SUIF char *_suif_command_line;

extern void error_line(int return_code, suif_object *the_object, const char *fmt,
                       ...);
extern void verror_line(int return_code, suif_object *the_object, const char *fmt,
                        va_list ap);
extern void warning_line(suif_object *the_object, const char *fmt, ...);
extern void vwarning_line(suif_object *the_object, const char *fmt, va_list ap);

/*
 *  The following functions are used to push and pop on a stack of
 *  positioning clues that the library maintains to help pinpoint
 *  errors.  The idea is to be able to easily see what SUIF code is
 *  being processed.  When the ``-err-context'' option is used on the
 *  command line of a SUIF program, any calls the the error and
 *  warning functions above or the assertion failure functions will
 *  attempt to list the information on this clue stack.
 *
 *  Note that the SUIF library will automatically push and pop some
 *  clues, but it is up to the user to call these functions while
 *  walking around SUIF code to get as much information as possible.
 *
 *  The pushes must have corresponding pops in the same order or an
 *  error occurs.  The reason the pops take arguments is as a sanity
 *  check.
 */
extern void push_clue(suif_object *the_object);
extern void push_clue(annote *the_annote);
extern void pop_clue(suif_object *the_object);
extern void pop_clue(annote *the_annote);

#define warn_once(params) \
  { \
    static boolean warning_done = FALSE; \
    if (!warning_done) \
      { \
        warning_done = TRUE; \
        warning_line params; \
      } \
  }

EXPORTED_BY_SUIF const char *__suif_assert_file;
EXPORTED_BY_SUIF const char *__suif_assert_module;
EXPORTED_BY_SUIF int __suif_assert_line;

extern void __suif_assert_msg(const char* ...);
#define assert_msg(expr, params) if (expr) ; else \
    __suif_assert_file = __FILE__, \
    __suif_assert_line = __LINE__, \
    __suif_assert_module = _MODULE_, \
    __suif_assert_msg params

extern void __suif_assert(const char*, const char*, int, const char*);
#ifdef assert
#undef assert
#endif
#define assert(expr) if (expr) ; else \
    __suif_assert(#expr, __FILE__, __LINE__, _MODULE_)


/*
 *  If this flag is set, warning messages aren't printed, warning_line() just
 *  returns with no effect.
 */
EXPORTED_BY_SUIF boolean ignore_warnings;


/*
 *  Flags to turn off printing instruction result types and symbol tables.
 *  Sometimes the printed output is easier to read without these things.
 */

EXPORTED_BY_SUIF boolean _suif_no_types;
EXPORTED_BY_SUIF boolean _suif_no_symtabs;


/*
 *  Flag to cause the symtab lookup_sym_id and lookup_type_id methods
 *  to create dummy sym_nodes/type_nodes for unknown ids.
 */

EXPORTED_BY_SUIF boolean _suif_raw_syms;


/* 
 *  Flag to cause all annotations to be printed as immed lists.  This
 *  is needed to allow them to be parsed and re-created.
 */

EXPORTED_BY_SUIF boolean _suif_flat_annotes;


/*
 *  Simple procedure iterator.  This function is available for new users or
 *  for those who only need to write very simple SUIF programs.  It just
 *  encapsulates the contents of a typical main function.  The user's function
 *  is applied to each procedure, which can then optionally be written out.
 */

class tree_proc;
typedef void (*prociter_f)(tree_proc * p);

void suif_proc_iter(int argc, char * argv[], prociter_f fun, 
                    boolean writeback=FALSE,
                    boolean exp_trees=TRUE,
                    boolean use_fortran_form=TRUE);


#endif /* MISC_H */
