/* include/TaskGraph_config.h.  Generated from TaskGraph_config.h.in by configure.  */
/* include/TaskGraph_config.h.in.  Generated from configure.ac by autoheader.  */

/* Compile with Cell Broadband Engine support */
/* #undef CELL_SUPPORT */

/* Compiling under Cygwin */
/* #undef ENV_CYGWIN */

/* Extra compile options for use in execl call to gcc */
#define EXTRA_GCC_COMPILE_ARGS "-march=core2",  "-fPIC", 

/* Extra compile options for use in execl call to icc */
#define EXTRA_ICC_COMPILE_ARGS "-fPIC", 

/* Path to gcc to be used to for run-time code generation */
#define GCC_PATH "/usr/bin/gcc"

/* define if the Boost library is available */
#define HAVE_BOOST /**/

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <float.h> header file. */
#define HAVE_FLOAT_H 1

/* Define to 1 if you have the `floor' function. */
#define HAVE_FLOOR 1

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `pow' function. */
#define HAVE_POW 1

/* Define to 1 if you have the `sqrt' function. */
#define HAVE_SQRT 1

/* Define to 1 if stdbool.h conforms to C99. */
/* #undef HAVE_STDBOOL_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `utime' function. */
#define HAVE_UTIME 1

/* Define to 1 if you have the <utime.h> header file. */
#define HAVE_UTIME_H 1

/* Define to 1 if `utime(file, NULL)' sets file's timestamp to the present. */
#define HAVE_UTIME_NULL 1

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if `fork' works. */
#define HAVE_WORKING_FORK 1

/* Define to 1 if `vfork' works. */
#define HAVE_WORKING_VFORK 1

/* Define to 1 if the system has the type `_Bool'. */
/* #undef HAVE__BOOL */

/* Path to icc to be used for run-time code generation */
#define ICC_PATH ""

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Number of SPEs to support */
/* #undef NUM_SPES */

/* Name of package */
#define PACKAGE "taskgraph"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "fpr02@doc.ic.ac.uk"

/* Define to the full name of this package. */
#define PACKAGE_NAME "TaskGraph"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "TaskGraph 20070919"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "taskgraph"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "20070919"

/* Path to ppuxlc to be used for run-time code generation */
#define PPUXLC_PATH ""

/* Path to spu-gcc to be used for run-time code generation */
#define SPUGCC_PATH ""

/* Path to spuxlc to be used for run-time code generation */
#define SPUXLC_PATH ""

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Use ROSE intermediate form */
/* #undef USE_ROSE_IR */

/* Use SUIF1 intermediate form */
#define USE_SUIF1_IR 1

/* Version number of package */
#define VERSION "20070919"

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef vfork */
