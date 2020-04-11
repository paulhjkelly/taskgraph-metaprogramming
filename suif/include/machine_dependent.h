/*  File "machine_dependent.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#ifdef SUIFLIB
#include "suif_copyright.h"
#else
#include <suif_copyright.h>
#endif

#if defined(__sparc) && defined(__svr4__)
/* This is needed for SOLARIS, since it defines boolean as an enum */
#ifndef boolean
#define boolean posix_boolean
#include <sys/types.h>
#undef boolean
#else
#define boolean_save_18429 boolean
#undef boolean
#define boolean posix_boolean
#include <sys/types.h>
#undef boolean
#define boolean boolean_save_18429
#undef boolean_save_18429
#endif
#endif

#if defined(__osf__) || defined(__linux)
/* This is needed for DEC OSF/1 3.2 (also known as Digital Unix),
 * since DEC's <string.h> defines ``index'' as a function and gcc
 * doesn't allow the same identifier for a C function and a class
 * template, which is how SUIF defines ``index''.  The same goes for
 * Linux. */
#define index __non_suif_index
#include <cstring>
#undef index
#endif

#include <cstdio>


/*
 *  This file is an attempt to concentrate anything that is machine
 *  dependent and that might need to be changed to port to a new
 *  system in one place.
 */

/*
 *  The ANSI C standard specifies that FOPEN_MAX is defined in
 *  <stdio.h>.  The standard guarantees FOPEN_MAX is at least 8.
 *
 *  Unfortunately, SunOS 4.1.3 has header files that do not meet the
 *  ANSI C standards -- in particular FOPEN_MAX is undefined.  So we
 *  include provisions to use sysconf() when FOPEN_MAX is undefined.
 *  The sysconf() function is reportedly a POSIX requirement, and it
 *  is defined by SunOS.
 */

#ifndef SUIF_FOPEN_MAX
#ifdef FOPEN_MAX
#define SUIF_FOPEN_MAX FOPEN_MAX
#else
#include <unistd.h>
#define SUIF_FOPEN_MAX (sysconf(_SC_OPEN_MAX))
#endif
#endif /* SUIF_FOPEN_MAX */


#if defined(_WIN32) && !defined(__CYGWIN32__)
/*
 *  This is experimental code to get SUIF to work under MicroSoft
 *  Visual C++ for Windows NT and IBM's Montana for Windows NT.
 */

#if defined(_MSC_VER)
/*
 *  Visual C++ doesn't seem to implement set_new_handler(), even
 *  though that's supposed to be standard.  Instead they use
 *  _setnewhandler().  The following should make the SUIF code use
 *  the Visual C++ scheme.
 */

#include <new>

// Visual C++ 5.0 includes a set_new_handler function, but it is 
// only a stub.  The real implementation is _set_new_handler, with 
// the leading underscore. (jsimmons)
#define set_new_handler(x) _set_new_handler((_PNH)x)
#endif

/*
 *  SUIF uses fcntl() to try to figure out which files are already
 *  open so it won't open too many.  But this only matters when
 *  running under a debugger or something else that will have some
 *  files open that SUIF doesn't know about.  NT compilers don't seem
 *  to implement fcntl() at all, so we can't use it.  We don't
 *  currently know an alternate way to get this functionality under
 *  NT.  Instead, we fake it here in a way that ensures SUIF thinks
 *  six files are already open.  That should leave enough slack for
 *  most purposes.
 */
#include <fcntl.h>
#define fcntl(x, y) ((x < 6) ? 1 : -1)

#endif // WIN32

// Mark exported symbols as DLL imports for Win32. (jsimmons)
#if defined(_WIN32) && !defined(SUIFLIB)
#define EXPORTED_BY_SUIF _declspec(dllimport) extern
#else
#define EXPORTED_BY_SUIF extern
#endif


#ifdef SUIF_NEED_GETOPT
/*
 *  If a declaration of getopt() is needed, SUIF_NEED_GETOPT should be
 *  defined.
 */

#if defined(__ultrix)
/* DEC Ultrix */
/*
 *  Note: The Ultrix man page for getopt() says to include <stdio.h>,
 *  but including that header does not give a declaration for getopt()
 *  on our Ultrix systems.  Indeed, on our systems getopt() does not
 *  appear anywhere in the /usr/include hierarchy.
 */
extern "C" int getopt(int argc, char *const *argv, const char *optstring);

#elif defined(sgi) || defined(hppa)
/* Silicon Graphics IRIX */
#include <getopt.h>

#elif defined(__sparc) && !defined(__svr4__)
/* SUN SunOS 4.x */
/*
 *  Note: On our SunOS systems, getopt() does not appear anywhere in
 *  the /usr/include hierarchy.
 */
extern "C" int getopt(int argc, char *const *argv, const char *optstring);

#elif defined(__sparc) && defined(__svr4__)
/* SUN Solaris */
#include <cstdlib>

#elif defined(__linux)
/* Linux */
#include <getopt.h>

#else
extern "C" int getopt(int argc, char *const *argv, const char *optstring);
#endif

#endif /* SUIF_NEED_GETOPT */



#ifdef SUIF_NEED_BZERO_BCOPY_BCMP
/*
 *  If a declaration for any of bzero(), bcopy(), or bcmp() is needed,
 *  SUIF_NEED_BZERO_BCOPY_BCMP should be defined.  They are lumped
 *  together because they are similar and their declarations tend to
 *  be lumped together in system header files.
 */

#if defined(__ultrix)
/* DEC Ultrix */
/*
 *  Note: Ultrix system headers don't seem to have declarations for
 *  any of these.
 */
extern "C" void bzero(void *, int);
extern "C" void bcopy(const void *, void *, int);
extern "C" int bcmp(const void *, const void *, int);

#elif defined(sgi) || defined(hppa)
/* Silicon Graphics IRIX  or  HP-UX */
#include <bstring.h>

#elif defined(__sparc) && !defined(__svr4__)
/* SUN SunOS 4.x */
/*
 *  Note: The SunOS system headers don't seem to have declarations for
 *  any of these.
 */
extern "C" void bzero(void *, int);
extern "C" void bcopy(const void *, void *, int);
extern "C" int bcmp(const void *, const void *, int);

#elif defined(__sparc) && defined(__svr4__)
/* SUN Solaris */
/*
 *  Note: The Solaris system headers don't seem to have declarations
 *  for any of these except when _KERNEL is defined.
 */
extern "C" void bzero(void *, int);
extern "C" void bcopy(const void *, void *, int);
extern "C" int bcmp(const void *, const void *, int);

#elif defined(__osf__) || defined(__linux)
/* DEC OSF/1 3.2 (also known as Digital Unix) */
#include <cstring>

#else
extern "C" void bzero(void *, int);
extern "C" void bcopy(const void *, void *, int);
extern "C" int bcmp(const void *, const void *, int);
#endif

#endif /* SUIF_NEED_BZERO_BCOPY_BCMP */



#ifdef SUIF_NEED_SBRK
/*
 *  If a declaration for sbrk() is needed, SUIF_NEED_SBRK should be
 *  defined.
 */

#if defined(__ultrix)
/* DEC Ultrix */
/*
 *  Note: Ultrix system headers don't seem to have a declaration for
 *  this.
 */
extern "C" char *sbrk(int);

#elif defined(sgi)
/* Silicon Graphics IRIX */
/*
 *  Note: IRIX system headers don't seem to have a declaration for
 *  this.
 */
extern "C" void *sbrk(int);

#elif defined(__sparc) && !defined(__svr4__)
/* SUN SunOS 4.x */
/*
 *  Note: The SunOS system headers don't seem to have a declaration
 *  for this.
 */
#include <sys/types.h>
extern "C" caddr_t sbrk(int);

#elif defined(__sparc) && defined(__svr4__)
/* SUN Solaris */
#include <unistd.h>

#elif defined(__osf__) || defined(__linux)
/* DEC OSF/1 3.2 (also known as Digital Unix) */
#include <unistd.h>

#else
extern "C" void *sbrk(int);
#endif

#endif /* SUIF_NEED_SBRK */



#ifdef SUIF_NEED_RLIMIT
/*
 *  If a declaration for setrlimit() or getrlimit() is needed,
 *  SUIF_NEED_RLIMIT should be defined.
 */

#if defined(__osf__)
/* DEC OSF/1 3.2 (also known as Digital Unix) */
#include <sys/resource.h>
extern "C" {
    int setrlimit(int resource, struct rlimit *rlp);
    int getrlimit(int resource, struct rlimit *rlp);
}
//	PGI: On linux, setrlimit and getrlimit are already declared, and
//	these declarations are wrong as resource is an enum not an int
//
#elif !defined(__linux)
#include <sys/resource.h>
extern "C" {
#if defined(__linux)
    /* Do not decalre *rlimit(), since these are defined in
     * resource.h.  Note that this is for glibc based Linuxes, for
     * RedHat 4.2 systems this should be defined.  Don't ask me how to
     * differentiate these using #ifdef's. -CC */
#else
    int setrlimit(int resource, const struct rlimit *rlp);
    int getrlimit(int resource, struct rlimit *rlp);
#endif
}
#endif

#endif /* SUIF_NEED_RLIMIT */



#ifdef SUIF_NEED_DIRECTORY_OPERATIONS

#ifndef HAVE_DIRECTORY_OPERATIONS

#if defined(__osf__) || defined(__linux) || defined(sgi)
#define USE_SCANDIR
#define HAVE_DIRECTORY_OPERATIONS
#elif defined(__sparc) && (defined(__svr4__) || defined(__SVR4))
#define USE_READDIR
#define HAVE_DIRECTORY_OPERATIONS
#else
#define USE_READDIR
#define HAVE_DIRECTORY_OPERATIONS
#endif

#endif /* !HAVE_DIRECTORY_OPERATIONS */

#ifdef USE_SCANDIR

#include <dirent.h>

#define DIRECTORY_EXISTS_BODY \
  { \
    struct dirent **dirent_list; \
    int retval = scandir(directory_name.chars(), &dirent_list, NULL, NULL); \
    if (retval < 0) \
        return FALSE; \
    free(dirent_list); \
    return TRUE; \
  }

#define FILES_IN_DIRECTORY_BODY \
  { \
    struct dirent **dirent_list; \
    int retval = \
            scandir(directory_name.chars(), &dirent_list, NULL, &alphasort); \
    adlist_tos<string> *result = new adlist_tos<string>; \
    if (retval < 0) \
      { \
        error("unable to read directory `%s' (%s)", directory_name.chars(), \
              strerror(errno)); \
        return result; \
      } \
    for (int entry_num = 0; entry_num < retval; ++entry_num) \
      { \
        dirent *this_dirent = dirent_list[entry_num]; \
        string this_name = this_dirent->d_name; \
        if ((this_name != ".") && (this_name != "..")) \
            result->append(this_name); \
        free(this_dirent); \
      } \
    free(dirent_list); \
    return result; \
  }

#elif defined(USE_READDIR)

#include <dirent.h>

#define DIRECTORY_EXISTS_BODY \
  { \
    DIR *directory_pointer = opendir(directory_name.chars()); \
    if (directory_pointer == NULL) \
        return FALSE; \
    closedir(directory_pointer); \
    return TRUE; \
  }

#define FILES_IN_DIRECTORY_BODY \
  { \
    DIR *directory_pointer = opendir(directory_name.chars()); \
    adlist_tos<string> *result = new adlist_tos<string>; \
    if (directory_pointer == NULL) \
      { \
        error("unable to read directory `%s' (%s)", directory_name.chars(), \
              strerror(errno)); \
        return result; \
      } \
    while (TRUE) \
      { \
        dirent *this_dirent = readdir(directory_pointer); \
        if (this_dirent == NULL) \
            break; \
        string this_name = this_dirent->d_name; \
        if ((this_name != ".") && (this_name != "..")) \
            result->append(this_name); \
      } \
    closedir(directory_pointer); \
    return result; \
  }

#endif

#endif /* SUIF_NEED_DIRECTORY_OPERATIONS */
