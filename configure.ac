#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([TaskGraph], [20070919], [fpr02@doc.ic.ac.uk])
AC_CONFIG_HEADERS([include/TaskGraph_config.h])
AM_INIT_AUTOMAKE([subdir-objects])
AC_DISABLE_STATIC
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
AC_CONFIG_SRCDIR([examples/complex/fft/fft.cc])
AC_LANG_CPLUSPLUS

# Checks for programs.
AC_PROG_AWK
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([float.h malloc.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_UTIME_NULL
AC_CHECK_FUNCS([floor gettimeofday pow sqrt utime])

AC_CONFIG_FILES([Makefile
                 docs/Makefile
		 examples/Makefile
		 examples/tests/Makefile
		 examples/toy/Makefile
		 examples/cell/Makefile
		 examples/cell/mandelbrot/Makefile
		 examples/cell/pipeline/Makefile
		 examples/complex/Makefile
		 examples/complex/fft/Makefile
		 examples/complex/multistage/Makefile
		 examples/complex/raytracer/Makefile
		 examples/intermediate/Makefile
		 examples/intermediate/interpreter/Makefile
		 examples/intermediate/expressions/Makefile
		 examples/intermediate/skewedgaussseidel/Makefile
		 examples/introductory/imgfilt/Makefile
		 examples/introductory/matrixmult/Makefile
		 examples/introductory/simple/Makefile
		 examples/introductory/Makefile
		 examples/others/mathintprt/Makefile
		 examples/others/mmiter/Makefile
		 examples/others/morton-gaussseidel/Makefile
		 examples/others/mortonmm/Makefile
		 examples/others/mortontiling/Makefile
		 examples/others/zeros/Makefile
		 examples/others/Makefile
                 include/Makefile
		 share/Makefile
		 suif/Makefile
		 suif/include/Makefile
		 suif/lib/Makefile
                 src/Makefile])

AC_ARG_WITH([gcc],
	    AC_HELP_STRING([--with-gcc=LOCATION],
            [Use LOCATION as the gcc compiler. Default is to search the path for a binary with name 'gcc'.]),
	    [AC_CHECK_FILE([$withval], gcc_path="$withval", AC_MSG_ERROR($withval not found))],
	    [AC_PATH_PROGS(gcc_path, "gcc")])

AC_ARG_WITH([icc],
            AC_HELP_STRING([--with-icc=LOCATION],
            [Use LOCATION as the icc compiler. Default is to search the path for a binary with name 'icc'.]),
	    [AC_CHECK_FILE([$withval], icc_path="$withval", AC_MSG_ERROR($withval not found))],
            [AC_PATH_PROGS(icc_path, "icc")])

AC_ARG_WITH([ppuxlc],
            AC_HELP_STRING([--with-ppuxlc=LOCATION],
            [Use LOCATION as the ppuxlc compiler. Default is to search the path for a binary with name 'ppuxlc'.]),
	    [AC_CHECK_FILE([$withval], ppuxlc_path="$withval", AC_MSG_ERROR($withval not found))],
            [AC_PATH_PROGS(ppuxlc_path, "ppuxlc")])

AC_ARG_WITH([spugcc],
            AC_HELP_STRING([--with-spugcc=LOCATION],
            [Use LOCATION as the spu-gcc compiler. Default is to search the path for a binary with name 'spu-gcc'.]),
	    [AC_CHECK_FILE([$withval], spugcc_path="$withval", AC_MSG_ERROR($withval not found))],
            [AC_PATH_PROGS(spugcc_path, "spu-gcc")])

AC_ARG_WITH([spuxlc],
            AC_HELP_STRING([--with-spuxlc=LOCATION],
            [Use LOCATION as the spuxlc compiler. Default is to search the path for a binary with name 'spuxlc'.]),
	    [AC_CHECK_FILE([$withval], spuxlc_path="$withval", AC_MSG_ERROR($withval not found))],
            [AC_PATH_PROGS(spuxlc_path, "spuxlc")])

AC_DEFINE_UNQUOTED(GCC_PATH, "${gcc_path}", [Path to gcc to be used to for run-time code generation])
AC_DEFINE_UNQUOTED(ICC_PATH, "${icc_path}", [Path to icc to be used for run-time code generation])
AC_DEFINE_UNQUOTED(SPUGCC_PATH, "${spugcc_path}", [Path to spu-gcc to be used for run-time code generation])
AC_DEFINE_UNQUOTED(PPUXLC_PATH, "${ppuxlc_path}", [Path to ppuxlc to be used for run-time code generation])
AC_DEFINE_UNQUOTED(SPUXLC_PATH, "${spuxlc_path}", [Path to spuxlc to be used for run-time code generation])

CXXFLAGS="-O2 -Wall -pedantic -pipe"

AX_GCC_ARCHFLAG([no], [CXXFLAGS="$CXXFLAGS $ax_cv_gcc_archflag" CCFLAGS="$CCFLAGS $ax_cv_gcc_archflag"
extra_gcc_compile_args="$extra_gcc_compile_args \"$ax_cv_gcc_archflag\", "], [])

AC_ARG_WITH([rose],
            AC_HELP_STRING([--with-rose=DIR],
	    [Use the Rose intermediate representation in dir]),
	    [
	      if test -f "$withval/include/rose.h"; then
	        CXXFLAGS="$CXXFLAGS -I$withval/include"
	        LDFLAGS="$LDFLAGS -R$withval/lib -L$withval/lib"
	      else
	        AC_MSG_ERROR("$withval does not appear to be a Rose directory")
	      fi
	    ]
	    )

AX_BOOST_BASE_TASKGRAPH([1.33])
CXXFLAGS="${CXXFLAGS} ${BOOST_CPPFLAGS}"
LDFLAGS="${LDFLAGS} ${BOOST_LDFLAGS}"

if test "$want_boost" = no; then
  AC_MSG_FAILURE([Boost is a required dependency and cannot be disabled.])
fi

case $build in
  *x86_64*|*powerpc*|*sparc*|*m68k*) extra_gcc_compile_args="$extra_gcc_compile_args \"-fPIC\", "; extra_icc_compile_args="$extra_icc_compile_args \"-fPIC\", ";
  AC_MSG_NOTICE([using -fPIC option for compiling dlopened objects with gcc]) ;;
  *) ;;
esac

AC_TRY_COMPILE([@%:@include <libspe2.h>], [], AC_MSG_NOTICE([found libspe2... building Cell support]); cell_support=true,
               AC_MSG_NOTICE([failed to find libspe2... not building Cell support]); cell_support=false)

AC_ARG_WITH([num-spes],
	    AC_HELP_STRING([--with-num-spes=COUNT],
            [Compile for this number of SPEs (Only used with Cell Broadband Engine support). Default is 6.]),
	    num_spes="$withval",
	    num_spes="6")

AM_CONDITIONAL(cell_support, false)
if test $cell_support = true; then
  AM_CONDITIONAL(cell_support, true)
  AC_DEFINE(CELL_SUPPORT, 1, [Compile with Cell Broadband Engine support])
  AC_DEFINE_UNQUOTED(NUM_SPES, $num_spes, [Number of SPEs to support])
  AC_TRY_COMPILE([@%:@include <png.h>], [],  AC_MSG_NOTICE([found libpng]), AC_MSG_FAILURE([Failed to find libpng]))
fi

AC_DEFINE_UNQUOTED(EXTRA_ICC_COMPILE_ARGS, [$extra_icc_compile_args], [Extra compile options for use in execl call to icc])
AC_DEFINE_UNQUOTED(EXTRA_GCC_COMPILE_ARGS, [$extra_gcc_compile_args], [Extra compile options for use in execl call to gcc])

intermediate="suif1"
AC_TRY_COMPILE([@%:@include <rose.h>], [], [AC_MSG_NOTICE([detected Rose]); intermediate="rose"], AC_MSG_NOTICE([did not detect Rose]))

AM_CONDITIONAL(using_suif1, false)
AM_CONDITIONAL(using_rose, false)

case $intermediate in
 suif1) AC_DEFINE(USE_SUIF1_IR, 1, [Use SUIF1 intermediate form]) AC_MSG_NOTICE([using SUIF1]); AM_CONDITIONAL(using_suif1, true) ;;
 rose) AC_DEFINE(USE_ROSE_IR, 1, [Use ROSE intermediate form]) AC_MSG_NOTICE([using Rose]); AM_CONDITIONAL(using_rose, true) ;;
 *) AC_MSG_FAILURE([Unknown intermediate representation.]) ;;
esac

case $host_os in
  *cygwin*) AC_DEFINE(ENV_CYGWIN, 1, [Compiling under Cygwin]) ;;
esac

AC_OUTPUT
