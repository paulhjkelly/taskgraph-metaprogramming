noinst_HEADERS = s2c/io_class.h s2c/ctree.h s2c/places.h s2c/s2c.h s2c/style_defaults.h

# These must be in topological order for install to work first time. See suif_library_dependencies.dot.
lib_LTLIBRARIES = libsuif1.la libuseful.la libbuilder.la libcheck.la libs2c.la libsuifmath.la libdependence.la libtransform.la

suif1/lib_ver.cc:
	./ver_create_ver_cc suif1/lib_ver.cc libsuif1

useful/lib_ver.cc:
	./ver_create_ver_cc useful/lib_ver.cc libuseful

builder/lib_ver.cc:
	./ver_create_ver_cc builder/lib_ver.cc libbuilder

check/lib_ver.cc:
	./ver_create_ver_cc check/lib_ver.cc libcheck

dependence/lib_ver.cc:
	./ver_create_ver_cc dependence/lib_ver.cc libdependence

suifmath/lib_ver.cc:
	./ver_create_ver_cc suifmath/lib_ver.cc libsuifmath

s2c/lib_ver.cc:
	./ver_create_ver_cc s2c/lib_ver.cc libs2c

transform/lib_ver.cc:
	./ver_create_ver_cc transform/lib_ver.cc libtransform

libsuif1_la_CXXFLAGS = -I../include -I../include/suif1 -DSUIFLIB  
libsuif1_la_SOURCES = suif1/misc.cc suif1/glist.cc suif1/dlist.cc suif1/alist.cc \
suif1/stringtable.cc suif1/aman.cc suif1/annote.cc suif1/bitset.cc \
suif1/callbyref.cc suif1/cmdparse.cc suif1/fileset.cc suif1/hash.cc \
suif1/operand.cc suif1/immed.cc suif1/initsuif.cc suif1/instruction.cc \
suif1/mtflist.cc suif1/opcodes.cc suif1/suifobj.cc suif1/suifstreams.cc \
suif1/symaddr.cc suif1/symbols.cc suif1/symtab.cc suif1/trees.cc \
suif1/tree_string_index.cc suif1/ts_ptr_index.cc suif1/types.cc suif1/vardef.cc \
suif1/xarray.cc
nodist_libsuif1_la_SOURCES =  suif1/lib_ver.cc

libuseful_la_CXXFLAGS = -I../include -I../include/useful  
libuseful_la_LIBADD = libsuif1.la
libuseful_la_SOURCES = useful/useful.cc useful/eval.cc useful/estimate.cc \
useful/arrayinfo.cc useful/init.cc useful/transform.cc useful/symbol.cc \
useful/expr_map.cc useful/no_sub_vars.cc useful/labinfo.cc useful/inumbers.cc \
useful/dismantle.cc useful/walk.cc useful/marker.cc useful/outline.cc \
useful/linkinfo.cc useful/positioning.cc useful/test_modification.cc \
useful/spill_instr_ops.cc useful/make_instr_ops.cc useful/namespace.cc \
useful/inline.cc
nodist_libuseful_la_SOURCES = useful/lib_ver.cc


libbuilder_la_CXXFLAGS = -I../include -I../include/builder  
libbuilder_la_LIBADD = libsuif1.la libuseful.la
libbuilder_la_SOURCES = builder/bldtypes.cc builder/builder.cc builder/butil.cc \
builder/cgen.cc builder/struct_access.cc
nodist_libbuilder_la_SOURCES = builder/lib_ver.cc

libcheck_la_CXXFLAGS = -I../include -I../include/check  
libcheck_la_LIBADD = libsuif1.la libuseful.la
libcheck_la_SOURCES = check/checktypes.cc check/check.cc
nodist_libcheck_la_SOURCES = check/lib_ver.cc

libdependence_la_CXXFLAGS = -I../include -I../include/dependence -DDEPENDENCELIB  
libdependence_la_LIBADD = libsuifmath.la libbuilder.la libuseful.la libsuif1.la
libdependence_la_SOURCES = dependence/access_vector.cc dependence/deptest_msc.cc \
dependence/ehash.cc dependence/mgcd.cc dependence/dep_node.cc dependence/dvector.cc \
dependence/exact.cc dependence/normalize.cc dependence/dodep.cc dependence/normalize_code.cc \
dependence/lin_fm_hook.cc dependence/named_sc_dep.cc dependence/named_array_func.cc
nodist_libdependence_la_SOURCES = dependence/lib_ver.cc

libsuifmath_la_CXXFLAGS = -I../include -I../include/suifmath -DSUIFMATHLIB  
libsuifmath_la_LIBADD = libbuilder.la libuseful.la libsuif1.la
libsuifmath_la_SOURCES = suifmath/int_matrix.cc suifmath/fract.cc suifmath/fract_vector.cc \
suifmath/fract_matrix.cc suifmath/vector_space.cc suifmath/matrix.cc suifmath/coeff.cc \
suifmath/linear_ineq.cc suifmath/bounds.cc suifmath/fmred.cc suifmath/print_code.cc \
suifmath/named_lin_ineq.cc suifmath/named_sc_init.cc suifmath/named_sc_fm.cc \
suifmath/named_sc_op.cc suifmath/named_sc_pr.cc suifmath/named_sc_merge.cc suifmath/code_context.cc \
suifmath/ffm.cc suifmath/debughelp.cc
nodist_libsuifmath_la_SOURCES = suifmath/lib_ver.cc

libs2c_la_CXXFLAGS = -I../include -Is2c  
libs2c_la_LIBADD = libuseful.la libsuif1.la
libs2c_la_SOURCES = s2c/ctree.cc s2c/main.cc s2c/util.cc s2c/preprocess.cc \
s2c/namespace.cc s2c/ldc.cc s2c/init.cc s2c/re_eval.cc s2c/array_copy.cc s2c/pass1.cc \
s2c/io_class.cc s2c/group.cc 
nodist_libs2c_la_SOURCES = s2c/lib_ver.cc

libtransform_la_CXXFLAGS = -I../include -I../include/transform -DTRANSFORMLIB  
libtransform_la_LIBADD = libdependence.la libsuifmath.la libbuilder.la libuseful.la libsuif1.la
libtransform_la_SOURCES = transform/lptrans.cc transform/bexpr.cc
nodist_libtransform_la_SOURCES = transform/lib_ver.cc

EXTRA_DIST = ver_create_ver_cc 
