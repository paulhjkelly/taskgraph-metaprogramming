lib_LTLIBRARIES = libtaskgraph.la

libtaskgraph_la_CXXFLAGS = -DINSTALL_DATA_PATH="\"${datarootdir}/${package}/src/\"" -DPKG_DATA_DIR_NAME="\"share\"" -I${top_srcdir}/share/
libtaskgraph_la_SOURCES = TaskGraph.cc TaskVariable.cc TaskCompilers.cc TaskException.cc TaskFunction.cc TaskTimer.cc TaskIterative.cc 
libtaskgraph_la_LDFLAGS = -ldl

if using_suif1
libtaskgraph_la_CXXFLAGS += -I$(top_srcdir)/include -I$(top_srcdir)/suif/include
libtaskgraph_la_SOURCES += suif1/TaskIRSuif.cc suif1/TaskFusion.cc suif1/TaskLoop.cc suif1/TaskContraction.cc suif1/SUIF_Porky_Scalarize.cc suif1/TaskRaiseInitialAssignments.cc suif1/TaskMallocLargeArrays.cc
libtaskgraph_la_LDFLAGS += -L$(top_srcdir)/suif/lib -lsuif1 -lbuilder -luseful -ldependence -lsuifmath -ls2c -ltransform
endif

if using_rose
libtaskgraph_la_CXXFLAGS += -I$(top_srcdir)/include
libtaskgraph_la_SOURCES  += rose/TaskIRRose.cc
libtaskgraph_la_LDFLAGS  += -lrose -ledg
endif

if cell_support
libtaskgraph_la_SOURCES += SpuMemFuncs.cc TaskCell.cc
libtaskgraph_la_LDFLAGS += -lpthread -lspe2 
endif
