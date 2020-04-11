# This file should be included by any Makefiles in this package
# that need to be linked against the TaskGraph library.

AM_CPPFLAGS=-I$(top_srcdir)/include -I$(top_srcdir)/share/ 
LDADD=$(top_srcdir)/src/libtaskgraph.la
