/*  Command-Line Parser Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#define RCS_BASE_FILE cmdparse_cc

#include "suif1.h"
#include <cstdarg>
#include <cstring>

RCS_BASE(
    "$Id$")


/*
 *  Parse a command line and extract options we know about.
 */

static int
parse_cmd_line_int(const char *s)
{
    int ret_int;
    
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
    {
	unsigned int tmp;
	sscanf(s+2, "%x", &tmp);
	ret_int = static_cast<int>(tmp);
    }
    else
    {
	ret_int = atoi(s);
    }

    return ret_int;
}

void
parse_cmd_line (int &argc, char *argv[], cmd_line_option clos[], int nclos)
{
    int i, ap;

    int *elem_num; /* current position in array for CLO_MULTI_STRING */
    elem_num = new int[nclos];

    /*
     * First initialize the default values
     */

    for (i=0; i<nclos; i++) {
	switch (clos[i].knd) {
	case CLO_NOARG:
	    *(boolean*)clos[i].data = FALSE;
	    break;
	case CLO_INT:
	    *(int*)clos[i].data = parse_cmd_line_int(clos[i].deflt);
	    break;
	case CLO_DOUBLE:
	    *(double*)clos[i].data = atof(clos[i].deflt);
	    break;
	case CLO_STRING:
	    *(const char**)clos[i].data = clos[i].deflt;
	    break;
	case CLO_MULTI_STRING:
	    elem_num[i] = 0;
	    (*(const char ***)clos[i].data)[0] = clos[i].deflt;
	    break;
	default:
	    assert_msg(FALSE, ("parse_cmd_line - unknown argument kind %d",
			       clos[i].knd));
	}
    }

    /*
     * Now search through the argument list for matches.
     */

    boolean foundone;
    
    for (ap=0; ap<argc;) {
	foundone = FALSE;
	for (i=0; i<nclos; i++) {
	    if (!strcmp(argv[ap], clos[i].name)) {
		switch (clos[i].knd) {
		case CLO_NOARG:
		    *(boolean*)clos[i].data = TRUE;
		    break;
		case CLO_INT:
		    if (ap == argc-1)
			error_line(1, NULL, "'%s' requires an argument",
				   clos[i].name);
		    *(int*)clos[i].data = parse_cmd_line_int(argv[ap+1]);
		    break;
		case CLO_DOUBLE:
		    if (ap == argc-1)
			error_line(1, NULL, "'%s' requires an argument",
				   clos[i].name);
		    *(double*)clos[i].data = atof(argv[ap+1]);
		    break;
		case CLO_STRING:
		    if (ap == argc-1)
			error_line(1, NULL, "'%s' requires an argument",
				   clos[i].name);
		    *(char**)clos[i].data = const_string(argv[ap+1]);
		    break;
		case CLO_MULTI_STRING:
		    if (ap == argc-1)
			error_line(1, NULL, "'%s' requires an argument",
				   clos[i].name);
		    (*(char ***)(clos[i].data))[elem_num[i]] =
			    const_string(argv[ap+1]);
		    ++(elem_num[i]);
		    (*(const char ***)(clos[i].data))[elem_num[i]] = clos[i].deflt;
		    break;
		}
		int ap2 = ap;
		if (clos[i].knd != CLO_NOARG)
		    ap2 = ap+2;
		else
		    ap2 = ap+1;
		for (int j=ap; ap2<=argc;) /* make sure to copy end NULL */
		    argv[j++] = argv[ap2++];
		if (clos[i].knd != CLO_NOARG)
		    argc -= 2;
		else
		    argc --;
		foundone = TRUE;
		break;
	    }
	}
	if (!foundone)
	    ap++;
    }

    delete[] elem_num;
}
