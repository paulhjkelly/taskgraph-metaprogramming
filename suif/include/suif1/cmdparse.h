/*  Command-Line Parser Declarations */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef CMDPARSE_H
#define CMDPARSE_H

/*
 *  The structure that defines an individual command line option.
 */

enum cmd_line_option_kind {
    CLO_NOARG,
    CLO_INT,
    CLO_DOUBLE,
    CLO_STRING,
    CLO_MULTI_STRING
};

struct cmd_line_option {
    cmd_line_option_kind knd;
    const char *name;
    const char *deflt;
    void *data;
};


/*
 *  Parse command line options.  Returns TRUE if there are more things
 *  left on the command line.
 */

void parse_cmd_line(int &argc, char *argv[], cmd_line_option clos[],
		    int nclos);

#endif /* CMDPARSE_H */
