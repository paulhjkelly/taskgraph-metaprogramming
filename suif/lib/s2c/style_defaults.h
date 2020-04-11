/* file "style_defaults.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file defines the default output style for the s2c program for
 *  the SUIF system.
 */

/*
 *  If you want to change the default parameters for the output C
 *  style, change the following lines.
 */

#define DEFAULT_STYLE_SPACE_AROUND_BINOPS            TRUE
#define DEFAULT_STYLE_SPACE_AROUND_ASSIGNMENTS       TRUE
#define DEFAULT_STYLE_SPACE_AFTER_COMMAS             TRUE
#define DEFAULT_STYLE_NEW_LINE_FOR_OPEN_BRACE        TRUE
#define DEFAULT_STYLE_NEW_LINE_FOR_ELSE              TRUE
#define DEFAULT_STYLE_INDENT_SINGLE_STATEMENT_BODIES TRUE
#define DEFAULT_STYLE_STATEMENT_INDENT                4
#define DEFAULT_STYLE_SINGLE_STATEMENT_BODY_INDENT    4
#define DEFAULT_STYLE_BRACE_INDENT                    2
#define DEFAULT_STYLE_PARAM_INDENT                    4
#define DEFAULT_STYLE_LABEL_INDENT                   -2
#define DEFAULT_STYLE_EXTERN_ON_FUNC_DECL            TRUE
#define DEFAULT_STYLE_MAX_COMMA_ITEMS_PER_LINE       26
