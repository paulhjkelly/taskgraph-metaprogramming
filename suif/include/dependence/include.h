/* file "include.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

extern int debug,verbose,fort;

//void clean_bad_nodes(ast_proc *,int);
void rewrite_for(tree_node_list *,int,int,int);
void rewrite_msc(tree_node_list *,int,int,int,int);

