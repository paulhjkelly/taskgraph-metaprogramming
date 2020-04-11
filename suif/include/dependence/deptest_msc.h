/* file "deptest_msc.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
// deptest_msc.h

//path *path_of_ldc(in_ldc *i);
int sym_modified(var_sym * r, tree_node_list *l, tree_for* =0);
int sym_modified(var_sym * r, operand *op, tree_for* =0);
int pb_modified(var_sym *pb, tree_node_list *l, int fortran);

/* nldc finds number of ldc of path instructions, at n or below.  If it is useful,
 * it is set to point to one of them, if one is found. */
int nldc(tree_instr *n, tree_instr **it=0);


tn_list *make_tnl(tree_node *s1,tree_node *s2);

tree_node * next_further_out(tree_node * tn, tree_kinds tk, boolean inclusive=FALSE);


/*
 * list_addr_info
 *
 * returns
 *		read: whether pb is read for sure
 *		write: whether pb is written for sure
 *		ambig_read: whether pb is ambiguously read
 *		ambig_write: whether pb is ambiguously written
 *
 * pass in the array info for this reference, and whether it's a
 * read or write, and as a bonus you get the dependence vectors
 * that arise from the for sure read/writes.
 */

class dvlist;
dvlist *list_addr_info(tree_node_list *l, var_sym *pb,
                    int *read,int *write,int *ambig_read,int *ambig_write,
                    int fortran,tree_instr * =0,int is_write = 0);

#define SIMPLE_IPA_IND_READ_PARAM 1
#define SIMPLE_IPA_IND_WRITE_PARAM 2
#define SIMPLE_IPA_READ_GLOB 4
#define SIMPLE_IPA_WRITE_GLOB 8
int simple_ipa_call_info(in_cal *ic);
