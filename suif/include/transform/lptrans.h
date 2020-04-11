/* file "lptrans.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Unimodular loop transformations and tiling */

#ifndef LPTRANS_H
#define LPTRANS_H

#pragma interface

#include <suif1.h>
#include <suifmath.h>
#include "bexpr.h"

RCS_HEADER(lptrans_h, "$Id$")

//
// Annotations
//
extern const char *k_doall;
extern const char *k_fpinner;
extern const char *k_tiled;
extern const char *k_tiledc;

struct tiled_annote
{
    int depth;
    boolean *doall_loops;
};


// 
// Loop Transformations
//
class loop_transform {
    friend boolean transform_body_src(instruction *i, operand *r, void *x);
    friend void transform_body_tree(tree_node *tn, void *x);

private:
    int depth;
    tree_for **loops;
    bizarre_bounds_info *binfo;
    boolean *doall_loops;
    boolean already_tiled;

    void generate_bounds();


    // Unimodular transformations
    //
    integer_matrix utrans;
    integer_matrix utrans_inv;

    void utransform();
    void generate_inequality(access_vector *av, tree_for *tf);
    int find_index(operand r);
    boolean new_access(operand r, access_vector *access_vec);
    void transform_body(instruction *ins);
    void transform_body(tree_node_list *l);

    // Tile transformations
    //
    int num_regions;             // number of tiling regions
    boolean *coalesce_region;    // coalesce the given region?
    int *trip_size;              // trip size for *loop* (not the region)
    int *first_loop;             // number of the first loop in region.  
                                 //   must have first_loop[num_regions] = depth

    void tile_loops();
    void tile_region(int region_num, int firsts, int last, 
		     var_sym **outer_indices);

public:
    loop_transform(int d, tree_for **tfs, boolean *doalls = NULL);
    ~loop_transform();

    tree_for *get_loop(int i)
	{ assert(i >= 0 && i < depth); 
	  return (loops[i]); }

    void annotate_doalls();
    boolean unimodular_transform(integer_matrix &m);
    boolean tile_transform(int *trip, int nregions, boolean *coalesce, 
			   int *first);
};

extern boolean bounds_ok(tree_for *tf);  // Are the bounds such that we can
                                         // transform the loop nest?

#endif







