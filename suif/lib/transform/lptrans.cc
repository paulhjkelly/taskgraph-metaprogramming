/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Unimodular loop transformations and tiling */

#define _MODULE_ "libtransform.a"
#pragma implementation "lptrans.h"

#define RCS_BASE_FILE lptrans_cc

#include "lptrans.h"
#include <suifmath.h>
#include <dependence.h>
#include <builder.h>
#include <useful.h>

RCS_BASE("$Id$")

/* Global variables */

const char *k_doall;
const char *k_tiled;
const char *k_tiledc;
const char *k_fpinner;

int debug_looptrans;

/* Local variables */

proc_sym *malloc_proc;
proc_sym *free_proc;

// Misc utility routines

boolean op_is_unity(tree_node_list *tnl)
{
    int val;

    if (tnl->op_is_intconst(&val) && (val == 1)) return TRUE;
    return FALSE;
}


static tree_for *get_perfect_inner(tree_node_list *tl)
{
    tree_for *inner = NULL;

    tree_node_list_iter it(tl);
    while (!it.is_empty()) {
	tree_node *n = it.step();

	switch (n->kind()) {
	    case TREE_FOR:
		if (inner) return NULL;
		inner = (tree_for *) n;
		break;

	    case TREE_INSTR:
		switch (((tree_instr *)n)->instr()->opcode()) {
		    case io_nop:
		    case io_mrk:
			break;
		    default:
			return NULL;
		}
		break;

            case TREE_BLOCK:
		if (inner) return NULL;
		inner = get_perfect_inner(((tree_block *) n)->body());
		break;
		
	    case TREE_IF:
	    case TREE_LOOP:
		return NULL;
	}
    }

    return inner;
}


tree_for *next_perfect_inner(tree_for *tf)
{
    return get_perfect_inner(tf->body());
}


boolean is_innermost(tree_for *tf)
{
    tree_node_list_iter it(tf->body());
    while (!it.is_empty()) {
        if (it.step()->kind() ==  TREE_FOR) return FALSE;
    }

    return TRUE;
}


//
// Manage annotations
//

//
// tiled and tiled-c annotations
//

static void *cfrom(const char * /* name */, immed_list *il, suif_object *)
{
    assert(!il->is_empty());
    tiled_annote *a = new tiled_annote;
    immed im = il->pop();
    assert(im.is_integer());
    a->depth = im.integer();
    assert(il->count() == a->depth);

    boolean *doalls = new boolean[a->depth];
    for (int i = 0; i < a->depth; i++) {
         im = il->pop();
	 assert(im.is_integer());
	 doalls[i] = im.integer();
    }
    a->doall_loops = doalls;
    return a;
}

static immed_list *cto(const char * /* name */, void *data)
{
    immed_list *il = new immed_list();
    tiled_annote *a = (tiled_annote *) data;

    il->append(immed(a->depth));
    for (int i = 0; i < a->depth; i++) {
        il->append(immed(a->doall_loops[i]));
    }

    return il;
}

static void freea(void *data) 
{ 
    delete (tiled_annote *) data;
}

static void prt(FILE *out, const char *name, void *data) 
{
   fprintf(out, "%s", name);
   tiled_annote *a = (tiled_annote *) data;

   fprintf(out, " %d ", a->depth);
   for (int i = 0; i < a->depth; i++)
      fprintf(out, "%d ", a->doall_loops[i]);
}


void init_transform(int& /* argc */, char * /* argv */ [])
{
  ANNOTE(k_doall, "doall", TRUE);
  ANNOTE(k_fpinner, "fp-inner", TRUE);

  STRUCT_ANNOTE(k_tiled, "tiled", TRUE, cfrom, cto, freea, prt);
  STRUCT_ANNOTE(k_tiledc, "tiled-c", TRUE, cfrom, cto, freea, prt);

  k_lbexpr = lexicon->enter("lbexpr")->sp;
  k_ubexpr = lexicon->enter("ubexpr")->sp;
}

void exit_transform(void)
{
  return;
}

// ===========================
//
// class loop_transform
//
// ===========================
//

boolean bounds_ok(tree_for *tf)
{
    boolean result = TRUE;

    bexpr *lb = new bexpr(tf->lb_op(), tf, 1);
    bexpr *ub = new bexpr(tf->ub_op(), tf, 1);

    if (lb->bx == NULL || ub->bx == NULL) {
        if (lb) delete lb;
        if (ub) delete ub;
	lb = NULL;
	ub = NULL;
	result = FALSE;
    } 

    bexpr *x;

    while ((x = (bexpr *) tf->get_annote(k_lbexpr)) != NULL) 
	delete x;
    while ((x = (bexpr *) tf->get_annote(k_ubexpr)) != NULL)
	delete x;    

    tf->append_annote(k_lbexpr, lb);
    tf->append_annote(k_ubexpr, ub);

    return result;
}

//
// Check that loops have been normalized and fill in lower/upper expressions
//
static boolean verify_transform(tree_for **loops, int depth)
{
    int i;
    for (i = depth-1; i >= 0; i--) {
	boolean normalize_ok;
        fill_in_access(loops[i], NORMALIZE_CODE, &normalize_ok);
	if (!normalize_ok) return FALSE;
    }

    for (i = 0; i < depth; i++) {
	boolean bexpr_ok = bounds_ok(loops[i]);
	if (!bexpr_ok) return FALSE;
    }

    return TRUE;
}

loop_transform::loop_transform(int d, tree_for **tfs, boolean *doalls)
{ 
    depth = d;
    loops = tfs;
    doall_loops = doalls;
    already_tiled = FALSE;

    utrans.ident(d);
    utrans_inv.ident(d);

    num_regions = 0;
    coalesce_region = NULL;
    trip_size = NULL;
    first_loop = NULL;

    for (int i = 0; i < depth; i++) {
        assert(loops[i]);
    }

    // Fill in access vectors and normalize:  Must first make sure we
    // have access vectors for the entire proc

    fill_in_access(loops[0]->proc()->block(), FALSE);   
    assert_msg(verify_transform(tfs, d), 
              ("Loops cannot be transformed due to bad bounds"));

    // see access_vector.h -- from here on in we want full control of
    // access vectors, because we know what's going on
    forget_about_mod_this = 1;

    binfo = new bizarre_bounds_info(loops, depth);
    block::set_proc(loops[0]->proc()->block());
}


loop_transform::~loop_transform()
{
    if (binfo) delete binfo;

    // Clean out annotes
    //
    for(int i = 0; i < depth; i++) {
        tree_for *tf = loops[i];

        bexpr *lb = (bexpr *) tf->get_annote(k_lbexpr); 
        bexpr *ub = (bexpr *) tf->get_annote(k_ubexpr); 
        assert(lb && ub);

	delete lb;
	delete ub;
    }
}


void loop_transform::generate_bounds()
{
    for(int i = 0; i < depth; i++) {
        tree_for *tf = loops[i];

        bexpr *lb = (bexpr *) tf->peek_annote(k_lbexpr); 
        bexpr *ub = (bexpr *) tf->peek_annote(k_ubexpr); 
        assert(lb && ub);

        tf->lb_op().remove();
	tf->ub_op().remove();

	base_symtab *scope = loops[i]->scope();
	assert(scope->is_block());

        operand new_lb = lb->generate_code(io_divceil,(block_symtab *) scope);
        operand new_ub = ub->generate_code(io_divfloor,(block_symtab *) scope);
        tf->set_lb_op(cast_op(new_lb, tf->index()->type()->unqual()));
        tf->set_ub_op(cast_op(new_ub, tf->index()->type()->unqual()));
    }
}


void loop_transform::annotate_doalls()
{
    assert(doall_loops);

    for (int i = 0; i < depth; i++) {
        if (doall_loops[i] && !loops[i]->peek_annote(k_doall))
	    loops[i]->append_annote(k_doall, new immed_list());
    }
}


// ===========================
//
//  tile transform
//
// ===========================
//


void loop_transform::tile_region(int region_num, int first, int last,
				 var_sym **outer_indices)
{
    boolean coalesce = coalesce_region[region_num];
    tree_node_list *tnl = loops[first]->parent();
    base_symtab *base_scope = tnl->scope();
    assert(base_scope->is_block());
    block_symtab *scope = (block_symtab *) base_scope;

    tree_for **outer_fors = new tree_for *[depth];	// the outer for loops
    // The tree_fors in loops become the inner for loops

    // If it's both non-perfectly nested and non-rectangular 
    // I can't properly coalesce right now.

    if (coalesce) {
        // best be perfectly nested
	int i;
        for (i = first; i < last; i++)
            if (next_perfect_inner(loops[i]) != loops[i+1])  break;

        if (i < last) {
            // ok, fine -- best be rectangular bounds:
	    int i;
            for(i = first+1; i <= last; i++) {
                for(int j = first; j < i; j++) {
		    bexpr *lb = (bexpr *) loops[i]->peek_annote(k_lbexpr);
		    bexpr *ub = (bexpr *) loops[i]->peek_annote(k_ubexpr);
		    assert(lb && ub);
 
                    if (lb->is_fctn(loops[j]) || ub->is_fctn(loops[j])) {
                        coalesce = FALSE;
                        break;
                    }
                }
            }
        }
    }
    
    //
    // Part 1: tile the inner loops and make copies of the bounds
    //
    int i;
    for (i = first; i <= last; i++) {
        if (i == first && coalesce) continue;

        var_sym *outer = outer_indices[i];
        bexpr *lb = (bexpr *) loops[i]->peek_annote(k_lbexpr);
	bexpr *ub = (bexpr *) loops[i]->peek_annote(k_ubexpr);
        assert(lb && ub);
    
        // Build skeleton of for loop: pass in index, lb, ub, body    
        // Note: just pass in bogus bounds of lb = 1 to ub = 2 to keep 
        // builder happy -- the bounds get fixed up later.
        outer_fors[i] = (tree_for *)
            block::FOR(block(outer), block(1), bop_leq, block(2),
	        block(new tree_node_list())).make_tree_node(tnl);

        // Before manipulating the ub and lb of the inners, make copies
        // for the outers.
        
        outer_fors[i]->append_annote(k_lbexpr, lb->duplicate());
        outer_fors[i]->append_annote(k_ubexpr, ub->duplicate());

	operand new_step = loops[i]->step_op().clone();
        outer_fors[i]->step_op().remove();
        outer_fors[i]->set_step_op(new_step);
        
        // Inner loops transformed as follows: 
        // Each lower bound becomes max(original, outer) 
        // and each upper bound  becomes min(original,outer+B-1).
        // Exception: for the lower bound of outermost loop, the max is
        // the outer index.
        
        // lower bound
        access_vector *av = new access_vector();
        av->add(outer_fors[i], 1);
        if(i == first) {
            lb = (bexpr *) loops[i]->get_annote(k_lbexpr);   // remove annote
            delete lb;
            lb = new bexpr(*av, 1);
	    loops[i]->append_annote(k_lbexpr, lb); // put new annote
        } 
        else {
            lb->insert_max(av,1);
        }
        
        // upper bound
        av->con += trip_size[i] - 1;
        ub->insert_min(av, 1);
        
        // step
        assert(op_is_unity(loops[i]->step_list()));
    }

    //
    // Part 2: fix the lb, ub and step of the outer loops.
    //
    
    for(i = first; i <= last; i++) {
        if (i == first && coalesce) continue;
        
        tree_for *cur_outer = outer_fors[i];

        bexpr *outer_lb = (bexpr *) cur_outer->peek_annote(k_lbexpr);
        bexpr *outer_ub = (bexpr *) cur_outer->peek_annote(k_ubexpr);
        assert(outer_lb && outer_ub);

        if (coalesce) {
            outer_lb = (bexpr *) cur_outer->get_annote(k_lbexpr);
            outer_ub = (bexpr *) cur_outer->get_annote(k_ubexpr);
            delete outer_lb;  delete outer_ub;

            outer_lb = new bexpr(binfo->lmin[i],1);
            outer_ub = new bexpr(binfo->umax[i],1);

            cur_outer->append_annote(k_lbexpr, outer_lb);
            cur_outer->append_annote(k_ubexpr, outer_ub);
        } 
        else {
            // For lower bound: 
            // For all indexes j in lb, use j if lb(j) is an increasing 
            // function of j and use lb(j+Bj-1) if j is a decreasing function.
            // For upper bound, reverse.  
            // Uses bexpr::mono() to test for mono inc/dec functions
            
            for (int j = first; j < i; j++) {
                // change inner index into outer index
                access_vector avj;
                avj.add(outer_fors[j], 1);
                
                access_vector avjb;
                avjb.add(outer_fors[j], 1);
                avjb.con = trip_size[j] - 1;
                
                outer_lb->replace(loops[j], &avj);
                outer_ub->replace(loops[j], &avj);
                
                switch(outer_lb->mono(outer_fors[j], 0)) {
                    case mn_independent:
                    case mn_inc:	 // increasing -- use j
                        break;

                    case mn_dec:	 // decreasing -- use j-Bj+1
                        outer_lb->replace(outer_fors[j], &avjb);
		        break;
    
                    case mn_linear:      // could be more clever with linear.
                    case mn_mess:
                         outer_lb = (bexpr *) cur_outer->get_annote(k_lbexpr);
                         delete outer_lb;  
                         outer_lb = new bexpr(binfo->lmin[i], 1);
                         cur_outer->append_annote(k_lbexpr, outer_lb);
       
                    default: assert(0);
                }

                switch(outer_ub->mono(outer_fors[j], 0)) {
                    case mn_independent:
                    case mn_dec:	// decreasing -- use j
                        break;

                    case mn_inc:	// increasing -- use j-Bj+1
                       outer_ub->replace(outer_fors[j], &avjb);
                       break;

                    case mn_linear:	// could be more clever with linear
                    case mn_mess:
                         outer_ub = (bexpr *) cur_outer->get_annote(k_ubexpr);
                         delete outer_ub;  
                         outer_ub = new bexpr(binfo->umax[i], 1);
                         cur_outer->append_annote(k_ubexpr, outer_ub);

                  default:
                    assert(0);
                }
            } // for
	}  // else
        
        // fix the step

        assert(cur_outer->step_op().is_expr());
        assert(op_is_unity(cur_outer->step_list()));

        instruction *ins = cur_outer->step_op().instr();
        assert(ins->format() == inf_ldc);
        in_ldc *il = (in_ldc *) ins;
        il->set_value(trip_size[i]);
    }

    //
    // Part 3: Weave them all into correct loop structure.
    //
    if (coalesce)
        tnl->insert_after(outer_fors[first+1], tnl->lookup(loops[first]));
    else
        tnl->insert_after(outer_fors[first], tnl->lookup(loops[first]));

    tnl->remove(tnl->lookup(loops[first]));

    for (i = first; i <= last; i++) {
        if(i == first && coalesce) continue;

        tree_node_list *bdy = new tree_node_list(outer_fors[i]);
        bdy->append(i == last ? loops[first] : outer_fors[i+1]);
        outer_fors[i]->set_body(bdy);
    }

    //
    // Part 4: Now fix the non-perfectly nested portions
    //
    for(i = first+1; i <= last; i++) {
      {
        tnl = loops[i]->parent();
        tree_node_list *lnew = new tree_node_list();
        tree_node_list_iter it(tnl);

        boolean boring = TRUE;
        while (!it.is_empty()) {
            tree_node *n = it.step();
            if (n == loops[i]) {
                break;
            } 
            else {
                if (n->kind() == TREE_INSTR && 
		   ((tree_instr *)n)->instr()->opcode() == io_mrk)  ;
                else boring = FALSE;

                lnew->append(tnl->pop());
            }
        }

        if (boring) delete lnew;
        else {
	    block curr(lnew, FALSE);

            // The if header is the only tricky part:
                
            for (int ii = i; ii <= last; ii++) {
               bexpr *temp_lb = 
		 (bexpr *) outer_fors[ii]->peek_annote(k_lbexpr);
               assert(temp_lb);

               block bmin(temp_lb->generate_code(io_divceil, scope));
#ifndef DEAL_WITH_GCC_BRAIN_DAMAGE
	       block bind(block(outer_fors[ii]->index()));
#else
//
//  gcc version 2.6.3 gets a parse error on the code above.  The code
//  looks like it should be perfectly legal, and both gcc 2.5.8 and
//  the IRIX 5.3 C++ compiler have no problem with it, so I'm inclined
//  to think the problem is caused by brain damage in the g++
//  front-end.  A work-around follows.
//
	       block b2(outer_fors[ii]->index());
	       block bind(b2);
#endif

	       curr.set(block::IF(block(bind == bmin), curr));
            }

            tree_if *ti = (tree_if *) curr.make_tree_node(tnl);
            tnl->insert_before(ti, tnl->lookup(loops[i]));
        }
      }

      // Likewise for other side

      {
        tnl = loops[i]->parent();
        tree_node_list *lnew = new tree_node_list();
        tree_node_list_iter it(tnl);

        assert(!it.is_empty());
        tree_node *n = it.step();

        if (n->kind() == TREE_IF) {
            assert(!it.is_empty());
            n = it.step();
        }

        assert(n == loops[i]);
        boolean boring = TRUE;

        while (!it.is_empty()) {
            // skip useless instructions
            n = it.step();
            if (n->kind() == TREE_INSTR &&
               ((tree_instr *)n)->instr()->opcode() == io_mrk) ;
            else {
                boring = FALSE;
                break;
            }
        }
    
        if (boring) {
            // perfectly nested on this side of for loop
            delete lnew;
        } 
        else {
            tnl->remove(tnl->lookup(n));
            lnew->append(n);

            while (!it.is_empty()) {
                tree_node *n = it.step();
                tnl->remove(tnl->lookup(n));
                lnew->append(n);
            }

            // The if header is the only tricky part:
    
	    block curr(lnew, FALSE);

            for (int ii = i; ii <= last; ii++) {
                tree_for *temp_outer = outer_fors[ii];

                // If last iteration when i+step > up
                // test (i+step > up)

                block bind(block(temp_outer->index()) +
                            block(temp_outer->step_op()));
                bexpr *temp_ub = (bexpr *) temp_outer->peek_annote(k_ubexpr);
                assert(temp_ub);

                block bmax(temp_ub->generate_code(io_divfloor, scope));
	        curr.set(block::IF(block(bind > bmax), curr));
            }

            tree_if *ti = (tree_if *) curr.make_tree_node(tnl);
            tnl->insert_after(ti, tnl->lookup(loops[i]));
        }
      }
    }

    //
    // part 5: fix the bounds of the outer loops, which are never fixed later
    //
    for(i = first; i <= last; i++) {
        if(i == first && coalesce) continue;

        tree_for *nn = outer_fors[i];  assert(nn);

        bexpr *temp_lb = (bexpr *) nn->get_annote(k_lbexpr);
        bexpr *temp_ub = (bexpr *) nn->get_annote(k_ubexpr);
        assert(temp_lb && temp_ub);

        nn->lb_op().remove();
        nn->ub_op().remove();

        operand new_lb = temp_lb->generate_code(io_divceil, scope);
        operand new_ub = temp_ub->generate_code(io_divfloor, scope);
        nn->set_lb_op(cast_op(new_lb, nn->index()->type()->unqual()));
        nn->set_ub_op(cast_op(new_ub, nn->index()->type()->unqual()));

        delete temp_lb;
        delete temp_ub;
    }

    //
    // part 6: Add the annotation that tells us we can parallelize
    //
    tiled_annote *a = new tiled_annote;
    a->depth = last-first+1;
    a->doall_loops = new boolean[a->depth];

    if (!coalesce) {
	int count = 0;
        for(i = first; i <= last; i++) {
            a->doall_loops[count++] = doall_loops[i];
        //    if (doall_loops[i]) outer_fors[i]->append_annote(k_doall, 
	//						     new immed_list());
        }
        outer_fors[first]->append_annote(k_tiled, a);
    } 
    else {
	int count = 0;
        for(i = first+1; i <= last; i++) {
            a->doall_loops[count++] = doall_loops[i];
        //    if (doall_loops[i]) outer_fors[i]->append_annote(k_doall, 
	//						     new immed_list());
        }
        a->doall_loops[count] = doall_loops[first];
        outer_fors[first+1]->append_annote(k_tiledc, a);
    }
    delete[] outer_fors;
}


//
// Here is the transformation we perform:
//
//	for i0 = l0,u0,1
//	 i1 = l1(i0),u1(i0),1
//	  i2 = l2(i0,i1),u2(i0,i1),1
//	   ...
//
// becomes
//
//	i0' = l0,u0,b0
//	 i1' = max(l1(i0'),l1(i0'+b0-1)),max(u1(i0'),u1(i0'+b0-1)),b1
//		-- compute these maxs and mins at compile time, if possible.
//		-- if not, it's ok to take the min and max over the entire
//		-- range.
//	  ...
//	   i0 = i0',min(i0'+b0-1,u0),1
//	    i1 = max(i1',l1(i0)),min(i1'+b1-1,u1(i0)),1
//	     ...
//
// Actually, it's a bit more complex than that.  The imperfectly nested
// regions have to be protected.  E.g.
//     for i
//        S1
//        for j
//          S2
//
// becomes
//     for i'
//       for j'
//         for i
//            if(j' == j'min) S1
//            for j
//               S2
//
void loop_transform::tile_loops()
{
    var_sym **outer_indices = new var_sym *[depth];
    int nregions = 0;
    base_symtab *tab = loops[0]->scope();

    int i;
    for (i=0; i < depth; i++) {
        if (first_loop[nregions] == i && first_loop[++nregions] == i+1) {
            outer_indices[i] = NULL;
        } 
        else {
            char buf[64];
            sprintf(buf, "%s_tile", loops[i]->index()->name());
            outer_indices[i] = 
	        tab->new_unique_var(loops[i]->index()->type(), buf);
        }
    }

    boolean innermost = is_innermost(loops[depth-1]);

    for(i=0; i < num_regions; i++) {
        int outer_loop = first_loop[i]; 
        int inner_loop = first_loop[i+1] - 1;

        if(outer_loop != inner_loop) {
            if (innermost && i == num_regions-1) {
                // mark innermost tilable nest
                for (int j = outer_loop; j <= inner_loop; j++)
                    loops[j]->append_annote(k_fpinner, new immed_list());
            }
    
            tile_region(i, outer_loop,inner_loop, outer_indices);
        }
    }

    delete[] outer_indices;
}


boolean loop_transform::tile_transform(int *trip, int nregions, 
				       boolean *coalesce, int *first)

{
    if (binfo->useless) return FALSE;

    num_regions = nregions;
    coalesce_region = coalesce;
    trip_size = trip;
    first_loop = first;

    // Verify inputs
    //
    assert_msg(doall_loops, ("Need doall info for tile transform\n"));
    assert_msg((first[nregions] == depth), 
 	       ("Bad input in first array to tile transform\n"));

    annotate_doalls();

    // The transformations proper

    tile_loops();

    // Generate code for the bounds:

    generate_bounds();

    already_tiled = TRUE;

    return TRUE;
}



// ===========================
//
//  unimodular transformation
//
// ===========================
//


//
// Code for access_vector >= 0, placed in lb or ub as appropriate 
//
void loop_transform::generate_inequality(access_vector *av, tree_for *tf)
{
    int val = -av->val(tf);

    if (debug_looptrans>2) {
      av->print(stdout);
      printf(" >= 0 ... %s\n",val?"interesting":"quick exit");
   
    }

    av->add(tf, val);

    if (val == 0) return;
    
    // Now we have *av >= val*f.  
    // If val > 0, then *av/val >= f and floor(*av/val) is an upper bound.  
    // If val < 0, then (-*av/-val) <= f and ceil(-*av/-val) is a lower bound.

    if(debug_looptrans) {
        if(val != 1) printf("%d*",val);
        printf("%s <=", tf->index()->name());
        av->print(stdout);
        printf("\n");
    }
      
    assert(!av->too_messy);

    bexpr *lb = (bexpr *) tf->peek_annote(k_lbexpr);
    bexpr *ub = (bexpr *) tf->peek_annote(k_ubexpr);

    if(debug_looptrans>1) {
        printf("%s before insertion: ",val>0?"upper":"lower");
        if(val > 0 && ub) ub->print(stdout);
        else if(lb) lb->print(stdout);
        printf("\n");
        fflush(stdout);
    }

    int konst = av->con;
    av->con = 0;
    access_vector new_av = *av / val;

    int divisor = 1;

    if (new_av.too_messy) {	// was worth a try :-(
        av->con = konst;
        divisor = val;
        new_av = *av;
    } 
    else {
        if (val > 0 && konst >= 0) new_av.con = konst / val;
        else if (val > 0) new_av.con = -(-konst + val - 1) / val;
        else if (konst <= 0) new_av.con = -konst / -val;
        else new_av.con = -(konst - val - 1) / -val;
    }

    if (val > 0) {
        if (ub) { 
	    ub->insert_min(&new_av, divisor); 
	}
        else {
            ub = new bexpr(new_av, divisor);
            tf->append_annote(k_ubexpr, ub);
        }
    } 
    else {
        if (lb) {
            lb->insert_max(&new_av, divisor);
        } 
        else {
            lb = new bexpr(new_av, divisor);
            tf->append_annote(k_lbexpr, lb);
        }
    }


    if(debug_looptrans>1) {
        printf("%s after insertion: ",val>0?"upper":"lower");
        if(val > 0) ub->print(stdout);
        else lb->print(stdout);
        printf("\n");
        fflush(stdout);
    }
}


int loop_transform::find_index(operand r)
{
    if (r.is_symbol()) {
        var_sym *vs = r.symbol();
        for (int i = 0; i < depth; i++)
            if (loops[i]->index() == vs) return i;
    }

    return -1;
}


boolean loop_transform::new_access(operand r, access_vector *access_vec)
{  
     access_vector av;
     int f;

     if ((f = find_index(r)) >= 0) {
         for (int j = 0; j < depth; j++) {
             if (utrans_inv[f][j]) av.add(loops[j], utrans_inv[f][j]);
	 }

         *access_vec = av;
         return TRUE;
     }    

     return FALSE;
}

boolean transform_body_src(instruction *i, operand *r, void *x)
{
    access_vector av;
    loop_transform *tr = (loop_transform *) x;

    if (r->is_instr()) {
	r->instr()->src_map(transform_body_src, x);
    }

    else if (tr->new_access(*r, &av)) {
	r->remove();
	base_symtab *scope = i->parent()->scope();
	assert(scope->is_block());
	*r = av.generate_code(block::get_proc(), (block_symtab *) scope);
	return TRUE;
    }

    return FALSE;
}


void transform_body_tree(tree_node *tn, void *x)
{
    loop_transform *tr = (loop_transform *) x;

    if (tn->kind() == TREE_INSTR) {
        instruction *ins = ((tree_instr *) tn)->instr();
        int f = tr->find_index(ins->dst_op());

        assert_msg(f < 0, 
           ("Index assigned to in loop %s", tr->loops[f]->index()->name()));

        ins->src_map(transform_body_src, x);
    }
}


//
//
// Performs the unimodular transformation:
//
// This routine transforms the bounds and the body, but not the
// index names.  For example, applying interchange to:
//
//   for i = 1,n
//     for j = i+1,n
//       a(i,j)
//
// is transformed into:
//   for i = 1,n
//     for j = 1,j-1
//       a(j,i)
//
// but NOT the following:
//   for j = 1,n
//     for i = 1,i-1
//       a(i,j)
//
void loop_transform::utransform()
{
    // If utrans is identity, then no work to do
    //

    if (debug_looptrans) {
        printf("utransform: bounds info and matrix:\n");
        binfo->print(stdout);
        utrans.print(stdout);
    }

    integer_matrix id;    
    id.ident(utrans.m());
    if (utrans == id) return;

    // The bounds info contains the max and mins for each index.
    // Now calculate the new maxs and mins for the transformed indices,
    // and transform the equations.

    bizarre_bounds_info *new_binfo = new bizarre_bounds_info();
    new_binfo->useless = binfo->useless; 
    new_binfo->loop = binfo->loop;
    new_binfo->loops = binfo->loops; 
    new_binfo->equations = binfo->equations;

    // Step 3 from Section VII of TPDS paper:
    //   
    int i;
    for (i = 0; i < depth; i++) {
        access_vector lower, upper;

        for(int j = 0; j < depth; j++) {
	    int val = utrans[i][j];
	    if (val > 0) {
               lower += val * binfo->lmin[j]; 
               upper += val * binfo->umax[j];
            }
	
            else if (val < 0) {
               lower += val * binfo->umax[j]; 
               upper += val * binfo->lmin[j];
            }
        }

	new_binfo->lmin[i] = lower; 
        new_binfo->umax[i] = upper;
    }

    // Update equations
    //
    for(i = 0; i < binfo->equations; i++) {
	*(new_binfo->eq[i].rhs) = *(binfo->eq[i].rhs);
	
	int j;
        for(j = 0; j < depth; j++) new_binfo->eq[i].coeff[j] = 0;
    
        for(j = 0; j < depth; j++) {
            int c = binfo->eq[i].coeff[j];
            if (c) {
                for(int k = 0; k < depth; k++)
                   new_binfo->eq[i].coeff[k] += c*utrans_inv[j][k];
            }
        }
    }

    if (debug_looptrans) {
        printf("Before unimodular transform\n");
	binfo->print(stdout);
    }

    // We now use the new bounds to solve our problem.  

    delete binfo;
    binfo = new_binfo;

    if (debug_looptrans) {
        printf("After unimodular transform\n");
	binfo->print(stdout);
    }
    
    for (i = 0; i < depth; i++) {

        bexpr *expr;
        if ((expr = (bexpr *)loops[i]->get_annote(k_lbexpr)) != NULL)
	    delete expr;
        if ((expr = (bexpr *)loops[i]->get_annote(k_ubexpr)) != NULL)
	    delete expr;

        // Step 4 of section VII of TPDS paper:
        // For each loop that will go outside loop[i], use the above 
        // inequalities to contribute to the max of the lower or the min of 
        // the upper.  For each loop that will be inside this, just use their 
        // max or min.
    
	if (debug_looptrans>1)  printf("i = %d\n", i);

        for (int e = 0; e < binfo->equations; e++) {
            
            // The access vector will be an expression that must be
            // greater than zero.

            if (debug_looptrans) printf("e = %d\n", e);
            
            access_vector temp = *(binfo->eq[e].rhs);
            access_vector av = -temp;

            int valx = binfo->eq[e].coeff[i];
            if(valx == 0) continue;
    
            for(int k = 0; k < binfo->loops; k++) {
                int val = binfo->eq[e].coeff[k];
                if (k > i) 
                    av += val * ((val > 0) ? binfo->umax[k] : binfo->lmin[k]);
		else
		    av.add(loops[k],val);
            }
            generate_inequality(&av, loops[i]);
        }

        bexpr *lb, *ub;
        assert((lb = (bexpr *) loops[i]->peek_annote(k_lbexpr)) != NULL);
        assert((ub = (bexpr *) loops[i]->peek_annote(k_ubexpr)) != NULL);

	if (debug_looptrans>1) {
            printf("before simplification lower bound: ");
            lb->print(stdout);
            printf("\n");
            printf("before simplification upper bound: ");
            ub->print(stdout);
            printf("\n");
            fflush(stdout);
        }
     
        lb->simplify(binfo);
        ub->simplify(binfo);

        if(debug_looptrans) {
            printf("lower bound: ");
            lb->print(stdout);
            printf("\n");
            printf("upper bound: ");
            ub->print(stdout);
            printf("\n");
            fflush(stdout);
        }
    }
    
    // Transform loop bodies 
    
    loops[0]->body()->map(transform_body_tree, this);
}
       
boolean loop_transform::unimodular_transform(integer_matrix &m)
{
    if (binfo->useless || already_tiled) return FALSE;

    int determ = m.determinant();
    assert_msg((determ == 1) || (determ == -1), 
       ("Matrix passed to unimodular_transform is not unimodular, det = %d",
	determ));       

    utrans = m;
    utrans_inv = m.inverse();

    // The transformations proper

    utransform();

    // Generate code for the bounds:

    generate_bounds();

    if (doall_loops) annotate_doalls();
    return TRUE;
}

