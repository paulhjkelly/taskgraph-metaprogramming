/* file "deptest_msc.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "deptest_msc.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "include.h"


tree_node * next_further_out(tree_node * tn, tree_kinds tk, boolean inclusive)
{
    if(inclusive && tn->kind() == tk) return tn;
    if(tn->parent() == NULL) return NULL;
    tree_node * t = tn->parent()->parent();
    if(t == NULL) return NULL;
    if(t->kind() == tk) return t;
    return next_further_out(t, tk);
}


// is r modified in the list
// if for_loop is set, do not search through for_loop's for loop

int sym_modified(var_sym * sym, tree_node_list *l, tree_for *for_loop)
{
    assert(sym);
    assert(l);
    tree_node_list_iter tni(l);
    while(!tni.is_empty()) {
        tree_node *n = tni.step();
        switch(n->kind()) {
          case TREE_FOR:
	  {
            tree_for *nf = (tree_for *)n;
            if(nf->index() == sym)
                return 1;
            if(sym_modified(sym, nf->lb_list())) return 1;
            if(sym_modified(sym, nf->ub_list())) return 1;
            if(sym_modified(sym, nf->step_list())) return 1;
            if (!for_loop || (for_loop != nf)) {
                if(sym_modified(sym, nf->body())) return 1;
            }
	  }
            break;
          case TREE_LOOP:
	  {
            tree_loop *nl = (tree_loop *)n;
            if(sym_modified(sym, nl->body())) return 1;
            if(sym_modified(sym, nl->test())) return 1;
	  }
            break;
          case TREE_IF:
	  {
            tree_if *ni = (tree_if *)n;
            if(sym_modified(sym, ni->header())) return 1;
            if(sym_modified(sym, ni->then_part())) return 1;
            if(sym_modified(sym, ni->else_part())) return 1;
	  }
            break;
        case TREE_BLOCK:
	  {
            tree_block * bi = (tree_block *)n;
            if(sym_modified(sym, bi->body())) return 1;
	  }
            break;
          case TREE_INSTR:
	  {
            instruction *i = ((tree_instr *)n)->instr();
            if(i->dst_op().is_symbol())
                if(i->dst_op().symbol() == sym) return 1;

	  }
            break;
          default:
            error_line(1, NULL,"unknown ast kind: %d",n->kind());
        }
    }
    return 0;
}

/*
 * Count number of ldc of path instructions here or below.
 * If something other than ldc path or arithmetic, return -1.
 */
/*
int nldc(operand *n, tree_instr **it)
{
    int l,r;

    if(n == 0) return 0;

    switch(n->instr()->opcode) {
      case io_ldc:
        in_ldc *i = (in_ldc *)n->instr;
        switch(i->value.kind()) {
          case ic_symbol: if(it) {*it = n;} return 1;
          case im_int: return 0;
          default: return -1;
        }
      case io_add: case io_sub:
      case io_mul: case io_div:
      case io_min: case io_max:
      case io_divceil: case io_divfloor:
      case io_lsl: case io_lsr: case io_asr:
      case io_array:
        l = nldc(n->left(),it);
        r = nldc(n->right(),it);
        return l == -1 || r == -1 ? -1 : l+r;
      default:
        return -1;
    }
}


path *path_of_ldc(in_ldc *i)
{
    assert(i->opcode == io_ldc);
    path *rval = 0;
    if(i->value.discriminator == ic_path) {
        annote *a = i->get_annote(k_path);
        if(a) {
            i->append(a);
            rval = (*a)[0].p();
        } else {
            rval = ((in_ldc *)i)->value.p();
        }
    }
    return rval;
}
*/

#define RP SIMPLE_IPA_IND_READ_PARAM
#define WP SIMPLE_IPA_IND_WRITE_PARAM
#define RG SIMPLE_IPA_READ_GLOB
#define WG SIMPLE_IPA_WRITE_GLOB

int simple_ipa_call_info(in_cal *ic)
{
    annote_list * al = ic->annotes();
    assert(al);
    annote *a = al->peek_annote("ipa");
    if(a == 0) return RP|WP|RG|WG;

    int rval = 0;
    const char *s = NULL; // = (*a)[0].string();
    assert(s);
    
    for(const char *t = s; *t; t++) {
        switch(*t) {
          case 'r': rval |= RP; break;
          case 'w': rval |= WP; break;
          case 'x': rval |= RP; break;
        }
    }

    a = al->peek_annote("ipa global x");
    if(a) {
        rval |= (RG|WG);
    }
    a = al->peek_annote("ipa global r");
    if(a) {
        rval |= RG;
    }
    a = al->peek_annote("ipa global w");
    if(a) {
        rval |= WG;
    }
    return rval;
}
#undef RP
#undef WP
#undef RG
#undef WG

tn_list *make_al(tree_node *s1,tree_node *s2)
{
    tree_for *r1f = (tree_for *) next_further_out(s1, TREE_FOR, TRUE);
    tree_for *r2f = (tree_for *) next_further_out(s2, TREE_FOR, TRUE);

    // find innermost common loop: that's n
    tree_for *n = r1f;
    for( ; n; n = (tree_for *)next_further_out(n, TREE_FOR)) {
        tree_for *nx = r2f;
        while(nx && nx != n)
            nx = (tree_for *) next_further_out(nx, TREE_FOR);
        if(nx == n)
            break;
    }
    assert(n);

    /* goofy part, necessary for current dependence analyzer */
    tree_for *n2 = n;
    if(r1f == n) n2 = r2f;
    else if(r2f == n) n2 = r1f;

    /* and now putting these on an tree_node_list */
    tn_list *al = new tn_list;
    for( ; n2; n2 = (tree_for *)next_further_out(n2, TREE_FOR)) {
        al->append(n2);
    }
    return al;
}


/*
 * returns
 *		ambig_addr: whether pb has its address taken for something other than
 *					a read or write.  For a global, returns true always.
 *		read: whether pb is read for sure
 *		write: whether pb is written for sure
 *		ambig_read: whether something is ambiguously read
 *		ambig_write: whether something is ambiguously written
 */

/*
#define LAI(l) list_addr_info_aux(l,pb,ambig_addr,read,write,ambig_read,ambig_write,calls,si,is_write)
#define AA (++(*ambig_addr))
#define R (++(*read))
#define W (++(*write))
#define AR (++(*ambig_read))
#define AW (++(*ambig_write))
#define C (++(*calls))

static dvlist *dv;  // is null only when bad


static void list_addr_info_aux(tree_node_list *l, sym_node *sym,
                               int *ambig_addr, int *read, int *write,
                               int *ambig_read, int *ambig_write,
                               int *calls, tree_instr *si, int is_write)
{
    assert(l);
    tree_node_list_iter tnli(l);

    while(!ani.is_empty()) {
        tree_node *n = tnli.step();
        switch(n->kind()) {
        case TREE_INSTR:
            in_ldc *ix = (in_ldc *) ((tree_instr *)n)->instr();
            path *pax = ix->opcode == io_ldc ? path_of_ldc((in_ldc *)ix) : 0;
            path_base *pa = pax ? pax->base : 0;
            if(pa && (*pa<=*sym || *sym<=*pa)){
                if(pa != sym) {AA; break;}
                // go up to previous load/store
                tree_instr *np=((tree_instr *)n)->parent();
                if_ops op;
                while(np && (op=np->instr->opcode)!=io_lod && op!=io_str)
                    np = np->parent();
                if(np == 0 || nldc(np->left()) != 1)
                    {AA;}
                else {
                    if(op == io_lod) R; else W;
                    if(op == io_str || is_write) {
                        array_info *ai2 = ((dep_instr_annote *)np->left())->ai;
                        // time for some dependence testing, I suppose
                        if(si == 0 || np->left() == 0 || np->left()->instr->opcode != io_array || si->ai == 0 || ai2 == 0) {
                            delete dv;
                            dv = 0;
                        } else if(dv) {
                            tree_node_list *al = make_al(si,np->left());
                            dvlist *tmp = new dvlist(si->ai,ai2,al);
                            delete al;
                            while(!tmp->is_empty())
                                dv->push(tmp->pop());
                            delete tmp;
                        }
                    }
                }
            } else if(ix->opcode == io_lod || ix->opcode == io_str) {
                // If a ld/st through a register, ambiguous.
                int nl = nldc(((tree_instr *)n)->left());
                if(nl != 1) ix->opcode == io_lod ? AR : AW;
            } else if(ix->opcode == io_cal) {
                int s = simple_ipa_call_info((in_cal *)ix);
                if(s) C; // cop out: anything interesting -> is call 
            }
            break;
        case TREE_FOR:
            tree_for *nf = (tree_for *)n;
            LAI(nf->lb_list());
            LAI(nf->ub_list());
            LAI(nf->step_list());
            LAI(nf->body());
            break;
        case TREE_IF:
            tree_if *ni = (tree_if *)n;
            LAI(ni->header());
            LAI(ni->then_part());
            LAI(ni->else_part());
            break;
        case TREE_LOOP:
            tree_loop *nl = (tree_loop *)n;
            LAI(nl->body());
            LAI(nl->test());
            break;
        case TREE_BLOCK:
            tree_block *nb = (tree_block *)n;
            LAI(nb->body());
            break;
        default:
            error_line(1, NULL,"bad tree_node in list_addr_info_aux");
        }
    }
    if(was_treelist) l->make_treelist(0,0);
}
#undef AA
#undef R
#undef W
#undef AR
#undef AW
#undef C
#undef LAI

// assumes trees are built, or treelist 
// returns 0 when dvlist "too messy" 
dvlist *list_addr_info(tree_node_list *l,path_base *sym,
                    int *read,int *write,int *ambig_read,int *ambig_write,
                    int fortran, dep_instr_annote *si,int is_write)
{
    ast_proc *p = l->procedure;
    path_base *pseudo_sym = get_path_base(p->name->base,"pseudo");
    path_base *slocal_sym = get_path_base(p->name->base,"static");
    int global = !(*sym <= *pseudo_sym || *sym <= *slocal_sym);

    *read = *write = *ambig_read = *ambig_write = 0;
    int calls = 0,ambig_addr = 0;

    dv = new dvlist;
    list_addr_info_aux(l,sym,&ambig_addr,read,write,
                                    ambig_read,ambig_write,&calls,
                                    si,is_write);

    if(!global && !fortran) {	// need ambig_addr for entire body 
        int dummy = 0;
        list_addr_info_aux(p->g->procbody,sym,&ambig_addr,&dummy,&dummy,&dummy,&dummy,&dummy,0,0);
    }

    if(global || ambig_addr) {
        *ambig_write += calls;
        *ambig_read += calls;
    }
    if(dv && *ambig_write) {delete dv; dv = 0;}
    if(dv && *ambig_read && is_write) {delete dv; dv = 0;}

    return dv;
}

int pb_modified(path_base *pb,tree_node_list *l,int fortran)
{
    int read,write,ambig_read,ambig_write;
    list_addr_info(l,pb,&read,&write,&ambig_read,&ambig_write,fortran);
    return write || ambig_write;
}
*/
