/* file "bexpr.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Bounds expressions */

#define _MODULE_ "libtransform.a"
#pragma implementation "bexpr.h"

#define RCS_BASE_FILE bexpr_cc

#include "bexpr.h"
#include <builder.h>

RCS_BASE("$Id$")

const char *k_lbexpr;
const char *k_ubexpr;

int debug_bexpr;

//-----------------------------------

static boundexpr *gen_boundexpr_reg(operand r, tree_node *tn);
static boundexpr *fastc(instruction *n, tree_node *tn);

equation::equation()
{
    rhs = new access_vector();
}

equation::~equation()
{
    delete rhs;
}

void bexpr::constructor_helper(instruction *n, tree_node *tn, int d)
{
    bx = fastc(n, tn);
    if (bx) {
        divisor = d;
    }
}

void bexpr::constructor_helper(operand r, tree_node *tn, int d)
{
    bx = gen_boundexpr_reg(r, tn);
    if(bx) {
        divisor = d;
    }
}


bexpr::bexpr(access_vector &av,int div):divisor(div)
{
    bx = new av_boundexpr(&av); 
}

bexpr::bexpr(instruction *n, int div)
{
    constructor_helper(n, n->parent(), div);
}


bexpr::bexpr(operand r, tree_node *parent, int div)
{
    if (r.is_instr()) constructor_helper(r.instr(), parent, div);
    else constructor_helper(r, parent, div);
}

bexpr::~bexpr()
{
    if(bx) delete bx;
}

void bexpr::insert_min(access_vector *av,int div)
{
    access_vector a = *av;
    assert(bx);

    assert(!a.too_messy);

    if((divisor%div) == 0) a *= (divisor/div), div = divisor;
    else if((div%divisor) == 0) *bx *= (div/divisor), divisor = div;

    if(divisor != div) a *= divisor, *bx *= div, divisor *= div;

    bx = new mn_boundexpr(bx,new av_boundexpr(&a));
}

void bexpr::insert_max(access_vector *av,int div)
{
    access_vector a = *av;
    assert(bx);

    assert(!a.too_messy);

    if((divisor%div) == 0) a *= (divisor/div), div = divisor;
    else if((div%divisor) == 0) *bx *= (div/divisor), divisor = div;

    if(divisor != div) a *= divisor, *bx *= div, divisor *= div;

    bx = new mx_boundexpr(bx,new av_boundexpr(&a));
}

operand bexpr::generate_code(if_ops op, block_symtab *scope)
{
    if (debug_bexpr>2) {
	print(stdout);
        printf("  bexpr::generate_code\n");
    }

    block bldr;
    assert(op == io_divceil || op == io_divfloor);
    assert(bx && divisor);

    binary_op bop = (op == io_divceil ? bop_divceil : bop_divfloor);

    operand retop = bx->generate_code(scope, divisor);

    if (divisor != 1) {
      retop = operand(bldr.op(block(retop), bop, 
                            block(divisor)).make_instruction(scope));
    }

    if (debug_bexpr>2) {
        if (retop.is_instr()) retop.instr()->print(stdout);
        printf("  bexpr::generate_code, returning\n");
    }

    return retop;
}

//---------------------------- constructors ----------------------------------

boundexpr::boundexpr()
{
}

static boundexpr *gen_boundexpr_reg(operand r, tree_node *tn)
{
    access_vector av(r, tn, TRUE);
    assert(!av.too_messy);
    boundexpr *b = new av_boundexpr(&av);
    return b;
}

static boundexpr *fastc(instruction *n, tree_node *tn)
{
    if_ops op = n->opcode();

    if (op == io_min || op == io_max) {
        in_rrr *i = (in_rrr *) n;
        boundexpr *l, *r;
        l = i->src1_op().is_instr() ? fastc(i->src1_op().instr(), tn) : 
	                             gen_boundexpr_reg(i->src1_op(), tn);
        r = i->src2_op().is_instr() ? fastc(i->src2_op().instr(), tn) : 
	                             gen_boundexpr_reg(i->src2_op(), tn);
        if (l == NULL || r == NULL) {
            if (l) delete l;
            if (r) delete r;
            return NULL;
        } else {
            if (op == io_min) return new mn_boundexpr(l,r);
            else return new mx_boundexpr(l,r);
        }
    } else if (op == io_cpy) {
        in_rrr *i = (in_rrr *) n;
        return i->src1_op().is_instr() ? fastc(i->src1_op().instr(), tn) : 
	                                gen_boundexpr_reg(i->src1_op(), tn);
    } else {
#ifndef DEAL_WITH_GCC_BRAIN_DAMAGE
        access_vector av(operand(n), n->parent(), TRUE);
#else
//
//  See the comments on gcc brain damage in lptrans.cc.
//
        operand op1(n);
        access_vector av(op1, n->parent(), TRUE);
#endif
        return av.too_messy ? 0 : new av_boundexpr(&av);
    }
}

mn_boundexpr::mn_boundexpr(boundexpr *l,boundexpr *r)
{
    assert(l && r);
    
    boundlist *b = new boundlist(r,0);
    mns = new boundlist(l,b);
}

mx_boundexpr::mx_boundexpr(boundexpr *l,boundexpr *r)
{
    assert(l && r);
    
    boundlist *b = new boundlist(r,0);
    mxs = new boundlist(l,b);
}

av_boundexpr::av_boundexpr(access_vector *a)
{
    av = new access_vector(a);
    assert(!a->too_messy);
}

//--------------------------------- destructors --------------------------

boundexpr::~boundexpr()
{
}

mn_boundexpr::~mn_boundexpr()
{
    boundlist *temp_mns1 = mns;
    boundlist *temp_mns2;

    while (temp_mns1) {
        delete temp_mns1->item;
        temp_mns2 = temp_mns1->link;
        delete temp_mns1;
        temp_mns1 = temp_mns2;
    }
}

mx_boundexpr::~mx_boundexpr()
{
    boundlist *temp_mxs1 = mxs;
    boundlist *temp_mxs2;

    while (temp_mxs1) {
        delete temp_mxs1->item;
        temp_mxs2 = temp_mxs1->link;
        delete temp_mxs1;
        temp_mxs1 = temp_mxs2;
    }
}

av_boundexpr::~av_boundexpr()
{
    delete av;
}

// ------------------------- print -----------------------

void bexpr::print(FILE *f)
{
    if(this == 0) error_line(1,NULL,"bexpr::print() called while null");
    assert(bx);
    bx->print(f);
    fprintf(f," [divisor=%d]",divisor);
}

void boundexpr::print(FILE *)
{
    assert(this);
    error_line(1,NULL,"boundexpr::print() called");
}

void mn_boundexpr::print(FILE *f)
{
    fprintf(f,"(min: ");
    for(boundlist *b = mns; b; b = b->link)
        b->item->print(f);
    fprintf(f,")");
}

void mx_boundexpr::print(FILE *f)
{
    fprintf(f,"(max: ");
    for(boundlist *b = mxs; b; b = b->link)
        b->item->print(f);
    fprintf(f,")");
}

void av_boundexpr::print(FILE *f)
{
    fprintf(f,"("); av->print(f); fprintf(f,")"); 
}

// kind

boundexprkind boundexpr::kind()
{
    error_line(1,NULL,"called boundexpr::kind()");
    return bk_av;	// to shut up the compiler
}

boundexprkind mn_boundexpr::kind()
{
    return bk_mn;
}

boundexprkind mx_boundexpr::kind()
{
    return bk_mx;
}

boundexprkind av_boundexpr::kind()
{
    return bk_av;
}

// gen

operand boundexpr::generate_code(block_symtab *scope, int divisor)
{
    return gen(scope, divisor);
}


operand boundexpr::gen(block_symtab * /* scope */, int /* divisor */)
{
    assert_msg(TRUE, ("called boundexpr::gen()"));
    return operand();
}


operand mn_boundexpr::gen(block_symtab *scope, int divisor)
{
    boundlist *b = mns;
    assert(b);

    if (debug_bexpr>2) {
      print(stdout);
      printf("  mx_boundexpr::gen\n");
    }
    
    if(!b->link) return b->item->gen(scope, divisor);


    binary_op bop = (divisor > 0 ? bop_min : bop_max);

    operand op(b->item->gen(scope, divisor));
    block Curr(op);
    for(b = b->link; b; b = b->link) 
        Curr.set(block::op(Curr, bop, block(b->item->gen(scope, divisor))));

    operand retop = operand(Curr.make_instruction(scope));

    if (debug_bexpr>2) {
       if (retop.is_instr()) retop.instr()->print(stdout);
       printf("  mn_boundexpr::gen returning\n");
    }

    return retop;
}


operand mx_boundexpr::gen(block_symtab *scope, int divisor)
{
    boundlist *b = mxs;
    assert(b);

    if (debug_bexpr>2) {
      print(stdout);
      printf("  mx_boundexpr::gen\n");
    }

    if (!b->link)  return b->item->gen(scope, divisor);

    binary_op bop = (divisor > 0 ? bop_max : bop_min);
    
    operand op(b->item->gen(scope, divisor));
    block Curr(op);
    for(b = b->link; b; b = b->link) {
        Curr.set(block::op(Curr, bop, block(b->item->gen(scope, divisor))));
    }

    operand retop = operand(Curr.make_instruction(scope));

    if (debug_bexpr>2) {
       if (retop.is_instr()) retop.instr()->print(stdout);
       printf("  mx_boundexpr::gen returning\n");
    }
    return retop;
}


operand av_boundexpr::gen(block_symtab *scope, int /* divisor */)
{
    if (debug_bexpr>2) {
      print(stdout);
      printf("  av_boundexpr::gen\n");
    }

    operand retop = av->generate_code(block::get_proc(), scope);

    if (debug_bexpr>2) {
       if (retop.is_instr()) retop.instr()->print(stdout);
       printf("  av_boundexpr::gen returning\n");
    }
    return retop;
}


// ------------------ range ------------------

void boundexpr::range(int *,int *,int *,int *)
{
    assert_msg(TRUE, ("called boundexpr::range()"));
}

void mn_boundexpr::range(int *mn,int *mnok,int *mx,int *mxok)
{
    int nok=0,n=0,xok=0,x=0; int giveupmin=0;
    for(boundlist *b = mns; b; b = b->link) {
        int mmn,mmx,mmnok,mmxok;
        b->item->range(&mmn,&mmnok,&mmx,&mmxok);
        if(mmnok == 0) {giveupmin=1;}
        else if(nok == 0) {nok = mmnok; n = mmn;}
        else {n = (n < mmn)?n:mmn; if(mmnok==2)nok=2;}
        if(mmxok == 0) {}
        else if(xok == 0) {xok = mmxok; x = mmx;}
        else {x = (x < mmx)?x:mmx; if(mmxok==2)xok=2;}
    }
    *mn = n; *mnok = nok; *mx = x; *mxok = xok;
    if(giveupmin) *mnok = 0;
}

void mx_boundexpr::range(int *mn,int *mnok,int *mx,int *mxok)
{
    int nok=0,n=0,xok=0,x=0;int giveupmax=0;
    for(boundlist *b = mxs; b; b = b->link) {
        int mmn,mmx,mmnok,mmxok;
        b->item->range(&mmn,&mmnok,&mmx,&mmxok);
        if(mmnok == 0) {}
        else if(nok == 0) {nok = mmnok; n = mmn;}
        else {n = (n > mmn)?n:mmn; if(mmnok==2)nok=2;}
        if(mmxok == 0) {giveupmax=0;}
        else if(xok == 0) {xok = mmxok; x = mmx;}
        else {x = (x > mmx)?x:mmx; if(mmxok==2)xok=2;}
    }
    *mn = n; *mnok = nok; *mx = x; *mxok = xok;
    if(giveupmax) *mxok = 0;
}

void av_boundexpr::range(int *mn,int *mnok,int *mx,int *mxok)
{
    assert(!av->too_messy);
    access_vector zero;
    av_compare_info ci(av,&zero);
    if(ci.identical_excluding_const()){
        *mn = *mx = av->con; *mnok = *mxok = 1;
    }else
        *mnok = *mxok = 0;
}

// ------------------ av_only, av_min_only, av_max_only ------------------

boolean boundexpr::av_only(){assert_msg(TRUE, ("called boundexpr::av_only()"));return 0;}
boolean av_boundexpr::av_only() {return 1;}
boolean mn_boundexpr::av_only() {return 0;}
boolean mx_boundexpr::av_only() {return 0;}

boolean boundexpr::av_min_only(){assert_msg(TRUE, ("called boundexpr::av_min_only()"));return 0;}
boolean av_boundexpr::av_min_only() {return 1;}
boolean mx_boundexpr::av_min_only() {return 0;}
boolean mn_boundexpr::av_min_only()
{
    for(boundlist *b = mns; b; b = b->link)
        if(!b->item->av_min_only()) return 0;
    return 1;
}

boolean boundexpr::av_max_only(){assert_msg(TRUE, ("called boundexpr::av_max_only()")); return 0;}
boolean av_boundexpr::av_max_only() {return 1;}
boolean mn_boundexpr::av_max_only() {return 0;}
boolean mx_boundexpr::av_max_only()
{
    for(boundlist *b = mxs; b; b = b->link)
        if(!b->item->av_max_only()) return 0;
    return 1;
}


//----------------------------------- simplify ---------------------------

void bexpr::simplify(bizarre_bounds_info *b)
{
    bx = bx->simplify(b, divisor);
    if(divisor != 1 && bx->divisible_by(divisor,0)) {
        assert(divisor);
        bx->divisible_by(divisor,1);
        divisor = 1;
    }
}

enum cm_type {cm_le, cm_ge, cm_eq, cm_cannot_tell};

boundexpr *boundexpr::simplify(bizarre_bounds_info *, int /* divisor */)
{
    assert_msg(TRUE, ("boundexpr::simplify() called"));
    return NULL;
}

boundexpr *av_boundexpr::simplify(bizarre_bounds_info *, int /* divisor */)
{
    return this;
}

cm_type compare(boundexpr *l,boundexpr *r,int divisor,bizarre_bounds_info *b)
{

    if (debug_bexpr>2) {
      printf("Comparing: ");
      l->print(stdout);
      printf(" and ");
      r->print(stdout);
      printf("\n");
    }

    int count = b->loops;
    tree_for **fors = b->loop;
    access_vector *mins = b->lmin;
    access_vector *maxs = b->umax;
    
    assert(l->kind() == bk_av || r->kind() == bk_av);
    access_vector av;

    if (divisor > 0)
      av = *(((av_boundexpr *)l)->av) - *(((av_boundexpr *)r)->av);
    else 
      av = *(((av_boundexpr *)r)->av) - *(((av_boundexpr *)l)->av);

    // check for equality
    //
    access_vector zero;

    if (debug_bexpr>2) {
      printf("Comparing for equal:\n");
      av.print(stdout);        printf("\n");
      zero.print(stdout);      printf("\n");
    }

    if(av_compare_info(&av,&zero).identical()) {
        return cm_eq;
    }
    
    // check for l >= r, avmin >=0
    
    access_vector avmin = av;
    int i;
    for(i=0; i<count; i++) {
        int val = avmin.val(fors[i]);
        avmin.add(fors[i],-val);
        if(val==0)	;
        else if(val>0)	avmin += val * mins[i];
        else		avmin += val * maxs[i];	
    }
    
    int m = avmin.con;
    avmin.con = 0;

    if (debug_bexpr>2) {
      printf("Comparing for greater than:\n");
      avmin.print(stdout);    printf("\n");
      zero.print(stdout);     printf("\n");
    }

    if(m >= 0 && av_compare_info(&avmin,&zero).identical()) {
        return cm_ge;
    }
    
    // check for r >= l, avmax <=0
    
    access_vector avmax = av;
    for(i=0; i<count; i++) {
        int val = avmax.val(fors[i]);
        avmax.add(fors[i],-val);
        if(val==0)	;
        else if(val>0)	avmax += val * maxs[i];
        else		avmax += val * mins[i];	
    }

    m = avmax.con;
    avmax.con = 0;

    if (debug_bexpr>2) {
      printf("Comparing for less than:\n");
      avmax.print(stdout);   printf("\n");
      zero.print(stdout);    printf("\n");
    }

    if(m <= 0 && av_compare_info(&avmax,&zero).identical()) {
        return cm_le;
    }
    
    return cm_cannot_tell;
}

static void combiner(boundlist **mm,boundexprkind bk,int divisor,
		     bizarre_bounds_info *bz)
{
    // put tree of mins or maxs into one list
    
    assert(*mm);

    boundlist *last = *mm;
    for(last = *mm; last->link; last=last->link) {
        last->item = last->item->simplify(bz, divisor);
    }

    boundlist *prev = NULL;
    for(boundlist *b = *mm; b; b=b->link) {
        if(b->item->kind() == bk) {
            if(bk == bk_mn) {
                mn_boundexpr *e = (mn_boundexpr *) b->item;
                assert(e->mns);
                while(e->mns) {
                    boundlist *eb = e->mns;
                    e->mns = eb->link;
                    eb->link = 0;
                    last->link = eb;
                    last = eb;
                }
                delete e;
		if (!prev) *mm = b->link;
		else prev->link = b->link;

            } else if (bk == bk_mx) {
                mx_boundexpr *e = (mx_boundexpr *) b->item;
                assert(e->mxs);
                while(e->mxs) {
                    boundlist *eb = e->mxs;
                    e->mxs = eb->link;
                    eb->link = 0;
                    last->link = eb;
                    last = eb;
                }
                delete e;
		if (!prev) *mm = b->link;
		else prev->link = b->link;

            } else
                assert(0);
	// mrm99: Made this an else
        } else {
		prev = b;
	}
    }
    
    // We compare all pairs.

    boundlist *pbl = 0,*nbl = NULL;
    for(boundlist *bl = *mm; bl; bl = nbl) {
        nbl = bl->link;
        if(bl->item->kind() != bk_av)
            continue;
        boundlist *pbr = bl,*nbr = NULL;
        for(boundlist *br = bl->link; br; br = nbr) {
            nbr = br->link;
            if(bl->item->kind() != bk_av)
                continue;
            cm_type ct = compare(bl->item,br->item,divisor,bz);
            int discardl = (ct == cm_le || ct == cm_eq);
            int discardr = ct == cm_ge;
            if(bk == bk_mn) {int h=discardl;discardl=discardr;discardr=h;}
            if(discardl) {
                if(pbl) pbl->link = nbl;
                else *mm = nbl;
                delete bl;
                continue;
            }
            if(discardr) {
                pbr->link = nbr;
                delete br;
		// mrm99
		if ( nbl == br )
			nbl = nbr;
            } else
                pbr = br;
        }
        pbl = bl;
    }
    
    assert(*mm);
}

boundexpr *mn_boundexpr::simplify(bizarre_bounds_info *bz, int divisor)
{
    combiner(&mns,bk_mn,divisor,bz);
    
    boundexpr *rx = 0;
    if(mns->link == 0) {
        rx = mns->item;
        delete mns;
        mns = 0;
        delete this;
    } else rx = this;
    
    return rx;
}
boundexpr *mx_boundexpr::simplify(bizarre_bounds_info *bz, int divisor)
{
    combiner(&mxs,bk_mx,divisor,bz);
    
    boundexpr *rx;
    if(mxs->link == 0) {
        rx = mxs->item;
        delete mxs;
        mxs = 0;
        delete this;
    } else
        rx = this;
    
    return rx;
}

// -------------------------------------- divisible by -----------------------

boolean boundexpr::divisible_by(int,int)
{
    assert_msg(TRUE, ("called boundexpr::divisible_by()"));
    return FALSE;  // shut up compiler
}

boolean mn_boundexpr::divisible_by(int div,int doit)
{
    for(boundlist *b = mns; b; b = b->link) {
        if(b->item->divisible_by(div,doit) == 0)
            return 0;
    }
    return 1;
}

boolean mx_boundexpr::divisible_by(int div,int doit)
{
    for(boundlist *b = mxs; b; b = b->link) {
        if(b->item->divisible_by(div,doit) == 0)
            return 0;
    }
    return 1;
}

boolean av_boundexpr::divisible_by(int div,int doit)
{
    assert(!av->too_messy);
    if(doit) {
        *av /= div;
        assert(!av->too_messy);
        return 1;
    } else {
        access_vector tmp(av);
        tmp /= div;
        return tmp.too_messy == 0;
    }
}

// -------------------------------------- replace ----------------------------

void bexpr::replace(tree_for *a,access_vector *av)
{
    bx->replace(a,av);
}

void boundexpr::replace(tree_for *,access_vector *)
{
    assert_msg(TRUE, ("called boundexpr::operator *=()"));
}


void mn_boundexpr::replace(tree_for *a,access_vector *av)
{
    for(boundlist *b = mns; b; b = b->link) {
        b->item->replace(a,av);
    }
}

void mx_boundexpr::replace(tree_for *a,access_vector *av)
{
    for(boundlist *b = mxs; b; b = b->link) {
        b->item->replace(a,av);
    }
}

void av_boundexpr::replace(tree_for *a,access_vector *avx)
{
    assert(av->val(a->index(),0) == 0);
    int val = av->val(a);
    if(val != 0) {
        av->add(a,-val);
        *av += val * *avx;
    }
}

// ---------------------------- operator *= ----------------------------

void bexpr::operator *=(int s)
{
    assert(s != 0);
    if((s%divisor) == 0) {divisor = 1; *bx *= s/divisor;}
    else if((divisor%s) == 0) divisor /= s;
    else *bx *= s;
}

void boundexpr::operator *=(int)
{
    error_line(1,NULL,"called boundexpr::operator *=()");
}

void mn_boundexpr::operator *=(int s)
{
    for(boundlist *b = mns; b; b = b->link) {
        *b->item *= s;
    }
}

void mx_boundexpr::operator *=(int s)
{
    for(boundlist *b = mxs; b; b = b->link) {
        *b->item *= s;
    }
}

void av_boundexpr::operator *=(int s)
{
    *av *= s;
}

// ---------------------------- operator += ----------------------------

void bexpr::operator +=(access_vector &a)
{
    assert(divisor == 1);
    *bx += a;
}

void boundexpr::operator +=(access_vector &)
{
    assert_msg(TRUE, ("called boundexpr::operator +=()"));
}

void mn_boundexpr::operator +=(access_vector &a)
{
    for(boundlist *b = mns; b; b = b->link) {
        *b->item += a;
    }
}

void mx_boundexpr::operator +=(access_vector &a)
{
    for(boundlist *b = mxs; b; b = b->link) {
        *b->item += a;
    }
}

void av_boundexpr::operator +=(access_vector &a)
{
    *av += a;
}

// ---------------------------- duplicate ----------------------------

bexpr *bexpr::duplicate()
{
    bexpr *b = new bexpr;
    b->divisor = divisor;
    b->bx = bx->duplicate();
    return b;
}

boundexpr *boundexpr::duplicate()
{
    assert_msg(TRUE, ("called boundexpr::duplicate()"));
    return 0;
}

boundexpr *mn_boundexpr::duplicate()
{
    mn_boundexpr *newthis = new mn_boundexpr;
    for(boundlist *b = mns; b; b = b->link) {
        boundlist *bb = new boundlist(b->item->duplicate(),newthis->mns);
        newthis->mns = bb;
    }
    return newthis;
}

boundexpr *mx_boundexpr::duplicate()
{
    mx_boundexpr *newthis = new mx_boundexpr;
    for(boundlist *b = mxs; b; b = b->link) {
        boundlist *bb = new boundlist(b->item->duplicate(),newthis->mxs);
        newthis->mxs = bb;
    }
    return newthis;
}

boundexpr *av_boundexpr::duplicate()
{
    return new av_boundexpr(av);
}

// -------------------------------------- mono ----------------------------

mn_type bexpr::mono(tree_for *a,int crash_on_bad)
{
    mn_type m = bx->mono(a,crash_on_bad);
    if(divisor < 0) {
        if(m == mn_dec) m = mn_inc;
        else if(m == mn_inc) m = mn_dec;
    }
    return m;
}

static mn_type both(mn_type x,mn_type y)
{
    mn_type rval;
    if(x == mn_independent) rval = y;
    else if(y == mn_independent) rval = x;
    else if(x == y) rval = x;
    else if(x == mn_mess || y == mn_mess) rval = mn_mess;
    else rval = mn_linear;	/* two of inc, dec, and linear */
    return rval;
}

static mn_type mok(int op,int chk,mn_type val)
{
    if(chk && val == mn_mess) {
        assert_msg(TRUE, ("mono messy, op = %d",op));
    }
    return val;
}

mn_type boundexpr::mono(tree_for *,int)
{
    assert_msg(TRUE, ("called boundexpr::mono()"));
    return mn_mess;	// shut up compiler
}

mn_type mn_boundexpr::mono(tree_for *a,int chk)
{
    mn_type t = mn_independent;
    for(boundlist *b = mns; b; b = b->link) {
        t = both(t,b->item->mono(a,chk));
    }
    return mok(io_min,chk,t);
}

mn_type mx_boundexpr::mono(tree_for *a,int chk)
{
    mn_type t = mn_independent;
    for(boundlist *b = mxs; b; b = b->link) {
        t = both(t,b->item->mono(a,chk));
    }
    return mok(io_max,chk,t);
}

mn_type av_boundexpr::mono(tree_for *a,int)
{
    assert(av->val(a->index(),0) == 0);
    int val = av->val(a);
    if(val == 0) return mn_independent;
    else if(val > 0) return mn_inc;
    else return mn_dec;
}

//---------------- bizarre bounds info -------------------------------

void bizarre_bounds_info::operator = (bizarre_bounds_info &b)
{
    loops = b.loops;
    equations = b.equations;
    useless = b.useless;
    loop = b.loop;
    int i;
    for(i=0; i<loops; i++)
        lmin[i] = b.lmin[i],umax[i] = b.umax[i];
    for(i=0; i<equations; i++) {
        *(eq[i].rhs) = *(b.eq[i].rhs);
        for(int j=0; j<loops; j++)
            eq[i].coeff[j] = b.eq[i].coeff[j];
    }
}

void bizarre_bounds_info::insbnd(int lb,int i,equation *eq,access_vector *av,int divisor)
{
    if(divisor<0) {
        divisor = -divisor;
        *av = -*av;
    }
    access_vector a = access_vector(*av);
    int j;
    for(j = 0; j < i; j++) {
        int v = a.val(loop[j]);
        eq->coeff[j] = lb ? -v : v;
        a.add(loop[j],-v);
    }
    eq->coeff[i] = lb ? divisor : -divisor;
    for(j = i+1; j < loops; j++) {
        eq->coeff[j] = 0;
    }
    a.mod_this = 0;
    if(lb) *(eq->rhs) = a;
    else *(eq->rhs) = -a;
}


bizarre_bounds_info::bizarre_bounds_info(tree_for **f,int c)
{
    loops = c;
    equations = 2*c;
    loop = f;
    useless = 0;

    // each equation: a1i1+...+anin >= rhs

    int i;
    for(i = 0; i < loops; i++) {
        bexpr *lbexpr = (bexpr *) loop[i]->peek_annote(k_lbexpr);
        bexpr *ubexpr = (bexpr *) loop[i]->peek_annote(k_ubexpr);

	assert(lbexpr->bx && ubexpr->bx);

        if(!lbexpr->bx->av_max_only()) useless++;
        if(!ubexpr->bx->av_min_only()) useless++;

        // 1st lb equation at position i, 1st ub at position i+loops,
        // the rest follow.  This is for no reason other than below,
        // where the lmin and umax are calculated discarding non-firsts.

        bexpr *bxpr = lbexpr;
        if(bxpr->bx->kind() == bk_av) {
            insbnd(1,i,&eq[i],((av_boundexpr*)bxpr->bx)->av,bxpr->divisor);
        } else {
            int cnt = 0;
            assert(bxpr->bx->kind() == bk_mx);
            for(boundlist *b=((mx_boundexpr *)bxpr->bx)->mxs; b; b = b->link) {
                assert(b->item->kind() == bk_av);
                if(!cnt++)
                    insbnd(1,i,&eq[i],((av_boundexpr*)b->item)->av,bxpr->divisor);
                else
                    insbnd(1,i,&eq[equations++],((av_boundexpr*)b->item)->av,bxpr->divisor);
            }
        }

        bxpr = ubexpr;
        if(bxpr->bx->kind() == bk_av) {
            insbnd(0,i,&eq[i+loops],((av_boundexpr*)bxpr->bx)->av,bxpr->divisor);
        } else {
            int cnt = 0;
            assert(bxpr->bx->kind() == bk_mn);
            for(boundlist *b=((mn_boundexpr *)bxpr->bx)->mns; b; b = b->link) {
                assert(b->item->kind() == bk_av);
                if(!cnt++)
                    insbnd(0,i,&eq[i+loops],((av_boundexpr*)b->item)->av,bxpr->divisor);
                else
                    insbnd(0,i,&eq[equations++],((av_boundexpr*)b->item)->av,bxpr->divisor);
            }
	  }
      }

    // conservatively figure the min lower and max upper bounds.
    // making a min too small or a max too large is ok.
    
    for(i=0; i<loops; i++) {
        lmin[i] = *(eq[i].rhs);
        umax[i] = -(*(eq[i+loops].rhs));
        for(int j=0; j<i; j++) {
            int v = -eq[i].coeff[j]; // coeff has been negated
	    if(v > 0) lmin[i] += v*lmin[j];
	    else if(v < 0) lmin[i] += v*umax[j];
            
            v = eq[i+loops].coeff[j]; // coeff hasn't been negated
            if(v > 0) umax[i] += v*umax[j];
            else if(v < 0) umax[i] += v*lmin[j];
        }
    }
}

void equation::print(FILE *f,int cnt)
{
    int i;
    for(i=0; i<cnt; i++) {
        fprintf(f,"%d\t",coeff[i]);
    }
    fprintf(f," >= ");
    rhs->print(f);
    fprintf(f,"\n");
}

void bizarre_bounds_info::print(FILE *f)
{
    fprintf(f,"BIZARRE BOUNDS INFO:");
    if(useless) {
        fprintf(f,"\tUSELESS\n");
        return;
    }
    fprintf(f,"\nLOOP\tMIN\tMAX\n");
    int i;
    for(i=0; i<loops; i++) {
        fprintf(f,"%s: ",loop[i]->index()->name());
        fprintf(f,"\t"); lmin[i].print(f);
        fprintf(f,"\t"); umax[i].print(f);
        fprintf(f,"\n");
    }
    fprintf(f,"EQUATIONS:\n");
    for(i=0; i<equations; i++) {
        for(int j=0; j<loops; j++)
            fprintf(f," %d",eq[i].coeff[j]);
        fprintf(f," >= ");
        eq[i].rhs->print(stdout);
        fprintf(f,"\n");
    }
    fflush(f);
}
