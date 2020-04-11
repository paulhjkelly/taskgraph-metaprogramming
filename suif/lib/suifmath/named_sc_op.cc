// This may look like C code, but it is really -*- C++ -*-

// This may look like C code, but it is really -*- C++ -*-

// This may look like C code, but it is really -*- C++ -*-

/* file "named_sc_op.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include "suifmath.h"

#define DO_CHECK



/***************************************************************************
 ***************************************************************************
 ****   Column Operations                                               ****
 ***************************************************************************
 ***************************************************************************/

void named_symcoeff_ineq::swap_col(int i, int j)
{
    name_table_entry tmp(cols()[i]);
    cols()[i].init(cols()[j]);
    cols()[j].init(tmp);

    if (m() > 0) {
        for (int c=0; c<p(); c++)
            L[c] = L[c].swap_col(i, j);
    }
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::add_col(immed im, int i)
{
    name_table_entry * nte = new name_table_entry(im);
    add_col(*nte, i);
}

void named_symcoeff_ineq::add_col(const name_table_entry & nte, int i)
{

    int xn = n();
    assert((i>=0)||(i<=xn));

    if(m() == 0) {
	for(int x=0; x<p(); x++)
	    (*this)[x].init(0, xn+1);
    } else {
	nim_op O;
	int x;
	if(i == n())
	    for(x=0; x<p(); x++)
		(*this)[x].init(Compose(1, 2,
		    O.NIM((*this)[x]),
		    O.NIM((*this)[x].m(), 1)));
	else
	    for(x=0; x<p(); x++)
		(*this)[x].init(Compose(1, 3,
		    O.NIM((*this)[x].resize(0, (*this)[x].m(),
			0, i)),
		    O.NIM((*this)[x].m(), 1),
		    O.NIM((*this)[x].resize(0, (*this)[x].m(),
			i, (*this)[x].n()))));
    }


    cols().insert(nte, i);
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::del_col(int i, int j)
{
    integer_row r(n());
    r = 0;
    for(int x=i; x<=j; x++) r[x] = 1;
    del_col(r);
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::del_col(integer_row & row)
{
    assert(row.n() == n());
    assert(row[0]==0);

    int nc = 1;
    int i;
    for(i=1; i<n(); i++)
        if(row[i]==0) nc++;

    name_table newcol(nc);
    int curr = 1;
    for(i=1; i<n(); i++)
        if(row[i]==0)
            newcol[curr++] = cols()[i];


    int oldn = n();
    int oldm = m();
    lin_ineq * oldL = L;
    L = NULL;
    cols().init(newcol);
    initL(oldm);

    for(int ip=0; ip<p(); ip++)
        for(int im=0; im<m(); im++) {
            int curr = 0;
            for(int in=0; in<oldn; in++)
                if(row[in]==0)
                    (*this)[ip][im][curr++] = oldL[ip][im][in];
        }

    delete[] oldL;
#ifdef DO_CHECK
    check();
#endif
}



/***************************************************************************
 ***************************************************************************
 ****   Plain Operations                                                ****
 ***************************************************************************
 ***************************************************************************/

void named_symcoeff_ineq::add_pln(immed sym, int i)
{
    name_table_entry * nte = new name_table_entry(sym);
    add_pln(*nte, i);
}

void named_symcoeff_ineq::add_pln(name_table_entry & nte, int i)
{
    int oldm = m();
    lin_ineq * oldL = L;
    L = NULL;
    planes().insert(nte, i);
    initL(oldm);

    int ip;
    for(ip=0; ip<i; ip++)
        (*this)[ip] = oldL[ip];

    for(ip=i+1; ip<p(); ip++)
        (*this)[ip] = oldL[ip-1];
    delete[] oldL;
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::del_pln(int /* i */, int /* j */)
{
    assert(0);
}

void named_symcoeff_ineq::del_pln(integer_row & row)
{
    assert(row.n() == p());
    assert(row[0]==0);

    int np = 1;
    int i;
    for(i=1; i<p(); i++)
        if(row[i]==0) np++;

    name_table newpln(np);
    int curr = 1;
    for(i=1; i<p(); i++)
        if(row[i]==0)
            newpln[curr++] = planes()[i];


    int oldm = m();
    int oldp = p();
    lin_ineq * oldL = L;
    L = NULL;
    planes().init(newpln);
    initL(oldm);
    curr = 0;
    for(int ip=0; ip<oldp; ip++)
        if(row[ip]==0)
            (*this)[curr++].init(oldL[ip]);

    delete[] oldL;
#ifdef DO_CHECK
    check();
#endif
}



/***************************************************************************
 ***************************************************************************
 ****   Inequality Operations                                           ****
 ***************************************************************************
 ***************************************************************************/

void named_symcoeff_ineq::add_ineq(int i)
{
    nim_op O;
    int x;
    if(m() == 0) {
        assert((i==0)||(i==1));
        for(x=0; x<p(); x++)
            (*this)[x].init(1, n());
    } else if(i == 0)
        for(x=0; x<p(); x++)
            (*this)[x].init(Compose(2, 1,
		O.NIM(1, (*this).n()),
		O.NIM((*this)[x])));
    else if(i == m())
        for(x=0; x<p(); x++)
            (*this)[x].init(Compose(2, 1,
		O.NIM((*this)[x]),
		O.NIM(1, (*this)[x].n())));
    else
        for(x=0; x<p(); x++)
            (*this)[x].init(Compose(3, 1,
		O.NIM((*this)[x].resize(0, i,
		    0, (*this)[x].n())),
		O.NIM(1, (*this)[x].n()),
		O.NIM((*this)[x].resize(i, (*this)[x].m(),
		    0, (*this)[x].n()))));
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::del_ineq(int i, int j)
{
    integer_row r(m());
    r = 0;
    for(int x=i; x<=j; x++) r[x] = 1;
    del_ineq(r);
}

void named_symcoeff_ineq::del_ineq(integer_row & row)
{
    assert(row.n() == m());

    int nm = 0;
    for(int i=0; i<m(); i++)
        if(row[i]==0) nm++;


    lin_ineq * oldL = L;
    L = NULL;
    initL(nm);

    int curr = 0;
    for(int im=0; im<m(); im++)
        if(row[im]==0) {
            for(int ip=0; ip<p(); ip++)
                for(int in=0; in<n(); in++)
                    (*this)[ip][curr][in] = oldL[ip][im][in];
            curr++;
        }

    delete[] oldL;
#ifdef DO_CHECK
    check();
#endif
}


/***************************************************************************
 * Assignment                                                              *
 ***************************************************************************/
named_symcoeff_ineq &
named_symcoeff_ineq::operator=(const named_symcoeff_ineq & c)
{
    if(this == &c) return *this;
    init(c);
    return *this;
}


/***************************************************************************
 * Projection                                                              *
 ***************************************************************************/
void named_symcoeff_ineq::project_away(immed im)
{
    named_sc_fm FM(this);
    int x = cols().find(im);
    assert(x>0);
    FM.fm_bounds(x,x+1);
    init(FM.internal_get());
#ifdef DO_CHECK
    check();
#endif
}

void named_symcoeff_ineq::project_away(name_table &)
{
    assert(0);
}

// ret(??, m, ??) += ex@(ip, 0, in) * ret@(sp, m, sn)
static instruction * do_substitute(instruction * in, var_sym * sym, instruction * expr, base_symtab * st, boolean first_in = TRUE)
{
    assert(in);
    if(first_in) in = in->clone(st);

    if(in->opcode() == io_ldc)
	if(((in_ldc *)in)->value() == immed(sym))
	    return expr->clone(st);

    for(unsigned i=0; i < in->num_srcs(); i++) {
	operand op(in->src_op(i));
	if(op.is_symbol()) {
	    if(op.symbol() == sym)
		in->set_src_op(i, operand(expr->clone(st)));
	} else if(op.is_instr()) {
	    instruction * cout = do_substitute(op.instr(), sym, expr, st, FALSE);
	    if(cout != op.instr())
		in->set_src_op(i, operand(cout));
	}
    }

    return in;
}


/**************************************************************************
 * a simple substitution method                                           *
 * This is a cheap alternative to expensive Fourier-Motzkin               *
 * substitution expression given in expr is of the form                   *
 * var >= sub_expr and var <= sub_expr or                                 *
 * sub_expr >= 0                                                          *
 * BUGBUG: This implementation is a gros hack.  Trying to do the          *
 * substitution involves dealing with too many cases. Since convertion    *
 * of expression trees to named_symcoefff_ineq already is doing most of   *
 * the cases, I am using that for now.  But this is ugly and inefficient  *
 * and should be rewritten.                                               *
 **************************************************************************/
named_symcoeff_ineq * named_symcoeff_ineq::substitute(immed var, named_symcoeff_ineq & expr)
{
    assert(var.is_symbol());
    assert(var.symbol()->is_var());

    int cpos = cols().find(var);
    int ppos = planes().find(var);
    if((cpos <= 0)&&(ppos <= 0))
	return new named_symcoeff_ineq(this);
    assert((cpos <= 0)||(ppos <= 0));

    named_symcoeff_ineq myex;
    if(expr.m() == 1) {
	assert_msg(expr.cols().find(var) == -1, ("Variable cannot be a part of the sub_expr in this form"));
	assert_msg(expr.planes().find(var) == -1, ("Variable cannot be a part of the sub_expr in this form"));
	myex = expr;
    } else {
	int pos = expr.cols().find(var);
	assert_msg(pos > 0, ("variable is not in the expression"));
	int ip;
	for(ip=0; ip<expr.p(); ip++)
	    for(int im=0; im<expr.n(); im++)
		assert_msg(expr[ip][0][im] + expr[ip][1][im] == 0, ("expr is not a equality given by two inequalities"));
	assert_msg(ABS(expr[0][0][pos]) == 1, ("Expr is not v = sub_expr, but %d*v = sub_expr", ABS(expr[0][0][pos])));
	for(ip=1; ip<expr.p(); ip++)
	    assert_msg(expr[ip][0][pos] == 0, ("Expr is not v = sub_expr, but %s*v = sub_expr", expr.planes()[ip].string()));

	lin_ineq * l;
	if(expr[0][0][pos] > 0)
	    l = expr.get_m(1);
	else
	    l = expr.get_m(0);
	myex.init(expr.p(), 1, expr.n());
	myex.cols().init(expr.cols());
	myex.planes().init(expr.planes());
	myex.set_m(0, l);
	delete l;

	myex.del_col(pos);
    }

    base_symtab * bt = NULL;
    bt = cols().get_symtab(bt);
    bt = planes().get_symtab(bt);
    bt = myex.cols().get_symtab(bt);
    bt = myex.planes().get_symtab(bt);

    instruction * inex = myex.create_expression();
    named_symcoeff_ineq ret;
    for(int i=0; i<m(); i++) {
	named_symcoeff_ineq curr;
	curr.init(p(), 1, n());
	curr.cols().init(cols());
	curr.planes().init(planes());
	lin_ineq * t = get_m(i);
	curr.set_m(i, t);
	delete t;

	instruction * incurr = curr.create_expression();
	instruction * insub = do_substitute(incurr, (var_sym *)var.symbol(), inex, bt);

	named_symcoeff_ineq * rep = convert_exp(insub);
	if(rep == NULL)
	    return NULL;

	ret || *rep;
	assert(rep->m() == 1);
	t = rep->get_m(0);
	ret.add_ineq(0);
	ret.set_m(0, t);
	delete t;
	delete rep;
	delete incurr;
    }

    return new named_symcoeff_ineq(ret);
}



void named_symcoeff_ineq::move_col2plane(name_table_entry & nte, boolean * too_messy)
{
    int c = cols().find(nte);
    assert_msg(c > 0, ("not a constant or variable"));

    assert_msg(planes().find(nte) == -1,
	("Error same variable in both"
	 " plane and column"));

    int xm;
    int xp;
    for(xp=1; xp<p(); xp++)
        for(xm=0; xm<m(); xm++) {
            if((*this)[xp][xm][c]) {
                if(too_messy) {
                    (*too_messy)++;
                    return;
                } else
                    assert_msg(0, ("Will become too messy because of (%d,%d,%d)",
			xp, xm, c));
            }
        }

    int ap = p();
    int am = m();
    int an = n();
    lin_ineq * AL = L;
    L = NULL;
    planes().insert(cols()[c], ap);
    initL(am);
    for(xp=0; xp<ap; xp++)
        for(xm=0; xm<am; xm++)
            for(int xn=0; xn<an; xn++)
                (*this)[xp][xm][xn] = AL[xp][xm][xn];

    delete[] AL;

    for(xm=0; xm<am; xm++)
        (*this)[ap][xm][0] = (*this)[0][xm][c];

    del_col(c);
#ifdef DO_CHECK
    check();
#endif
}


void align_symcoeff(named_symcoeff_ineq & use,
                    named_symcoeff_ineq & update,
                    boolean * too_messy = NULL)
{
    for(int i=1; i<use.planes().n(); i++)
        if(update.cols().find(use.planes()[i]) > 0)
            update.move_col2plane(use.planes()[i], too_messy);
}


/***************************************************************************
 * Align two different sets of inequalities such that n-th  column of A    *
 * and B refers to the same variable.                                      *
 ***************************************************************************/
void align(named_symcoeff_ineq & A,
           named_symcoeff_ineq & B)
{

    align_symcoeff(A, B);
    align_symcoeff(B, A);

    name_table & ca = A.cols();
    name_table & cb = B.cols();
    integer_row ica(ca.n());
    integer_row icb(cb.n());
    name_table * newcol = name_table::align_tables(ca, cb, ica, icb);

    name_table & pa = A.planes();
    name_table & pb = B.planes();
    integer_row ipa(pa.n());
    integer_row ipb(pb.n());
    name_table * newpln = name_table::align_tables(pa, pb, ipa, ipb);


    int ap = A.p();
    int am = A.m();
    int an = A.n();
    lin_ineq * AL = A.L;
    A.L = NULL;
    A.cols().init(newcol);
    A.planes().init(newpln);
    A.initL(am);
    int p;
    for(p=0; p<ap; p++)
        for(int m1=0; m1<am; m1++)
            for(int n1=0; n1<an; n1++)
                A[ipa[p]][m1][ica[n1]] = AL[p][m1][n1];
    delete[] AL;

    int bp = B.p();
    int bm = B.m();
    int bn = B.n();
    lin_ineq * BL = B.L;
    B.L = NULL;
    B.cols().init(newcol);
    B.planes().init(newpln);
    B.initL(bm);
    for(p=0; p<bp; p++)
        for(int m2=0; m2<bm; m2++)
            for(int n2=0; n2<bn; n2++)
                B[ipb[p]][m2][icb[n2]] = BL[p][m2][n2];
    delete[] BL;

    delete newcol;
    delete newpln;
}

void  named_symcoeff_ineq::nt_align(name_table * col, name_table * pln)
{
    if(col)
        if(!name_table::is_aligned(*col, cols())) {
	    name_table * nt = name_table::mk_align(cols(), *col);
	    assert(nt);
            for(int ip=0; ip<p(); ip++) {
                named_lin_ineq tmp(cols(), (*this)[ip]);
                tmp.align(*nt);
                (*this)[ip] = tmp.ineqs();
            }
            cols().init(*nt);
        }


    if(pln)
        if(!name_table::is_aligned(*pln, planes())) {
            assert_msg(0, ("To be implemented"));
        }
#ifdef DO_CHECK
    check();
#endif

}


named_symcoeff_ineq named_symcoeff_ineq::inverse_all()
{
    named_symcoeff_ineq * ret = new named_symcoeff_ineq(this);
    for(int ip=0; ip<p(); ip++)
        (*ret)[ip] *= -1;
    for(int im=0; im<m(); im++)
        (*ret)[0][im][0] += -1;

    return *ret;
}



/***************************************************************************
 * Concatinate two sets of inequalities.                                   *
 ***************************************************************************/
named_symcoeff_ineq
named_symcoeff_ineq::operator&(const named_symcoeff_ineq & c)
{
    named_symcoeff_ineq A(this);
    named_symcoeff_ineq B(c);
    align(A, B);
    named_symcoeff_ineq * ret = new named_symcoeff_ineq;
    ret->cols().init(A.cols());
    ret->planes().init(A.planes());
    ret->initL(A.m()+B.m());

    for(int p=0; p<A.p(); p++) {
        nim_op O;
        (*ret)[p] = Compose(2, 1,
	    O.NIM(A[p]),
	    O.NIM(B[p]));
    }

    return *ret;
}


/***************************************************************************
 * Add the set of inequalities to self.                                    *
 ***************************************************************************/
named_symcoeff_ineq &
named_symcoeff_ineq::operator&=(const named_symcoeff_ineq & c)
{
    named_symcoeff_ineq A(this);
    named_symcoeff_ineq B(c);
    align(A, B);
    cols().init(A.cols());
    planes().init(A.planes());
    initL(A.m()+B.m());

    for(int p=0; p<A.p(); p++) {
        nim_op O;
        (*this)[p] = Compose(2, 1,
                             O.NIM(A[p]),
                             O.NIM(B[p]));
    }

    return *this;
}


/***************************************************************************
 * Concatinate two sets of inequalities. Either or both sets can be empty. *
 * If del flag is set, after calling this treat that ineq as gone.         *
 ***************************************************************************/
named_symcoeff_ineq * named_symcoeff_ineq::opand(named_symcoeff_ineq * c1,
                                             named_symcoeff_ineq * c2,
                                             boolean del1,
                                             boolean del2)
{
    if((c1 == NULL)&&(c2 == NULL))
        return NULL;
    else if(c1 == NULL)
        return (del2)?c2:(new named_symcoeff_ineq(c2));
    else if(c2 == NULL)
        return (del1)?c1:(new named_symcoeff_ineq(c1));
    else if(del1) {
        *c1 &= *c2;
        if(del2) delete c2;
        return c1;
    } else if(del2) {
        *c2 &= *c1;
        return c2;
    } else {
        named_symcoeff_ineq * r = new named_symcoeff_ineq(*c1 & *c2);
        return r;
    }
}


void named_symcoeff_ineq::operator||(named_symcoeff_ineq & c)
{
    align(*this, c);
}



boolean named_symcoeff_ineq::is_same(named_symcoeff_ineq & c)
{

    if(p() != c.p()) return FALSE;
    if(m() != c.m()) return FALSE;
    if(n() != c.n()) return FALSE;
    if(!name_table::is_aligned(planes(), c.planes())) return FALSE;
    if(!name_table::is_aligned(cols(), c.cols())) return FALSE;

    for(int i=0; i<p(); i++)
	for(int j=0; j<m(); j++)
	    for(int k=0; k<n(); k++)
		if(c[i][j][k] != (*this)[i][j][k]) return FALSE;

    return TRUE;
}


boolean named_symcoeff_ineq::operator==(named_symcoeff_ineq &)
{
    assert(0);
    return TRUE;
}

boolean named_symcoeff_ineq::operator>>(named_symcoeff_ineq &)
{
    assert(0);
    return FALSE;
}

boolean named_symcoeff_ineq::operator~()
{
      named_sc_fm fm(this, FALSE);
    boolean valid = fm.fm_step(0, n());
    return (!valid);
}



/***************************************************************************
 ***************************************************************************
 ****   Filter functions                                                ****
 ***************************************************************************
 ***************************************************************************/

named_symcoeff_ineq named_symcoeff_ineq::filter_thru(constraint * column_kernal,
                                                   constraint * plane_kernal,
                                                   int sign) const
{
    named_symcoeff_ineq ret = filter(column_kernal, plane_kernal, sign, 0);
    return ret;
}


named_symcoeff_ineq named_symcoeff_ineq::filter_away(constraint * column_kernal,
                                                   constraint * plane_kernal,
                                                   int sign) const
{
    named_symcoeff_ineq ret = filter(column_kernal, plane_kernal, sign, 1);
    return ret;
}



named_symcoeff_ineq named_symcoeff_ineq::filter(constraint * column_kernal,
                                              constraint * plane_kernal,
                                              int sign, int tora) const
{
    if(m()==0) {
        return *this;
    }

    int xtora = (tora)?0:1;
    assert(column_kernal || plane_kernal);
    int * filt = new int[m()];
    for(int i=0; i<m(); i++) filt[i] = tora;

    if(column_kernal) {
        for(int ip=0; ip<p(); ip++) {
            const lin_ineq & leq = r(ip);
            for(int im=0; im<m(); im++)
                for(int in=0; in<n(); in++)
                    if((*column_kernal)[in]) {
                        int v = leq.r(im).c(in);
                        if((sign*v > 0)||((sign==0)&&v))
                            filt[im] = xtora;
                    }
        }
    }


    if(plane_kernal) {
        for(int ip=0; ip<p(); ip++)
            if((*plane_kernal)[ip]) {
                const lin_ineq & leq = r(ip);
                for(int im=0; im<m(); im++)
                    for(int in=0; in<n(); in++) {
                        int v = leq.r(im).c(in);
                        if((sign*v > 0)||((sign==0)&&v))
                            filt[im] = xtora;
                    }
            }
    }

    named_symcoeff_ineq * ret =  filter(filt);
    delete filt;
    named_symcoeff_ineq retcopy(ret);
    delete ret;
    return retcopy;
}

named_lin_ineq * named_symcoeff_ineq::nli() const
{
    if(p() != 1) return NULL;
    lin_ineq * lq = get_p(0);
    named_lin_ineq * ret = new named_lin_ineq(const_cols(), *lq);
    delete lq;
    return ret;
}



/***************************************************************************
 * Perform the filtering of inequalities according to filt. If filt[i]     *
 * then, i-th inequality is in the result.                                 *
 ***************************************************************************/
named_symcoeff_ineq * named_symcoeff_ineq::filter(int * filt) const
{
    int cnt = 0;
    int i;
    for(i=0; i<m(); i++)
        if(filt[i]) cnt++;

    named_symcoeff_ineq * ret  = new named_symcoeff_ineq();
    ret->init(p(), cnt, n());
    ret->planes().init(const_planes());
    ret->cols().init(const_cols());

    int xm=0;
    for(i=0; i<m(); i++)
        if(filt[i]) {
            for(int xp = 0; xp < p(); xp++)
                for(int xn = 0; xn < n(); xn++)
                    (*ret)[xp][xm][xn] = r(xp).r(i).c(xn);
            xm++;
        }
    assert(xm == cnt);

    return ret;
}



void named_symcoeff_ineq::cleanup()
{
    if((m() == 0)||(n() == 0)||(p() == 0)) return;
    integer_row unused_pln(p());
    integer_row unused_col(n());
    integer_row unused_ineq(m());
    unused_pln = 1;
    unused_col = 1;
    unused_ineq = 1;
    unused_col[0] = 0;
    unused_pln[0] = 0;

    for(int ip=0; ip<p(); ip++)
	for(int im=0; im<m(); im++)
	    for(int in=0; in<n(); in++)
                if((*this)[ip][im][in]) {
                    unused_pln[ip] = 0;
                    unused_col[in] = 0;
                    unused_ineq[im] = 0;
                }

    del_pln(unused_pln);
    del_ineq(unused_ineq);
    del_col(unused_col);
#ifdef DO_CHECK
    check();
#endif

}



/***************************************************************************
 ***************************************************************************
 ***
 *** Creating a named_symcoeff_ieq out of an expression tree
 ***
 ***************************************************************************
 ***************************************************************************/
static int is_const(operand op, int *val)
{
    if(!op.is_instr()) return 0;
    instruction * in = op.instr();
    if(!in) return 0;
    if(in->opcode() == io_cvt)
        return is_const(((in_rrr *)in)->src1_op(), val);
    if(in->opcode() != io_ldc) return 0;
    immed ic = ((in_ldc *) in)->value();
    if(ic.kind() == im_int) *val = ic.integer();
    return (ic.kind() == im_int);
}

static tree_for * is_index(var_sym * v, tree_node *tn)
{
    assert(v);
    for(tree_node *n = tn; n; n = n->parent()->parent()) {
        if(n->kind() == TREE_FOR &&
           ((tree_for *)n)->index() == v)
            return (tree_for *) n;
        if(n->parent()==NULL) break;
    }
    return NULL;
}



boolean is_index_var_in(operand op, tree_node * par)
{
    if(op.is_instr())
        for(unsigned i=0; i<op.instr()->num_srcs(); i++) {
            if(is_index_var_in(op.instr()->src_op(i), par))
                return TRUE;
        }
    else if(op.is_symbol()) {
        if(is_index(op.symbol(), par))
            return TRUE;
    }
    return FALSE;
}


static void add_sym(named_symcoeff_ineq & res, int & too_messy, var_sym * ind,
                    int int_coeff, named_lin_ineq * sym_coeff)
{
    int xc = -1;
    int xp = -1;


    if(ind == NULL) // adding a constant
        xc = 0;
    else if((xc = res.cols().find(ind)) == -1) {        // not a indvar
        if((xp = res.planes().find(ind)) == -1) {       // not a plane
                                                        // add as a ind var
            name_table_entry * nte = new name_table_entry(immed(ind));
            res.add_col(*nte, 1);
            xc = 1;
        }
    }


    if(sym_coeff == NULL) {                             // no symcoeffs
        if(xc >= 0)
            res[0][0][xc] += int_coeff;
        else
            res[xp][0][0] += int_coeff;
    } else {
        if(xc == -1) {                         // symcoeffs and ind is symcoeff too!!
            too_messy++;
            return;
        }
        named_symcoeff_ineq sc;                // make symcoeff from input
        sc.init(sym_coeff->n(), 1, 1);
        sc.planes().init(sym_coeff->names());
        for(int i=0; i < sym_coeff->n(); i++)
            sc[i][0][0] = sym_coeff->ineqs()[0][i];

        align_symcoeff(res, sc, &too_messy);
        if(too_messy) return;

        res || sc;

        if(xc != 0)
            xc = res.cols().find(ind);
        if(xc == -1) {
            too_messy++;
            return;
        }

        for(int ip=0; ip<res.p(); ip++)
            res[ip][0][xc] += int_coeff*sc[ip][0][0];
    }
}


static void put_index_info(operand op,
                           named_symcoeff_ineq & res,
                           int & too_messy,
			   int int_factor = 1,
                           named_lin_ineq * sc_factor = NULL)
{
    if(op.kind() == OPER_SYM) {
        if(op.symbol()->is_var() == 0) {
            too_messy++;
            return;
        }
        add_sym(res, too_messy, (var_sym *)op.symbol(),
                int_factor, sc_factor);
        return;
    }

    if(op.kind() != OPER_INSTR) {
        too_messy++;
        return;
    }
    instruction * ins = op.instr();
    in_rrr *i3r = (in_rrr *) ins;
    if(ins->format() == inf_rrr) {
        int val;
	if(is_const(operand(ins), &val)) {
            add_sym(res, too_messy, NULL,
                    val*int_factor, sc_factor);
            return;
	}
    } else if(ins->format() == inf_ldc) {
        in_ldc * ld = (in_ldc *)ins;
        if(ld->value().is_integer())
            add_sym(res, too_messy,  NULL,
                    (ld->value().integer())*int_factor, sc_factor);
        else
            too_messy++;
        return;
    }

    switch(ins->opcode()) {
        // not that io_cvt is too messy.  That way we can assume that
        // lods have the correct type, and in general it allows us
        // to rebuild from the access vectors.
    default:
        too_messy++;
        break;
    case io_lod:
        too_messy++;
        break;
    case io_neg:
        put_index_info(i3r->src1_op(), res, too_messy,
                       -int_factor, sc_factor);
        break;
    case io_cvt:
        put_index_info(i3r->src1_op(), res, too_messy,
                       int_factor, sc_factor);
        break;
    case io_add:
        put_index_info(i3r->src1_op(), res, too_messy,
                       int_factor, sc_factor);
        put_index_info(i3r->src2_op(), res, too_messy,
                       int_factor, sc_factor);
        break;
    case io_sub:
        put_index_info(i3r->src1_op(), res, too_messy,
                       int_factor, sc_factor);
        put_index_info(i3r->src2_op(), res, too_messy,
                       -int_factor, sc_factor);
        break;
    case io_mul: {
        int val;
        // var*const or const*var
        // if(sc_factor is NULL) then var*sym_const or sym_const*var
        if(is_const(i3r->src1_op(), &val))
            put_index_info(i3r->src2_op(), res, too_messy,
                           int_factor * val, sc_factor);
        else if(is_const(i3r->src2_op(), &val))
            put_index_info(i3r->src1_op(), res, too_messy,
                           int_factor * val, sc_factor);
        else if(sc_factor == NULL) {
            boolean ind1 = is_index_var_in(i3r->src1_op(), i3r->parent());
            boolean ind2 = is_index_var_in(i3r->src2_op(), i3r->parent());
            named_symcoeff_ineq  path;
            path.add_ineq(1);
            if(ind1 && ind2)
                too_messy++;
            else if(ind1) {
                put_index_info(i3r->src2_op(), path, too_messy,
                               1, NULL);
                if(path.p() > 1) too_messy++;
                named_lin_ineq lin_path(path.cols(), path[0]);
                put_index_info(i3r->src1_op(), res, too_messy,
                               int_factor, &lin_path);
            } else {
                put_index_info(i3r->src1_op(), path, too_messy,
                               1, NULL);
                if(path.p() > 1) too_messy++;
                named_lin_ineq lin_path(path.cols(), path[0]);
                put_index_info(i3r->src2_op(), res, too_messy,
                               int_factor, &lin_path);
            }
        } else
            too_messy++;
    }
        break;
    case io_div:
    case io_divfloor:
    case io_divceil: {
        int val;
        // var/const
        if(!is_const(i3r->src2_op(),&val) || (val == 0) || int_factor % val)
            too_messy++;
        else
            put_index_info(i3r->src1_op(), res, too_messy,
                           int_factor / val, sc_factor);
    }
        break;
    case io_lsl: {
        int val;
        if(!is_const(i3r->src2_op(), &val) || val < 0)
            too_messy++;
        else
            put_index_info(i3r->src1_op(), res, too_messy,
                           int_factor<<val, sc_factor);
    }
        break;
    case io_cpy:
        put_index_info(i3r->src1_op(), res, too_messy,
                       int_factor, sc_factor);
        break;
    }
}


named_symcoeff_ineq * named_symcoeff_ineq::convert_exp(instruction * ins)
{
    named_symcoeff_ineq * ret = new named_symcoeff_ineq;

    ret->add_ineq(1);
    int too_messy = 0;

    put_index_info(operand(ins), *ret, too_messy);

    if(too_messy > 0) {
        delete ret;
        return NULL;
    }
    return ret;
}


named_symcoeff_ineq * named_symcoeff_ineq::convert_exp(operand op)
{
    named_symcoeff_ineq * ret = new named_symcoeff_ineq;

    ret->add_ineq(1);
    int too_messy = 0;

    put_index_info(op, *ret, too_messy);

    if(too_messy > 0) {
        delete ret;
        return NULL;
    }
    return ret;
}




static boolean do_mk_sc(named_symcoeff_ineq & ret, operand op, boolean lb)
{
    named_symcoeff_ineq * curr = NULL;
    if(op.is_instr()) {
	instruction * in = op.instr();
	in_rrr * rrr = (in_rrr *)in;
	if(in->opcode() == io_cvt)
	    return do_mk_sc(ret, rrr->src1_op(), lb);
	else if(in->opcode() == io_max) {
	    if(!lb) return FALSE;
	    return do_mk_sc(ret, rrr->src1_op(), lb) && do_mk_sc(ret, rrr->src2_op(), lb);
	} else if(in->opcode() == io_min) {
	    if(lb) return FALSE;
	    return do_mk_sc(ret, rrr->src1_op(), lb) && do_mk_sc(ret, rrr->src2_op(), lb);
	} else
	    curr = named_symcoeff_ineq::convert_exp(op);
    } else
	curr = named_symcoeff_ineq::convert_exp(op);

    if(curr) {
	ret || *curr;
	ret.add_ineq(0);
	lin_ineq * xm = curr->get_m(0);
	ret.set_m(0, xm);
	delete curr;
	return TRUE;
    }
	return FALSE;
}


named_symcoeff_ineq * named_symcoeff_ineq::mk_sc(operand op, immed ind,
						 boolean lb)
{
    named_symcoeff_ineq  ret;
    if(do_mk_sc(ret, op, lb)) {
	ret.add_col(ind, 1);
	int im;
	for(im=0; im<ret.m(); im++)
	    ret[0][im][1] = -1;
	if(lb)
	    for(int ip=0; ip<ret.p(); ip++)
		for(im=0; im<ret.m(); im++)
		    for(int in=0; in<ret.n(); in++)
			ret[ip][im][in] *= -1;
	return new named_symcoeff_ineq(ret);
    }
    return NULL;

}



named_symcoeff_ineq * include_sc_for(tree_for * tf)
{
    assert(tf);

    boolean forward = TRUE;
    boolean ub_neq = FALSE;
    boolean lb_neq = FALSE;
    switch(tf->test()) {
    case FOR_SLTE:
    case FOR_ULTE:
	break;

    case FOR_SLT:
    case FOR_ULT:
	ub_neq = TRUE;
	break;

    case FOR_SGTE:
    case FOR_UGTE:
	forward = FALSE;
	break;

    case FOR_SGT:
    case FOR_UGT:
	forward = FALSE;
	lb_neq = TRUE;
	break;

    default:
	assert_msg(0, ("Test should not happen outside frontend\n"));
    }



    immed idx(tf->index());
    operand lop = (forward)?(tf->lb_op()):(tf->ub_op());
    named_symcoeff_ineq * cl = named_symcoeff_ineq::mk_sc(lop, idx, TRUE);
    if(cl == NULL) {
        fprintf(stderr,"Unknown lower bound for loop at line %d\n",
			source_line_num(tf));
        return NULL;
    }

    operand uop = (forward)?(tf->ub_op()):(tf->lb_op());
    named_symcoeff_ineq * cu = named_symcoeff_ineq::mk_sc(uop, idx, FALSE);
    if(cu == NULL) {
        fprintf(stderr,"Unknown upper bound for loop at line %d\n",
			source_line_num(tf));
        delete cl;
        return NULL;
    }

    if(ub_neq) {
	// index < UB    change to   index <= UB-1
	for(int i=0; i < cu->m(); i++)
	    (*cu)[0][i][0] -= 1;
    }

    if(lb_neq) {
	// LB < index   change to   LB+1 <= index
	for(int i=0; i < cl->m(); i++)
	    (*cl)[0][i][0] += 1;
    }

    cu = named_symcoeff_ineq::opand(cl, cu, 0, 1);

    named_symcoeff_ineq * stp = named_symcoeff_ineq::convert_exp(tf->step_op());
    boolean stpbad = TRUE;
    if(stp)
        if((stp->n() == 1)&&                            // no sym variables
	   (stp->p() == 1)&&                            // no sym Step
           (stp->m() == 1)) {                           // no max or mins
            if(ABS((*stp)[0][0][0]) == 1)               // unit step
		stpbad = FALSE;
        }
    if(stpbad) {
        fprintf(stderr,"Unknown or symbolic step size for loop at line %d\n",
			source_line_num(tf));
        delete cl;
        delete cu;
        if(stp) delete stp;
        return NULL;
    }

    cu->cleanup();

    delete cl;
    delete stp;

    return cu;
}


static named_symcoeff_ineq * get_cond(operand op)
{
    assert(op.is_instr());
    assert(op.instr()->format() == inf_rrr);
    in_rrr * ir = (in_rrr *)op.instr();

    named_symcoeff_ineq * l = named_symcoeff_ineq::convert_exp(ir->src1_op());
    named_symcoeff_ineq * r = named_symcoeff_ineq::convert_exp(ir->src2_op());
    if((l == NULL)||(r == NULL)) {
	if(l) delete l;
	if(r) delete r;
	return NULL;
    }

    named_symcoeff_ineq ret;
    switch(ir->opcode()) {
    case io_seq:                       // ==
	ret = sub(*l, *r);
	ret = ret & sub(*r, *l);
	break;

    case io_sl:                       // <
	ret = sub(*r, *l);
	ret[0][0][0]--;
	break;

    case io_sle:                       // <=
	ret = sub(*r, *l);
	break;

    default:
	delete l;
	delete r;
	return NULL;
    }
    return new named_symcoeff_ineq(ret);
}


static named_symcoeff_ineq * get_cond(tree_node_list * hdr)
{
    if(hdr->count() != 1) return NULL;
    assert((*hdr)[0]->is_instr());
    tree_instr * ti = (tree_instr *)(*hdr)[0];
    instruction * in = ti->instr();
    if(in->opcode() != io_bfalse) return NULL;

    in_bj * ibf = (in_bj *)in;
    return get_cond(ibf->src_op());
}



named_symcoeff_ineq * include_sc_if_true(tree_if * ti)
{
    named_symcoeff_ineq * nsi = get_cond(ti->header());
    return nsi;
}

named_symcoeff_ineq * include_sc_if_false(tree_if * ti)
{
    named_symcoeff_ineq * nsi = get_cond(ti->header());
    if(nsi == NULL) return NULL;
    if(nsi->m() > 1) {
	delete nsi;
	return NULL;
    }

    *nsi = mul(*nsi, -1);
    (*nsi)[0][0][0] -= 1;
    return nsi;
}




named_symcoeff_ineq add(const named_symcoeff_ineq & a,
			const named_symcoeff_ineq & b)
{
    named_symcoeff_ineq A(a);
    named_symcoeff_ineq B(b);

    A || B;
    assert(A.m() == B.m());

    for(int ip =0; ip < A.p(); ip++)
	A[ip] = A[ip] + B[ip];

    return A;
}

named_symcoeff_ineq sub(const named_symcoeff_ineq & a,
			const named_symcoeff_ineq & b)
{
    named_symcoeff_ineq A(a);
    named_symcoeff_ineq B(b);

    A || B;
    assert(A.m() == B.m());

    for(int ip =0; ip < A.p(); ip++)
	A[ip] = A[ip] - B[ip];

    return A;
}


named_symcoeff_ineq minus(const named_symcoeff_ineq & a)
{
    named_symcoeff_ineq A(a);
    for(int ip =0; ip < A.p(); ip++)
	A[ip] = A[ip]*-1;
    return A;
}

named_symcoeff_ineq mul(const named_symcoeff_ineq & a, int i)
{
    named_symcoeff_ineq A(a);
    for(int ip =0; ip < A.p(); ip++)
	A[ip] = A[ip]*i;
    return A;
}

named_symcoeff_ineq mul(int i, const named_symcoeff_ineq & a)
{
    return mul(a, i);
}


// BUGBUG: This is also a gross hack.
named_symcoeff_ineq * mul(const named_symcoeff_ineq & a,
			  const named_symcoeff_ineq & b)
{
    base_symtab * bt = NULL;
    bt = a.const_cols().get_symtab(bt);
    bt = a.const_planes().get_symtab(bt);
    bt = b.const_cols().get_symtab(bt);
    bt = b.const_planes().get_symtab(bt);

    instruction * ina = a.create_expression();
    instruction * inb = b.create_expression();

    block mul = block(ina) * block(inb);

    instruction * inab = mul.make_instruction((block_symtab *)bt);

    named_symcoeff_ineq * ret = named_symcoeff_ineq::convert_exp(inab);

    return ret;
}
