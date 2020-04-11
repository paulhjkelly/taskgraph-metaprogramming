
/* file "named_lin_ineq.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "named_lin_ineq.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include "suifmath.h"


int named_lin_ineq::unum_cnt = 0;



/* ##################################################
   #####   name_table_entry                     #####
   ################################################## */

name_table_entry::~name_table_entry()
{

}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/

void name_table_entry::init(const immed & im)
{
    if(im.is_symbol()) {
        assert(im.symbol()->is_var());
        is_var = TRUE;
        vsname.v = (var_sym *)im.symbol();
        knd = nte_symconst;
    } else if(im.is_string()) {
        is_var = FALSE;
        vsname.s = im.string();
        knd = nte_symconst;
    } else {
        is_var = FALSE;
        vsname.s = NULL;
        knd = nte_aux;
    }
}


void name_table_entry::init(name_table_entry_kind k,
                            const immed & im)
{
    init(im);
    // assert(knd != nte_aux);
    knd = k;

}

void name_table_entry::set_name(const immed & im)
{
    name_table_entry_kind kk = kind();
    init(im);
    if(kind() != nte_aux) {
        knd = kk;
        if((kind() == nte_aux)||
           (kind() == nte_none))
            knd = nte_symconst;
    }
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table_entry::init(const name_table_entry & nte)
{
    knd = nte.knd;
    is_var = nte.is_var;
    if(is_var)
        vsname.v = nte.vsname.v;
    else
        vsname.s = nte.vsname.s;
}


/***************************************************************************
 *  Following functions will change the type of a table entry              *
 ***************************************************************************/
void name_table_entry::mark_sym()
{
    assert(vsname.s);
    assert(knd != nte_aux);
    knd = nte_symconst;
}

void name_table_entry::mark_cond()
{
    assert(vsname.s);
    assert(knd != nte_aux);
    knd = nte_cond;
}

void name_table_entry::mark_loop()
{
    assert(vsname.s);
    assert(knd != nte_aux);
    knd = nte_loop;
}

void name_table_entry::mark_dim()
{
    assert(vsname.s);
    assert(knd != nte_aux);
    knd = nte_dim;
}

void name_table_entry::mark_summary()
{
    assert(vsname.s);
    assert(knd != nte_aux);
    knd = nte_summary;
}

void name_table_entry::mark_aux()
{
    knd = nte_aux;
    is_var = FALSE;
    vsname.s = NULL;
}


 /***************************************************************************
 *                                                                         *
 ***************************************************************************/
boolean name_table_entry::operator==(const name_table_entry & nte) const
{
    if((nte.knd != knd)||
       (knd == nte_aux))
        return FALSE;
    if(is_var && nte.is_var)
        return (var() == nte.var()) || var()->overlaps(nte.var());
    else
        return (nte.name() == name());
}


immed name_table_entry::name() const
{
    if(is_var) {
        assert(vsname.v);
        return immed(vsname.v);
    } else if(vsname.s)
        return immed(vsname.s);
    else
        return immed(0);
}

const char* name_table_entry::string() const
{
    if(is_var) {
        assert(vsname.v);
        return vsname.v->name();
    } else
        return vsname.s;
}


static boolean is_ancestor(base_symtab * ans, base_symtab * chld)
{
    if(chld == ans) return TRUE;
    if(chld->parent() == NULL) return FALSE;
    return is_ancestor(ans, chld->parent());
}


static base_symtab * inner_symtab(base_symtab * st1, base_symtab * st2)
{
    if(is_ancestor(st1, st2)) return st2;
    if(is_ancestor(st2, st1)) return st1;
    // assert_msg(0, ("Symbols are in different scopes, no single scope that covers all the symbols used"));
    return NULL;
}


base_symtab * name_table_entry::get_symtab(base_symtab * in) const
{
    if((kind() == nte_aux)||(!is_var))
        return in;

    base_symtab * st = var()->parent();
    assert(st);

    if(in == NULL)
        return st;
    else
        return inner_symtab(st, in);
}




/* ##################################################
   #####   name_table                           #####
   ################################################## */

#define SETTABLE(N)  if(n() > N) { \
                                   if(v##N) \
                                       L[N-1].set_name(*(v##N));\
                                   else \
                                       L[N-1].mark_aux(); \
                                   }

name_table::name_table(const immed * v1,  const immed * v2,  const immed * v3,
                       const immed * v4,  const immed * v5,  const immed * v6,
                       const immed * v7,  const immed * v8,  const immed * v9,
                       const immed * v10, const immed * v11, const immed * v12,
                       const immed * v13, const immed * v14, const immed * v15,
                       const immed * v16)
{
    L = NULL;
    sz = 1;
    rsz = 1;
    int s = 1;
    if(v1) s=2;
    if(v2) s=3;
    if(v3) s=4;
    if(v4) s=5;
    if(v5) s=6;
    if(v6) s=7;
    if(v7) s=8;
    if(v8) s=9;
    if(v9) s=10;
    if(v10) s=11;
    if(v11) s=12;
    if(v12) s=13;
    if(v13) s=14;
    if(v14) s=15;
    if(v15) s=16;
    if(v16) s=17;

    init(s);

    SETTABLE(1);
    SETTABLE(2);
    SETTABLE(3);
    SETTABLE(4);
    SETTABLE(5);
    SETTABLE(6);
    SETTABLE(7);
    SETTABLE(8);
    SETTABLE(9);
    SETTABLE(10);
    SETTABLE(11);
    SETTABLE(12);
    SETTABLE(13);
    SETTABLE(14);
    SETTABLE(15);
    SETTABLE(16);
}
#undef SETTABLE

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table::~name_table()
{
    if(L) delete[] L;
}


name_table &name_table::operator=(const name_table &nt)
{
    init(nt);
    return *this;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table::resize(int newsz)
{
    assert(newsz >= 0);
    if (newsz >= rsz) {
	int osz = MIN(sz,newsz);
	name_table_entry *L1 = L;
	rsz = newsz;
        L = (rsz-1>0)?(new name_table_entry[rsz-1]):NULL;

	for(int i=1; i<osz; i++)
	    L[i-1] = L1[i-1];
        if(L1) delete[] L1;
    }
    sz = newsz;
}

void name_table::init(const name_table &nt)
{
    if(&nt == this) return;

    if(nt.n() >= rsz) {
        if(L) delete[] L;
	rsz = nt.n();
        L = (rsz-1>0)?(new name_table_entry[rsz-1]):NULL;
    };
    sz = nt.n();

    for(int i=1; i<n(); i++)
        L[i-1].init(nt.e(i));
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table::init(int s)
{
    if(s > rsz) {
        if(L) delete[] L;
	rsz = s;
        L = (rsz-1>0)?(new name_table_entry[rsz-1]):NULL;
    };
    sz = s;

    for(int i=1; i<n(); i++)
        L[i-1].init();
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
int name_table::find(const name_table_entry & nte) const
{
    for(int i=1; i<n(); i++)
        if(L[i-1] == nte) return i;
    return -1;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
int name_table::find(const immed & v) const
{
    for(int i=1; i<n(); i++)
        if(L[i-1].name() == v) return i;
    return -1;
}

constraint name_table::lio_code_type() const
{
    constraint c(n());
    c[0] = NM_CONSTANT;
    for(int i=1; i<n(); i++)
        switch(e(i).kind()) {
        case nte_symconst:
            c[i] = NM_CONSTANT;
            break;
        case nte_cond:
        case nte_loop:
        case nte_dim:
        case nte_summary:
            c[i] = NM_LOCATIONS;
	    break;
        case nte_aux:
        case nte_none:
            assert(0);
        }
    return c;
}

void name_table::remove(int i, int j)
{
    assert((0<i)&&(i<=j)&&(j<n()));

    int src=j+1;
    int dst=i;

    while (src<n()) {
	L[dst-1] = L[src-1];
	dst++; src++;
    };
    sz = n()-(j-i+1);
}

void name_table::remove(const integer_row & mask)
{
    int src, dst;
    assert(mask.n() == n());

    for (dst=1, src=1; (src <  n()); src++) {
	if (!mask.c(src)) {
	    L[dst-1] = L[src-1];
	    dst++;
	};
    }

    sz = dst;
}

void name_table::insert(const name_table_entry & nte, int i)
{
    assert((i>=1)&&(i<=n()));
    if (sz >= rsz) {
	int lln = MAX(n()-1+1,(n()-1)*2);
	name_table_entry * LL = (lln>0)?(new name_table_entry[lln]):NULL;

	int k;
	for(k=1; k<i; k++)
	    LL[k-1] = L[k-1];
	LL[i-1].init(nte);
	for(k=i; k<n(); k++)
	    LL[k] = L[k-1];

	if(L) delete[] L;
	L = LL;
	sz++;
	rsz = lln+1;
    } else {
	for (int k=sz; k>i; k--)
	    L[k-1] = L[k-2];
	L[i-1].init(nte);
	sz++;
    }
}

void name_table::insert(const immed & v, int i)
{
    insert(name_table_entry(v), i);
}



/***************************************************************************
 * use B and change A                                                      *
 ***************************************************************************/
void name_table::change_name_types(name_table & na, name_table & nb)
{
    for(int i=1; i<nb.n(); i++) {
        immed bv(nb.e(i).name());
        if(bv.is_symbol() || bv.is_string()) {     // not an aux
            int j = na.find(bv);
            if(j > 0) {
                name_table_entry_kind ka = na.e(j).kind();
                name_table_entry_kind kb = nb.e(i).kind();
                name_table_entry_kind kk = nte_none;
                if(ka == kb)
                    kk = kb;
                else if(ka == nte_none)
                    kk = kb;
                else if(kb == nte_none) /* can't happen */
                    kk = ka;
                else if(ka == nte_symconst)
                    kk = kb;
                else if(kb == nte_symconst)
                    kk = ka;
                else if((ka == nte_cond)&&(kb == nte_loop))
                    kk = nte_cond;
                else if((kb == nte_cond)&&(ka == nte_loop))
                    kk = nte_cond;
                else if((ka == nte_summary)||(kb == nte_summary))
                    kk = nte_summary;
                else
                    assert(0);
                na[j].knd = kk;
                nb[i].knd = kk;
            }
        }
    }
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
boolean name_table::is_aligned(const name_table & na,
                               const name_table & nb)
{
    if(na.n() != nb.n()) return FALSE;

    for(int i=1; i<na.n(); i++) {
        if(na.e(i).name() != nb.e(i).name())
          if (na.e(i).kind() != nte_summary)
            return FALSE;
        if(na.e(i).name() == immed(0))
            return FALSE;
        else if(na.e(i).kind() != nb.e(i).kind())
            return FALSE;
    }
    // make sure table is ordered;
    name_table_entry_kind oldkind=nte_none;
    for (int j=1; j<na.n(); j++) {
	name_table_entry_kind newkind=na.e(j).kind();
	switch(newkind) {
	  case nte_symconst:
	    if (oldkind == nte_cond) return FALSE;
	  case nte_cond:
	    if (oldkind == nte_loop) return FALSE;
	  case nte_loop:
	    if (oldkind == nte_dim) return FALSE;
	  case nte_dim:
	    if (oldkind == nte_summary) return FALSE;
	  case nte_summary:
	    if (oldkind == nte_aux) return FALSE;
	  case nte_aux:
	  case nte_none:
	    break;
	}
	oldkind = newkind;
    }

    return TRUE;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table name_table::operator&(const name_table & b) const
{
    name_table ret(this);
    ret &= b;
    return ret;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table &name_table::operator&=(const name_table & b)
{
    if(is_aligned(*this, b)) {
        return *this;
    }
    integer_row ca(n());
    integer_row cb(b.n());
    this->align_with(b,ca,cb);
    return *this;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table * name_table::align_tables(const name_table &na,
                                      const name_table &nb,
                                      integer_row & ca,
                                      integer_row & cb)
{
    name_table *newnt = new name_table(na);

    newnt->align_with(nb, ca, cb);

    return newnt;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table::align_with(const name_table &nb,
			    integer_row & ca,
			    integer_row & cb)
{
    int ii;
    name_table nbcopy(nb);
    assert(n() == ca.n());
    assert(nb.n() == cb.n());
    change_name_types(*this, nbcopy);

    for(ii=1; ii<n(); ii++)
        assert(e(ii).kind() != nte_none);
    for(ii=1; ii<nbcopy.n(); ii++)
        assert(nbcopy.e(ii).kind() != nte_none);

    ca[0] = cb[0] = 0;                  // consant position does not change

    for (ii=1; ii <ca.n(); ii++)
	ca[ii] = ii;
    int curr = ca.n();

    for (ii=1; ii < nbcopy.n(); ii++) {
	if ((cb[ii] = find(nbcopy.e(ii))) == -1)
	    curr++;
    }
    resize(curr);

    curr = ca.n();
    for (ii=1; ii < nbcopy.n(); ii++) {
	if ((cb[ii]) == -1) {
	    (*this)[curr] = nbcopy.e(ii);
	    cb[ii] = curr++;
	}
    };
    integer_row extrac(n());
    if (do_reorder_table(extrac)) {
	integer_row newca(ca.n()), newcb(cb.n());
	for (ii=1; ii < ca.n(); ii++) {
	    // was:     this[ca[ii]] = old[ii];
	    // becomes: this[extrac[ii]] =
	    // ii moves to elt extrac[ca[ii]]
	    newca[ii] = extrac[ca[ii]];
	}
	ca.init(newca);
	for (ii=1; ii < cb.n(); ii++) {
	    newcb[ii] = extrac[cb[ii]];
	}
	cb.init(newcb);
    }
}

static void find_elements(name_table_entry_kind k,
			  const name_table &na, integer_row &ca,
			  int &curr)
{
    int currloc = curr;
    for (int ai=1; ai<na.n(); ai++) {
	if (na.e(ai).kind()==k) {
	    ca[ai] = currloc++;
	};
    }
    curr = currloc;
}


boolean name_table::do_reorder_table(integer_row &ca)
{
    int curr=1;
    assert(ca.n() == n());
    find_elements(nte_symconst, *this, ca, curr);
    find_elements(nte_cond, *this, ca, curr);
    find_elements(nte_loop, *this, ca, curr);
    find_elements(nte_dim, *this, ca, curr);
    find_elements(nte_summary, *this, ca, curr);
    find_elements(nte_aux, *this, ca, curr);
    assert(curr == ca.n());

    name_table thiscopy(*this);
    int i;
    for (i=1; i<n(); i++)
	(*this)[ca[i]] = thiscopy[i];
    for (i=1; i<n(); i++)
	if (ca[i] != i)
	    return TRUE;
    return FALSE;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table * name_table::mk_align(const name_table & na,
                                  const name_table & nb)
{
    name_table * newnt;
    if(is_aligned(na, nb))
        newnt = new name_table(na);
    else {
        // indexes to the new list
        integer_row ca(na.n());
        integer_row cb(nb.n());

        newnt = name_table::align_tables(na, nb, ca, cb);
    }

    return newnt;
}

base_symtab * name_table::get_symtab(base_symtab * in) const
{
    base_symtab * curr = in;
    for(int i=1; i<n(); i++)
        curr = e(i).get_symtab(curr);

    return curr;
}




/* ##################################################
   #####   named_lin_ineq                       #####
   ################################################## */

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq::~named_lin_ineq()
{

}

void named_lin_ineq::init()
{
    unum=unum_cnt++;
}

void named_lin_ineq::init(int m, int n)
{
    unum=unum_cnt++;
    names().init(n);
    ineqs().init(m,n);
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::init(const named_lin_ineq & c)
{
    nt.init(c.nt);
    lq = c.lq;
}


int named_lin_ineq::init(const immed_list & il, int c)
{
    unum = il[c++].integer();
    unum_cnt = MAX(unum_cnt, unum+1);

    int num = il[c++].integer();

    names().init(num);
    for(int i=1; i<num; i++)
    {
       const int ind_1 = c++;
       const int ind_2 = c++;
       names()[i].init((name_table_entry_kind)il[ind_1].integer(),il[ind_2]);
    }
    c = ineqs().integer_matrix::init(il, c);
    return c;
}

immed_list * named_lin_ineq::cvt_immed_list() const
{
    immed_list * L = new immed_list;

    L->append(immed(uid()));
    L->append(immed(n()));
    for(int i=1; i<n(); i++) {
        L->append(immed((int)const_names().e(i).kind()));
        L->append(const_names().e(i).name());
    }
    immed_list * iq = const_ineqs().cvt_immed_list();
    L->append(iq);
    return L;
}

void named_lin_ineq::swap_col(int i, int j)
{
    name_table_entry tmp(names()[i]);
    names()[i].init(names()[j]);
    names()[j].init(tmp);

    if(ineqs().m() > 0) {
        assert(names().n() == ineqs().n());
        lq.do_swap_col(i, j);
    }
}

void named_lin_ineq::del_col(int i, int j)
{
    assert(j>=i);
    assert((i>0)&&(j>0));
    assert((i<n())&&(j<n()));

    names().remove(i,j);
    lq.do_del_col(i,j);
}


void named_lin_ineq::del_col(const integer_row & rem)
{
    assert(rem.n() == n());
    names().remove(rem);
    ineqs().do_del_columns(rem);
    assert(names().n() == ineqs().n());
}

boolean named_lin_ineq::is_clean_false() const
{
    if ((m()==1) && (n()==1) && (const_ineqs().r(0).c(0) < 0))
	return TRUE;
    else
	return FALSE;
}

static void normalize_aux_offset(lin_ineq &leq, int auxcol)
{
    int bestrow = -1;
    int bestcoeff = 0;
    for (int i1=0; i1<leq.m(); i1++) {
	const integer_row &rowi = leq.r(i1);
	int coeff = rowi.c(auxcol);
	if (coeff > 0) {
	    if ((bestcoeff <= 0) || (coeff < bestcoeff)) {
		bestrow = i1;
		bestcoeff = coeff;
	    }
	} else if (coeff < 0) {
	    if ((bestcoeff == 0) || (coeff > bestcoeff)) {
		bestrow = i1;
		bestcoeff = coeff;
	    }
	}
    }

    int bestconst = leq[bestrow][0];
    int abscoeff = ABS(bestcoeff);
    int newconst = bestconst % abscoeff;
    newconst = (newconst > 0) ? newconst : newconst + abscoeff;
    int constdiff = (newconst - bestconst)/bestcoeff;

    for (int i2=0; i2<leq.m(); i2++) {
	int coeffi = leq[i2][auxcol];
	if (coeffi != 0) {
	    leq[i2][0] += constdiff * coeffi;
	}
    }
}

void named_lin_ineq::cleanup()
{
    del_zero_cols();
    ineqs().del_zeros();
    if(ineqs().m() == 0)
        return;

    if (is_clean_false()) return;

    if(~ineqs() == TRUE) {
	name_table nt0(1);
	lin_ineq leq0(1,1);
	leq0[0][0] = -1;
	init(named_lin_ineq(nt0, leq0));
        return;
    }

    integer_row rem(n());
    rem = 0;
    boolean remmed = FALSE;

    // remove aux vars that are empty.
    // also remove aux vars that are multiple of 1
    // also remove auxes which are only a lower or upper bound
    for(int i=1; i<n(); i++)
        if(names()[i].kind() == nte_aux) {
            boolean zero_or_one = TRUE;
	    int upper_bds = 0;
	    int upper_row = 0;
	    int lower_bds = 0;
	    int lower_row = 0;
            for(int j=0; (j<ineqs().m()); j++)
                if(ineqs()[j][i] != 0) {
		    if (ineqs()[j][i] > 0) {
			lower_bds++;
			lower_row = j;
		    } else {
			upper_bds++;
			upper_row = j;
		    }
		    if (ABS(ineqs()[j][i]) > 1)
			zero_or_one = FALSE;
		    else {
			fprintf(stderr,
				"\n### Warning: bad aux var ineq in cleanup\n");
			print(stderr);
		    }
		}
            if (zero_or_one || (upper_bds == 0) || (lower_bds == 0))
		rem[i] = remmed = TRUE;
	    else if ((upper_bds == 1) && (lower_bds == 1)) {
		constraint upper_bd(ineqs()[upper_row]);
		constraint lower_bd(ineqs()[lower_row]);
		upper_bd += ineqs()[lower_row];
		if (upper_bd.rank() == 0) {
		    int diffi = ineqs()[upper_row][0] + ineqs()[lower_row][0];
		    int mult = ineqs()[lower_row][i];
		    if (ABS(diffi) >= mult-1) {
			// S has aux X with only constraints
			// c1 <= Y + mX <= c2 where c2-c1 >= m,
			// so remove X and its constraints
			rem[i] = remmed = TRUE;
		    }
		};
	    }
        }

    // check if two auxilary vars are doing the same job
    for(int i1=1; i1<n(); i1++)
        if((names()[i1].kind() == nte_aux)&&(!rem[i1]))
            for(int i2=i1+1; i2<n(); i2++)
                if((names()[i2].kind() == nte_aux)&&(!rem[i2])) {
                    constraint filter(n());
                    filter = 0;
                    filter[i1] = 1;
                    filter[i2] = 1;             // c1*i1=a(); c2*i2=b()
		    // get rows with occurrences of i1 or i2;
                    lin_ineq L = ineqs().filter_thru(filter, 0);
                    // assert(!(~L));              // has to have an answer
                    L.do_add_rows(1);
                    L[L.m()-1] = 0;
                    L[L.m()-1][0]  = 1;
                    L[L.m()-1][i1] = 1;         // if both are the same
                    L[L.m()-1][i2] = -1;        // c1 > c2 is empty
                    if(~L) {
                        L[L.m()-1][i1] = -1;    // and c2 > c1 is empty
                        L[L.m()-1][i2] =  1;
                        if(~L) rem[i2] = remmed = TRUE;
                    }
                }

    if (remmed) {
	ineqs().do_filter_away(rem,0);
	del_zero_cols();
    }

    // canonicalize aux offsets;
    for(int i3=1; i3<n(); i3++)
        if (names()[i3].kind() == nte_aux)
	    normalize_aux_offset(ineqs(),i3);
}

void named_lin_ineq::simplify()
{
    ineqs().normalize(TRUE);
    ineqs().sort();
    ineqs().del_repetition();
    ineqs().del_loose();
    cleanup();
    return;
}

void named_lin_ineq::del_zero_cols()
{
    integer_row zeros(n());

    for(int i=n()-1; i>=1; i--) {
        int zero = zeros[i] = 1;
        for(int j=0; zero && (j<ineqs().m()); j++)
            if(ineqs()[j][i] != 0) {
		zeros[i] = zero = 0;
	    }
    }
    del_col(zeros);
}


named_lin_ineq & named_lin_ineq::operator=(const named_lin_ineq & c)
{
    if(&c == this) return *this;

    this->init(c);
    return *this;
}


named_lin_ineq & named_lin_ineq::operator=(const lin_ineq & l)
{
    assert(l.n() == names().n());
    ineqs().init(l);
    return *this;
}

void named_lin_ineq::align_named_lin_ineqs(named_lin_ineq & A,
					   named_lin_ineq & B)
{
    A.align_with(B);
}

boolean named_lin_ineq::operator==(const named_lin_ineq & c2) const
{
    if((this->const_ineqs().m() == 0)&&(c2.const_ineqs().m() == 0))
        return TRUE;
    if((this->const_ineqs().m() == 0)||(c2.const_ineqs().m() == 0))
        return FALSE;

    named_lin_ineq * l1;
    named_lin_ineq * l2;
    boolean delend = FALSE;

    if(is_aligned(*this, c2)) {
        l1 = (named_lin_ineq *)this;
        l2 = (named_lin_ineq *)&c2;
    } else {
        l1 = new named_lin_ineq(this);
        l2 = new named_lin_ineq(c2);
        delend = TRUE;
        l1->align_with(*l2);
    }

    boolean ret = (((*l1) >> (*l2))&&
		   ((*l2) >> (*l1)));
    if(delend) {
        delete l1;
        delete l2;
    }
    return ret;
}



named_lin_ineq named_lin_ineq::operator&(const named_lin_ineq & c) const
{
    if (m() == 0)
	if (c.m() == 0)
	    return named_lin_ineq();
	else
	    return c;
    else if (c.m() == 0)
	return *this;
    else if (is_aligned(*this, c)) {
	named_lin_ineq a1(this);
	a1 &= c.const_ineqs();
	return a1;
    } else {
	named_lin_ineq a1(this);
	named_lin_ineq a2(c);
	a1.align_with(a2);

	a1 &= a2.const_ineqs();
	return a1;
    }
}

// Deletes are there to save memory
named_lin_ineq * named_lin_ineq::opand(named_lin_ineq * c1,
                                     named_lin_ineq * c2,
                                     boolean del1,
                                     boolean del2)
{
    if (c1)
	if (c2) {
	    named_lin_ineq * ret = new named_lin_ineq(c1);
	    *ret &= *c2;
	    if(del1) delete c1;
	    if(del2) delete c2;
	    return ret;
	} else {
	    if (del1) return c1;
	    else
		return new named_lin_ineq(c1);
	}
    else
	if (c2) {
	    if(del2) return c2;
	    else
		return new named_lin_ineq(c2);
	} else {
	    return 0;
	}
}

named_lin_ineq & named_lin_ineq::operator&=(const named_lin_ineq & c)
{
    named_lin_ineq cc(c);
    this->align_with(cc);
    (*this) &= cc.ineqs();
    simplify();
    return *this;
}

named_lin_ineq & named_lin_ineq::operator&=(const lin_ineq & l)
{
    assert(l.n() == names().n());

    int oldm = m();

    ineqs() &= l;

    // get rid of linearly dependent new rows;
    ineqs().del_repetition(oldm, m()-1);
    ineqs().del_loose(oldm, m()-1);

    return *this;
}

// Given lower and upper bounds for two seperate aux vars
// see if they can be combined toghter to form a single aux var
// returns TRUE if it works, and swaps i1 and i2 in bounds2;
boolean nli_merged_aux(lin_ineq & bounds1, lin_ineq & bounds2,
		       int i1, int i2)
{
    if (bounds1.m() != bounds2.m()) return FALSE;

    bounds2.do_swap_col(i1, i2);

    // faster approximation to bounds1[1:n-1] == bounds2[1:n-1];
    integer_row used(bounds2.m());
    int i, j, k;
    int m = bounds1.m();
    int n = bounds1.n();
    for (i=0; i<m; i++) {
	for (j=0; j<m; j++) {
	    if (used[j]==0) {
		const constraint &b1 = bounds1.r(i);
		const constraint &b2 = bounds2.r(j);

		// ignore constant term;
		for (k=1; k<n; k++) {
		    if (b1.c(k) != b2.c(k))
			break;
		}
		if (k<n)
		    continue;
		used[j] = 1;
		break;
	    };
	};
	if (j >= m) {
	    // failed;
	    bounds2.do_swap_col(i1,i2);
	    return FALSE;
	}
    }

    return TRUE;

    /*
      int i, j;
      for (i=0; i<
      if ((bounds1 <= bounds2) &&
          (bounds2 <= bounds1)) {
	  return TRUE;
      }

      bounds2.do_swap_col(i1, i2);
      return FALSE;
      */
}

int named_lin_ineq::merge_auxes(named_lin_ineq &c)
{
    named_lin_ineq *r1 = this;
    named_lin_ineq *r2 = &c;

    // do we need these?
    r1->del_zero_cols();
    r2->del_zero_cols();

    r1->align_with(*r2);

    int naux = 0;
    constraint indx(r1->n());
    int i, j, ix, jx;

    // count the number of aux variables and record their positions
    for(i=1; i<r1->n(); i++)
	if(r1->names()[i].kind() == nte_aux) {
	    indx[naux] = i;
	    naux++;
	}

    if (naux > 0) {
	// For each aux variable, find the lower bounds and the upper bounds
	// BUGBUG: ignore relations between different aux vars for now;
	// this is conservative, at least;
	lin_ineq * baux1 = new lin_ineq[naux];
	lin_ineq * baux2 = new lin_ineq[naux];
	constraint kernel(r1->n());
	constraint kernel2(r1->n());
	kernel = 0;
	kernel2 = 0;
	for(i=0; i<naux; i++) {
	    ix = indx[i];
	    kernel2[ix] = 1;
	}
	for(i=0; i<naux; i++) {
	    ix = indx[i];
	    kernel[ix] = 1;
	    kernel2[ix] = 0;
	    baux1[i].init(r1->ineqs().filter_thru(kernel, 0));
	    baux1[i].init(baux1[i].filter_away(kernel2, 0));
	    baux2[i].init(r2->ineqs().filter_thru(kernel, 0));
	    baux2[i].init(baux2[i].filter_away(kernel2, 0));
	    kernel[ix] = 0;
	    kernel2[ix] = 1;
	}

	// See if two aux variables can be merged toghter.
	// one var has to be of r1 and other has to be of r2
	// ignore aux interactions for now;
	int mergedaux = 0;
	integer_row rmauxlist(r1->n());
	for (i=0; i<naux; i++)
	    if (baux1[i].m()>0) {
		for (j=0; j<naux; j++)
		    if (baux2[j].m()>0) {
			assert(i != j);
			ix = indx[i];
			jx = indx[j];
			// try to equate ix(in 1) and jx(in 2);
			if (nli_merged_aux(baux1[i], baux2[j], ix, jx)) {
			    // merge found, record it and
			    // mark the aux to be removed.
			    rmauxlist[jx] = 1;
			    mergedaux += 1;
			    r2->ineqs().do_swap_col(ix,jx);
			    baux1[i].clear();
			    baux2[j].clear();
			    break;
			}
		    };
	    }
	delete[] baux1;
	delete[] baux2;

	if (mergedaux > 0) {
	    r1->del_col(rmauxlist);
	    r2->del_col(rmauxlist);
	}
	return mergedaux;
    } else
	return 0;
}

// err towards FALSE;
boolean named_lin_ineq::operator>=(const named_lin_ineq & c) const
{
    named_lin_ineq l1(*this);
    named_lin_ineq l2(c);

    int i;
    int naux1 = 0;
    int naux2 = 0;

    // do we need these?
    l1.del_zero_cols();
    l2.del_zero_cols();

    const name_table &nt1 = l1.const_names();
    const name_table &nt2 = l2.const_names();

    // count the number of aux variables;
    for(i=1; i<nt1.n(); i++) {
	const name_table_entry &ntei = nt1.e(i);
	if(ntei.kind() == nte_aux) {
	    naux1++;
	} else if (nt2.find(ntei) < 0)
	    // this constrains a variable not constrained by c;
	    // so there is no way this contains c;
	    return FALSE; //
    }

    for(i=1; i<nt2.n(); i++) {
	const name_table_entry &ntei = nt2.e(i);
	if(ntei.kind() == nte_aux)
	    naux2++;
    }

    if (naux1 > naux2) return FALSE; // same problem with this;

    if (naux1 > 0) {
	int mergedauxes = l1.merge_auxes(l2);

	if (naux1 > mergedauxes) return FALSE;

	// BUGBUG; still doesnt handle auxes right in general;
	// consider { x = 2*aux } and { x = 4*aux };
    } else
	l1.align_with(l2);

    if(~(l1.ineqs() && l2.ineqs())) {
	return FALSE;
    }
    boolean ret = (l1.ineqs() >= l2.ineqs());
    return ret;
}

/**************************************************************************
 * a simple substitution method                                           *
 * This is a cheap alternative to expensive Fourier-Motzkin               *
 * substitution expression given in expr is of the form                   *
 * var >= sub_expr and var <= sub_expr or                                 *
 * sub_expr >= 0                                                          *
 **************************************************************************/
named_lin_ineq * named_lin_ineq::substitute(const immed & var,
                                            const named_lin_ineq & expr) const
{
    named_lin_ineq *ret = new named_lin_ineq(this);
    ret->do_substitute(var, expr);
    return ret;
}

void named_lin_ineq::do_substitute(const immed & var,
				   const named_lin_ineq & expr)
{
    int pos = find(var);
    if(pos <= 0)
        return;

    int col = expr.find(var);

    boolean tested = 1;

    tested &= (expr.m() == 2);
    tested &= ABS(expr.const_ineqs().r(0).c(col) == 1);
    for(int i=0; tested && i<expr.n(); i++)
	tested &=
	    (expr.const_ineqs().r(0).c(i) + expr.const_ineqs().r(1).c(i) == 0);
    assert_msg(tested,("do_substitute, expr not of right form"));

    // leave in row with -var;
    int row = (expr.const_ineqs().r(0).c(col) > 0) ? 0 : 1;
    named_lin_ineq myex(expr);
    myex.del_col(col);
    myex.ineqs().do_del_row(row);

    align_with(myex);
    pos = find(var);
    for(int im=0; im<m(); im++) {
        int coeff = ineqs()[im][pos];
        if(coeff) {
            ineqs()[im].do_addmul(myex.ineqs()[0], coeff);
            ineqs()[im][pos] = 0;
        }
    }

    del_col(pos);
}

void named_lin_ineq::find_bounds()
{
    poly_iterator Poly(ineqs());
    constraint del_list(ineqs().n());
    del_list = 0;
    for(int a=1; a< ineqs().n(); a++)
        if(names()[a].kind() == nte_symconst)
            del_list[a] = -1;
    // sort by symconsts, then by rank;
    Poly.set_sort_order(del_list.data_array());

    assert(!~ineqs());
    // fm-pairwise project ineqs, sort them;
    lin_ineq *M = Poly.get_iterator(0);
    assert(!~*M);
    // try to get rid of some of the extra constraints;
    M = Poly.reduce_extra_constraints2_ptr();
    assert(!~*M);
    ineqs().init(M);
    // sort them again;
    ineqs().sort();
}


void named_lin_ineq::add_col(const name_table_entry & nte, int i)
{
    names().insert(nte, i);
    ineqs().do_insert_col(i);
    assert(ineqs().n() == names().n());
}


void named_lin_ineq::project()
{
    constraint del_list(n());
    del_list = 0;

    // sort order is approximately;
    // symbolic constants eqns last, then others vaguely by rank;
    // (really something close to (rank+sum(i+1 | col i is non-0)));
    for(int a=1; a<n(); a++)
        if(names()[a].kind() == nte_symconst)
            del_list[a] = -1;
        else
            del_list[a] = 1;

    // project
    poly_iterator Poly(ineqs());
    Poly.set_sort_order(del_list.data_array());
    Poly.get_iterator(1);
    ineqs().init(Poly.reduce_extra_constraints2_ptr());
    ineqs().sort();

    cleanup();
}

void named_lin_ineq::project_away(const immed & var, int *changedp)
{
    name_table N;
    N.insert(name_table_entry(var), 1);
    project_away(N, changedp);
}

extern lin_ineq * fast_fourier_motzkin(lin_ineq & in, integer_row *elim = 0,
				       boolean iterate=TRUE);

static void del_unit_stride_rows(lin_ineq &lq,
				 const integer_row &elim)
{
    int i, j;
    integer_row delrows(lq.m());
    for (j=0; j<lq.n(); j++) {
	if (elim.c(j))
	    for (i=0; i<lq.m(); i++) {
		int entry = lq[i][j];
		if (entry)
		    if (ABS(entry) <= 1)
			delrows[i]=1; // do del it;
	    }
    }
    lq.do_del_rows(delrows);
}

int use_alt_project = 0;
int debug_suifmath_project = 0;

void named_lin_ineq::project_away(const name_table & N, int *changed)
{
    constraint elim(n());
    int i;
    int j=1;

    for(i=1; i<N.n(); i++) {           // for each name to be deleted
	int pos = names().find(N.e(i).name());  // position of that name
	if ((pos > 0) && !elim[pos]) {
	    elim[pos] = 1;               // filter anything in that pos;
	    swap_col(pos, n()-j);         // switch it to the end;
	    elim.do_swap_col(pos, n()-j);
	    j++;
	}
    }
    if (j==1) return;  // nothing to project;
    if (changed) *changed = 1;

    // look for existing aux vars; eliminate them, too;
    for(i=1; i<n(); i++) {
	if (names()[n()-i].kind() == nte_aux) {
	    if (i>j) {
		swap_col(n()-i, n()-j);
	    }
	    elim[n()-j] = 1;
	    j++;
	};
    }

    // project;
    lin_ineq involved = ineqs().filter_thru(elim,0);
    if (involved.m() > 0) {
	ineqs().do_filter_away(elim,0);
	if (debug_suifmath_project) {
	    fprintf(stderr,"for_project: filtering away, elim:\n");
	    elim.print(stderr);
	    fprintf(stderr,"uninvolved:\n");
	    ineqs().print(stderr);
	    fprintf(stderr,"involved:\n");
	    involved.print(stderr);
	}
	lin_ineq *projeq =
	     fast_fourier_motzkin(involved, &elim, (use_alt_project > 1));
	if (debug_suifmath_project) {
	    fprintf(stderr,"after projection:\n");
	    projeq->print(stderr);
	}
	if (use_alt_project > 0) {
	    del_unit_stride_rows(*projeq, elim);
	    if (debug_suifmath_project) {
		fprintf(stderr,"after unit rows deleted:\n");
		projeq->print(stderr);
	    }
	    constraint noelim(n());
	    noelim = 1;
	    noelim -= elim;
	    noelim[0] = 0;
	    projeq->do_filter_thru(noelim,0);
	} else {
	    projeq->do_filter_away(elim,0);
	}
	if (debug_suifmath_project) {
	    fprintf(stderr,"after uninteresting rows deleted:\n");
	    projeq->print(stderr);
	}
	ineqs() &= *projeq;
	if (debug_suifmath_project) {
	    fprintf(stderr,"replacing uninvolved rows:\n");
	    ineqs().print(stderr);
	}
	delete projeq; projeq = 0;
	for (i=0; i<n(); i++) {
	    if (elim[i])
		names()[i].mark_aux();
	}
	if (debug_suifmath_project) {
	    fprintf(stderr,"before cleanup:\n");
	    print(stderr);
	}
    }

    ineqs().sort();
    ineqs().del_repetition();
    ineqs().del_loose();
    cleanup();
    return;
}

boolean named_lin_ineq::operator~() const
{
    return ~const_ineqs();
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::print(FILE *fp) const
{
//    fprintf(fp, "       ");
//    for(int i=1; i<names().n(); i++) {
//        fprintf(fp, "%6s",
//                (names()[i].map())?names()[i].map()->name():"       ");
//        fprintf(fp, "%s", (names()[i].kind() == nte_loop)?".":" ");
//    }
//    for(; i<9; i++) fprintf(fp, "       ");
//    fprintf(fp, "%3d %s  <%X>\n", uid(), (is_killed()?"XXX":"   "),
//            get_home());

    fprintf(fp, "[%6d] ", unum);
    int i;
    for(i=1; i<const_names().n(); i++)
        fprintf(fp, "%7s",
                (const_names().e(i).string())?const_names().e(i).string():
                "<aux>");
    fprintf(fp, "     [ ");
    for(i=1; i<const_names().n(); i++) {
      char c = '.';
      switch(const_names().e(i).kind()) {
      case nte_none:       c = '?'; break;
      case nte_symconst:   c = 's'; break;
      case nte_cond:       c = 'c'; break;
      case nte_loop:       c = 'l'; break;
      case nte_dim:        c = 'd'; break;
      case nte_summary:    c = 'y'; break;
      case nte_aux:        c = 'a'; break;
      }
      fprintf(fp,"%c", c);
    }
    fprintf(fp, "]\n");
    const_ineqs().print(fp);
    fprintf(fp, "\n");
}

void named_lin_ineq::print_exp(print_exp_type type, FILE *fp) const
{
    if(type == pet_single) {
      assert(const_ineqs().m() == 1);
    }

    for(int x=0; x<const_ineqs().m(); x++) {
        switch(type) {
        case pet_system_nl:
            break;
        case pet_system:
            if(const_ineqs().m() > 1)
                fprintf(fp, "(");
            break;
        case pet_min:
        case pet_max:
            if((const_ineqs().m() > 1) && (x==0))
                fprintf(fp, "%s(", (type==pet_min)?"MIN":"MAX");
            break;
        default:
            break;
        }

        int pr = 0;
        integer_row R = const_ineqs().r(x);
        if(R[0]) {
            fprintf(fp, "%d ", R[0]);
            pr = 1;
        }

        for(int i=1; i<n(); i++)
            if(R[i]) {
                if(R[i] == 1) {
                    if(pr)
                        fprintf(fp, "+ ");
                } else if(R[i] == -1) {
                    if(pr)
                        fprintf(fp, "- ");
                    else
                        fprintf(fp, "-");
                } else if(R[i] > 0) {
                    if(pr)
                        fprintf(fp, "+ %d", R[i]);
                    else
                        fprintf(fp, "%d", R[i]);
                } else {
                    if(pr)
                        fprintf(fp, "- %d", -R[i]);
                    else
                        fprintf(fp, "%d", R[i]);
                }
                fprintf(fp, "%s ", const_names().e(i).string());
                pr = 1;
            }
        if(pr==0) fprintf(fp, "0 ");
        switch(type) {
        case pet_system:
            fprintf(fp, ">= 0");
            if(const_ineqs().m() > 1) fprintf(fp, ")");
            if(x < const_ineqs().m()-1)
                fprintf(fp, "&&");
            break;
        case pet_system_nl:
            fprintf(fp, ">= 0\n");
            break;
        case pet_min:
        case pet_max:
            if(x < const_ineqs().m()-1)
                fprintf(fp, ", ");
            if((const_ineqs().m() >1)&&(x == const_ineqs().m()-1))
                fprintf(fp, ")");
            break;
        default:
            break;
        }
    }
}


instruction * named_lin_ineq::mk_bounds() const
{
    block bnf(0);
    constraint filter(n());
    filter = 0;
    int i;
    for(i=1; i<n(); i++)
        if(const_names().e(i).kind() == nte_aux)
            filter[i] = 1;
    lin_ineq Laux    = const_ineqs().filter_thru(filter, -1);
            // only A - aux >= 0
    lin_ineq Lbounds = const_ineqs().filter_away(filter, 0);

    boolean first = TRUE;

    for(i=0; i<Lbounds.m(); i++) {
        block stmt(Lbounds[i][0]);
        for(int j=1; j<n(); j++)
            if(const_names().e(j).kind() != nte_aux) {
                int val = Lbounds[i][j];
                block sym((var_sym *)const_names().e(j).name().symbol());
                if(val == 1)
                    stmt.set(stmt + sym);
                else if(val == -1)
                    stmt.set(stmt - sym);
                else if(val != 0)
                    stmt.set(stmt + block(val)*sym);
            }
        if(first) {
            bnf.set(stmt >= block(0));
            first = FALSE;
        } else
            bnf.set(bnf & (stmt >= block(0)));
    }

    for(i=0; i<Laux.m(); i++) {
        block stmt(Laux[i][0]);
        int done = FALSE;
        for(int j=1; (j<n())&&(!done); j++) {
            int val = Laux[i][j];
            if(const_names().e(j).kind() != nte_aux) {
                block sym((var_sym *)const_names().e(j).name().symbol());
                if(val == 1)
                    stmt.set(stmt + sym);
                else if(val == -1)
                    stmt.set(stmt - sym);
                else if(val != 0)
                    stmt.set(stmt + block(val)*sym);
            } else if(val) {
                done = TRUE;
                if(first) {
                    bnf.set((stmt % block(-val)) == block(0));
                    first = FALSE;
                } else
                    bnf.set(bnf & ((stmt % block(-val)) == block(0)));
            }
        }
    }

    base_symtab * bs = const_names().get_symtab();
//    bnf.print();
    instruction * ins = bs ? bnf.make_instruction((block_symtab *)bs) : 0;
    return ins;
}


instruction * named_lin_ineq::create_expression(const immed & v,
                                                boolean is_ub,
                                                block_symtab * sym) const
{
    assert(const_ineqs().m() > 0);
    int pos = find(v);
    assert(pos > 0);

    if(!sym)
        sym = (block_symtab *)const_names().get_symtab();
    if(!sym)
	return 0;

    constraint filter(n());
    filter = 0;
    filter[pos] = 1;
    lin_ineq L = const_ineqs().filter_thru(filter, (is_ub)?-1:1);
    if(!is_ub) L *= -1;

    block exp;
    for(int i=0; i<L.m(); i++) {
        block curr(L[i][0]);
        int val = -1*L[i][pos];
        L[i][pos] = 0;
        for(int j=1; j<n(); j++)
            if(const_names().e(j).name().is_symbol() ||
               const_names().e(j).name().is_string()) {
                block var((var_sym *)const_names().e(j).name().symbol());
                if(L[i][j] == 1)
                    curr.set(curr + var);
                else if(L[i][j] == -1)
                    curr.set(curr - var);
                else if(L[i][j] != 0)
                    curr.set(curr + block(L[i][j])*var);
            }
        if(val != 1)
            curr.set(block::op(curr,
                               (is_ub)?bop_divfloor:bop_divceil,
                               block(val)));
        if(i==0)
            exp.set(curr);
        else
            exp.set(block::op(exp, (is_ub)?bop_min:bop_max, curr));
    }
    instruction * ret = exp.make_instruction(sym);
    return ret;
}



/***************************************************************************
 *                                                                         *
 ***************************************************************************/
boolean named_lin_ineq::is_aligned(const named_lin_ineq & A,
                                   const named_lin_ineq & B)
{
    const name_table & na = A.const_names();
    const name_table & nb = B.const_names();

    return name_table::is_aligned(na, nb);
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::align(const name_table & ant)
{
    if(name_table::is_aligned(ant, names())) return;

    named_lin_ineq tmp(this);
    integer_row rmap(n());
    rmap[0] = 0;
    int i;
    for(i=1; i<n(); i++) {
        assert_msg(names()[i].kind() != nte_aux,
                   ("Handling aux. vars. is not implemented"));
        int x = ant.find(names()[i]);
        assert_msg(x > 0, ("The variable %s is not in align name table",
                           names()[i].string()));
        rmap[i] = x;
    }

    names().init(ant);
    ineqs().init(m(), ant.n());

    for(i=0; i<m(); i++)
        for(int j=0; j<tmp.n(); j++)
            ineqs()[i][rmap[j]] = tmp.ineqs()[i][j];
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::align_with(named_lin_ineq & B)
{
    named_lin_ineq & A = *this;
    if(is_aligned(A,B)) return;

    A.cleanup();
    B.cleanup();

    name_table & na = A.names();
    name_table & nb = B.names();

    // indexes to the new list
    integer_row ca(na.n());
    integer_row cb(nb.n());

    name_table * newnt = name_table::align_tables(na, nb, ca, cb);

    A.nt.init(newnt);
    B.nt.init(newnt);

    // Readjust the inequalities to match the new name table
    lin_ineq leqa(A.lq.m(), newnt->n());
    int i;
    for(i=0; i<A.lq.m(); i++)
        for(int j1=0; j1<A.lq.n(); j1++)
            leqa[i][ca[j1]] = A.lq[i][j1];
    A.lq.init(leqa);

    lin_ineq leqb(B.lq.m(), newnt->n());
    for(i=0; i<B.lq.m(); i++)
        for(int j2=0; j2<B.lq.n(); j2++)
            leqb[i][cb[j2]] = B.lq[i][j2];
    B.lq.init(leqb);

    delete newnt;
}

// reorders the table so element kind ordering matches historical ordering;
// if ca, then sets ca so that new[*ca[i]] = old[i];
void named_lin_ineq::do_reorder_table(integer_row *retca)
{
    integer_row ca(n());
    boolean changed = nt.do_reorder_table(ca);

    if (changed) {
	integer_row tmp(n());
	for (int j=0; j<m(); j++) {
	    constraint &rowj = lq[j];
	    tmp[0] = rowj[0];
	    for (int i=1; i<n(); i++)
		tmp[ca[i]] = rowj[i];
	    rowj.init(tmp);
	}
    }
    if (retca) retca->init(ca);
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::merge_named_lin_ineqs(
        const named_lin_ineq & A, const named_lin_ineq & B)
{
    named_lin_ineq * nA = new named_lin_ineq(A);
    named_lin_ineq * nB = new named_lin_ineq(B);
    align_named_lin_ineqs(*nA, *nB);

    (*nA) &= nB->ineqs();

    delete nB;
    return nA;
}





/* ##################################################
   #####   other routines for debugging only    #####
   ################################################## */

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table_entry::print(FILE * fp) const
{
//     fprintf(fp, "(");
//     switch(kind()) {
//     case nte_none:
//         fprintf(fp, "n ");
//         break;
//     case nte_symconst:
//         fprintf(fp, "s ");
//         break;
//     case nte_cond:
//         fprintf(fp, "c ");
//         break;
//     case nte_loop:
//         fprintf(fp, "l ");
//         break;
//     default:
//         fprintf(fp, "? ");
//         break;
//     }
//     if(name)
//         name->print();
//     else
//         fprintf(fp, "?");
//     fprintf(fp, " ");
//     if(old_name) old_name->print();
//     fprintf(fp, ")");
    fprintf(fp, "%s ", (string())?string():"?");
}


void name_table::print(FILE * fp) const
{
    for(int i=1; i<n(); i++) e(i).print(fp);
    fprintf(fp, "\n");
}


void name_table::print_symtabs(FILE * fp) const
{
    fprintf(fp, "     ");
    for(int i=1; i<n(); i++) {
      if(e(i).is_var)
	fprintf(fp, "%7s", e(i).vsname.v->parent()->name());
    }
    fprintf(fp, "\n");
}


