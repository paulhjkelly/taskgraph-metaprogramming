/* file "named_sc_init.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "named_symcoeff_ineq.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include "suifmath.h"


int named_symcoeff_ineq::unum_cnt = 0;



/* ##################################################
   #####   named_symcoeff_ineq                   #####
   ################################################## */


/***************************************************************************
 *   Constructors                                                          *
 ***************************************************************************/
named_symcoeff_ineq::named_symcoeff_ineq()
{
    L = NULL;
    init();
}

named_symcoeff_ineq::named_symcoeff_ineq(const named_symcoeff_ineq & c)
{
    L = NULL;
    init();
    init(c);
}

named_symcoeff_ineq::named_symcoeff_ineq(const named_symcoeff_ineq * c)
{
    L = NULL;
    init();
    init(*c);
}

named_symcoeff_ineq::named_symcoeff_ineq(name_table &p,
                                       name_table &c, 
                                       lin_ineq ** ll)
{
    L = NULL;
    init();
    init(p, c, ll);
}

named_symcoeff_ineq::named_symcoeff_ineq(name_table &p, name_table &c, 
                                       lin_ineq * l0, lin_ineq * l1, 
                                       lin_ineq * l2, lin_ineq * l3, 
                                       lin_ineq * l4, lin_ineq * l5, 
                                       lin_ineq * l6, lin_ineq * l7, 
                                       lin_ineq * l8)
{
    L = NULL;
    init();
    init(p, c, l0, l1, l2, l3, l4, l5, l6, l7, l8);
}
 
named_symcoeff_ineq::named_symcoeff_ineq(name_table &n, 
                                       lin_ineq * l0, 
                                       immed * v1, lin_ineq * l1,
                                       immed * v2, lin_ineq * l2, 
                                       immed * v3, lin_ineq * l3,
                                       immed * v4, lin_ineq * l4,
                                       immed * v5, lin_ineq * l5,
                                       immed * v6, lin_ineq * l6,
                                       immed * v7, lin_ineq * l7,
                                       immed * v8, lin_ineq * l8)
{
    L = NULL;
    init();
    init(n, l0, 
         v1, l1, v2, l2, v3, l3, v4, l4, 
         v5, l5, v6, l6, v7, l7, v8, l8);
}

named_symcoeff_ineq::named_symcoeff_ineq(name_table & p,
                                       named_lin_ineq ** ll)
{
    L = NULL;
    init();
    init(p, ll);
}

named_symcoeff_ineq::named_symcoeff_ineq(name_table & p,
                                       named_lin_ineq * l0,
                                       named_lin_ineq* l1, named_lin_ineq* l2,
                                       named_lin_ineq* l3, named_lin_ineq* l4,
                                       named_lin_ineq* l5, named_lin_ineq* l6,
                                       named_lin_ineq* l7, named_lin_ineq* l8)
{
    L = NULL;
    init();
    init(p, l0, l1, l2, l3, l4, l5, l6, l7, l8);

}

named_symcoeff_ineq::named_symcoeff_ineq(named_lin_ineq * l0,
                                       immed * v1, named_lin_ineq * l1,
                                       immed * v2, named_lin_ineq * l2,
                                       immed * v3, named_lin_ineq * l3,
                                       immed * v4, named_lin_ineq * l4,
                                       immed * v5, named_lin_ineq * l5,
                                       immed * v6, named_lin_ineq * l6,
                                       immed * v7, named_lin_ineq * l7,
                                       immed * v8, named_lin_ineq * l8)
{
    L = NULL;
    init();
    init(l0, 
         v1, l1, v2, l2, v3, l3, v4, l4, 
         v5, l5, v6, l6, v7, l7, v8, l8);
}

named_symcoeff_ineq::named_symcoeff_ineq(const named_lin_ineq & cc)
{
    L = NULL;
    init();
    init(cc);
}

named_symcoeff_ineq::named_symcoeff_ineq(immed_list & il)
{
    L = NULL;
    init();
    init(il);
}

named_symcoeff_ineq::~named_symcoeff_ineq()
{
    if(L) delete[] L;
}

/***************************************************************************
 *   Set L according to the given number of inequalities. # of planes and  *
 *   columns are unchanged.                                                *
 ***************************************************************************/
void named_symcoeff_ineq::initL(int r)
{
    if(L) delete[] L;
    if(p())
        L = new lin_ineq[p()];
    else
        L = NULL;
    for(int i=0; i<p(); i++)
        L[i].init(r, n());
}


/***************************************************************************
 *   List of inequalities given; one per each plane.  # of planes, columns *
 *   and ineqs are already set. Will only set the data.                    *
 *   All columns are already aligned.                                     *
 ***************************************************************************/
void named_symcoeff_ineq::initL(lin_ineq ** list)
{
    if(L) delete[] L;

    if(p())
        L = new lin_ineq[p()];
    else
        L = NULL;
    for(int i=0; i<p(); i++) {
        lin_ineq * c = list[i];
        assert(c);
        L[i] = *c;
        assert(c->n() == n());
        assert(c->m() == m());
    }
}


/***************************************************************************
 *   List of inequalities given; one per each plane.  # of planes, columns *
 *   and ineqs are already set. Will only set the data. Since columns of   *
 *   each plane are not aligned, first align them.                       *
 ***************************************************************************/
void named_symcoeff_ineq::initL(named_lin_ineq ** list)
{
    // copy into a local mutable structure
    named_lin_ineq * lista = (p())?(new named_lin_ineq[p()]):NULL;
    int i;
    for(i=0; i<p(); i++) {
        assert(list[i]);
        lista[i] = *list[i];
    }

    // align everything s.t. tmp is the global alignment
    named_lin_ineq tmp(cols(), lin_ineq(0, n()));
    for(i=0; i<p(); i++) {
        tmp || lista[i];
    }
    nt_c.init(tmp.names());

    if(L) delete[] L;

    // create the local planes. Need to align again with tmp
    L = (p())?(new lin_ineq[p()]):NULL;
    for(i=0; i<p(); i++) {
        tmp || lista[i];
        L[i] = lista[i].ineqs();;
        assert(L[i].m() == m());
        assert(L[i].n() == n());
    }

    if(lista) delete[] lista;
}



/***************************************************************************
 *  set-up data and calls void named_symcoeff_ineq::initL(lin_ineq ** list) *
 ***************************************************************************/
void named_symcoeff_ineq::initL(lin_ineq * l0, lin_ineq * l1, lin_ineq * l2, 
                               lin_ineq * l3, lin_ineq * l4, lin_ineq * l5, 
                               lin_ineq * l6, lin_ineq * l7, lin_ineq * l8)
{
    lin_ineq ** list = (p())?(new p_lin_ineq[p()]):NULL;
    if(p() > 0) list[0] = l0;
    if(p() > 1) list[1] = l1;
    if(p() > 2) list[2] = l2;
    if(p() > 3) list[3] = l3;
    if(p() > 4) list[4] = l4;
    if(p() > 5) list[5] = l5;
    if(p() > 6) list[6] = l6;
    if(p() > 7) list[7] = l7;
    if(p() > 8) list[8] = l8;
    initL(list);
    if(list) delete[] list;
}


/***************************************************************************
 *  set-up and calls  named_symcoeff_ineq::initL(named_lin_ineq ** list)    *
 ***************************************************************************/
void named_symcoeff_ineq::initL(const named_lin_ineq * l0, 
				named_lin_ineq * l1, 
                               named_lin_ineq * l2, named_lin_ineq * l3, 
                               named_lin_ineq * l4, named_lin_ineq * l5, 
                               named_lin_ineq * l6, named_lin_ineq * l7, 
                               named_lin_ineq * l8)
{
    named_lin_ineq ** list = (p())?(new p_named_lin_ineq[p()]):NULL;
    named_lin_ineq copyl0(l0);
    if(p() > 0) list[0] = &copyl0;
    if(p() > 1) list[1] = l1;
    if(p() > 2) list[2] = l2;
    if(p() > 3) list[3] = l3;
    if(p() > 4) list[4] = l4;
    if(p() > 5) list[5] = l5;
    if(p() > 6) list[6] = l6;
    if(p() > 7) list[7] = l7;
    if(p() > 8) list[8] = l8;
    initL(list);
    if(list) delete[] list;
}


/***************************************************************************
 *   check for correctness                                                 *
 *                                                                         *
 *   all planes should have the same number of rows and columns.           *
 ***************************************************************************/
void named_symcoeff_ineq::check()
{
    for(int i=0; i<p(); i++) {
        assert(L[i].m() == m());
        assert(L[i].n() == n());
    }
}        


/***************************************************************************
 *   Set the systems to empty                                              *
 ***************************************************************************/
void named_symcoeff_ineq::init()
{
    if (L != NULL) delete[] L;
    L = NULL;
    unum = unum_cnt++;
    nt_c.init(1);
    nt_p.init(1);
    initL(0);
}


/***************************************************************************
 *   Initialize the system so that the data-set to have the given          *
 *   dimension.  Note: Need to assign names to symcoeffs of the planes and *
 *   variables of the columns.                                             *
 ***************************************************************************/
void named_symcoeff_ineq::init(int xp, int xr, int xc)
{
    nt_p.init(xp);
    nt_c.init(xc);
    if (L != NULL) delete[] L;
    L = NULL;
    initL(xr);
}



/***************************************************************************
 ****   Initializers                                                    ****
 ***************************************************************************/

/***************************************************************************
 *   How about a copy of an existing one?                                  *
 ***************************************************************************/
void named_symcoeff_ineq::init(const named_symcoeff_ineq & c)
{
    nt_c.init(c.const_cols());
    nt_p.init(c.const_planes());

    if (L != NULL) delete[] L;
    L = (p())?(new lin_ineq[p()]):NULL;
    for(int i=0; i<p(); i++) 
        L[i] = c.L[i];

    check();
}


/***************************************************************************
 *   Names of the planes and columns are given, data as a list of          *
 *   inequalities.                                                         *
 ***************************************************************************/
void named_symcoeff_ineq::init(name_table &p, 
                              name_table &c,
                              lin_ineq ** ll)
{
    nt_p.init(p);
    nt_c.init(c);
    initL(ll);
}


/***************************************************************************
 *   Names of the planes and columns are given, data as an explicit list   *
 *   of inequalities.                                                      *
 ***************************************************************************/
void named_symcoeff_ineq::init(name_table &p, name_table &c,
                              lin_ineq * l0, lin_ineq * l1, lin_ineq * l2,
                              lin_ineq * l3, lin_ineq * l4, lin_ineq * l5, 
                              lin_ineq * l6, lin_ineq * l7, lin_ineq * l8)
{
    nt_p.init(p);
    nt_c.init(c);
    initL(l0, l1, l2, l3, l4, l5, l6, l7, l8);
}


/***************************************************************************
 *   Names of the columns are given. Data of each plane is given with the  *
 *   name of the symcoeff for that plane.                                  *
 ***************************************************************************/
void named_symcoeff_ineq::init(name_table &c, 
                              lin_ineq * l0,
                              immed * v1, lin_ineq * l1,
                              immed * v2, lin_ineq * l2,
                              immed * v3, lin_ineq * l3,
                              immed * v4, lin_ineq * l4,
                              immed * v5, lin_ineq * l5,
                              immed * v6, lin_ineq * l6,
                              immed * v7, lin_ineq * l7,
                              immed * v8, lin_ineq * l8)
{
    nt_p.init(name_table(v1, v2, v3, v4, v5, v6, v7, v8));
    nt_c.init(c);
    initL(l0, l1, l2, l3, l4, l5, l6, l7, l8);
}


/***************************************************************************
 *   Names of the planes are given. Data of each plane is given, need to   *
 *   align the columns and find the column names.                          *
 ***************************************************************************/
void named_symcoeff_ineq::init(name_table & p,
                              named_lin_ineq ** ll)
{
    nt_p.init(p);
    initL(ll);
}


/***************************************************************************
 *   Names of the planes are given. Data of each plane is given, need to   *
 *   align the columns and find the column names.                          *
 ***************************************************************************/
void named_symcoeff_ineq::init(name_table & p,
                              named_lin_ineq * l0,
                              named_lin_ineq * l1, named_lin_ineq * l2,
                              named_lin_ineq * l3, named_lin_ineq * l4,
                              named_lin_ineq * l5, named_lin_ineq * l6,
                              named_lin_ineq * l7, named_lin_ineq * l8)
{
    nt_p.init(p);
    initL(l0, l1, l2, l3, l4, l5, l6, l7, l8);
}


/***************************************************************************
 *   Data of each plane is given with the name for the symcoeff for that   *
 *   plane.  Need to align the columns and find the column names.         *
 ***************************************************************************/
void named_symcoeff_ineq::init(const named_lin_ineq * l0,
                              immed * v1, named_lin_ineq * l1,
                              immed * v2, named_lin_ineq * l2,
                              immed * v3, named_lin_ineq * l3,
                              immed * v4, named_lin_ineq * l4,
                              immed * v5, named_lin_ineq * l5,
                              immed * v6, named_lin_ineq * l6,
                              immed * v7, named_lin_ineq * l7,
                              immed * v8, named_lin_ineq * l8)
{
    nt_p.init(name_table(v1, v2, v3, v4, v5, v6, v7, v8));
    initL(l0, l1, l2, l3, l4, l5, l6, l7, l8);
}


/***************************************************************************
 *   Only a single plane, no symcoeffs as coefficients.                    *
 ***************************************************************************/
void named_symcoeff_ineq::init(const named_lin_ineq & cc)
{
    init(&cc);
}


/***************************************************************************
 *   Convert to/from SUIF annotation format.                               *
 ***************************************************************************/
int named_symcoeff_ineq::init(immed_list & il, int c)
{
    unum = il[c++].integer();
    unum_cnt = MAX(unum_cnt, unum+1);
    int i;

    int num = il[c++].integer();
    cols().init(num);
    for(i=1; i<num; i++)
    {
       const int ind_1 = c++;
       const int ind_2 = c++;
       cols()[i].init((name_table_entry_kind)il[ind_1].integer(),il[ind_2]);
    }

    num = il[c++].integer();
    planes().init(num);
    for(i=1; i<num; i++)
    {
       const int ind_1 = c++;
       const int ind_2 = c++;
       planes()[i].init((name_table_entry_kind)il[ind_1].integer(),il[ind_2]);
    }
    if (L != NULL) delete[] L;
    L = new lin_ineq [num];
    for(i=0; i<num; i++) 
       c = ((integer_matrix *) L+i)->init(il, c);
    return c;
}


immed_list * named_symcoeff_ineq::cvt_immed_list()
{
    immed_list * lstL = new immed_list;
    int i;

    lstL->append(immed(uid()));
    lstL->append(immed(n()));
    for(i=1; i<n(); i++) {
        lstL->append(immed((int)cols()[i].kind()));
        lstL->append(cols()[i].name());
    }
    lstL->append(immed(p()));
    for(i=1; i<p(); i++) {
        lstL->append(immed((int)planes()[i].kind()));
        lstL->append(planes()[i].name());
    }
    for (i=0; i<p(); i++) {
      immed_list * iq = L[i].cvt_immed_list();
      lstL->append(iq);
    }
    return lstL;
}


/***************************************************************************
 *   get/set parts of the data from different perspectives.                *
 ***************************************************************************/
lin_ineq * named_symcoeff_ineq::get_p(int i) const
{
    lin_ineq * ret = new lin_ineq(r(i));
    return ret;
}


lin_ineq * named_symcoeff_ineq::get_m(int i) const
{
    lin_ineq * ret = new lin_ineq(p(), n());
    for(int ip=0; ip<p(); ip++)
        for(int in=0; in<n(); in++)
            (*ret)[ip][in] = r(ip).r(i).c(in);
    return ret;
}


lin_ineq * named_symcoeff_ineq::get_n(int i) const
{
    lin_ineq * ret = new lin_ineq(p(), m());
    for(int ip=0; ip<p(); ip++)
        for(int im=0; im<m(); im++)
            (*ret)[ip][im] = r(ip).r(im).c(i);
    return ret;
}


void named_symcoeff_ineq::set_p(int i, lin_ineq * l)
{       
    assert(l);
    assert(l->m() == m());
    assert(l->n() == n());
    (*this)[i] = *l;
}


void named_symcoeff_ineq::set_m(int i, lin_ineq * l)
{       
    assert(l);
    assert(l->m() == p());
    assert(l->n() == n());
    for(int ip=0; ip<p(); ip++)
        for(int in=0; in<n(); in++)
            (*this)[ip][i][in] = (*l)[ip][in];
}


void named_symcoeff_ineq::set_n(int i, lin_ineq * l)
{       
    assert(l);
    assert(l->m() == p());
    assert(l->n() == m());
    for(int ip=0; ip<p(); ip++)
        for(int im=0; im<m(); im++)
            (*this)[ip][im][i] = (*l)[ip][im];
}



constraint * named_symcoeff_ineq::get_pm(int ip, int im) const
{
    constraint * ret = new constraint(n());
    for(int in=0; in<n(); in++)
        (*ret)[in] = r(ip).r(im).c(in);
    return ret;
}

constraint * named_symcoeff_ineq::get_pn(int ip, int in) const
{
    constraint * ret = new constraint(m());
    for(int im=0; im<m(); im++)
        (*ret)[im] = r(ip).r(im).c(in);
    return ret;
}


constraint * named_symcoeff_ineq::get_mn(int im, int in) const
{
    constraint * ret = new constraint(p());
    for(int ip=0; ip<p(); ip++)
        (*ret)[ip] = r(ip).r(im).c(in);
    return ret;
}


void named_symcoeff_ineq::set_pm(int ip, int im, constraint * c)
{
    assert(c);
    assert(c->n() == n());
    for(int in=0; in<n(); in++)
        (*this)[ip][im][in] = (*c)[in];
}


void named_symcoeff_ineq::set_pn(int ip, int in, constraint * c)
{
    assert(c);
    assert(c->n() == m());
    for(int im=0; im<m(); im++)
        (*this)[ip][im][in] = (*c)[im];
}


void named_symcoeff_ineq::set_mn(int im, int in, constraint * c)
{
    assert(c);
    assert(c->n() == p());
    for(int ip=0; ip<p(); ip++)
        (*this)[ip][im][in] = (*c)[ip];
}




named_symcoeff_ineq named_symcoeff_ineq::ineq(int i)
{
    named_symcoeff_ineq ret;

    ret.init(p(), 1, n());
    ret.cols().init(cols());
    ret.planes().init(planes());
    for(int ip=0; ip < p(); ip++)
	for(int in=0; in < n(); in++)
	    (ret)[ip][0][in] = (*this)[ip][i][in];

    return ret;
}
