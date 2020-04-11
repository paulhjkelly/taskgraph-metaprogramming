/* file "named_lin_ineq.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "named_lin_ineq.h"

#include <cstdio>
#include <suif.h>
#include <suifmath.h>
#include "dependence.h"
#include <builder.h>


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
void name_table_entry::init(immed & im)
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
                            immed & im)
{  
    init(im);
    assert(knd != nte_aux);
    knd = k;

}

void name_table_entry::set_name(immed & im)
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
void name_table_entry::init(name_table_entry & nte)
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
boolean name_table_entry::operator==(name_table_entry & nte)
{ 
    if((nte.knd != knd)||
       (knd == nte_aux)) 
        return FALSE;
    return (nte.name() == name());
}


immed name_table_entry::name()
{
    if(is_var) {
        assert(vsname.v);
        return immed(vsname.v);
    } else if(vsname.s)
        return immed(vsname.s);
    else 
        return immed(0);
    return immed(0);
}

char * name_table_entry::string()
{
    if(is_var) {
        assert(vsname.v);
        return vsname.v->name();
    } else
        return vsname.s;
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
                                       
name_table::name_table(immed * v1,  immed * v2,  immed * v3,  immed * v4,
                       immed * v5,  immed * v6,  immed * v7,  immed * v8,
                       immed * v9,  immed * v10, immed * v11, immed * v12,
                       immed * v13, immed * v14, immed * v15, immed * v16) 
{
    L = NULL;
    sz = 1;
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


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table::init(name_table &nt)
{
    if(&nt == this) return;

    if(nt.n() != n()) {
        if(L) delete[] L;
        sz = nt.n();
        L = (n()-1>0)?(new name_table_entry[n()-1]):NULL;
    }
    
    for(int i=1; i<n(); i++)
        (*this)[i].init(nt[i]);
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table::init(int s)
{
    if(s != n()) {
        if(L) delete[] L;
        sz = s;
        L = (n()-1>0)?(new name_table_entry[n()-1]):NULL;
    }
    
    for(int i=1; i<n(); i++)
        (*this)[i].init();
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
int name_table::find(name_table_entry & nte)
{
    for(int i=1; i<n(); i++)
        if((*this)[i] == nte) return i;
    return -1;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
int name_table::find(immed & v)
{
    for(int i=1; i<n(); i++)
        if((*this)[i].name() == v) return i;
    return -1;
}

constraint name_table::lio_code_type()
{
    constraint c(n());
    c[0] = NM_CONSTANT;
    for(int i=1; i<n(); i++)
        switch((*this)[i].kind()) {
        case nte_symconst:
            c[i] = NM_CONSTANT;
            break;
        case nte_cond: 
        case nte_loop: 
        case nte_dim: 
        case nte_summary:
            c[i] = NM_LOCATIONS;
        case nte_aux: 
        case nte_none:
            assert(0);
        }
    return c;
}

void name_table::remove(int i, int j)
{
    assert(j>=i);
    assert((i>0)&&(j>0));
    assert((i<n())&&(j<n()));

    int lln = n()-(j-i+1)-1;
    name_table_entry * LL = (lln>0)?(new name_table_entry[lln]):NULL;

    int cnt=0;
    for(int k=1; k<i; k++)
        LL[cnt++].init((*this)[k]);
    for(k=j+1; k<n(); k++)
        LL[cnt++].init((*this)[k]);
    
    if(L) delete[] L;
    L = LL;
    assert(cnt+1 == n()-(j-i+1));
    sz = cnt+1;
}

void name_table::insert(name_table_entry & nte, int i)
{
    assert((i>=1)&&(i<=n()));
    int lln = n()-1+1;
    name_table_entry * LL = (lln>0)?(new name_table_entry[lln]):NULL;
    
    int cnt=0;
    for(int k=1; k<i; k++)
        LL[cnt++].init((*this)[k]);
    LL[cnt++].init(nte);
    for(k=i; k<n(); k++)
        LL[cnt++].init((*this)[k]);
    
    if(L) delete[] L;
    L = LL;
    assert(cnt+1 == n()+1);
    sz = cnt+1;
}

void name_table::insert(immed & v, int i)
{
    insert(name_table_entry(v), i);
}



/***************************************************************************
 * use A and change B                                                      *
 ***************************************************************************/
void name_table::change_name_types(name_table & na, name_table & nb)
{
    for(int i=1; i<nb.n(); i++) {
        immed bv(nb[i].name());
        if(bv.is_symbol() || bv.is_string()) {     // not an aux
            int j = na.find(bv);
            if(j > 0) {
                name_table_entry_kind ka = na[j].kind();
                name_table_entry_kind kb = nb[i].kind();
                name_table_entry_kind kk = nte_none;
                if(ka == kb)
                    kk = kb;
                else if(ka == nte_none)
                    kk = kb;
                else if(kb == nte_none)
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
boolean name_table::is_aligned(name_table & na, 
                                name_table & nb)
{
    if(na.n() != nb.n()) return FALSE;

    for(int i=1; i<na.n(); i++) {
        if(na[i].name() != nb[i].name()) 
            return FALSE;
        else if(na[i].kind() != nb[i].kind()) 
            return FALSE;
    }
    return TRUE;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
static int merge_two(name_table_entry_kind k, 
                     name_table & na, name_table & nb,
                     integer_row & ca, integer_row & cb,
                     int curr)
{
    for(int ai=1; ai<na.n(); ai++) 
        if(na[ai].kind()== k) {
            ca[ai] = curr++;
        }

    for(int bi=1; bi<nb.n(); bi++) 
        if(nb[bi].kind()== k) {
            boolean found = FALSE;
            for(int aj=1; ((aj<na.n())&&(!found)); aj++)      
                if(nb[bi] == na[aj]) {      // is already in A?
                    cb[bi] = ca[aj];
                    found = TRUE;
                }
            if(!found) {
                cb[bi] = curr++;     // not in A
            }
    }

    return curr;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table name_table::operator&(name_table & b)
{
    name_table * ret;
    if(is_aligned(*this, b)) {
        ret = new name_table(this);
        return *ret;
    }
    integer_row ca(n()); 
    integer_row cb(b.n());
    ret = align_tables(*this, b, ca, cb);
    return *ret;
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table * name_table::align_tables(name_table na, 
                                        name_table nb,
                                        integer_row & ca,
                                        integer_row & cb)
{
    assert(na.n() == ca.n());
    assert(nb.n() == cb.n());
    change_name_types(na, nb);
    
    for(int ii=1; ii<na.n(); ii++) 
        assert(na[ii].kind() != nte_none);
    for(ii=1; ii<nb.n(); ii++) 
        assert(nb[ii].kind() != nte_none);
    
    ca[0] = cb[0] = 0;                  // consant position does not change
    
    int curr = 1;                       // start with the 1'st element
    curr = merge_two(nte_symconst, na, nb, ca, cb, curr);
    curr = merge_two(nte_cond,     na, nb, ca, cb, curr);
    curr = merge_two(nte_loop,     na, nb, ca, cb, curr);
    curr = merge_two(nte_dim,      na, nb, ca, cb, curr);
    curr = merge_two(nte_summary,  na, nb, ca, cb, curr);
    curr = merge_two(nte_aux,      na, nb, ca, cb, curr);
    
    // Create the new name table
    name_table * newnt = new name_table(curr);
    for(int i=1; i<na.n(); i++)
        (*newnt)[ca[i]].init(na[i]);
    for(i=1; i<nb.n(); i++)
        if((*newnt)[cb[i]].kind() == nte_none)
            (*newnt)[cb[i]].init(nb[i]);
        else 
            assert((*newnt)[cb[i]].kind() == nb[i].kind());
    
    return newnt;
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
name_table * name_table::mk_align(name_table & na, 
                                   name_table & nb)
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

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::init(named_lin_ineq & c)
{
    nt.init(c.nt);
    lq = c.lq;
}


int named_lin_ineq::init(immed_list & il, int c)
{
    unum = il[c++].integer();
    unum_cnt = MAX(unum_cnt, unum+1);

    int num = il[c++].integer();
    
    names().init(num);
    for(int i=1; i<num; i++) 
       names()[i].init((name_table_entry_kind)il[c++].integer(),il[c++]);
    c = ((integer_matrix &)ineqs()).init(il, c);
    return c;
}

immed_list * named_lin_ineq::cvt_immed_list()
{
    immed_list * L = new immed_list;

    L->append(immed(uid()));
    L->append(immed(n()));
    for(int i=1; i<n(); i++) {
        L->append(immed(names()[i].kind()));
        L->append(names()[i].name());      
    }
    immed_list * iq = ((integer_matrix &)ineqs()).cvt_immed_list();
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
        lq = lq.swap_col(i, j);
    }
}

void named_lin_ineq::del_col(int i, int j)
{
    assert(j>=i);
    assert((i>0)&&(j>0));
    assert((i<n())&&(j<n()));
    
//    if(ineqs().m() == 0)
//        ineqs().init(0, ineqs().n()-(j-i));
//BUGBUG: Cannot remember why this was commented. Uncommented to get rid of a bug.

    nim_op O;
    if(ineqs().m() == 0)
        ineqs().init(0, ineqs().n()-(j-i+1));
    else if(j == n()-1) 
        ineqs().init(ineqs().resize(0, ineqs().m(),
                                    0, i));
    else
        ineqs().init(Compose(1, 2,
                             O.NIM(ineqs().resize(0, ineqs().m(),
                                                  0, i)),
                             O.NIM(ineqs().resize(0, ineqs().m(),
                                                j+1, ineqs().n()))));
    
    
    names().remove(i, j);
    n();
}


void named_lin_ineq::del_col(integer_row & rem)
{
    assert(rem.n() == n());
    for(int i=n()-1; i>=1; i--) {
        if(rem[i])
            del_col(i);
        assert(names().n() == ineqs().n());
    }

    ineqs().del_zeros();
}


void named_lin_ineq::cleanup()
{
    del_zero_cols();
    if(ineqs().m() == 0)  
        return;

    if(~ineqs() == TRUE) {
        init(named_lin_ineq());
        return;
    }


    integer_row rem(n());
    rem = 0;

    // remove aux vars that are empty.
    // also remove aux vars that are multiple of 1
    for(int i=1; i<n(); i++) 
        if(names()[i].kind() == nte_aux) {
            boolean zero_or_one = TRUE;
            for(int j=0; (j<ineqs().m())&&(zero_or_one); j++)
                if((ineqs()[j][i] != 0)&&(ABS(ineqs()[j][i]) != 1)) 
                    zero_or_one = FALSE;
            if(zero_or_one) rem[i] = TRUE;
        }

    // check if two auxilary vars are doing the same job
    for(int i1=1; i1<n(); i1++) 
        if((names()[i1].kind() == nte_aux)&&(!rem[i1]))
            for(int i2=i1+1; i2<n(); i2++) 
                if((names()[i2].kind() == nte_aux)&&(!rem[i1])) {
                    constraint filter(n());
                    filter = 0;
                    filter[i1] = 1;
                    filter[i2] = 1;             // c1*i1=a(); c2*i2=b()
                    lin_ineq L = ineqs().filter_thru(filter, 0);
                    assert(!(~L));              // has to have an answer
                    L.resize_offset(0, 1, 0, 0);  
                    L[L.m()-1] = 0;
                    L[L.m()-1][0]  = 1;
                    L[L.m()-1][i1] = 1;         // if both are the same
                    L[L.m()-1][i2] = -1;        // c1 > c2 is empty
                    if(~L) {
                        L[L.m()-1][i1] = -1;    // and c2 > c1 is empty
                        L[L.m()-1][i2] =  1;
                        if(~L) rem[i2] = TRUE;
                    }
                }

    del_col(rem);
}


void named_lin_ineq::del_zero_cols()
{
    for(int i=n()-1; i>=1; i--) {
        boolean zero = TRUE;
        for(int j=0; (j<ineqs().m())&&(zero); j++)
            if(ineqs()[j][i] != 0) zero = FALSE;
        if(zero) del_col(i);
    }   
    ineqs().del_zeros();
}


named_lin_ineq & named_lin_ineq::operator=(named_lin_ineq & c)
{
    if(&c == this) return *this;
    
    this->init(c); 
    return *this; 
}


named_lin_ineq & named_lin_ineq::operator=(lin_ineq & l)
{
    assert(l.n() == names().n());
    ineqs().init(l); 
    return *this; 
}

void named_lin_ineq::operator||(named_lin_ineq & c)
{
    align_named_lin_ineqs(*this, c);
}

boolean named_lin_ineq::operator==(named_lin_ineq & c2)
{ 
    if(!is_aligned(*this, c2))
        return FALSE;

    if((this->ineqs().m() == 0)&&(c2.ineqs().m() == 0)) 
        return TRUE;
    if((this->ineqs().m() == 0)||(c2.ineqs().m() == 0)) 
        return FALSE;
    return ((c2 >> (*this))&&
            ((*this) >> c2)); 
}



named_lin_ineq named_lin_ineq::operator&(named_lin_ineq & c)
{
    named_lin_ineq a1(this);
    named_lin_ineq a2(c);
    a1||a2;
    
    if(a1.ineqs().m() == 0) {                           // A1=0 A2==?
        if(a2.ineqs().m() != 0)                         // A1=0 A2>0
            a1.ineqs().init(a2.ineqs());
    } else {                                            // A1>0 A2==?
        if(a2.ineqs().m() != 0)                         // A1>0 A2>0
            a1 &= a2.ineqs();
                                                        // else A1>0 A2==0
    }
    return a1;
}

// Deletes are there to save memory
named_lin_ineq * named_lin_ineq::and(named_lin_ineq * c1, 
                                     named_lin_ineq * c2, 
                                     boolean del1,      
                                     boolean del2)
{
    if((c1==NULL)&&(c2==NULL)) return NULL;
    if(c1==NULL) {
        if(del2) return c2;
        return new named_lin_ineq(c2);
    }
    if(c2==NULL) {
        if(del1) return c1;
        return new named_lin_ineq(c1);
    } else {
        named_lin_ineq * ret = new named_lin_ineq((*c1)&(*c2));
        if(del1) delete c1;
        if(del2) delete c2;
        return ret;
    }
}


named_lin_ineq & named_lin_ineq::operator&=(named_lin_ineq & c)
{
    named_lin_ineq cc(c);
    (*this)||cc;
    (*this) &= cc.ineqs();
    return *this; 
}

named_lin_ineq & named_lin_ineq::operator&=(lin_ineq & l)
{
    assert(l.n() == names().n());
    nim_op O;
    ineqs().init(Compose(2, 1,
                         O.NIM(ineqs()),
                         O.NIM(l))); 
    return *this; 
}

boolean named_lin_ineq::operator>>(named_lin_ineq & c)
{
    assert(is_aligned(*this, c));

    // check if no intersection
    if(~(ineqs()||c.ineqs())) return FALSE;

// BUGBUG
    return (this->ineqs()>>c.ineqs());

    integer_row aux(n());       
    aux = 0;
    for(int i=1; i<n(); i++) 
        if(names()[i].kind() == nte_aux) aux[i] = 1;
    return FALSE;
}




void named_lin_ineq::find_bounds()
{
    poly_iterator Poly(ineqs());

    constraint del_list(ineqs().n());
    del_list = 0;
    for(int a=1; a< ineqs().n(); a++)
        if(names()[a].kind() == nte_symconst)
            del_list[a] = -1;
    Poly.set_sort_order(del_list.data_array());
    
    assert(!~ineqs());
    lin_ineq M = Poly.get_iterator(0);
    assert(!~M);
    M = Poly.reduce_extra_constraints2();
    assert(!~M);
    M.sort();
    ineqs().init(M);
}


void named_lin_ineq::add_col(name_table_entry & nte, int i)
{
    names().insert(nte, i);
    nim_op O;
    if(ineqs().m() == 0)
        ineqs().init(0, names().n());
    else if(i == ineqs().n())
        ineqs().init(Compose(1, 2,
                             O.NIM(ineqs()),
                             O.NIM(ineqs().m(), 1)));
    else 
        ineqs().init(Compose(1, 3,
                             O.NIM(ineqs().resize(0, ineqs().m(),
                                                  0, i)),
                             O.NIM(ineqs().m(), 1),
                             O.NIM(ineqs().resize(0, ineqs().m(),
                                                  i, ineqs().n()))));

    assert(ineqs().n() == names().n());
}


void named_lin_ineq::project()
{
    lin_ineq L(ineqs());

    lin_ineq del_list(1, n());
    del_list[0] = 0;

    // set which are symbolic constants
    for(int a=1; a<L.n(); a++)
        if(names()[a].kind() == nte_symconst)
            del_list[0][a] = -1;
        else
            del_list[0][a] = 1;

    // project
    poly_iterator Poly(L);
    Poly.set_sort_order(del_list[0].data_array());
    L = Poly.get_iterator(0);
    L = Poly.reduce_extra_constraints2();
    L.sort();

    ineqs().init(L);

    cleanup();
}

void named_lin_ineq::project_away(immed & var)
{
    name_table N;
    N.insert(name_table_entry(var), 1);
    project_away(N);
}


void named_lin_ineq::project_away(name_table & N)
{
    lin_ineq L(ineqs());

    lin_ineq del_list(1, n());
    del_list[0] = 0;

    // set which are symbolic constants
    for(int a=1; a<L.n(); a++)
        if(names()[a].kind() == nte_symconst)
            del_list[0][a] = -1;
        else
            del_list[0][a] = 1;

    constraint filter(L.n());
    filter = 0;
    lin_ineq  * perm = NULL;
    for(int i=1; i<N.n(); i++) {                // for each name to be deleted
        int pos = names().find(N[i].name());     // position of that name
        if(pos > 0) {
            filter[pos] = 1;                    // filter anything in that pos
            lin_ineq pt = colswitch(L, pos, L.n()-i); // switch it to the end
            if(perm) 
                *perm = (*perm) * pt;
            else
                perm = new lin_ineq(pt);
         }
    }

    if(perm == NULL) return;                 // not a single thing to project
    
    // change the ineqs and del_list to the current ordering
    L = L*(*perm);
    del_list = del_list*(*perm);

    // project
    poly_iterator Poly(L);
    Poly.set_sort_order(del_list[0].data_array());
    L = Poly.get_iterator(0);
    L = Poly.reduce_extra_constraints2();
    L.sort();

    // back to normal ordering
    L = L*perm;    

    L = L.filter_away(filter, 0);

    ineqs().init(L);

    for(i = filter.n()-1; i>0; i--) 
        if(filter[i])
            del_col(i);

    cleanup();
    delete perm;
}


boolean named_lin_ineq::operator~()
{
    return ~ineqs();
}

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::print()
{
//    printf("       ");
//    for(int i=1; i<names().n(); i++) {
//        printf("%6s", (names()[i].map())?names()[i].map()->name():"       ");
//        printf("%s", (names()[i].kind() == nte_loop)?".":" ");
//    }
//    for(; i<9; i++) printf("       ");
//    printf("%3d %s  <%X>\n", uid(), (is_killed()?"XXX":"   "), get_home());

    printf("       ");
    for(int i=1; i<names().n(); i++)
        printf("%7s", (names()[i].string())?names()[i].string():"<aux>");
    printf("\n");
    ineqs().print();
    printf("\n");
}

void named_lin_ineq::print_exp(print_exp_type type)
{
    if(type == pet_single) assert(ineqs().m() == 1);
    
    for(int x=0; x<ineqs().m(); x++) {
        switch(type) {
        case pet_system_nl:
            break;
        case pet_system:
            if(ineqs().m() > 1) 
                printf("(");
            break;
        case pet_min:
        case pet_max:
            if((ineqs().m() > 1) && (x==0))
                printf("%s(", (type==pet_min)?"MIN":"MAX");
            break;
        default:
            break;
        }
            
        int pr = 0;
        integer_row R = ineqs()[x];
        if(R[0]) {
            printf("%d ", R[0]);
            pr = 1;
        }
        
        for(int i=1; i<n(); i++) 
            if(R[i]) {
                if(R[i] == 1) {
                    if(pr)
                        printf("+ ");
                } else if(R[i] == -1) {
                    if(pr)
                        printf("- ");
                    else
                        printf("-");
                } else if(R[i] > 0) {
                    if(pr)
                        printf("+ %d", R[i]);
                    else
                        printf("%d", R[i]);
                } else {
                    if(pr)
                        printf("- %d", -R[i]);
                    else
                        printf("%d", R[i]);
                }
                printf("%s ", names()[i].string());
                pr = 1;
            }
        if(pr==0) printf("0 ");
        switch(type) {
        case pet_system:
            printf(">= 0");
            if(ineqs().m() > 1) printf(")");
            if(x < ineqs().m()-1)
                printf("&&");
            break;
        case pet_system_nl:
            printf(">= 0\n");
            break;
        case pet_min:
        case pet_max:
            if(x < ineqs().m()-1)
                printf(", ");
            if((ineqs().m() >1)&&(x == ineqs().m()-1))
                printf(")");
            break;
        default:
            break;
        }
    }
}


instruction * named_lin_ineq::mk_bounds()
{
    block bnf;
    
    constraint filter(n());
    filter = 0;
    for(int i=1; i<n(); i++)
        if(names()[i].kind() == nte_aux) 
            filter[i] = 1;
    lin_ineq Laux    = ineqs().filter_thru(filter, -1); // only A - aux >= 0 
    lin_ineq Lbounds = ineqs().filter_away(filter, 0);
    
    boolean first = TRUE;
    
    for(i=0; i<Lbounds.m(); i++) {
        block stmt(Lbounds[i][0]);
        for(int j=1; j<n(); j++) 
            if(names()[j].kind() != nte_aux) {
                int val = Lbounds[i][j];
                block sym((var_sym *)names()[j].name().symbol());
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
            if(names()[j].kind() != nte_aux) {
                block sym((var_sym *)names()[j].name().symbol());
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
    
    bnf.print();
    instruction * ins = bnf.make_instruction();
    return ins;
}


instruction * named_lin_ineq::create_expression(immed & v, 
                                                boolean is_ub,
                                                block_symtab * sym)
{
    assert(ineqs().m() > 0);
    int pos = find(v);
    assert(pos);
 
    constraint filter(n());
    filter = 0;
    filter[pos] = 1;
    lin_ineq L = ineqs().filter_thru(filter, (is_ub)?-1:1);
    if(!is_ub) L *= -1;

    block exp;
    for(int i=0; i<L.m(); i++) {
        block curr(L[i][0]);
        int val = -1*L[i][pos];    
        L[i][pos] = 0;
        for(int j=1; j<n(); j++) 
            if(names()[j].name().is_symbol() || names()[j].name().is_string()) { 
                block var((var_sym *)names()[j].name().symbol());
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
    instruction * ret = (sym)?exp.make_instruction(sym):exp.make_instruction();
    return ret;
}



/***************************************************************************
 *                                                                         *
 ***************************************************************************/
boolean named_lin_ineq::is_aligned(named_lin_ineq & A, 
                                    named_lin_ineq & B)
{
    name_table & na = A.names();
    name_table & nb = B.names();

    return name_table::is_aligned(na, nb);
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void named_lin_ineq::align(name_table & ant)
{
    cleanup();

    if(name_table::is_aligned(ant, names())) return;

    named_lin_ineq tmp(this);
    integer_row rmap(n());
    rmap[0] = 0;
    for(int i=1; i<n(); i++) {
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
void named_lin_ineq::align_named_lin_ineqs(named_lin_ineq & A, 
                                            named_lin_ineq & B)
{
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
    for(int i=0; i<A.lq.m(); i++)
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



/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::merge_named_lin_ineqs(named_lin_ineq & A, 
                                                       named_lin_ineq & B)
{
    named_lin_ineq * nA = new named_lin_ineq(A);
    named_lin_ineq * nB = new named_lin_ineq(B);
    align_named_lin_ineqs(*nA, *nB);

    (*nA) &= nB->ineqs();

    delete nB;
    return nA;
}



void convert_const(named_lin_ineq * c)
{
    char * st;

    if(c == NULL) return;
    for(int i=1; i<c->n(); i++) {
        immed v(c->names()[i].name());

        if (v.is_symbol())
            st = v.symbol()->name();
        else if (v.is_string())
            st = v.string();
        else      // an aux
            continue;

        if(strncmp(st, "c__", 3) == 0) {
            int val;
            sscanf(&st[3], "%d", &val);
            for(int j=0; j<c->ineqs().m(); j++) {
                c->ineqs()[j][0] += c->ineqs()[j][i]*val;
                c->ineqs()[j][i] = 0;
            }
        }
    }
    c->cleanup();
}

  
/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::mk_named_lin_ineq(array_info & ai,     
                                                   immed & ind, 
                                                   boolean lb)
{
    named_lin_ineq * ret = NULL;
    array_info_iter iter(&ai);
    while(!iter.is_empty()) {
        access_vector * av = iter.step();
        if(av->too_messy) 
            return NULL;

        named_lin_ineq * curr = mk_named_lin_ineq(*av, ind, lb);
        ret = named_lin_ineq::and(ret, curr, 1, 1);
    }
    convert_const(ret);
    return ret;
}



/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::mk_named_lin_ineq(access_vector & av, 
                                                   immed & ind, 
                                                   boolean lb)
{
    if(av.too_messy) return NULL;

    named_lin_ineq * ret = new named_lin_ineq;

    int ncol = 1 + av.conregs.count() + av.elts.count() + 1;

    ret->nt.init(ncol);
    ret->lq.init(1, ncol);

    ret->lq[0][0] = av.con;

    int cnt = 1;
    access_list_iter iter1(&av.conregs);
    while(!iter1.is_empty()) {
        access_list_e *e = iter1.step();
        ret->lq[0][cnt] = e->val();
        var_sym * v = (var_sym *) e->var();
        ret->nt[cnt++].init(immed(v));
    }
    
    access_list_iter iter2(&av.elts);
    while(!iter2.is_empty()) {
        access_list_e *e = iter2.step();
        ret->lq[0][cnt] = e->val();
        tree_node *n = (tree_node *) e->var();
        var_sym * v = ((tree_for *)n)->index();
        ret->nt[cnt].init(immed(v));
        ret->nt[cnt++].mark_sym();
    }

    ret->lq[0][cnt] = -1;
    ret->nt[cnt].init(ind);
    ret->nt[cnt].mark_sym();
    if(lb) ret->lq[0] *= -1;

    convert_const(ret);
    return ret;
}





/* ##################################################
   #####   other routines for debugging only    #####
   ################################################## */

/***************************************************************************
 *                                                                         *
 ***************************************************************************/
void name_table_entry::print(FILE * fp)
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


void name_table::print(FILE * fp)
{
    for(int i=1; i<n(); i++) (*this)[i].print(fp);
    fprintf(fp, "\n");
}


named_lin_ineq * include_for(tree_for * tf, named_lin_ineq * c)
{
    dep_for_annote *dfa = 
        (dep_for_annote *) tf->peek_annote(k_dep_for_annote);
    assert(dfa);

    immed idx(tf->index());
    named_lin_ineq * cl = named_lin_ineq::mk_named_lin_ineq(*dfa->lb, idx, TRUE); 
    if(cl == NULL) {
        printf("Unknown lower bound for loop at line %d\n", 
            source_line_num(tf));
        if(c) return new named_lin_ineq(c);
        return NULL;
    }
    named_lin_ineq * cu = named_lin_ineq::mk_named_lin_ineq(*dfa->ub, idx, FALSE);
    if(cu == NULL) {
        printf("Unknown upper bound for loop at line %d\n", 
            source_line_num(tf));
        delete cl;
        if(c) return new named_lin_ineq(c);
        return NULL;
    }
    
    cu = named_lin_ineq::and(cl, cu, 0, 1);
    
    named_lin_ineq * stp = named_lin_ineq::mk_named_lin_ineq(*dfa->stp, idx, FALSE);
    boolean stpbad = TRUE;
    if(stp)
        if((stp->n() == 2)&&                            // no sym variables
           (stp->ineqs().m() == 1)) {                   // no max or mins
            stpbad = FALSE;
            if(stp->ineqs()[0][0] != 1) {               // non-unit step
                int stride = stp->ineqs()[0][0];
                named_lin_ineq str(cl);        
                name_table_entry aux;
                aux.mark_aux();
                str.add_col(aux, str.n());
                str.ineqs()[0][str.n()-1] = stride;
                str &= str.ineqs()*-1;
                cu = named_lin_ineq::and(cu, &str, 1, 0);
            }
        }
    if(stpbad) {
        printf("Unknown or symbolic step size for loop at line %d\n", 
            source_line_num(tf));
        delete cl;
        delete cu;
        if(stp) delete stp;
        if(c) return new named_lin_ineq(c);
        return NULL;
    }
    
    cu = named_lin_ineq::and(c, cu, 0, 1);
    cu->cleanup();
    
    delete cl;
    delete stp;

    return cu;
}
