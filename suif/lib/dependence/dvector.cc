/* file "dvector.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

//dvector.c
#define _MODULE_ "libdependence.a"
#pragma implementation "dvector.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"

//
// dvector.c
//
// routines to create a distance vector
// taking as input two array instructions
//

// a1 and a2 are two memory references which have the same base array
// --- possibly NULL or unknown
// if a1 and a2 are array accesses, generate the access vectors using
//	gcd
// if they are not, try to determine if they are independent, if not
//	dependence vector is (*)

//
// interface the access vectors to the gcd program and find
// the distance vector
//
int Gcd(coeff, coeff **);
//overload int is_good_symb(access_vector *av1,access_list *symbols);
access_list *unite_unequals(access_list *a1, access_list *a2);
//overload int inline is_good_symb(access_vector *av1);


dvlist::dvlist(array_info *a1, array_info *a2,
         int minnest, int valid_dim, tn_list * al, int lexpos, int flow)
{
    dv_calc(a1,a2,minnest,valid_dim,al,flow);
    if(lexpos) make_lexpos();
}

distance_vector::distance_vector(array_info *a1, array_info *a2,
	int minnest, int valid_dim, tn_list * al,int flow)
{
    dvlist *dvl=new dvlist(a1,a2,minnest,valid_dim,al,flow);
    dvl->collapse(this);
    delete dvl;
}

static int dim_is_ok(access_vector *a,tn_list *al)
{
    if(a->too_messy)
        return 0;
    alist_iter ai(&a->elts);
    while(!ai.is_empty()) {
        alist_e *ae = ai.step();
        if((long)ae->info != 0 && !al->lookup((tree_node *)ae->key))
            return 0;
    }
    return 1;
}

static int wierd_constant_fields_same(access_vector *av1,access_vector *av2)
{
    av_compare_info ans(av1,av2);
    return ans.same_paths() && ans.same_conregs() && ans.same_memregs();
}


void dvlist::clear()
{
    dvlist_iter iter(this);
    while(!iter.is_empty()) {
	dvlist_e * de = iter.step();
	if(de->dv) delete de->dv;
	remove(iter.cur_elem());
	delete de;
    }
    glist::clear();
}






dvlist::dvlist(array_info *a1, array_info *a2, tn_list *al,int lexpos, int flow)
{
    int valid_dim = 0;
    array_info *ai1 = new array_info;
    array_info *ai2 = new array_info;
    array_info_iter aii1(a1);
    array_info_iter aii2(a2);
    while(!aii1.is_empty()) {
        assert(!aii2.is_empty());
        access_vector *av1 = aii1.step();
        access_vector *av2 = aii2.step();
        if(dim_is_ok(av1,al) && dim_is_ok(av2,al)) {
//		   wierd_constant_fields_same(av1,av2)) {
            valid_dim++;
            ai1 -> append(new access_vector(av1));
            ai2 -> append(new access_vector(av2));
        }
    }
    dv_calc(ai1,ai2,al->count(),valid_dim,al,flow);
    if(lexpos) make_lexpos();
    delete ai1;
    delete ai2;

}


distance_vector::distance_vector(array_info *a1,array_info *a2,
			tn_list *al,int flow)
{
    int valid_dim = 0;
    array_info *ai1 = new array_info;
    array_info *ai2 = new array_info;
    array_info_iter aii1(a1);
    array_info_iter aii2(a2);
    while(!aii1.is_empty()) {
        assert(!aii2.is_empty());
        access_vector *av1 = aii1.step();
        access_vector *av2 = aii2.step();
        if(dim_is_ok(av1,al) && dim_is_ok(av2,al) &&
           wierd_constant_fields_same(av1,av2)) {
            valid_dim++;
            ai1 -> append(new access_vector(av1));
            ai2 -> append(new access_vector(av2));
        }
    }
    dvlist *dvl=new dvlist(ai1,ai2,al->count(),valid_dim,al,flow);
    dvl->collapse(this);
    delete ai1; delete ai2;
}

void dvlist::dv_calc(array_info *a1, array_info *a2,
			int minnest, int valid_dim, tn_list * al,int flow)
{
//=    printf("a1:"); a1->print();
//=    printf("\na2:"); a2->print();
//=    printf("\nminnest=%d, valid_dim=%d, flow=%d\n", minnest, valid_dim, flow);
/*    assert(al);
    tn_list_iter titer(al);
    printf("[");
    while(!titer.is_empty()) {
        tree_node * tn = titer.step();
        assert(tn->is_for());
        printf("%s ", ((tree_for*)tn)->index());
    }
    printf("]\n"); */

    array_info_iter * a1i, *a2i;
    access_vector * av1, * av2;
    tn_list_iter * loops;
    tn_list_iter * loops2;
    dependency_test *dt=new dependency_test();
    set_dep();  // initialization
    int DD = 2*minnest;
    int *write_used = new int[minnest]; // which loops are used by
    // the write, assume a1 is
    // the write

    valid_dim = elim_bad_symb(a1, a2); // dims with bad symbols
    if (!valid_dim && flow) {
        dir_array *da = new dir_array(minnest);
        int *is_used = new int[2*minnest];
        for (int i=0; i<minnest; i++) {
            da->data[i].set_direction(d_star);
            is_used[2*i] = 0;
            is_used[2*i+1] = 0;
        }
        dir_list *dl = new dir_list(da);
        dl->flow_reduce(is_used);
        dir_array *da2;
        while ((da2=dl->pop())) {
            distance_vector *dv2 = new distance_vector();
            for (int s=0; s<da2->size; s++) {
                distance dis=da2->data[s];
                dv2->append(dis);
            }
            push(new dvlist_e(dv2));
        }
        return;
    }


    access_list *symbols = find_symbs(a1,a2);
    int num_symb = symbols->count();
    DD += num_symb;
/*
  printf("\nThe two references are \n");
  a1->print(stdout); printf("\n");
  a2->print(stdout); printf("\n");
  */
// find the bounds from al and shove into L and U
    integer_matrix *L = new integer_matrix();
    integer_matrix *U = new integer_matrix();
    boolean *lb_valid = new boolean[DD+1];  // does this bound exist
    boolean *ub_valid = new boolean[DD+1];
    void SetLU(tn_list *, int, integer_matrix *, integer_matrix * ,boolean * ,
               boolean *,access_list *,int,int flow=0);
    SetLU(al,DD,L,U,lb_valid,ub_valid,symbols,num_symb,flow);

// throw out constant dimensions
    a1i = new array_info_iter(a1);
    a2i = new array_info_iter(a2);

    int has_const_dim = 0;
    // throw out constant dimensions
    array_info *newa1 = new array_info();
    array_info *newa2 = new array_info();
    while(!a1i->is_empty()) {
        assert(!a2i->is_empty());
        av1=a1i->step();
        av2=a2i->step();
        av_compare_info avc(av1,av2);
        // will subracting the two will create a constant
        if (av1->elts.is_empty() && av2->elts.is_empty() &&
            avc.same_conregs() && avc.same_memregs() &&
            avc.same_paths()) {
            if (av1->con != av2->con) {
                if (minnest > 0) dt->const_test++;
                set_indep();
                //printf("It's independent \n");
                return;
            }
            has_const_dim = 1;
            valid_dim--;
        } else {
            newa1->append(av1);
            newa2->append(av2);
        }
    }
    delete a1i; delete a2i;
    a1->clear(); a2->clear();

    // degenerate case
    if (valid_dim == 0) {  // set all elements to star
        if (has_const_dim && (minnest >0)) dt->const_test++;
        distance_vector *dv = new distance_vector();
        for (int i=0; i<minnest; i++) {
            distance *d = new distance();
            distance_vector_e *temp = new distance_vector_e(*d);
            dv->push(temp);
        }
        push(new dvlist_e(dv));
//=        printf("The result is1 \n"); this->print(stdout); printf("\n");
        return;
    }

    a1i = new array_info_iter(newa1);
    a2i = new array_info_iter(newa2);

// check which induction variables are actually used and get a count
    int *loop_used = new int[2*minnest+num_symb];
    void SetLoopUsed(int *,int ,int,int *, int *, array_info_iter *,
                     array_info_iter*,tn_list *,integer_matrix *,
                     integer_matrix*,int,int *);
    SetLoopUsed(loop_used,minnest,valid_dim,lb_valid,ub_valid,
                a1i,a2i,al,L,U,num_symb,write_used);
    int loop1_num = 0;
    int loop2_num = 0;
    int i;
    for (i=2*(minnest-1); i>=0; i-=2) {
        if (loop_used[i+num_symb]) loop1_num++;
    }
    for (i=2*minnest-1; i>=0; i-=2) {
        if (loop_used[i+num_symb]) loop2_num++;
    }
    int num_used = loop1_num + loop2_num;

    // throw out unused variables from write_used
    assert(num_used%2 == 0);
    int i2=0;
    for (i=0; i<num_used/2; i++) {
        if (loop_used[2*i+num_symb] || loop_used[2*i+num_symb+1]) {
            write_used[i2++] = write_used[i];
        }
    }
//=    printf("L:\n");L->print();
//=    printf("U:\n");U->print();
//=    for(int x=0; x<DD+1; x++)
//=        printf("(%d %d)", lb_valid[x], ub_valid[x]);
// put equations into a coeff A
    coeff A;
    coeff *X=new coeff;
    int total_vars = 2*minnest+num_symb;
    X->vars = new int[total_vars*total_vars];
    X->constant = new int[total_vars];
    A.n = num_used+num_symb;
    A.m = valid_dim;
    A.vars = new int[A.n*A.m];
    A.constant = new int[valid_dim];

/* now set the A matrix, eliminating unused variables */
    a1i = new array_info_iter(newa1);
    a2i = new array_info_iter(newa2);
    for (i=0; i<valid_dim; i++) {
        av1 = a1i -> step();
        av2 = a2i -> step();
	assert(av1 && av2);
        loops = new tn_list_iter(al);
        loops2 = new tn_list_iter(al);
        int k=2*minnest-1;
	int j;
        for (j=num_used-1; j>=0; j--) {
            int j2 = j+num_symb;
            while(!loop_used[k+num_symb]) {
                if ((k %2) ==0) av1->val(loops->step());
                else av2->val(loops2->step());
                k--;
            }
            if ((k % 2) == 0)  {
                A.vars[j2*valid_dim+i] = av1->val(loops -> step());
            } else {
                A.vars[j2*valid_dim+i] = -av2->val(loops2 -> step());
            }
            k--;
//=            printf("%d %d %d---\n", i, j, k); A.print();
        }
        delete loops; delete loops2;
        A.constant[i] = av1->con - av2->con;

        // add in symbolic components
        access_list_iter temp_symb(symbols);
        for (j=num_symb-1; j>=0; j--) {
            assert(!temp_symb.is_empty());
            access_list_e *e = temp_symb.step();
            A.vars[j*valid_dim+i] = av1->conregs.val(e->var())-
                av2->conregs.val(e->var());
//=            printf("%d %d ---\n", i, j); A.print();
        }
    }

    //eliminate unused variables for L and U
    int olddd = DD;
    DD = num_used+num_symb;
    int numlow = 0; // number of lower bounds
    int numup = 0;
    for (i=1; i<=olddd; i++) {
        if (loop_used[i-1]) {
            numlow += lb_valid[i];
            numup += ub_valid[i];
        }
    }
    integer_matrix *L2 = new integer_matrix();
    integer_matrix *U2 = new integer_matrix();
    L2->init(num_symb+numlow+1,DD+1);
    U2->init(num_symb+numup+1,DD+1);
    int oldlow,oldup,newlow,newup;
//=    printf("L:\n");L->print();
//=    printf("U:\n");U->print();
    oldlow = oldup = newlow = newup = 1;
    i2 = 1;
    for (i=1; i<=olddd; i++) {
        if (loop_used[i-1]) {
            lb_valid[i2] = lb_valid[i]; ub_valid[i2] = ub_valid[i];
	    int a;
            for (a=1; a<= lb_valid[i]; a++, oldlow++, newlow++) {
                (*L2)[newlow][0] = (*L)[oldlow][0];
                int j2=1;
		int j;
                for (j=1; j<i; j++) {
                    if (loop_used[j-1]) {
                        (*L2)[newlow][j2] = (*L)[oldlow][j];
                        j2++;
                    }
                }
            }
            for (a=1; a<= ub_valid[i]; a++, oldup++, newup++) {
                (*U2)[newup][0] = (*U)[oldup][0];
                int j2=1;
		int j;
                for (j=1; j<i; j++) {
                    if (loop_used[j-1]) {
                        (*U2)[newup][j2] = (*U)[oldup][j];
                        j2++;
                    }
                }
            }
            i2++;
        } else {
            oldlow += lb_valid[i];
            oldup += ub_valid[i];
        }
    }
    delete L; delete U;
    L = L2; U = U2;
//=    printf("L:\n");L->print();
//=    printf("U:\n");U->print();
//=    A.print();

    // check degenerate cases
    if (minnest == 0) { // if all constants equal then set to star
        // else set to independent
        for (i=0; i<valid_dim; i++) {
            if (A.constant[i] != 0) {
                set_indep();
                break;
            }
        }
	// do Gcd
    } else if (Gcd(A,&X)) {
        distance_vector *dv = new distance_vector();
        for (i=num_used/2-1; i >= 0; i--) { // once for each d_vec dim
            int i2 = 2*i+num_symb;
            int done = FALSE;
            distance *d = NULL;
            for (int j=0; j<X->n && !done; j++) {
                if (X->vars[j*X->m+i2] !=
                    X->vars[j*X->m+i2+1]) {
// non constant distance vector
                    d = new distance(); // star
                    done = TRUE;
                }
            }

            if (!done) {
                int c=X->constant[i2+1]-X->constant[i2];
                d = new distance(-c);
            }
            distance_vector_e *temp = new distance_vector_e(*d);
            dv->push(temp);
        }

        if (dt->exact()) {
// convert from coefficient structure to integer_matrix needed by exact
            integer_matrix S;
            S.init(DD+1,DD+1);
            for (int l=0; l<X->m; l++) {
                assert(l+1 < (DD+1));
                S[l+1][0] = X->constant[l];
                for (int m=0; m<X->n; m++) {
                    assert((m+1) < (DD+1));
                    S[l+1][m+1]=X->vars[m*X->m + l];
                }
            }
// convert direction vector to dir_array needed by exact
            dir_array *da = new dir_array(num_used/2);
            boolean *const_dist = new boolean[num_used/2];
            distance_vector_iter di(dv);
            for (i=0; i<num_used/2; i++) {
                distance d;
                d = di.step()->d;
                da->data[i].set_direction(d.dir());
                const_dist[i] = d.is_const();
            }

            exact *ex = new exact(&S,DD,da,const_dist,L,U,
                                  lb_valid,ub_valid,num_symb,flow,write_used);
            if (ex->is_indep()) {
//=                printf("ex makes it independent \n");
                set_indep();
            } else {
//= vvv
/*                printf("Gcd gave ");
                dv->print(stdout); printf("\n");
                printf("The set of direction vectors is\n");
                ex->directions()->print(stdout);
                printf("\n");
                */
//= ^^^

                dir_list *dl = ex->directions();
                if (!indep()) dl->add_unused(loop_used+num_symb,
                                             2*minnest);
                if (flow) {
                    dl->flow_reduce(loop_used+num_symb);
                }
                while(dl) {
                    distance_vector *dv2 =
                        new distance_vector();
                    dir_array *da = dl->data;
                    dl = dl->next;
                    distance_vector_iter iter1(dv);
                    for (int s=0; s<da->size; s++) {
                        distance dis;
                        if (loop_used[num_symb+2*s]) {
                            assert(!iter1.is_empty());
                            dis = iter1.step()->d;
                            if (!dis.is_const()) {
                                dis = da->data[s];
                            }
                        } else {
                            dis=da->data[s];
                        }
                        dv2->append(dis);
                    }
                    push(new dvlist_e(dv2));
                }
                delete dl;
            }
            delete ex;
        } else {
            dv->add_unused(loop_used+num_symb, 2*minnest);
            push(new dvlist_e(dv));
        }
    } else {
        set_indep();
    }
    delete a1i; delete a2i; delete newa1; delete newa2;
    delete[] loop_used; delete L; delete U;
    delete symbols;
//=    printf("The result is2 \n"); this->print(stdout); printf("\n");
}


// collapse a list of direction vectors into 1
// by oring in the directions
void dvlist::collapse(distance_vector *output)
{
    if (this->indep()) output->set_indep();
    else {
        output->set_dep();
        int first_time=1;
        dvlist_iter iterator(this);
        for (; !iterator.is_empty();) {
            distance_vector *dv = iterator.step()->dv;
            if (first_time) {
                first_time = 0;
                distance_vector_iter iter1(dv);
                while(!iter1.is_empty()) {
                    output->append(iter1.step()->d);
                }
            } else {
                distance_vector_iter iter1(output);
                distance_vector_iter iter2(dv);
                while(!iter1.is_empty()) {
                    assert(!iter2.is_empty());
                    distance *d1 = &iter1.step()->d;
                    distance *d2 = &iter2.step()->d;
                    if (d1->is_const() && d2->is_const()) {
                        assert(d1->dist()==d2->dist());
                    } else {
                        direction newd = direction(d1->dir()|d2->dir());
                        d1->set_direction(newd);
                    }
                }
            }
        }
    }
}




//
// initialize a distance vector to the same as another
//
distance_vector::distance_vector(distance_vector * dv)
{
    if (dv -> indep()) {
        set_indep();
    } else {
        set_dep();
        distance_vector_iter iterator(dv);
        for(; !iterator.is_empty();) {
            append(iterator.step()->d);
        }
    }
}


//
// print a distance vector
//
void distance_vector::print(FILE *str)
{
    if (indep()) {
        fprintf(str,"These vectors are independent \n");
    } else {
        fprintf(str,"( ");
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            iterator.step()->d.print(str);
        }
        fprintf(str,")\n");
    }
}


//
// print a dvlist
//
void dvlist::print(FILE *str)
{
    if (indep()) {
        fprintf(str,"These vectors are independent \n");
    } else {
        dvlist_iter iterator(this);
        for(; !iterator.is_empty();) {
            iterator.step()->dv->print(str);
        }
    }
}


// print a distance
void distance::print(FILE *str)
{
    if (is_const()) {
        fprintf(str,"%d ",dist());
        if(dist() > 0) assert(dir() == d_lt);
        else if(dist() == 0) assert(dir() == d_eq);
        else assert(dir() == d_gt);
    } else {
        direction d=dir();
        switch(d) {
        case d_lt: fprintf(str,"+ "); break;   // lt
        case d_gt: fprintf(str,"- "); break;   // gt
        case d_eq: assert(0);
        case d_le: fprintf(str,"+/0 "); break;   // le
        case d_ge: fprintf(str,"0/- "); break;   // ge
        case d_lg: fprintf(str,"+/- "); break;   // lg
        case d_star: fprintf(str,"* "); break;
        }
    }
}


//
// is distance vector 0
//
int distance_vector::is_zero()
{
    if (indep()) {
        return TRUE;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            if (!(iterator.step() -> is_zero())) {
                return FALSE;
            }
        }
        return TRUE;
    }
}

/* turn leading >= into = */
void distance_vector::make_pos()
{
    distance d;
    distance_vector_e *dve;
    if (indep()) {
        return;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            dve = iterator.step();
            d = dve -> d;
            if (d.dir() == d_lt) {
                return;
            }
            if (d.dir() == d_gt) {
                return;
            }
            if (d.dir() == d_ge) {
                dve -> d.set_direction(d_eq);
            }
            if (d.dir() == d_lg) {
                return;
            }
            if (d.dir() == d_star) {
                return;
            }
            if (d.dir() == d_le) {
                return;
            }
            /* (d.dir() == d_eq) */
        }
        return;
    }
}

//
// is distance vector positive
//
int distance_vector::is_pos()
{
    distance d;

    if (indep()) {
        return FALSE;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            d = iterator.step() -> d;
            if (d.dir() == d_lt) {
                return TRUE;
            }
            if (d.dir() == d_gt) {
                return FALSE;
            }
            if (d.dir() == d_ge) {
                return FALSE;
            }
            if (d.dir() == d_lg) {
                return FALSE;
            }
            if (d.dir() == d_star) {
                return FALSE;
            }
            if ((d.dir() == d_le) || (d.dir() == d_eq));
        }
        return FALSE;
    }
}

//
// is distance vector negative
//
int distance_vector::is_neg()
{
    distance d;

    if (indep()) {
        return FALSE;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            d = iterator.step() -> d;
            if (d.dir() == d_gt)
                return TRUE;
            if (d.dir() == d_lt)
                return FALSE;
            if (d.dir() == d_le)
                return FALSE;
            if (d.dir() == d_lg)
                return FALSE;
            if (d.dir() == d_star)
                return FALSE;
            if ((d.dir() == d_ge) || (d.dir() == d_eq));
        }
        return FALSE;
    }
}

//
// is distance vector a star
//
int distance_vector::is_star()
{
    distance d;

    if (indep()) {
        return FALSE;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            d = iterator.step() -> d;
            if (d.is_star())
                return TRUE;
            if (d.dir() == d_eq);
            else return FALSE;
        }
        return FALSE;
    }
}


//
// return level of dependence
//
int distance_vector::level()
{
    distance d;
    int i;

    if (indep()) {
        return FALSE;
    } else {
        distance_vector_iter iterator(this);
        for(i = 1; !iterator.is_empty(); i++) {
            d = iterator.step() -> d;
            if (!d.is_zero() && !d.is_star())
                return i;
        }
        return i-1;
    }
}



//
// return number of elements in graph
//
int distance_vector::size()
{
    int i;

    if (indep()) {
        return 0;
    } else {
        distance_vector_iter iterator(this);
        for(i = 0; !iterator.is_empty(); i++) {
            iterator.step();
        }
        return i;
    }
}


//
// return level of dependence
//
distance *distance_vector::thresh(int level)
{
    distance d;
    int i;

    assert(!indep());
    distance_vector_iter iterator(this);
    for(i = 1; !iterator.is_empty(); i++) {
        d = iterator.step() -> d;
        if (i == level) {
            return new distance(&d);
        }
    }
    return(new distance(0));

}

//
// return level of first non-= or * dependence
// return 0 if all ='s or *'s
//
int distance_vector::first_not_eq()
{
    distance d;
    int i;

    assert(!indep());
    distance_vector_iter iterator(this);
    for(i = 1; !iterator.is_empty(); i++) {
        d = iterator.step() -> d;
        if (d.is_star() || !d.is_zero())
            return i;
    }
    return(0);

}


//
// return entry for level
//
distance distance_vector::entry(int level)
{
    int i;

    assert(!indep());
    distance_vector_iter iterator(this);
    for(i = 1; !iterator.is_empty(); i++) {
        distance_vector_e *e = iterator.step();
        if (i == level) {
            return e->d;
        }
    }
    assert(0);
    return(* new distance());
}

void distance_vector::set_entry(distance dd,int level)
{
    int i;

    assert(!indep());
    distance_vector_iter iterator(this);
    for(i = 1; !iterator.is_empty(); i++) {
        distance_vector_e *e = iterator.step();
        if (i == level) {
            e->d = dd;
            return;
        }
    }
    assert(0);
}

//
// negate a distance
//
void distance::negate()
{
    if (is_const()) {
        int d = dist();
        set_distance(-d);
    } else {
        direction di=dir();
        switch (di) {
        case d_lt: set_direction(d_gt); break;
        case d_gt: set_direction(d_lt); break;
        case d_le: set_direction(d_ge); break;
        case d_ge: set_direction(d_le); break;
        default: break;
        }
    }
}

//
// negate all the distances
//
void distance_vector::negate()
{
    distance * d;

    if (indep()) {
        return;
    } else {
        distance_vector_iter iterator(this);
        for(; !iterator.is_empty();) {
            d = &(iterator.step() -> d);
            d->negate();
        }
    }
}




// check which loop variables are actually used by array statements
/* a variable is unused if it and its prime is not used either in the */
/* expression nor in a triangular loop */
// also check which write variables are used, assume a1 is the write
void SetLoopUsed(int *loop_used,int minnest,int valid_dim, int *lb_valid,
		 int *ub_valid, array_info_iter *a1i, array_info_iter *a2i,
		 tn_list *al,integer_matrix *L, integer_matrix *U,int num_symb,
		 int *write_used)
{
    access_vector *av1, *av2;
    int n=num_symb;

    int j;
    for (j=0; j<2*minnest; j++) loop_used[j+num_symb]=0;
    for (j=0; j<num_symb; j++) loop_used[j] = 1;
    for (j=0; j<minnest; j++) write_used[j] = 0;
    for (int i=0; i<valid_dim; i++) {
        av1 = a1i -> step();
        av2 = a2i -> step();
	assert(av1 && av2);
        tn_list_iter *loops = new tn_list_iter(al);
        for (j=2*(minnest-1); j>=0; j-=2) {
            if (av1->val(loops->step()) != 0) {
                loop_used[j+n]=1;
                write_used[j/2]=1;
            }
        }
        loops = new tn_list_iter(al);
        for (j=(2*minnest)-1; j>=0; j-=2) {
            if (av2->val(loops->step()) != 0) loop_used[j+n]=1;
        }
        delete loops;
    }

    // check unused ones in case they're used in triangular loops
    for (j=1; j<=2*minnest; j++) {
        if (!loop_used[j-1+n]) {
            int ltotal = 1;
            int utotal = 1;
            for (int i=1; i<=2*minnest; i++) {
                int lbounds = lb_valid[i+n];
		int k;
                for (k=ltotal; k<ltotal+lbounds; k++) {
                    if (lb_valid[i+n] && (*L)[k][j+n]) {
                        loop_used[j-1+n] = 1;
                        write_used[(j-1)/2] = 1;
                    }
                }
                ltotal += lbounds;

                int ubounds = ub_valid[i+n];
                for (k=utotal; k<utotal+ubounds; k++) {
                    if (ub_valid[i+n] && (*U)[k][j+n]) {
                        loop_used[j-1+n] = 1;
                        write_used[(j-1)/2] = 1;
                    }
                }
                utotal += ubounds;
            }
        }
    }

// check if pair of unused is used
    for (j=0; j<2*minnest; j+=2) {
        if (loop_used[j+n]) loop_used[j+1+n] =1;
        if (loop_used[j+1+n]) loop_used[j+n] =1;
    }
}



//
// use the tree_node_list and symbols to set a matrix of lower and upper bounds
// symbolic variables are placed first
//
void SetLU(tn_list *al, int DD, integer_matrix *L, integer_matrix *U,
	    boolean *lb_valid,
	    boolean *ub_valid, access_list *symbols, int num_symb,
	    int flow=0)
{
    int is_good_symb(access_vector *,access_list *);
    int low_bounds = 0;
    int up_bounds = 0;
    // init lb_valid and ub_valid
    int i;
    for (i=0; i<=DD; i++) {
        lb_valid[i] = ub_valid[i] = 0;
    }

    // find out how many bounds there are
    tn_list_iter *loops = new tn_list_iter(al);
    for (i= DD; i>= 1+num_symb; i-=2) {	// for each loop
        tree_node *induct = loops->step();
        assert(induct->kind() == TREE_FOR);
        dep_for_annote * df = (dep_for_annote *)induct->peek_annote(k_dep_for_annote);
        assert(df);

        if (flow) {
          assert(df->lb->count() <= 1);
        }
        array_info_iter ai(df->lb);
        while (!ai.is_empty()) { // for each bound
            access_vector *lb = ai.step();
            if (is_good_symb(lb,symbols)) {
                low_bounds += 2;
                lb_valid[i]++;
                lb_valid[i-1]++;

            }

        }

        if (flow) {
          assert(df->ub->count() <= 1);
        }
        array_info_iter ai2(df->ub);
        while (!ai2.is_empty()) { // for each bound
            access_vector *ub = ai2.step();
            if (is_good_symb(ub,symbols)) {
                up_bounds += 2;
                ub_valid[i]++;
                ub_valid[i-1]++;
            }
        }
    }
    L->init(low_bounds+1,DD+1);
    U->init(up_bounds+1,DD+1);

    // set bounds of induction variables
    delete loops;
    loops = new tn_list_iter(al);
    int uconstr = up_bounds;
    int lconstr = low_bounds;
    for (i = DD; i>=1+num_symb; i-=2) {  // for each loop
        tree_node *induct = loops->step();
        dep_for_annote * df = (dep_for_annote *)induct->peek_annote(k_dep_for_annote);
        assert(df);

        array_info_iter ai(df->lb);
        int count = 0;

        while (!ai.is_empty()) {
            access_vector *lb = ai.step();
            if (is_good_symb(lb,symbols)) {
                count ++;
                (*L)[lconstr][0] = lb->con;
                (*L)[lconstr-lb_valid[i]][0] = lb->con;
                tn_list_iter temp(al);
                temp.set(loops->glist_iter::peek());
		int j;
                for (j=i-2;j>=1+num_symb;j-=2) {
                    // for each induct element
                    assert(!temp.is_empty());
                    tree_node *element = temp.step();
                    (*L)[lconstr][j] =
                        (*L)[lconstr-lb_valid[i]][j-1]
                        = lb->val(element);
                }

                access_list_iter temp_symb(symbols);
                for (j=num_symb; j>=1; j--) {
                    assert(!temp_symb.is_empty());
                    access_list_e *e = temp_symb.step();
                    (*L)[lconstr][j] =
                        (*L)[lconstr-lb_valid[i]][j] =
                        lb->conregs.val(e->var());
                }
                lconstr --;
            }
        }

        assert(count == lb_valid[i]);
        lconstr -= lb_valid[i];
        array_info_iter ai2(df->ub);
        count = 0;
        while (!ai2.is_empty()) {
            access_vector *ub = ai2.step();
            if (is_good_symb(ub,symbols)) {
                count ++;
                (*U)[uconstr][0] = ub->con;
                (*U)[uconstr-ub_valid[i]][0] = ub->con;
                tn_list_iter temp(al);
                temp.set(loops->glist_iter::peek());
		int j;
                for (j=i-2;j>=1+num_symb;j-=2) {
                    assert(!temp.is_empty());
                    tree_node *element = temp.step();
                    (*U)[uconstr][j] =
                        (*U)[uconstr-ub_valid[i]][j-1]=
                        ub->val(element);
                }

                access_list_iter temp_symb(symbols);
                for (j=num_symb; j>=1; j--) {
                    assert(!temp_symb.is_empty());
                    access_list_e *e = temp_symb.step();
                    (*U)[uconstr][j] =
                        (*U)[uconstr-ub_valid[i]][j] =
                        ub->conregs.val(e->var());
                }
                uconstr -= 1;
            }
        }
        assert(count == ub_valid[i]);
        uconstr -= ub_valid[i];
    }
    delete loops;
}



// use the bit vector used to add unused components back into dvector
// if not flow any unused component is a star
void distance_vector::add_unused(boolean *used, int n)
{
    if (indep()) return;
    for (int i=0; i<n; i+=2) {
        if (!used[i]) {
            assert(!used[i+1]);
            this->append(new distance());
        } else {
            this->append(this->pop()->d);
        }
    }

}

// use the bit vector used to add unused components back into dir_array
// any unused component is a star
void dir_list::add_unused(boolean *used, int n)
{
    assert(n%2==0);
    dir_list *temp = this;
    while (temp) {
        dir_array *dir = temp->data;
        distance *new_data = new distance[n/2];
        int i2=0;
        for (int i=0; i<n; i+=2) {
            if (!used[i]) {
                assert(!used[i+1]);
                new_data[i/2].set_direction(d_star);
            } else {
                new_data[i/2] = dir->data[i2];
                i2++;
            }
        }
        delete[] dir->data;
        dir->data = new_data;
        dir->size = n/2;

        temp = temp->next;
    }
}




// is this access vector good
// it's bad if memregs or conpaths or mod_this is not nil
inline int is_good_symb(access_vector *av1)
{
    return (!av1->too_messy && av1->memregs.is_empty() &&
            !av1->mod_this);
}

// same as above but can only refer to symbols on list
int is_good_symb(access_vector *av1,access_list *symbols)
{
    if (!is_good_symb(av1)) {
        return 0;
    }
    access_list *temp = new access_list;
    temp->unite(symbols,&av1->conregs);
    int answer = (temp->count() == symbols->count());
    delete temp;
    return answer;
}

// eliminate dimensions which have anything but conregs and induct vars
// and where mod_this is not nil
// return the number of valid dims
int dvlist::elim_bad_symb(array_info *a1, array_info *a2)
{
    array_info_iter ai1(a1);
    array_info_iter ai2(a2);
    array_info newa1;
    array_info newa2;

    int valid_dim = 0;
    while (!ai1.is_empty() && !ai2.is_empty()) {
        access_vector *av1 = ai1.step();
        access_vector *av2 = ai2.step();

        if(is_good_symb(av1) && is_good_symb(av2)) {
            valid_dim++;
            newa1.append(new access_vector(av1));
            newa2.append(new access_vector(av2));
        }
    }
    a1->clear(); a1->glist::append(&newa1);
    a2->clear(); a2->glist::append(&newa2);
    newa1.clear(); newa2.clear();
    return valid_dim;
}

// return a list of all symbols used in the array_info
// only looks at conregs
access_list *dvlist::find_symb(array_info *a)
{
    access_list *al = new access_list();
    array_info_iter *ai = new array_info_iter(a);
    while (!ai->is_empty()) {
        access_vector *av = ai->step();
        al->unite(&av->conregs);
    }
    return al;
}

// return a list of all symbols used in either array_info
// disregard case where for the same dimension, both
// sybols have the same value (ie a[n] vrs a[n])
// only looks at conregs
access_list *dvlist::find_symbs(array_info *a1, array_info *a2)
{
    access_list *al = new access_list();
    array_info_iter ai1(a1);
    array_info_iter ai2(a2);
    while(!ai1.is_empty()) {
        assert(!ai2.is_empty());
        access_list *temp = unite_unequals(&ai1.step()->conregs,&ai2.step()->conregs);

        al->unite(temp);
        delete temp;
    }
    assert(ai2.is_empty());
    return al;
}



// return the union of two access_lists throwing out elements with the
// same value
access_list *unite_unequals(access_list *a1, access_list *a2)
{
    access_list *result = new access_list();

    access_list_iter ai(a1);
    while (!ai.is_empty()) {
        access_list_e *e = ai.step();
        access_list_e *e2 = a2->search(e->var());
        if (!e2 || (e2->val() != e->val())) {
            result->enter(e->var(),e->val());
        }
    }

    access_list_iter ai2(a2);
    while (!ai2.is_empty()) {
        access_list_e *e = ai2.step();
        access_list_e *e2 = a1->search(e->var());
        if (!e2 || (e2->val() != e->val())) {
            result->enter(e->var(),e->val());
        }
    }
    return result;
}

int distance_vector::operator==(distance_vector &d)
{
    distance_vector_iter ti(this),di(&d);
    while(!ti.is_empty() && !di.is_empty()) {
        if(ti.step()->d != di.step()->d)
            return 0;
    }
    return ti.is_empty() && di.is_empty();
}

static void insert_dvector(dvlist *l,distance_vector *d)
{
    // don't insert any duplicates ... pretty dumb
    dvlist_iter di(l);
    while(!di.is_empty()) {
        if(*di.step()->dv == *d) {
            delete d;
            return;
        }
    }
    l->append(new dvlist_e(d));
}

static void lexpos_decomp(dvlist *l, distance_vector *d,
			  boolean strict_pos=FALSE)
{
    assert(d->ind == 0);
    distance_vector_iter di(d);

    for(int i=1; !di.is_empty(); i++) {
        distance_vector_e *dd = di.step();
        switch(dd->d.dir()) {
        case d_lt:
            insert_dvector(l,d);
            return;
        case d_gt:
            delete d;
            return;
        case d_eq:
            break;
        case d_le:
        case d_star: {
            distance_vector *copy1 = new distance_vector(d);
            dd->d.set_direction(d_eq);
            distance ddd;
            ddd.set_direction(d_lt);
            copy1->set_entry(ddd,i);
            insert_dvector(l,copy1);
        }
            break;
        case d_ge:
            dd->d.set_direction(d_eq);
            break;
        case d_lg:
            dd->d.set_direction(d_lt);
            insert_dvector(l,d);
            return;
        default:
            assert(0);
        }
    }

    // if strictly lex. positive, DO NOT include loop independent dependences
    if (!strict_pos)
	insert_dvector(l,d);
    else delete d;
}

void dvlist::make_lexpos(boolean strict_pos)
{
    // make a list of dependence vectors all lexicographically positive.
    // actually, makes it lexpos or zero, unless strict_pos is set to TRUE.
    // Those who wish can remove blatantly redundant entries from list.

    glist l;
    while(!is_empty())
        l.append(pop());

    while(!l.is_empty()) {
        dvlist_e *de = (dvlist_e *)l.pop();
        lexpos_decomp(this,de->dv,strict_pos);
        de->dv = 0;
        delete de;
    }
}





