/* file "int_matrix.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "int_matrix.h"

#include <suif1.h>

#include "suifmath.h"
#include <cstring>


class coeff;

int compose_print_flag = FALSE;

void init_suifmath(int & /* argc */, char * /* argv */ [])
{
//    k_named_sc_merge_mark_annote = lexicon->enter("named_sc_merge_mark")->sp;
    ANNOTE(k_named_sc_merge_mark_annote, "named_sc_merge_mark", FALSE);
    
    STRUCT_ANNOTE(code_context_annote::k_annote,
	"code_context",
	TRUE,
	code_context_annote_from,
	code_context_annote_to,
	code_context_annote_free,
	code_context_annote_print);
}

void exit_suifmath(void)
{

}

/********************************************************************************
 * gcd  (Gratest Common Dinominator)                                            *
 ********************************************************************************/
int gcd(int a, int b)
{
    assert((a>=0)&&(b>=0));

    if(a==0)
	return b;
    else if (b==0)
	return a; 
    else if(b>a) 
        return gcd(b%a, a);
    else
        return gcd(a%b, b);
}

/********************************************************************************
 * lcm (Least Common Multiple)                                              *
 ********************************************************************************/
int lcm(int a, int b)
{
    assert((a>=0)&&(b>=0));
    if(a==b) return a;
    long gcdab = gcd(a,b);
    long aL = a/gcdab;
    long bL = b;
    long out = aL*bL;
    assert(out/bL == aL); // check for overflow
    return out;
}



/* ##################################################
   #####   integer_row                          #####
   ################################################## */

/********************************************************************************
 * initializers                                                                 *
 ********************************************************************************/
void integer_row::mknew(int s)
{  
    if(rsz < s) {
        clear();
	rsz = s;
        R = new int[rsz]; 
    };
    sz = s;
}

integer_row::integer_row() 
{  
    R=0; 
    sz=0; 
    rsz=0;
}

integer_row::integer_row(const integer_row & rw) 
{ 
    R=0; 
    sz=0; 
    rsz=0;
    init(rw); 
}

integer_row::integer_row(const integer_row * rw) 
{ 
    R=0; 
    sz=0; 
    rsz=0;
    init(rw); 
}

integer_row::integer_row(int s) 
{
    R=0; 
    sz=0; 
    rsz=0;
    init(s); 
}

void integer_row::clear() 
{ 
    if(R) {
        delete[] R; 
        R=NULL; 
    } 
    sz=0; 
    rsz=0;
}

void integer_row::init(const integer_row & rw)
{
    if (R) {
        if(rw.n() <= rsz) {
	    sz = rw.n();
            memcpy(R, rw.R, n()*sizeof(int));
            return;
        } else
            clear();
    }

    rsz = sz = rw.n();
    if(n() > 0) {
        R = new int[n()];
        memcpy(R, rw.R, n()*sizeof(int));
    }
}


void integer_row::init(int s)
{
    assert(s > 0);
    if (R) {
        if(s <= rsz) {
	    sz = s;
            memset(R, 0, n()*sizeof(int));
            return;
        } else  
            clear();
    }

    rsz = sz = s;
    R = new int[n()];
    memset(R, 0, n()*sizeof(int));
}

void integer_row::init(const int * ilist, int s)
{
    assert(s > 0);
    if (R) {
        if(s <= rsz) {
	    sz = s;
            memcpy(R, ilist, n()*sizeof(int));
            return;
        } else
            clear();
    }

    rsz = sz = s;
    R = new int[n()];
    memcpy(R, ilist, n()*sizeof(int));
}


    

/********************************************************************************
 * row operations                                                               *
 ********************************************************************************/
boolean integer_row::operator==(const integer_row & a) const
{
    return ((a.n() == n()) &&
	    (memcmp(&a.R[0], &R[0], sizeof(int)*n()) == 0));

    
/* Speed hack above
    for(int i=0; i<n(); i++)
        if(a.R[i] != R[i]) return 0;
    return 1;
*/
}



integer_row & integer_row::operator=(const integer_row & row)
{
    if(&row == this) return *this;

    mknew(row.n());

    memcpy(R, row.R, n()*sizeof(int));

    return *this;
}


// Swap rows
void integer_row::operator^=(integer_row & row)
{
    // assert(n() == row.n());
    int *tmp = R;
    R = row.R;
    row.R = tmp;

    int tmp2 = sz;
    sz = row.n();
    row.sz = tmp2;

    tmp2 = rsz;
    rsz = row.rsz;
    row.rsz = tmp2;
}

integer_row integer_row::swap_col(int i, int j) const
{
    integer_row R(this);
    R.do_swap_col(i, j);
    return R;
}


void  integer_row::do_swap_col(int i, int j)
{
    if(i == j) return;

    assert((i>=0)&&(i<n()));
    assert((j>=0)&&(j<n()));

    
    int tmp = (*this)[i];
    (*this)[i] = (*this)[j];
    (*this)[j] = tmp;
}   



integer_row integer_row::operator+(const integer_row & row) const
{
    assert(n() == row.n());
    integer_row result(n());

    for(int i=0; i<n(); i++)
        result[i] = R[i] + row.c(i);

    return result;
}

integer_row integer_row::operator-(const integer_row & row) const
{
    assert(n() == row.n());

    integer_row result(n());

    for(int i=0; i<n(); i++)
        result[i] = R[i] - row.c(i);

    return result;
}


integer_row integer_row::operator*(const integer_row & row) const
{
    assert(n() == row.n());

    integer_row result(n());

    for(int i=0; i<n(); i++)
        result[i] = R[i] * row.c(i);

    return result;
}


integer_row integer_row::operator/(const integer_row & row) const
{
    assert(n() == row.n());

    integer_row result(n());
    for(int i=0; i<n(); i++)
        result[i] = R[i] / row.c(i);

    return result;
}


// shuffle operator ret[x] = this[row[x]]
integer_row integer_row::operator%(const integer_row & row) const
{
    assert(n() == row.n());

    integer_row result(n());
    result = 0;
    for(int i=0; i<n(); i++) {
        assert((row.R[i] >= 0)&&(row.R[i] < n()));
        result[i] = R[row.R[i]];
    }

    return result;
}


integer_row & integer_row::operator+=(const integer_row & row)
{
    assert(n() == row.n());

    for(int i=0; i<n(); i++)
        R[i] += row.R[i];

    return *this;
}

integer_row & integer_row::operator-=(const integer_row & row)
{
    assert(n() == row.n());

    for(int i=0; i<n(); i++)
        R[i] -= row.R[i];

    return *this;
}


integer_row & integer_row::operator*=(const integer_row & row)
{
    assert(n() == row.n());

    for(int i=0; i<n(); i++)
        R[i] *= row.R[i];

    return *this;
}
 

integer_row & integer_row::operator/=(const integer_row & row)
{
    assert(n() == row.n());

    for(int i=0; i<n(); i++)
        R[i] /= row.c(i);

    return *this;
}

void integer_row::do_addmul(const integer_row & row, int mul)
{
    assert(n() == row.n());

    if (mul == 1)
	*this += row;
    else
	for(int i=0; i<n(); i++)
	    R[i] += mul*row.c(i);
}

integer_row & integer_row::operator=(int val)
{
    for(int i=0; i<n(); i++)
        R[i] = val;
    return *this;
}

integer_row & integer_row::operator+=(int val)
{
    for(int i=0; i<n(); i++)
        R[i] += val;
    return *this;
}

integer_row & integer_row::operator-=(int val)
{
    for(int i=0; i<n(); i++)
        R[i] -= val;
    return *this;
}

integer_row & integer_row::operator*=(int val)
{
    for(int i=0; i<n(); i++)
        R[i] *= val;
    return *this;
}

integer_row & integer_row::operator/=(int val)
{
    for(int i=0; i<n(); i++)
        R[i] /= val;
    return *this;
}



/********************************************************************************
 * delete column                                                                *
 ********************************************************************************/
integer_row  integer_row::del_col(int i, int j) const
{
    assert(i<= j);
    assert((i>=0)&&(j<n()));
    integer_row ret(n() - (j - i + 1));

    int cnt = 0;
    int a;
    for(a = 0; a < i; a++)
        ret[cnt++] = R[a];

    for(a = j+1; a < n(); a++)
        ret[cnt++] = R[a];

    assert(cnt == ret.n());

    return ret;
}

void  integer_row::do_del_col(int i, int j)
{
    assert(i<= j);
    assert((i>=0)&&(j<n()));

    int st, fn;
    for (st = i, fn = j+1; (fn < sz); st++, fn++)
	R[st] = R[fn];

    sz = sz - (j - i + 1);
}

void integer_row::do_del_columns(const integer_row & mask)
{
    int src, dst;
    assert(mask.n() == n());

    for (dst=0, src=0; (src <  n()); src++) {
	if (!mask.R[src]) {
	    R[dst] = R[src];
	    dst++;
	};
    }

    sz = dst;
}

/********************************************************************************
 * insert column                                                                *
 ********************************************************************************/
integer_row  integer_row::insert_col(int i) const
{
    assert((i>=0)&&(i<=n()));
    integer_row  ret(n()+1);

    int cnt = 0;
    int a;
    for(a = 0; a < i; a++)
        ret[cnt++] = R[a];

    ret[cnt++] = 0;

    for(a = i; a < n(); a++)
        ret[cnt++] = R[a];

    assert(cnt == ret.n());

    return ret;
}

void integer_row::do_insert_col(int i)
{
    assert((i>=0)&&(i<=n()));
    if (sz < rsz) {
	int src, dst;
	sz++;
	for (src=i, dst=i+1; dst < sz; dst++, src++)
	    R[dst] = R[src];
	R[i] = 0;
    } else {
	assert((i>=0)&&(i<=n()));
	integer_row  ret(MAX(n()+1,n()*2));
	ret.sz = n()+1;

	int cnt = 0;
	int a;
	for(a = 0; a < i; a++)
	    ret[cnt++] = R[a];
	
	ret[cnt++] = 0;
	
	for(a = i; a < n(); a++)
	    ret[cnt++] = R[a];
	
	assert(cnt == ret.n());

	*this ^= ret;
    }
    return;
}

int integer_row::hashkey() const
{
    int h = 0;
    for(int i=0; i<n(); i++)
        h += R[i]*i*i;
    return h;
}

/* ##################################################
   #####   integer_matrix                       #####
   ################################################## */


integer_row *integer_matrix::alloc_rows(int num_rows)
{
    realmm = num_rows;
    return new integer_row[num_rows];
}


/********************************************************************************
 * constructors                                                                 *
 ********************************************************************************/
integer_matrix::integer_matrix(int rows,int cols) 
{
    A=0;
    nn = mm = realmm = 0;
    init(rows, cols);
}


integer_matrix::integer_matrix(const integer_matrix *m) 
{
    A = 0;
    nn = mm = realmm = 0;
    init(m);
}


integer_matrix::integer_matrix(const integer_matrix  & m) 
{
    A = 0;
    nn = mm = realmm = 0;
    init(m);
}

integer_matrix::integer_matrix(const integer_matrix *m, int rows) 
{
    A = 0;
    nn = mm = realmm = 0;
    init(m, rows);
}

integer_matrix::integer_matrix(const integer_matrix  & m, int rows) 
{
    A = 0;
    nn = mm = realmm = 0;
    init(m, rows);
}

integer_matrix::integer_matrix(const coeff * c)
{
    A = 0;
    nn = mm = realmm = 0;
    init(c);
}

integer_matrix::integer_matrix()
{
    A = 0;
    mm = nn = realmm = 0;
}

integer_matrix::~integer_matrix() 
{ 
    if(A) {
        delete[] A;
        A = 0;
        mm = nn = realmm = 0;
    }
}


void integer_matrix::clear()
{
    if(A) {
        delete[] A;
        A = 0;
        mm = nn = realmm = 0;
    }
    assert((mm == 0) && (realmm == 0));
}

/********************************************************************************
 * init                                                                         *
 ********************************************************************************/

void integer_matrix::mknew(int rows, int cols)
{
    assert((rows >= 0)&&(cols >= 0));

    if (A) {
        if((rows<=realmm)&&(cols<=n())) {
	    mm = rows; nn = cols;
            for(int i=0; i<mm; i++) A[i].mknew(cols);
            return;
        } else
            clear();
    }

    if(rows > 0) {
        A = alloc_rows(rows);
        for(int i=0; i<rows; i++) A[i].mknew(cols);
    }
    nn=cols;
    mm=rows;
}

void integer_matrix::init(int rows,int cols) 
{
    assert((rows >= 0)&&(cols >= 0));

    if (A) {
        if((rows<=realmm)&&(cols<=n())) {
	    mm = rows; nn = cols;
            for(int i=0; i<mm; i++) A[i].init(cols);
            return;
        } else
            clear();
    }

    if(rows > 0) {
        A = alloc_rows(rows);
        for(int i=0; i<rows; i++) A[i].init(cols);
    }
    nn=cols;
    mm=rows;
}


void integer_matrix::init(const integer_matrix & M) 
{
    assert(&M);

    if (A) {
        if((M.m()<=realmm)&&(M.n()<=n())) {
	    mm = M.m(); nn = M.n();
            for(int i=0; i<m(); i++) A[i] = M.r(i);
            return;
        } else
            clear();
    }

    if(M.m() > 0) {
        assert(M.n()>0);
        A = alloc_rows(M.m());
        for(int i=0; i<M.m(); i++) A[i] = M.r(i);
    }

    nn=M.n();
    mm=M.m();	
}



void integer_matrix::init(const integer_matrix & M, int rows) 
{
    assert(&M);
    assert(rows >= 0);

    if (A) {
        if((M.m()<=realmm)&&(M.n()<=n())) {
	    mm = M.m(); nn = M.n();
            for(int i=0; i<m(); i++) A[i] = M.r(i);
            return;
        } else
            clear();
    }
    
    nn=M.nn;
    mm=rows;
    A = alloc_rows(m());
    
    if(m() <= M.m())
        for(int i=0; i<m(); i++) A[i] = M.r(i);
    else {
	int i;
        for(i=0; i< M.m(); i++) A[i] = M.r(i);
        for(i=M.m(); i< m(); i++) A[i].init(n());
    }
}


void integer_matrix::init(const coeff * c)
{
    assert(c);
    init(c->m, c->n+1);
    for(int i=0; i<c->m; i++) {
        (*this)[i][0] = (c->constant)[i];
        for(int j=0; j<c->n; j++)
            (*this)[i][j+1] = *((c->vars)+i*(c->n)+j);
    }

}


void integer_matrix::init(FILE * fp)
{
    int r, c;

    fscanf(fp, "%d %d\n", &r, &c);
    fprintf(stderr,"(%d x %d)\n", r, c);

    init(fp, r, c);
}


void integer_matrix::init(FILE * fp, int r, int c)
{
    init(r, c);
    assert((r >= 0)&&(c >= 0));

    for(int i=0; i<r; i++)
        for(int j=0; j<c; j++)
            fscanf(fp, "%d ", &(*this)[i][j]);
}

void integer_matrix::init(const int * data, int rows, int cols)
{
    assert((rows >= 0)&&(cols >= 0));

    for(int i=0; i<rows; i++)
	for(int j=0; j<cols; j++)
	    A[i][j] = (*this)[i][j];

    if (A) {
        if((rows<=realmm)&&(cols<=n())) {
	    mm = rows; nn = cols;
            for(int i=0; i<mm; i++) 
		A[i].init(data+i*cols,cols);
            return;
        } else
            clear();
    }

    if(rows > 0) {
        A = alloc_rows(rows);
        for(int i=0; i<rows; i++) 
	    A[i].init(data+i*cols,cols);
    }
    nn=cols;
    mm=rows;
}



/********************************************************************************
 * operators                                                                    *
 ********************************************************************************/
integer_matrix & integer_matrix::operator=(const integer_matrix & in) 
{ 
    if(this == &in)
        return *this;
    init(in);
    return *this;
}


boolean integer_matrix::operator==(const integer_matrix & a) const
{
    if((a.n() == n()) && (a.m() == m())) {
        int noeq = 0;
        for(int i=0; i<a.m(); i++) if(a.r(i) != r(i)) noeq = 1;
        if(!noeq) return 1;
    }
    return 0;
}

integer_matrix integer_matrix::operator%(const integer_row & shuffle) const
{
    assert(n() == shuffle.n());

    integer_matrix result(m(), n());

    for(int i=0; i<m(); i++) {
        result[i] = r(i) % shuffle;
    }

    return result;
}



/********************************************************************************
 * delete column                                                                *
 ********************************************************************************/
integer_matrix  integer_matrix::del_col(int i, int j) const
{
    assert(i<= j);
    assert((i>=0)&&(j<n()));
    integer_matrix  ret(m(), n() - (j - i + 1));

    for(int a=0; a<m(); a++)
	ret[a] = r(a).del_col(i, j);

    return ret;
}

integer_matrix  integer_matrix::insert_col(int i) const
{
    assert((i>=0)&&(i<=n()));
    integer_matrix ret(m(), n() + 1);

    for(int a=0; a<m(); a++)
	ret[a] = r(a).insert_col(i);

    return ret;
}

integer_matrix  integer_matrix::del_columns(const integer_row & mask) const
{
    int cnt = 0;
    int i, j;
    assert(mask.n() == n());
    for(j=0; j<mask.n(); j++)
        if(!mask.c(j)) cnt++;

    integer_matrix ret(m(), cnt);

    int c = 0;
    for(j=0; j<n(); j++)
        if(!mask.c(j)) {
            for(i=0; i<m(); i++)
                ret[i][c] = this->r(i).c(j);
            c++;
        }
    assert(c == cnt);

    return ret;
}


integer_matrix  integer_matrix::swap_col(int i, int j) const
{
    assert((i>=0)&&(i<n()));
    assert((j>=0)&&(j<n()));

    integer_matrix ret(this);

    for(int a=0; a<m(); a++)
        ret[a].do_swap_col(i, j);

    return ret;
}

/* in-place versions */

void integer_matrix::do_del_col(int i, int j)
{
    assert(i<= j);
    assert((i>=0)&&(j<n()));

    for(int a=0; a<m(); a++)
	A[a].do_del_col(i, j);

    nn = nn - (j - i + 1);
}

void integer_matrix::do_insert_col(int i)
{
    assert((i>=0)&&(i<=n()));

    for(int a=0; a<m(); a++)
	A[a].do_insert_col(i);
    nn = nn+1;
}

void integer_matrix::do_del_columns(const integer_row & mask)
{
    int cnt = 0;
    int j;
    assert(mask.n() == n());

    for(j=0; j<mask.n(); j++)
        if(!mask.c(j)) cnt++;

    for(j=0; j<m(); j++)
	A[j].do_del_columns(mask);
    nn = cnt;
}

void integer_matrix::do_swap_col(int i, int j)
{
    assert((i>=0)&&(i<n()));
    assert((j>=0)&&(j<n()));

    for(int a=0; a<m(); a++)
        A[a].do_swap_col(i, j);
}

void integer_matrix::do_swap_row(int i, int j)
{
    assert((i>=0)&&(i<m()));
    assert((j>=0)&&(j<m()));

    A[i] ^= A[j];
}

void integer_matrix::do_add_rows(int rws)
{
    int newmm = mm+rws;
    int i;
    if (newmm > realmm) {
	integer_row *A1 = alloc_rows(MAX(newmm,mm*2));
	for (i=0; i<mm; i++) {
	    A[i] ^= A1[i];
	}
	delete[] A;
	A = A1;
    }
    for (i=mm; i<newmm; i++) {
	A[i].init(nn);
    }
    mm = newmm;
}

/********************************************************************************
 * delete row                                                                   *
 ********************************************************************************/
integer_matrix  integer_matrix::del_row(int i, int j) const
{
    assert(i<= j);
    assert((i>=0)&&(j<m()));
    integer_matrix ret(m() - (j - i + 1), n());

    int cnt = 0;
    int a;
    for(a = 0; a < i; a++)
        ret[cnt++] = A[a];
    
    for(a = j+1; a < m(); a++)
        ret[cnt++] = A[a];

    assert(cnt == ret.m());

    return ret;
}

void integer_matrix::do_del_row(int i, int j)
{
    assert(i<= j);
    assert((i>=0)&&(j<m()));

    int src, dst;

    for (dst=i, src=j+1; src < m(); dst++, src++)
	A[dst] ^= A[src];

    mm = m() - (j - i + 1);
}

void integer_matrix::do_del_rows(const integer_row & mask)
{
    int src, dst;
    assert(mask.n() == m());

    dst = 0;
    while (dst < m() && !mask.R[dst])
	dst++;
    src = dst;
    while (src < m()) {
	if (!mask.R[src]) {
	    if (src != dst)
		A[dst] ^= A[src];
	    dst++;
	};
	src++;
    }

    mm = dst;
}

void integer_matrix::do_rowadd(int rto, int radd, int mul)
{
    assert((radd>=0)&&(radd<m()));
    assert((rto>=0)&&(rto<m()));
    A[rto].do_addmul(A[radd],mul);
}

void integer_matrix::do_coladd(int cto, int cadd, int mul)
{
    assert((cto>=0)&&(cto<n()));
    assert((cadd>=0)&&(cadd<n()));
    if (mul == 1) {
	for (int i=0; i<m(); i++)
	    A[i][cto] += A[i][cadd];
    } else {
	for (int i=0; i<m(); i++)
	    A[i][cto] += A[i][cadd] * mul;
    }
}


/********************************************************************************
 * output                                                                       *
 ********************************************************************************/
void integer_matrix::print(FILE *fp, int flag) const
{
    for (int i=0; i<mm; i++)  {
        if(flag == 1) fprintf(fp,"%2d:  ", i);
        for (int j=0; j<nn; j++)  {
            fprintf(fp," %6d",r(i).c(j));
        }
        fprintf(fp,"\n");
    }
}



void integer_matrix::check() const
{

    if((n() == 0)||(m() == 0)) return;

    assert(A);

    for(int i=0; i<m(); i++) {
        assert(A[i].n() == n());
        assert(A[i].R);
    }

}


integer_matrix integer_matrix::operator+(const integer_matrix & a) const
{
    assert(mm == a.mm);
    assert(nn == a.nn);

    integer_matrix res(mm, nn);

    for(int i=0; i<m(); i++)
        res.A[i] = A[i] + a.A[i];

    return res;
}


integer_matrix integer_matrix::operator-(const integer_matrix & a) const
{
    assert(mm == a.mm);
    assert(nn == a.nn);

    integer_matrix res(mm, nn);

    for(int i=0; i<m(); i++) 
        res.A[i] = A[i] - a.A[i];

    return res;
}


integer_matrix integer_matrix::operator*(const integer_matrix & a) const
{
    assert(nn == a.mm);


    integer_matrix res(mm, a.nn);

    for(int i=0; i<m(); i++) 
        for(int j=0; j<a.n(); j++) {
            int acc = 0;
            for(int k=0; k<n(); k++) 
                acc += r(i).c(k) * a.r(k).c(j);
            res.A[i][j] = acc;
        }

    return res;
}



integer_matrix integer_matrix::operator*(int val) const
{
    integer_matrix res(this);

    for(int i=0; i<res.m(); i++) 
        for(int j=0; j<res.n(); j++) {
            res.A[i][j] *= val;
        }

    return res;
}

integer_matrix integer_matrix::operator/(int val) const
{
    integer_matrix res(this);

    for(int i=0; i<res.m(); i++) 
        for(int j=0; j<res.n(); j++) {
            res.A[i][j] /= val;
        }

    return res;
}

integer_matrix integer_matrix::operator+(int val) const
{
    integer_matrix res(this);

    for(int i=0; i<res.m(); i++) 
        for(int j=0; j<res.n(); j++) {
            res.A[i][j] += val;
        }

    return res;
}

integer_matrix integer_matrix::operator-(int val) const
{
    integer_matrix res(this);

    for(int i=0; i<res.m(); i++) 
        for(int j=0; j<res.n(); j++) {
            res.A[i][j] -= val;
        }

    return res;
}

integer_matrix & integer_matrix::operator+=(const integer_matrix & in)
{
    assert(m() == in.m());
    assert(n() == in.n());

    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] += in.r(i).c(j);
    return *this;
}


integer_matrix & integer_matrix::operator-=(const integer_matrix & in)
{
    assert(m() == in.m());
    assert(n() == in.n());

    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] -= in.r(i).c(j);
    return *this;
}


integer_matrix & integer_matrix::operator+=(int x)
{
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] += x;
    return *this;
}

integer_matrix & integer_matrix::operator-=(int x)
{
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] -= x;
    return *this;
}

integer_matrix & integer_matrix::operator*=(int x)
{
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] *= x;
    return *this;
}

integer_matrix & integer_matrix::operator/=(int x)
{
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] /= x;
    return *this;
}

integer_matrix integer_matrix::transpose() const
{
    integer_matrix res(nn, mm);

    for(int i=0; i<m(); i++) 
        for(int j=0; j<n(); j++) 
            res.A[j][i] = A[i][j];

    return res;
}

int integer_matrix::determinant() const
{
    assert(n() == m());

    if(n() == 1) return A[0][0];
        
    integer_matrix rdel;
    rdel = this->del_row(0);
    
    int ret = 0;
    for(int sign=1, i=0; i<n();i++, sign *= -1)
        ret += sign*A[0][i]*rdel.del_col(i).determinant();

    return ret;
}


int lcm(int, int);
int gcd(int, int);


integer_matrix integer_matrix::inverse(int * det) const
{
    assert(nn == mm);
    integer_matrix curr(this);

    integer_matrix res;
    res.ident(curr.m());

    integer_matrix sw;
    int i;
    for(i=0; i<m(); i++) {
        if(curr[i][i] == 0) {
            for(int j=i+1; j<m(); j++) 
                if(curr[j][i] != 0) {
		    curr.do_swap_row(i,j);
		    res.do_swap_row(i,j);
                    break;
                }
        }
        assert(curr[i][i]);
        for(int j=0; j<m(); j++) {
            if((i != j)&&(curr[j][i])){
                int g = gcd(ABS(curr[i][i]), ABS(curr[j][i]));
                curr[j] *= ABS(curr[i][i])/g;
                res[j]  *= ABS(curr[i][i])/g;

		int mul = -curr[j][i]/curr[i][i];
		curr.do_rowadd(j, i, mul);
		res.do_rowadd(j, i, mul);
            }
        }

    }
    
    if (det) {
	int l = 1;
	for(i=0; i<m(); i++) {
	    l = lcm(l, ABS(curr[i][i]));
	    if(l > 10000) {
		fprintf(stderr, "Error lcm getting too large\n");
		this->print(stderr);
		return res;
	    }
	}
	
	*det = l;
	
	for(i=0; i<m(); i++) {
	    int norm = curr[i][i];
	    curr[i] /= norm;
	    res[i]  *= l/norm;
	} 
    } else {
	for(i=0; i<m(); i++) {
	    int norm = curr[i][i];
	    curr[i] /= norm;
	    res[i]  /= norm;
	}
    }

    return res;
}

void integer_matrix::ident(int v)
{
    mknew(v, v);

    for(int i=0; i<m(); i++) {
        A[i] = 0;
        A[i][i] = 1;
    }
}


integer_matrix integer_matrix::resize_offset(int r1, int r2, int c1, int c2,
                                             int fill) const
{

    assert(mm-r1+r2 >=0);
    assert(nn-c1+c2 >=0);

//    if((mm-r1+r2 ==0)||(nn-c1+c2 ==0)) {
//        integer_matrix res;
//        return res;
//    }

    integer_matrix res(mm-r1+r2, nn-c1+c2);

    int i;
    for(i=0; i<res.m(); i++) res.A[i] = fill;

    for(i=MAX(0,r1); i < mm+MIN(0,r2); i++) 
        for(int j=MAX(0,c1); j < nn+MIN(0,c2); j++) 
            res.A[i-r1][j-c1] = A[i][j];

    return res;
}


integer_matrix integer_matrix::r_merge(const integer_matrix & a1,
                                       const integer_matrix & a2) const
{
    assert(a1.n() == a2.n());
    integer_matrix res(a1.m()+a2.m(),0);
    int i,j;
    for (i=0; i<a1.m(); i++)
	res.A[i] = a1.A[i];
    for (j=0; j<a2.m(); j++,i++)
	res.A[i] = a2.A[j];
    res.nn=a1.n();
    return res;
}

integer_matrix integer_matrix::c_merge(const integer_matrix & a1,
                                       const integer_matrix & a2) const
{
    assert(a1.m() == a2.m());
    integer_matrix res(a1.m(),a1.n()+a2.n());
    
    int off1 = a1.n()*sizeof(int);
    int off2 = a2.n()*sizeof(int);
    for (int i=0; i<a1.m(); i++) {
	memcpy(res.A[i].R, a1.A[i].R, off1);
	memcpy(res.A[i].R+a1.n(), a2.A[i].R, off2);
    }

    return res;
}


// Permutation matricies

integer_matrix doswitch(int i, int x1, int x2)
{
    integer_matrix res;
    res.ident(i);

    res.A[x1][x1] = 0;
    res.A[x2][x2] = 0;
    res.A[x1][x2] = 1;
    res.A[x2][x1] = 1;

    return res;
}

integer_matrix rowswitch(const integer_matrix & A, int r1, int r2)
{
    assert((r1>=0)&&(r1<A.m()));
    assert((r2>=0)&&(r2<A.m()));

    return doswitch(A.m(), r1, r2);
}


integer_matrix colswitch(const integer_matrix & A, int c1, int c2)
{
    assert((c1>=0)&&(c1<A.n()));
    assert((c2>=0)&&(c2<A.n()));

    return doswitch(A.n(), c1, c2);
}

integer_matrix doadd(int i, int r, int c, int val)
{
    integer_matrix res;
    res.ident(i);
    
    assert(r!=c);

    res[r][c] = val;

    return res;
}

integer_matrix rowadd(const integer_matrix & A, int r1, int r2, int val)
{
    assert((r1>=0)&&(r1<A.m()));
    assert((r2>=0)&&(r2<A.m()));

    return doadd(A.m(), r1, r2, val);
}

integer_matrix coladd(const integer_matrix & A, int c1, int c2, int val)
{
    assert((c1>=0)&&(c1<A.n()));
    assert((c2>=0)&&(c2<A.n()));
    
    return doadd(A.n(), c2, c1, val);
}

void compose_print(int fg)
{
    compose_print_flag = fg;
}

integer_matrix Compose(int r_comp, int c_comp, 
                       const integer_matrix * m1, const integer_matrix * m2,
                       const integer_matrix * m3, const integer_matrix * m4,
                       const integer_matrix * m5, const integer_matrix * m6,
                       const integer_matrix * m7, const integer_matrix * m8,
                       const integer_matrix * m9, const integer_matrix * m10,
                       const integer_matrix * m11, const integer_matrix * m12,
                       const integer_matrix * m13, const integer_matrix * m14,
                       const integer_matrix * m15, const integer_matrix * m16,
                       const integer_matrix * m17, const integer_matrix * m18,
                       const integer_matrix * m19, const integer_matrix * m20,
                       const integer_matrix * m21, const integer_matrix * m22,
                       const integer_matrix * m23, const integer_matrix * m24,
                       const integer_matrix * m25, const integer_matrix * m26,
                       const integer_matrix * m27, const integer_matrix * m28,
                       const integer_matrix * m29, const integer_matrix * m30,
                       const integer_matrix * m31, const integer_matrix * m32)
{
    integer_matrix R;

    return R.compose(r_comp, c_comp,  
                     m1, m2, m3, m4, m5, m6, m7, m8, 
                     m9, m10, m11, m12, m13, m14, m15, m16, 
                     m17, m18, m19, m20, m21, m22, m23, m24,
                     m25, m26, m27, m28, m29, m30, m31, m32);
}

integer_matrix integer_matrix::compose(int r_comp, int c_comp,
                                       const integer_matrix * m1,
                                       const integer_matrix * m2,
                                       const integer_matrix * m3,
                                       const integer_matrix * m4,
                                       const integer_matrix * m5,
                                       const integer_matrix * m6,
                                       const integer_matrix * m7,
                                       const integer_matrix * m8,
                                       const integer_matrix * m9,
                                       const integer_matrix * m10,
                                       const integer_matrix * m11,
                                       const integer_matrix * m12,
                                       const integer_matrix * m13,
                                       const integer_matrix * m14,
                                       const integer_matrix * m15,
                                       const integer_matrix * m16,
                                       const integer_matrix * m17,
                                       const integer_matrix * m18,
                                       const integer_matrix * m19,
                                       const integer_matrix * m20,
                                       const integer_matrix * m21,
                                       const integer_matrix * m22,
                                       const integer_matrix * m23,
                                       const integer_matrix * m24,
                                       const integer_matrix * m25,
                                       const integer_matrix * m26,
                                       const integer_matrix * m27,
                                       const integer_matrix * m28,
                                       const integer_matrix * m29,
                                       const integer_matrix * m30,
                                       const integer_matrix * m31,
                                       const integer_matrix * m32) const
{
    assert(r_comp > 0);
    assert(c_comp > 0);
    int val = r_comp*c_comp;
    assert(val <= 32);
    const integer_matrix **list = new const integer_matrix *[val];

    list[0] = m1;
    if(val>1) list[1] = m2;
    if(val>2) list[2] = m3;
    if(val>3) list[3] = m4;
    if(val>4) list[4] = m5;
    if(val>5) list[5] = m6;
    if(val>6) list[6] = m7;
    if(val>7) list[7] = m8;
    if(val>8) list[8] = m9;
    if(val>9) list[9] = m10;
    if(val>10) list[10] = m11;
    if(val>11) list[11] = m12;
    if(val>12) list[12] = m13;
    if(val>13) list[13] = m14;
    if(val>14) list[14] = m15;
    if(val>15) list[15] = m16;
    if(val>16) list[16] = m17;
    if(val>17) list[17] = m18;
    if(val>18) list[18] = m19;
    if(val>19) list[19] = m20;
    if(val>20) list[20] = m21;
    if(val>21) list[21] = m22;
    if(val>22) list[22] = m23;
    if(val>23) list[23] = m24;
    if(val>24) list[24] = m25;
    if(val>25) list[25] = m26;
    if(val>26) list[26] = m27;
    if(val>27) list[27] = m28;
    if(val>28) list[28] = m29;
    if(val>29) list[29] = m30;
    if(val>30) list[30] = m31;
    if(val>31) list[31] = m32;

    integer_matrix ret = composeL(r_comp, c_comp, list);

    delete[] list;
    return ret;
}


// DECLARE_DLIST_CLASSES(nim_list_base, nim_e,
// 		      nim_list_iter, integer_matrix *);
// 
// class nim_list: public nim_list_base {
// public:
//     void clear();
// };
// 
// void nim_list::clear()
// {
//     nim_list_iter iter(this);
//     while(!iter.is_empty()) {
//         integer_matrix * m = iter.step();
//         delete m;
//     }
//     glist::clear();
// }
// 
// nim_list NIM_List;

nim_op::~nim_op()
{
    if(m) delete m;
    if(next) delete next;
}

integer_matrix * nim_op::NIM(const integer_matrix & A)  
{
    nim_op * no = new nim_op(this);
    no->m = new integer_matrix(A);
    return no->m;
}

integer_matrix * nim_op::NIM(const integer_matrix * A)  
{
    assert(A);
    nim_op * no = new nim_op(this);
    no->m = new integer_matrix(A);
    return no->m;
}

integer_matrix * nim_op::NIM(int i, int j)
{
    nim_op * no = new nim_op(this);
    no->m = new integer_matrix(i, j);
    return no->m;
}

integer_matrix * nim_op::NIM(int i)
{
    nim_op * no = new nim_op(this);
    no->m = new integer_matrix(1, 1);
    (no->m)[0][0] = i;
    return no->m;
}

integer_matrix * NIM(const integer_matrix & A)  
{
    integer_matrix * m = new integer_matrix(A);
    return m;
}


integer_matrix * NIM(const integer_matrix * A)  
{
    assert(A);
    integer_matrix * m = new integer_matrix(A);
    return m;
}

integer_matrix * NIM(int i, int j)
{
    integer_matrix * m = new integer_matrix(i, j);
    return m;
}

integer_matrix * NIM(int i)
{
    integer_matrix * m = new integer_matrix(1, 1);
    (*m)[0][0] = i;
    return m;
}



#define ELEM(i, j)  list[i*c_comp+j]

integer_matrix integer_matrix::composeL(int r_comp, int c_comp,
                                        const integer_matrix *const * list,
                                        int exit_on_error) const
{
    assert(r_comp > 0);
    assert(c_comp > 0);
//    int val = r_comp*c_comp;
    
    int * r_seg_sz = new int[r_comp];
    int * c_seg_sz = new int[c_comp];
    int rsz, csz;
    int i, j;
    
    rsz = 0;
    for(i=0; i<r_comp; i++) {
        int rseg = -1;
        for(j=0; j<c_comp; j++) 
            if(ELEM(i,j) != 0) {
                if(rseg == -1) rseg = ELEM(i,j)->m();
                if(rseg != ELEM(i,j)->m()) {
                    composeL_print(TRUE, r_comp, c_comp, list);
                    if(exit_on_error)
                        error_line(1, NULL, "Row has unmached row count: element [%d, %d] row count %d, should be %d\n", i, j, ELEM(i,j)->m(), rseg);
                    else return integer_matrix(0, 0);
                }


            }
        if(rseg == -1) {
            composeL_print(TRUE, r_comp, c_comp, list);
            if(exit_on_error)
                error_line(1, NULL, "Row %d has no row count\n", i);
            else return integer_matrix(0, 0);
        } 
        r_seg_sz[i] = rseg;
        rsz += rseg;
    }

    csz = 0;
    for(j=0; j<c_comp; j++) {
        int cseg = -1;
        for(i=0; i<r_comp; i++) 
            if(ELEM(i,j) != 0) {
                if(cseg == -1) cseg = ELEM(i,j)->n();
                if(cseg != ELEM(i, j)->n()) {
                    composeL_print(TRUE, r_comp, c_comp, list);
                    if(exit_on_error)
                        error_line(1, NULL,"Column has unmatched column count: element [%d, %d] column count %d, should be %d\n", i, j, ELEM(i,j)->n(), cseg);
                    else return integer_matrix(0, 0);
                }
            }
        if(cseg == -1) {
            composeL_print(TRUE, r_comp, c_comp, list);
            if(exit_on_error)
                error_line(1, NULL, "Column %d has  no column count \n", j);
            else return integer_matrix(0, 0);
        }
        c_seg_sz[j] = cseg;
        csz += cseg;
    }
    
    integer_matrix ret(rsz, csz);

    int rorg = 0;
    for(i=0; i<r_comp; i++) {
        int corg = 0;
        for(j=0; j<c_comp; j++) {
            const integer_matrix * M;
            if((M = ELEM(i, j)) != 0) {
                for(int ii = 0; ii < r_seg_sz[i]; ii++)
                    for(int jj = 0; jj < c_seg_sz[j]; jj++)
                        ret[ii+rorg][jj+corg] = M->r(ii).c(jj);
            }
            corg += c_seg_sz[j];
        }
        rorg += r_seg_sz[i];
    }
            
    
    delete r_seg_sz;
    delete c_seg_sz;

    if(compose_print_flag)  
        composeL_print(FALSE, r_comp, c_comp, list);
    return ret;
}

void integer_matrix::composeL_print(int er, int r_comp, int c_comp,
                                    const integer_matrix *const * list) const
{
    int i, j;

    if(er) fprintf(stderr, "Composing error:\n");
    for(i=0; i< r_comp; i++) {
        for(j=0; j< c_comp; j++)
            if(ELEM(i,j) == 0)
                fprintf(stderr, " -----  ");
            else
                fprintf(stderr, "[%2dx%2d] ", ELEM(i,j)->m(), ELEM(i,j)->n());
        fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
}


int integer_matrix::init(const immed_list & ilist, int c)
{
    int xm = ilist[c++].integer();
    int xn = ilist[c++].integer();
    init(xm, xn);
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++)
            (*this)[i][j] = ilist[c++].integer();

    return c;
}


immed_list * integer_matrix::cvt_immed_list() const
{
    immed_list * il = new immed_list;
    il->append(immed(m()));
    il->append(immed(n()));
    for(int i=0; i<m(); i++)
        for(int j=0; j<n(); j++) 
            il->append(immed(r(i).c(j)));
    return il;
}


integer_matrix Compose(int r_comp, int c_comp,
                       int a1,  int a2,  int a3,  int a4, int a5,  int a6,
                       int a7,  int a8, int a9,  int a10, int a11, int a12,
                       int a13, int a14, int a15, int a16, int a17, int a18,
                       int a19, int a20, int a21, int a22, int a23, int a24,
                       int a25, int a26, int a27, int a28, int a29, int a30,
                       int a31, int a32, int a33, int a34, int a35, int a36,
                       int a37, int a38, int a39, int a40, int a41, int a42,
                       int a43, int a44, int a45, int a46, int a47, int a48,
                       int a49, int a50, int a51, int a52, int a53, int a54,
                       int a55, int a56, int a57, int a58, int a59, int a60)
{
    int params[60];

    params[0] = a1;
    params[1] = a2;
    params[2] = a3;
    params[3] = a4;
    params[4] = a5;
    params[5] = a6;
    params[6] = a7;
    params[7] = a8;
    params[8] = a9;
    params[9] = a10;
    params[10] = a11;
    params[11] = a12;
    params[12] = a13;
    params[13] = a14;
    params[14] = a15;
    params[15] = a16;
    params[16] = a17;
    params[17] = a18;
    params[18] = a19;
    params[19] = a20;
    params[20] = a21;
    params[21] = a22;
    params[22] = a23;
    params[23] = a24;
    params[24] = a25;
    params[25] = a26;
    params[26] = a27;
    params[27] = a28;
    params[28] = a29;
    params[29] = a30;
    params[30] = a31;
    params[31] = a32;
    params[32] = a33;
    params[33] = a34;
    params[34] = a35;
    params[35] = a36;
    params[36] = a37;
    params[37] = a38;
    params[38] = a39;
    params[39] = a40;
    params[40] = a41;
    params[41] = a42;
    params[42] = a43;
    params[43] = a44;
    params[44] = a45;
    params[45] = a46;
    params[46] = a47;
    params[47] = a48;
    params[48] = a49;
    params[49] = a50;
    params[50] = a51;
    params[51] = a52;
    params[52] = a53;
    params[53] = a54;
    params[54] = a55;
    params[55] = a56;
    params[56] = a57;
    params[57] = a58;
    params[58] = a59;
    params[59] = a60;

    assert(r_comp*c_comp <= 60);
    integer_matrix M(r_comp, c_comp);
    int * ptr = params;
    for(int i=0; i<r_comp; i++)
        for(int j=0; j<c_comp; j++)
            M[i][j] = *ptr++;
    return M;
}



