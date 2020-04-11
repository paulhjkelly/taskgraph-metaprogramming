/* file "int_matrix.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
//
// two dimensional integer matrix class to allow easy referencing
//

#ifndef INT_MATRIX_H
#define INT_MATRIX_H

#ifndef ABS
#define ABS(A) (((A)<0) ? -1*(A):(A))
#endif

#ifdef MAX
#undef MAX
#endif
inline int    MAX(int A, int B) { return (A > B)?A:B; }
inline double MAX(double A, double B) { return (A > B)?A:B; }

#ifdef MIN
#undef MIN
#endif
inline int    MIN(int A, int B) { return (A < B)?A:B; }
inline double MIN(double A, double B) { return (A < B)?A:B; }

#include <cstdlib>

int gcd(int, int);
int lcm(int, int);


/**********************************************************************************
 *** integer row                                                                ***
 ***                                                                            ***
 **********************************************************************************/
class integer_row {
    friend class integer_matrix;
    friend class lin_ineq;
    int sz;

protected:
    int * R;
    int rsz;

public:
    integer_row();
    integer_row(const integer_row & rw);
    integer_row(const integer_row * rw);
    integer_row(int s);
//    integer_row(const int * ilist, int s) { R=0; sz=0; init(ilist, s); }
    ~integer_row() { clear(); }
    void clear();
private:
    void mknew(int s);
public:
    void init(const integer_row & rw);
    void init(const integer_row * rw) { init(*rw); }
    void init(int s);
    void init(const int * ilist, int s);
    void init() { R=0; sz=0; rsz=0; }

    int n() const { return sz; }

    int & operator[](int i) { assert((i>=0)&&(i<sz)); return R[i]; }
    int c(int i) const { assert((i>=0)&&(i<sz)); return R[i]; }

    boolean operator==(const integer_row & a) const;
    boolean operator!=(const integer_row & a) const { return !((*this) == a); }
    integer_row & operator=(const integer_row & row);
    void operator^=(integer_row & row); // Swap Rows
    integer_row operator+(const integer_row & row) const;
    integer_row operator-(const integer_row & row) const;
    integer_row operator*(const integer_row & row) const;
    integer_row operator/(const integer_row & row) const;
//    integer_row operator+(int r) const;
//    integer_row operator-(int r) const;
//    integer_row operator*(int r) const;
//    integer_row operator/(int r) const;

    integer_row operator%(const integer_row & row) const;
        // operator%: shuffle operator ret[x] = this[row[x]]

    integer_row & operator+=(const integer_row & row);
    integer_row & operator-=(const integer_row & row);
    integer_row & operator*=(const integer_row & row);
    integer_row & operator/=(const integer_row & row);
    integer_row & operator=(int val);
    integer_row & operator+=(int val);
    integer_row & operator-=(int val);
    integer_row & operator*=(int val);
    integer_row & operator/=(int val);

    void do_addmul(const integer_row &, int mul=1);

    void print(FILE * fp=stdout) const { for(int i=0; i< n(); i++) fprintf(fp, "%d ", R[i]); fprintf(fp, "\n"); }
    integer_row del_col(int i, int j) const;
    integer_row del_col(int i) const { return del_col(i, i); }
    integer_row insert_col(int i) const;

    integer_row swap_col(int i, int j) const;

    void do_del_col(int i, int j);
    void do_del_col(int i) { do_del_col(i, i); };
    void do_del_columns(const integer_row &mask); // delete entries with 1s;
    void do_insert_col(int i);
    void do_swap_col(int i, int j);
public:

    int * data_array() { return R; }

    int hashkey() const;
};


class coeff;
/**********************************************************************************
 *** integer matrix                                                             ***
 ***                                                                            ***
 **********************************************************************************/
class integer_matrix {

public:
	int nn;	// number of columns;
	int mm;  // number of rows;
	integer_row * A;  // the matrix;

protected:
	int realmm;
	virtual integer_row *alloc_rows(int num_rows);
	void mknew(int rows, int cols);
public:
	integer_matrix(int rows,int cols);
	integer_matrix(const integer_matrix & m); 
	integer_matrix(const integer_matrix * m); 
	integer_matrix(const integer_matrix & m, int rows);
	integer_matrix(const integer_matrix * m, int rows);
        integer_matrix(const coeff *);
	integer_matrix();

	virtual ~integer_matrix();
        void clear();
	void init(int rows,int cols); 
	void init(const integer_matrix & M);
	void init(const integer_matrix * m) { init(*m); }
	void init(const integer_matrix & M, int rows);
	void init(const integer_matrix * m, int rows) { init(*m, rows); }
        void init(FILE *);
        void init(FILE *, int, int);
        void init(const coeff *);
	void init(const int *data, int rows, int cols);
        int init(const immed_list & ilist, int c=0);


        immed_list * cvt_immed_list() const;


        boolean operator==(const integer_matrix &  a) const;
	boolean operator!=(const integer_matrix &  a) const
            { return (!((*this) == a)); }

	integer_row & operator [](int row) { assert((row>=0)&&(row<mm)); return A[row]; }
        const integer_row &r(int row) const
            { assert((row>=0)&&(row<mm)); return A[row]; }
        integer_matrix & operator=(const integer_matrix & in);

        integer_matrix  operator%(const integer_row & shuffle) const;
	int n() const { return nn; }
	int m() const { return mm; }

        integer_matrix del_row(int i, int j) const;
        integer_matrix del_row(int i) const { return del_row(i, i); }

        integer_matrix del_col(int i, int j) const;
        integer_matrix del_col(int i) const { return del_col(i, i); }
        integer_matrix del_columns(const integer_row & r) const;
        integer_matrix insert_col(int i) const;

        integer_matrix swap_col(int i, int j) const;

        void do_del_row(int i, int j);
        void do_del_row(int i) { do_del_row(i, i); };
        void do_del_rows(const integer_row & r);

        void do_del_col(int i, int j);
        void do_del_col(int i) { do_del_col(i, i); }
        void do_del_columns(const integer_row & r);
        void do_insert_col(int i);
	void do_add_rows(int rws); // adds rows to end;

        void do_swap_col(int i, int j);
        void do_swap_row(int i, int j);

	void do_rowadd(int rto, int radd, int mul=1);
	void do_coladd(int cto, int cadd, int mul=1);

	void print(FILE *fp=stdout, int flag=0) const;

        void check() const;


        // matrix operations
        integer_matrix  operator*(const integer_matrix &) const;
        integer_matrix  operator+(const integer_matrix &) const;
        integer_matrix  operator-(const integer_matrix &) const;
        integer_matrix  operator*(int i) const;
        integer_matrix  operator/(int i) const;
        integer_matrix  operator+(int i) const;
        integer_matrix  operator-(int i) const;

        integer_matrix & operator+=(const integer_matrix & in);
        integer_matrix & operator-=(const integer_matrix & in);
        integer_matrix & operator+=(int);
        integer_matrix & operator-=(int);
        integer_matrix & operator*=(int);
        integer_matrix & operator/=(int);
        void ident(int v);
        void ident() { assert(mm == nn); ident(mm); }
        integer_matrix transpose() const;
        int determinant() const;
        integer_matrix inverse(int * det = 0) const;
	
        void swap(int i, int j) { (*this)[i] ^= (*this)[j]; }

	// returns (m-r1+r2)x(n-c1+c2) matrix, obtained from block;
	// with corners at (r1,c1) to (m+r2,m+c2);
	// puts fill value at new locations not corresponding to original;
        integer_matrix  resize_offset(int r1, int r2, int c1, int c2,
                                      int fill=0) const;
	// selects block (r1,c1) to (r2,c2) from this;
        integer_matrix  resize(int r1, int r2, int c1, int c2, int fill=0)
            const { return resize_offset(r1, r2-mm, c1, c2-nn, fill); }
	
	// puts a2 rows below a1;
        integer_matrix r_merge(const integer_matrix & a1,
                               const integer_matrix & a2) const;
        integer_matrix c_merge(const integer_matrix & a1,
                               const integer_matrix & a2) const;
        integer_matrix  operator|(const integer_matrix & A) const
            { return c_merge(*this, A); }
        integer_matrix  operator&(const integer_matrix & A) const
            { return r_merge(*this, A); }

        integer_matrix composeL(int r_comp, int c_comp,
                                const integer_matrix *const * list,
                                int exit_on_error=TRUE) const;
        integer_matrix compose(int r_comp, int c_comp,
                               const integer_matrix * m1=0,
                               const integer_matrix * m2=0,
                               const integer_matrix * m3=0,
                               const integer_matrix * m4=0,
                               const integer_matrix * m5=0,
                               const integer_matrix * m6=0,
                               const integer_matrix * m7=0,
                               const integer_matrix * m8=0,
                               const integer_matrix * m9=0,
                               const integer_matrix * m10=0,
                               const integer_matrix * m11=0,
                               const integer_matrix * m12=0,
                               const integer_matrix * m13=0,
                               const integer_matrix * m14=0,
                               const integer_matrix * m15=0,
                               const integer_matrix * m16=0,
                               const integer_matrix * m17=0,
                               const integer_matrix * m18=0,
                               const integer_matrix * m19=0,
                               const integer_matrix * m20=0,
                               const integer_matrix * m21=0,
                               const integer_matrix * m22=0,
                               const integer_matrix * m23=0,
                               const integer_matrix * m24=0,
                               const integer_matrix * m25=0,
                               const integer_matrix * m26=0,
                               const integer_matrix * m27=0,
                               const integer_matrix * m28=0,
                               const integer_matrix * m29=0,
                               const integer_matrix * m30=0,
                               const integer_matrix * m31=0,
                               const integer_matrix * m32=0) const;
    private:
        void composeL_print(int er, int r_comp, int c_comp,
                            const integer_matrix *const * list) const;
};

typedef integer_matrix * p_integer_matrix;

inline integer_matrix Ident(int i) { integer_matrix tmp; tmp.ident(i); return tmp; }

// Permutation matrices
integer_matrix rowswitch(const integer_matrix & A, int r1, int r2);
integer_matrix colswitch(const integer_matrix & A, int c1, int c2);
integer_matrix rowadd(const integer_matrix & A, int rto, int radd, int mul=1);
integer_matrix coladd(const integer_matrix & A, int cto, int cadd, int mul=1);

integer_matrix * NIM(const integer_matrix & A);
integer_matrix * NIM(const integer_matrix * A);
integer_matrix * NIM(int i, int j);
integer_matrix * NIM(int i);

class nim_op {
    nim_op * next;
    integer_matrix * m;
public:
    nim_op()                    { next = NULL; 
                                  m = NULL; }
    nim_op(nim_op * t)          { assert(t); 
                                  next = t->next; 
                                  t->next = this; 
                                  m = NULL; }
    ~nim_op();    
    integer_matrix * NIM(const integer_matrix & A);
    integer_matrix * NIM(const integer_matrix * A);
    integer_matrix * NIM(int i, int j);
    integer_matrix * NIM(int i);
};
    



void compose_print(int);
integer_matrix Compose(int r_comp, int c_comp, 
                       const integer_matrix * m1=0,
                       const integer_matrix * m2=0,
                       const integer_matrix * m3=0,
                       const integer_matrix * m4=0,
                       const integer_matrix * m5=0,
                       const integer_matrix * m6=0,
                       const integer_matrix * m7=0,
                       const integer_matrix * m8=0,
                       const integer_matrix * m9=0,
                       const integer_matrix * m10=0,
                       const integer_matrix * m11=0,
                       const integer_matrix * m12=0,
                       const integer_matrix * m13=0,
                       const integer_matrix * m14=0,
                       const integer_matrix * m15=0,
                       const integer_matrix * m16=0,
                       const integer_matrix * m17=0,
                       const integer_matrix * m18=0,
                       const integer_matrix * m19=0,
                       const integer_matrix * m20=0,
                       const integer_matrix * m21=0,
                       const integer_matrix * m22=0,
                       const integer_matrix * m23=0,
                       const integer_matrix * m24=0,
                       const integer_matrix * m25=0,
                       const integer_matrix * m26=0,
                       const integer_matrix * m27=0,
                       const integer_matrix * m28=0,
                       const integer_matrix * m29=0,
                       const integer_matrix * m30=0,
                       const integer_matrix * m31=0,
                       const integer_matrix * m32=0);

integer_matrix Compose(int r_comp, int c_comp, 
                       int a1,    int a2=0,  int a3=0,  int a4=0,
                       int a5=0,  int a6=0,  int a7=0,  int a8=0,
                       int a9=0,  int a10=0, int a11=0, int a12=0,
                       int a13=0, int a14=0, int a15=0, int a16=0,
                       int a17=0, int a18=0, int a19=0, int a20=0,
                       int a21=0, int a22=0, int a23=0, int a24=0,
                       int a25=0, int a26=0, int a27=0, int a28=0,
                       int a29=0, int a30=0, int a31=0, int a32=0,
                       int a33=0, int a34=0, int a35=0, int a36=0,
                       int a37=0, int a38=0, int a39=0, int a40=0,
                       int a41=0, int a42=0, int a43=0, int a44=0,
                       int a45=0, int a46=0, int a47=0, int a48=0,
                       int a49=0, int a50=0, int a51=0, int a52=0,
                       int a53=0, int a54=0, int a55=0, int a56=0,
                       int a57=0, int a58=0, int a59=0, int a60=0);


#endif /* INT_MATRIX_H */
