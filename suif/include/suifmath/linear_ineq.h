/* file "linear_ineq.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
//
// System of linear inequalities
//

#ifndef LINEAR_INEQ_H
#define LINEAR_INEQ_H


class name_store;

/**********************************************************************************
 *** constraint                                                                 ***
 ***                                                                            ***
 **********************************************************************************/
class constraint:public integer_row {
    friend class lin_ineq;
public:
    constraint(const constraint & rw):integer_row(rw) { }
    constraint(const constraint * rw):integer_row(rw) { }
    constraint(const integer_row & rw):integer_row(rw) { }
    constraint(const integer_row * rw):integer_row(rw) { }
    constraint(int s):integer_row(s) { }
    constraint() { }
    ~constraint() { }
    void init(const constraint & rw) { integer_row::init(rw); }
    void init(const constraint * rw) { integer_row::init(rw); }
    void init(int s) { integer_row::init(s); }
    void init(const int * ilist, int s) { integer_row::init(ilist, s); }
    void init(){ integer_row::init(); }

    int row_gcd() const;
    int row_lcm() const;

    /* if (!norm_bounds), normalizethis /= row_gcd();
       if (norm_bounds),
          g = row_gcd(except elt 0)
          normalizethis /= g; (taking floor of this[0]/g);
	  ie, reduce (R*g+C1*g+C2 >= 0) for (g>C2>=0) to (R + C1 >= 0);
       returns TRUE if anything changed
       */
    boolean normalize(boolean norm_bounds=FALSE);

    /* turns R into -R-1; */
    void complement();

    /* return location of last non-0 entry in row */
    int rank() const;
    /* return TRUE if exactly 1 non-0 term outside of constant term ([0])*/
    boolean unique() const;

    /* returns Sum{so[i] | R[i] != 0} */
    int highest_order(const constraint & so) const;

    int hashkey() const { return integer_row::hashkey(); }

    // Complement
    constraint operator-() const
      { constraint x(*this); x.complement(); return x; }

    boolean operator==(const constraint & a) const
      { return integer_row::operator==(a); }
    boolean operator!=(const constraint & a) const
      { return !((*this)==a); }

    constraint & operator=(const constraint & row)
      { integer_row::operator=(row); return *this; }
    void operator^=(constraint & row)
      { integer_row::operator^=(row); } // Swap Rows
    constraint operator+(const constraint & row) const
      { return integer_row::operator+(row); }
    constraint operator-(const constraint & row) const
      { return integer_row::operator-(row); }
    constraint operator*(const constraint & row) const
      { return integer_row::operator*(row); }
    constraint operator/(const constraint & row) const
      { return integer_row::operator/(row); }
//    constraint operator+(int r) const { return integer_row::operator+(r); }
//    constraint operator-(int r) const { return integer_row::operator-(r); }
//    constraint operator*(int r) const { return integer_row::operator*(r); }
//    constraint operator/(int r) const { return integer_row::operator/(r); }
    constraint operator%(const constraint & row) const
      { return integer_row::operator%(row); } // shuffle
    constraint & operator+=(const constraint & row)
      { integer_row::operator+=(row); return *this; }
    constraint & operator-=(const constraint & row)
      { integer_row::operator-=(row); return *this; }
    constraint & operator*=(const constraint & row)
      { integer_row::operator*=(row); return *this; }
    constraint & operator/=(const constraint & row)
      { integer_row::operator/=(row); return *this; }
    constraint & operator=(int val)
      { integer_row::operator=(val); return *this; }
    constraint & operator+=(int val)
      { integer_row::operator+=(val); return *this; }
    constraint & operator-=(int val)
      { integer_row::operator-=(val); return *this; }
    constraint & operator*=(int val)
      { integer_row::operator*=(val); return *this; }
    constraint & operator/=(int val)
      { integer_row::operator/=(val); return *this; }

    // 1 for the lower-th to upper-th of type type
    /*
       if (type==0)
       then result[i]=1 if lower<=this[i]<=upper
       else result[i]=1 if lower<=leftofthis[type,i]<=upper
                          where leftofthis[type,i] = sum(0,i,this[i]==type)-1
			  */
    constraint make_filter(int type, int lower, int upper) const;
    /* if (type==0)
       then result[i]=1 if this[i]==pos;
       else result[i]=1 if leftofthis[type,i]==pos */
    constraint make_filter(int type, int pos) const
      { return make_filter(type, pos, pos); };
    /* if (type==0)
       then result[i]=1 if 0<=this[i]<=1000;
       else result[i]=1 if 0<=leftofthis[type,i]<=1000
       */
    constraint make_filter(int type) const
      { return make_filter(type, 0, 1000); };
    /*
       returns AND(0<=i<n(),kernal[i] && same_sign(this[i],sign))
       */
    int filter(const constraint & kernal, int sign) const;
};


/**********************************************************************************
 *** linear inequality                                                          ***
 ***                                                                            ***
 **********************************************************************************/
class lin_ineq:public integer_matrix {

protected:
    virtual integer_row *alloc_rows(int num_rows);

public:
    lin_ineq(int rows,int cols):integer_matrix(rows, cols) {  }
    lin_ineq(const lin_ineq * m):integer_matrix(m) {  }
    lin_ineq(const lin_ineq & m):integer_matrix(m) {  }
    lin_ineq(const lin_ineq * m, int rows):integer_matrix(m, rows) {  }
    lin_ineq(const lin_ineq & m, int rows):integer_matrix(m, rows) {  }
    lin_ineq() {  }
    lin_ineq(const integer_matrix * m):integer_matrix(m) {  }
    lin_ineq(const integer_matrix & m):integer_matrix(m) {  }
    lin_ineq(const coeff * c):integer_matrix(c) {  }
    lin_ineq(const coeff & c):integer_matrix(&c) {  }

    virtual ~lin_ineq() { }

    void init(int rows,int cols) { integer_matrix::init(rows, cols); }
    void init(const lin_ineq * m) { integer_matrix::init(m); }
    void init(const lin_ineq & m) { integer_matrix::init(m); }
    void init(const lin_ineq * m, int rows) {integer_matrix::init(m, rows);}
    void init(FILE * fp) { integer_matrix::init(fp); }
    void init(FILE * fp, int r, int c) { integer_matrix::init(fp, r, c); }
    void init(const lin_ineq & m, int rows) {integer_matrix::init(m, rows);}
    constraint & operator [](int row) { assert((row>=0)&&(row<mm));
                                        return static_cast<constraint &>(A[row]); }
    const constraint &r(int row) const { assert((row>=0)&&(row<mm));
                                         return static_cast<const constraint &>(A[row]); }

    lin_ineq & operator=(const lin_ineq & in)
        { integer_matrix::operator=(in); return *this; }

    /* left in for saman compatibility -- do not add new uses */
    lin_ineq operator||(const lin_ineq & mat) const { return *this && mat; };
    int operator>>(const lin_ineq & mat) const { return (mat >= (*this)); }
    int operator<<(const lin_ineq & mat) const { return (mat <= (*this)); }
    lin_ineq conjunct(const lin_ineq &mat) const { return *this && mat; };
    void del_repatition() { del_repetition(); };
  private:
    void del_repatition(int r1, int r2) { del_repetition(r1, r2);};
  public:
    void min_constant() { del_loose(); }; // why do we have 2 identical fns?;
    lin_ineq operator&&(const lin_ineq &mat) const;
    /* end saman compatibility */

    int operator~() const;       // True if no answer exist for linear ineqs;
    // concatenate mat with this;
    lin_ineq &operator&=(const lin_ineq & mat);
    // Is contained in operator ( mat is contained in this);
    boolean operator>=(const lin_ineq & mat) const;
    boolean operator<=(const lin_ineq & mat) const { return (mat >= (*this)); }
    boolean operator==(const lin_ineq & mat) const
	{ return ((mat >= (*this))&&((*this) >= mat)); };

    // shuffle each row according to shuff;
    lin_ineq operator%(const constraint & shuff) const;

    int col_lcm(int col) const;
    int col_gcd(int col) const;

    // normalize each row; optionally do floor of column 0;
    void normalize(boolean norm_bounds=FALSE);
    void del_zeros();  // remove 0 rows;
    void del_unused_var(); // remove 0 columns;
    void del_repetition(); // remove identical rows;
    // rem identical rows in (incl) range as compared to whole array;
    // if deleted, must have n() = this->m() and be 0ed;
    // 1s are put in elts corresponding to deleted rows;
    void del_repetition(int r1, int r2, integer_row *deleted=0);
    void del_loose(); // rem otherwise identical row with bigger constant;
    // del_loose in (incl) range; as compared to whole array;
    // if deleted, must have n() = this->m() and be 0ed;
    // 1s are put in elts corresponding to deleted rows;
    void del_loose(int r1, int r2, integer_row *deleted=0);

    void row_swap(int i, int j);

    // if s_order, sort according to;
    //   row.highest_order(s_order)+row.rank();
    //  (recall row.highest_order(s_order) = Sum{i s.t. row[i]!=0} s_order[i]);
    // dir > 0 => sort into row[i] <= row[i+1];
    void sort(const constraint * sort_order = 0, int dir = 1);

    int num_constraint() const { return m(); }
    int max_rank() const;
    int num_variable() const { return n(); }
    int is_empty() const { return m() == 0; }

    lin_ineq del_col(int i) const { return integer_matrix::del_col(i); }
    lin_ineq del_col(int i, int j) const
      { return integer_matrix::del_col(i,j); }

    void print(FILE *fp=stdout, int flag=0) const;
    void sprint_eq(char * buffer, const char * names[], int upper, int lhs=0)
            const;

public:
    /* deletes rows for which filter(kernal,sgn) is not true */
    void do_filter_thru(const constraint & kernal, int sign);
    lin_ineq filter_thru(const constraint & kernal, int sign) const;
    /* deletes rows for which filter(kernal,sgn) is true */
    void do_filter_away(const constraint & kernal, int sign);
    lin_ineq filter_away(const constraint & kernal, int sign) const;

    /*
       Takes a row with a non-0 for every variable whose bounds matter.
       Modifies the row to take the transitive closure over this leq,
       adding every variable related to those vars.
       */
    void do_bounds(integer_row &init_bounds) const;
};

typedef lin_ineq * p_lin_ineq;


class lin_ineq_negate_iter {
    lin_ineq L;
    unsigned long curr, prev;
    unsigned long done_mark;
public:
    lin_ineq_negate_iter(const lin_ineq & leq) { init(leq); }
    lin_ineq_negate_iter(const lin_ineq * leq) { init(*leq); }
    int done() const { return (curr & done_mark); }
    lin_ineq * step();
private:
    void init(const lin_ineq & l);
};

class lin_ineq_difference_iter {
    int curr;
    lin_ineq D;
    int indep;
public:
    lin_ineq_difference_iter(const lin_ineq & A, const lin_ineq & B)
      { curr=0; init(A, B); }
    lin_ineq_difference_iter(const lin_ineq * A, const lin_ineq * B)
      { curr=0; init(*A, *B); }
    ~lin_ineq_difference_iter();
    int done() const { return (!indep)?(curr==D.n()):(curr>0); }
    lin_ineq * step();
private:
    void init(const lin_ineq & A, const lin_ineq & B);
};




/**********************************************************************************
 ***  operations on linear inequality wrt set of loops and array space          ***
 ***                                                                            ***
 **********************************************************************************/
class lin_ineq_op {
    constraint kind;

public:
    lin_ineq_op(int sz, const int * k);
    lin_ineq_op(int i, int j);
    lin_ineq_op(const constraint & k) { kind = k; }

    // keeps only rows with a - value in a column i with exactly nloc+1;
    // NM_LOCATIONS up through column i in kind (inclusive);
    lin_ineq get_upper(const lin_ineq & LEQ, int nloc) const
      { return get(LEQ, nloc, -1); }
    // keeps only rows with a + value in a column i with exactly nloc+1;
    // NM_LOCATIONS up through column i in kind (inclusive);
    lin_ineq get_lower(const lin_ineq & LEQ, int nloc) const
      { return get(LEQ, nloc,  1); }
    // keeps only rows with a + value in a column i with at least nloc+1;
    // NM_LOCATIONS up through column i in kind (inclusive);
    lin_ineq get_lesser(const lin_ineq & LEQ, int nloc) const;
    // opposite of get_lesser();
    lin_ineq get_greater(const lin_ineq & LEQ, int nloc) const;

    // keeps only rows with a value of the right sign in some column i;
    // with exactly nloc+1 NM_LOCATIONS to the left of it in kind;
    lin_ineq get(const lin_ineq & LEQ, int nloc, int sgn) const;
    // opposite of get();
    lin_ineq rem(const lin_ineq & LEQ, int nloc, int sgn) const;
private:
    lin_ineq rem_upper(const lin_ineq & LEQ, int nloc) const
      { return rem(LEQ, nloc, -1); }
    lin_ineq rem_lower(const lin_ineq & LEQ, int nloc) const
      { return rem(LEQ, nloc,  1); }
    // returns rows with a value of right sign in column;
    // of nloc+1st NM_LOCATIONS with no non-0s in other NM_LOCATIONS cols;
    lin_ineq get_exclusive(const lin_ineq & LEQ, int nloc, int sgn) const;
    // returns rows with - coefficient for nlocth NM_LOCATION and others 0;
    lin_ineq get_exclusive_upper(const lin_ineq & LEQ, int nloc) const
      { return get_exclusive(LEQ, nloc, -1); }
    // returns rows with + coefficient for nlocth NM_LOCATION and others 0;
    lin_ineq get_exclusive_lower(const lin_ineq & LEQ, int nloc) const
      { return get_exclusive(LEQ, nloc,  1); }
    int check_gap(const lin_ineq & a, const lin_ineq & b) const;

public:
    lin_ineq overlap_or_contig(const lin_ineq & a, const lin_ineq & b,
                               int nloc) const;

private:

public:
    int print_code(const lin_ineq & a, const lin_ineq * strides,
                   name_store * nm, FILE * fp=stdout) const;

    void print_code(lin_ineq a, const lin_ineq * strides,
                    char ** name, FILE * fp=stdout) const;

    void single_update(lin_ineq & a, constraint repl, int rnk) const;

    int new_print_code(const lin_ineq & a, const lin_ineq * strides,
                       char ** name, FILE * fp=stdout) const;
};

#endif /* LINEAR_INEQ_H */
