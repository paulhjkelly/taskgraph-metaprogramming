/* file "dvector.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
#ifndef DVECTOR
#define DVECTOR

#ifdef CC2
#define FCONST const
#else
#define FCONST
#endif

/* mgcd.h */



int Gcd(coeff A, coeff **X);
class dvlist;
class tn_list;

enum direction {d_lt=1,d_gt=2,d_eq=4,d_le=5,d_ge=6,d_lg=3,d_star=7};

class distance {
friend class distance_vector_e;		// so it can call set_distance.
#define DIST_BIT 8
#define DIST_SHIFT 4
#define MAX_DIST (1<<20)
    // assumptions: if DIST_BIT is not set, then all upper bits are zero.
    int val;
public:
    void set_direction(direction newdr)
    {val = newdr; if (newdr == d_eq) {val |= DIST_BIT;assert(is_const());}}
    void set_star() {set_direction(d_star);}
    void set_distance(int newd) {
        assert(newd >= -MAX_DIST && newd <= MAX_DIST);
        val = (newd << DIST_SHIFT) | DIST_BIT;
        if(newd > 0) val |= int(d_lt);
        else if(newd < 0) val |= int(d_gt);
        else val |= int(d_eq);}
    int is_const() FCONST	// returns true if distance is known
    {return val&DIST_BIT?TRUE:FALSE;}
    int dist() FCONST	// returns true distance
    {assert(is_const()); return val >> DIST_SHIFT;}
    direction dir()	FCONST	// returns true direction
    {return static_cast<direction>(val&(DIST_BIT-1));}
    int is_star() FCONST  	// returns true if direction is unknown
    {return(dir()==d_star);}
    int has_eq() { return (val == d_eq) || (val == d_le) ||
                   (val == d_ge) || (val == d_star);};
    void print(FILE *);
    distance(int dst) {set_distance(dst);}
    distance(){set_star();}
    distance(distance *ds){val = ds->val;}
    ~distance() {}
    int is_zero() FCONST
    {int ans = dir()==d_eq;
     if(ans) { assert(val==(d_eq|DIST_BIT)); }
     return ans;}
    void negate();			/* negate the distance */
    int operator ==(distance &d) FCONST
    {return val == d.val;}
    int operator !=(distance &d) FCONST
    {return val != d.val;}
    void operator =(distance &d) {val = d.val;}
    distance(const distance &ds) {val = ds.val;}
#undef DIST_BIT
#undef DIST_SHIFT
#undef MAX_DIST
};


struct distance_vector_e: public glist_e {
    distance d;
    distance_vector_e(distance dd): glist_e()
    {d = dd; }
    ~distance_vector_e() {}
    int is_zero() {return d.is_zero();}
};


struct distance_vector: public glist {
    short ind;	// is there no dependency

    void set_indep()  { ind = TRUE; }
    void set_dep()  { ind = FALSE; }
    distance_vector_e *pop()
    {return static_cast<distance_vector_e *>(glist::pop());}
    void push(distance_vector_e *d)
    {glist::push(static_cast<glist_e *>(d));}
    void append(distance d)
    {glist::append(static_cast<glist_e *>(new distance_vector_e(d)));}
    distance_vector(short in) {ind = in;}
    distance_vector(array_info *,array_info *, tn_list *, int flow=0);
    distance_vector (array_info *, array_info *,
                     int minnest, int valid_dim, tn_list * al,int flow=0);
    distance_vector (distance_vector *);
    distance_vector() { set_dep(); };
    ~distance_vector() {}
    short indep() { return ind; }
    void add_unused(int *used,int n);  // add in unused components
    void print(FILE *);
    boolean is_empty()
    {return glist::is_empty();}
    int is_zero();			/* is the vector 0? */
    int is_pos();			/* is first entry positive? */
    int is_neg();			/*                negative? */
    int is_star();			/*                star? */
    int level();			/* what is the level of this dep? */
    int size();			/* how big is the dvector? */
    void make_pos();		/* turn leading >= into = */
    distance entry(int level);	/* what is the level entry? */
    void set_entry(distance,int level);	/* set the level entry? */
    distance *thresh(int level);     /* what is the threshold? must be # */
    void negate();			/* negate all of the distances */
    int first_not_eq();		/* level of the first non-= or * */
    /*   if all =, returns 0 */
    int operator==(distance_vector &); /* if two vectors identical */
    int operator!=(distance_vector &d)
    {return !(*this == d);}
};

struct distance_vector_iter: public glist_iter {
	distance_vector_iter(distance_vector *dv): glist_iter(static_cast<glist *>(dv)){}
	distance_vector_e *step()
		{return static_cast<distance_vector_e *>(glist_iter::step());}
	distance_vector_e *next() { return(static_cast<distance_vector_e*>(nxt)); }
	int is_empty() {return glist_iter::is_empty();}
};


// list of distance vectors
struct dvlist_e: public glist_e {
    distance_vector *dv;
    dvlist_e(distance_vector *dd): glist_e()
    { dv = dd; }
    ~dvlist_e() {}

};


class dvlist: public glist {
private:
    short ind;
    void dv_calc(array_info *, array_info *, int, int, tn_list*,int flow);
public:
    void set_indep()  { ind = TRUE; }
    void set_dep()  { ind = FALSE; }
    int indep() { return ind; }
    void collapse(distance_vector *out); // collapse into distance_vector
    dvlist_e *pop()
    {return static_cast<dvlist_e *>(glist::pop()); }
    void push (dvlist_e *d)
    {glist::push(static_cast<glist_e *>(d)); }
    dvlist(array_info *, array_info *, tn_list *,int lexpos=0,int flow=0);
    dvlist(array_info *, array_info *,
           int minnest, int valid_dim, tn_list *al,int lexpos=0,int flow=0);
    dvlist(){set_dep();}
    ~dvlist() {}
    void clear();
    void print(FILE *);
    int elim_bad_symb(array_info *a1, array_info *a2);
    access_list *find_symb(array_info *a);
    access_list *find_symbs(array_info *a1,array_info *a2);
    void make_lexpos(boolean strict_pos = FALSE);
};

struct dvlist_iter: public glist_iter {
	dvlist_iter(dvlist *dv): glist_iter(static_cast<glist *>(dv)) {}
	dvlist_e *step()
		{ return static_cast<dvlist_e *>(glist_iter::step());}
	dvlist_e *next() { return static_cast<dvlist_e*>(nxt); }
	int is_empty() {return glist_iter::is_empty();}
};

#endif

