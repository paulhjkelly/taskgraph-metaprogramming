
/* file "access_vector.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef ACCESS_VECTOR
#define ACCESS_VECTOR

class tree_node;
class tree_instr;
class tree_node_list;
class tree_proc;


// stupid c++ won't let this be a static class member
extern int forget_about_mod_this;

class access_vector;

class av_compare_info {
    int flag;
public:
    av_compare_info(const access_vector *, const access_vector *);
    int same_indregs() {return flag&1;}
    int same_conregs() {return (flag>>1)&1;}
    int same_memregs() {return (flag>>2)&1;}
    int same_paths() {return (flag>>3)&1;}
    int same_const() {return (flag>>4)&1;}
    int identical() {return (flag&31) == 31;}
    int identical_excluding_const() {return (flag&15) == 15;}
};

struct access_list_e: public alist_e {
friend class access_list;
friend class access_vector;
    access_list_e(void *v,int i): alist_e(v,reinterpret_cast<void *>(i)) {}
    ~access_list_e() {}
    void *var() {return key;}
    int val() {return reinterpret_cast<long>(info);}
};


struct access_list: public alist {
    access_list() {}
    access_list_e *search(void *v) const
             {return static_cast<access_list_e *>(alist::search(v));}
    int val(void *v) const
             {access_list_e *e=search(v); return e ? e->val() : 0;}
    int count();
    void enter(void *v,int i);  // compiler bug, can't inline
    access_list_e *pop() {return static_cast<access_list_e *>(alist::pop());}
    void intersect(access_list *a1, access_list *a2);
    void intersect(access_list *a1);
    void unite(access_list *a1, access_list *a2);
    void unite(access_list *a1);
};

struct access_list_iter: public alist_iter {
    access_list_iter(const access_list *c): alist_iter(c) {}
    ~access_list_iter() {}
    access_list_e *step() {return static_cast<access_list_e *>(alist_iter::step());}
    int is_empty() {return alist_iter::is_empty();}
    void print(FILE *f=stdout);
};

// this holds the information for one index

class access_vector: public glist_e {
    friend int av_compare_access_lists(const access_list *,
				       const access_list *);
    void normalize_step_ref(access_list_e *);
    void enter_als(const access_vector *);
public:
    int too_messy;		// if not a perfect access vector
    access_list elts;	        // access vector itself (for induction vars)
    access_list conregs;	// access vector for non-induction registers
    access_list memregs;	// access vector for indirections thru regs
    int con;		        // constant to add to vector
    tree_for *mod_this;	        // innermost for which defines a variable in
    // this access vector (may make conserv assump)
    // nil means this is completely loop constant
    // if not enclosed in a for loop, also nil
    // ignores references to induction variables
    void set_mod_this(tree_node * ti);
    int *min, *max;		// min and max values for this dimension
    void set_min_max();	        // set these values
    access_vector(): glist_e(),too_messy(0),con(0),mod_this(0),min(0),max(0) {}
    access_vector(const access_vector &);
    access_vector(tree_instr * n, int fancy=TRUE);
    access_vector(operand op, tree_node * tn, int fancy=TRUE);
    access_vector(instruction * inst, int fancy=TRUE);
    access_vector(const access_vector *);
    ~access_vector();
    int val(tree_node *a) {return elts.val(a);}
    int val(var_sym * s, int indirect)
        {access_list *l = (indirect) ? &memregs : &conregs;
         return l->val(reinterpret_cast<void *>(s));}
    void add(tree_node *a, int i) {elts.enter(a,i);}
    void add(var_sym * s, int indirect, int i)  {
        access_list *l = (indirect) ? &memregs : &conregs;
        l->enter(s, i);}
    int is_const() { return (!too_messy && elts.is_empty() &&
                             conregs.is_empty() && memregs.is_empty()); }

    void print(FILE *f=stdout);
    void normalize_step_ref();  //normalize so equiv to loop stp=1
    operand generate_code(tree_proc *p, block_symtab * sym = NULL);
    int operator ==(const access_vector &a)
        {class av_compare_info ci(this,&a);
         return ci.identical();}
    int operator !=(const access_vector &a)
        {return !(*this==a);}
    void operator =(const access_vector &);
    void operator += (const access_vector &a);
    void operator -= (const access_vector &a);
    void operator *= (int f);
    void operator /= (int f);
    access_vector operator +(const access_vector &a)
        {access_vector tmp(*this); tmp += a; return tmp;}
    access_vector operator -(const access_vector &a)
        {access_vector tmp(*this); tmp -= a; return tmp;}
    access_vector operator -()
        {access_vector tmp(*this); tmp *= -1; return tmp;}
    friend inline access_vector operator *(int f,const access_vector &a);
    friend inline access_vector operator *(const access_vector &a,int f);
    friend inline access_vector operator /(const access_vector &a,int f);
};

inline access_vector operator *(int f,const access_vector &a)
{access_vector tmp(a); tmp *= f; return tmp;}
inline access_vector operator *(const access_vector &a,int f)
{access_vector tmp(a); tmp *= f; return tmp;}
inline access_vector operator /(const access_vector &a,int f)
{access_vector tmp(a); tmp /= f; return tmp;}

// a list of access vectors, one for each index of the array (in order)
class array_info: public glist {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    array_info(const array_info &)  { assert(FALSE); }
    void operator=(const array_info &)  { assert(FALSE); }
public:
    array_info(): glist() {}
    array_info(instruction * ins, int fancy=TRUE);
    array_info(array_info *);
    ~array_info();
    access_vector *pop() {return static_cast<access_vector *>(glist::pop());}
    void append(access_vector *a) {glist::append(static_cast<glist_e *>(a));}
    int is_empty() {return glist::is_empty();}
    int count() {return glist::count();}
    void print(FILE *f=stdout);
    void normalize_step_ref();  //normalize so equiv to loop step=1
    access_vector *first() {return static_cast<access_vector *>(glist::head());}
    access_vector *last() {return static_cast<access_vector *>(glist::tail());}
};

// the iterator, to examine the access vector for each index in turn
class array_info_iter: public glist_iter {
public:
    array_info_iter(array_info *v): glist_iter(static_cast<glist *>(v)) {}
    ~array_info_iter() {}
    access_vector *step()
        {return static_cast<access_vector *>(glist_iter::step());}
    access_vector *next()
        {return static_cast<access_vector *>(glist_iter::nxt);}
    int is_empty() {return glist_iter::is_empty();}
};

void normalize_step_loops(array_info *,array_info *,
                          access_vector *, tree_for_test);
void normalize_test(array_info *, array_info  *,access_vector *,
                    tree_for_test);

#endif /* ACCESS_VECTOR */
