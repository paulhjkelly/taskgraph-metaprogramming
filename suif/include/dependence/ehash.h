/* file "ehash.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

// hash.h
//
// two sets of caches:
//    1 caches U and D of GCD test, this hits if same variable refs
//    other caches X of GCD test, this only hits if same vars and constants
//
// CACHE FOR UD

// one entry in the hash table
class UD_hash_entry {
	friend class UD_hash_table;
	boolean full;
	int *U, *D;
	coeff *A;
	UD_hash_entry *next;

public:
	UD_hash_entry() { A = 0; D = 0; U = 0; full = FALSE; next=0;}
	~UD_hash_entry() { delete A; delete[] U; delete[] D; }
};


class UD_hash_table {
	UD_hash_entry *table;  // the table
	int size;	    // number of entries in table
	UD_hash_entry *ref(coeff *);

public:
	int num_hits;
	int num_misses;
	UD_hash_table(int n) {table = new UD_hash_entry[n]; size=n;
			   num_hits =0; num_misses=0;}
	~UD_hash_table() { delete[] table; table = 0; }
	boolean hit_or_miss(coeff *,int **, int **);  
	void add_entry(coeff *, int *, int *);  
};



// X cache

// one entry in the hash table
class X_hash_entry {
	friend class X_hash_table;
	boolean full;
	boolean indep;
	coeff *X;
	coeff *A;
	X_hash_entry *next;

public:
	X_hash_entry() { A = 0; X = 0; full = FALSE; next=0;}
	~X_hash_entry() { delete A; delete X; }
};


class X_hash_table {
	X_hash_entry *table;  // the table
	int size;	    // number of entries in table
	X_hash_entry *ref(coeff *);

public:
	int num_hits;
	int num_misses;
	X_hash_table(int n) {table = new X_hash_entry[n]; size=n;
			   num_hits =0; num_misses=0;}
	~X_hash_table() { delete[] table; table = 0; }
	boolean hit_or_miss(coeff *,coeff **,boolean *);
	void add_entry(coeff *, coeff *,boolean);
};



// bounds cache

// one entry in the hash table
class full_hash_entry {
	friend class full_hash_table;
	boolean full;
	boolean indep;
	integer_matrix *S;
	integer_matrix *L;
	integer_matrix *U;
	boolean *lb_valid;
	boolean *ub_valid;
	int num_symb;
	dir_list *result;
	full_hash_entry *next;

public:
	full_hash_entry() { lb_valid = 0; ub_valid = 0; full = FALSE; next=0;}
	~full_hash_entry() { delete[] lb_valid; delete[] ub_valid; }
};


class full_hash_table {
	full_hash_entry *table;  // the table
	int size;	    // number of entries in table
	full_hash_entry *ref(integer_matrix *S);

public:
	int num_hits;
	int num_misses;
	full_hash_table(int n);
	~full_hash_table();
	boolean hit_or_miss(integer_matrix *S,integer_matrix *L,
			    integer_matrix *U, boolean *lb_valid,
			    boolean *ub_valid, dir_list **dl,boolean *,int);
	void add_entry(integer_matrix *S, integer_matrix *L, integer_matrix *U,
		       boolean *lb_valid, boolean *ub_valid,dir_list *dl,
			boolean indep,int num_symb);
};


