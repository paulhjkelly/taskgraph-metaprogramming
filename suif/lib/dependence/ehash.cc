/* file "ehash.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
//
//  hash.c
//
//  routines to cache U and D of GCD test
//  hash on A
//     hash function is size/2 + m+n+A.vars[0] + 2*A.vars[1]+4*A.vars[2] ...
//
//  and routines to cache X of GCD test
//  hash on A
//     hash function is size/2 + m+n+A.vars[0] + 2*A.vars[1]+4*A.vars[2] ...
//                       + (2^m*n)*A.constant[0] ...
//
// and routines to cache solution to general problem with bounds
// hash on S
//	hash function is size/2 + m+n+S[0][0] + 2*S[0][1]...
//
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "include.h"
#include "ehash.h"
#include <cstdio>
#include <cstring>

full_hash_table::full_hash_table(int n)
{
	table = new full_hash_entry[n]; 
	size=n;
	num_hits =0; 
	num_misses=0;
}

full_hash_table::~full_hash_table() {
	delete[] table;
}

inline int int_abs(int x) { return x >= 0 ? x : -x; }

// UD hash table

// return the hash entry corresponding to A
UD_hash_entry *UD_hash_table::ref(coeff *A)
{
	int sum = this->size/2 + A->m + A->n;

	for (int i=0; i < A->m*A->n; i++) {
		sum += (2<<i)*A->vars[i];
	}
	sum = int_abs(sum % this->size);
	return(table + sum);
}

//  is this a cache hit or a cache miss
// if a hit then set U and D
boolean UD_hash_table::hit_or_miss(coeff *A, int **U, int **D) 
{
	UD_hash_entry *entry = this->ref(A);
	boolean hit;

	do {
	    hit =
		(entry->full && (A->n == entry->A->n) && 
		(A->m == entry->A->m) &&
	        !memcmp(A->vars, entry->A->vars,
			A->n*A->m*sizeof(*(A->vars))));
	    if (!hit) entry=entry->next;
	} while(!hit && entry);

	if (hit) {
		num_hits++; 
		*U = entry->U;
		*D = entry->D;
	} else { 
		num_misses++;
	}
	return hit;
}


// add an entry to the table 
void UD_hash_table::add_entry(coeff *A, int *U, int *D)
{
	UD_hash_entry *entry = this->ref(A);

	if (entry->full) {
		while(entry->next) entry = entry->next;
		entry->next = new UD_hash_entry;
		entry = entry->next;
	}
	
	entry->full = TRUE;
	entry->U = U;
	entry->D = D;
	entry->A = A;
}


// X hash table

// return the hash entry corresponding to A
X_hash_entry *X_hash_table::ref(coeff *A)
{
	int sum = this->size/2 + A->m + A->n;

	int i;
	for (i=0; i < A->m*A->n; i++) {
		sum += (2<<i)*A->vars[i];
	}
	for (i=0; i<A->m; i++) {
		sum += (2 << (A->m*A->n + i))*A->constant[i];
	}
	sum = int_abs(sum % this->size);
	return(table + sum);
}

//  is this a cache hit or a cache miss
// if a hit then set X and indep
boolean X_hash_table::hit_or_miss(coeff *A, coeff **X,boolean *indep)
{
	X_hash_entry *entry = this->ref(A);
	
	boolean hit;
	do {
	    hit =
		(entry->full && (A->n == entry->A->n) && 
		(A->m == entry->A->m) &&
		!memcmp(A->constant, entry->A->constant,
			A->m * sizeof(*(A->constant))) &&
	        !memcmp(A->vars, entry->A->vars,
			A->n*A->m*sizeof(*(A->vars))));
	    if (!hit) entry=entry->next;
	} while(!hit && entry);


	if (hit) {
		num_hits++; 
		*X = entry->X;
		*indep = entry->indep;
	} else {
		num_misses++;
	}
	return hit;
}


// add an entry to the table 
void X_hash_table::add_entry(coeff *A, coeff *X,boolean ind)
{
	X_hash_entry *entry = this->ref(A);

	if (entry->full) {
		while(entry->next) entry = entry->next;
		entry->next = new X_hash_entry;
		entry = entry->next;
	}
	
	entry->full = TRUE;
	entry->A = A;
	if (!ind) entry->X = X;
	entry->indep = ind;
}


// full hash table

// return the hash entry corresponding to S
full_hash_entry *full_hash_table::ref(integer_matrix *S)
{
	int sum = this->size/2 + (*S).m() + (*S).n();

	int count =0;
	for (int i=0; i < (*S).m(); i++) {
		for (int j=0; j<(*S).n(); j++) {
			sum += (2<<count)*(*S)[i][j];
			count++;
		}
	}
	sum = int_abs(sum % this->size);
	return(table + sum);
}

//  is this a cache hit or a cache miss
// if a hit then set dl and indep
boolean full_hash_table::hit_or_miss(integer_matrix *S, integer_matrix *L, 
				integer_matrix *U,boolean *lb_valid,
				boolean *ub_valid,dir_list **dl,boolean *indep,
				int num_symb)
{
	full_hash_entry *entry = this->ref(S);
	
	boolean hit;
	int size = L->n() * sizeof(boolean);
	do {
	    hit =
		(entry->full && 
		(num_symb == entry->num_symb) &&
		((*S) == *entry->S) && ((*L) == *entry->L) && 
		((*U) == *entry->U) &&
		!memcmp(lb_valid, entry->lb_valid, size) &&
		!memcmp(ub_valid, entry->ub_valid, size)); 
	    if (!hit) entry=entry->next;
	} while(!hit && entry);


	if (hit) {
		num_hits++; 
		if (entry->result) {
			*dl = new dir_list(entry->result);
		} else {
			*dl = 0;
		}
		*indep = entry->indep;
	} else {
		num_misses++;
	}
	return hit;
}


// add an entry to the table 
void full_hash_table::add_entry(integer_matrix *S, integer_matrix *L, integer_matrix *U,
			   boolean *lb_valid, boolean *ub_valid,dir_list *dl, 
			   boolean indep,int num_symb)
{
	full_hash_entry *entry = this->ref(S);

	if (entry->full) {
		while(entry->next) entry = entry->next;
		entry->next = new full_hash_entry;
		entry = entry->next;
	}
	
	entry->full = TRUE;
	entry->S = S;
	entry->L = L;
	entry->U = U;
	entry->num_symb = num_symb;
	entry->lb_valid = lb_valid;
	entry->ub_valid = ub_valid;
	if (!indep) {
		entry->result = dl;
	} else {
		entry->result = 0;
	}
	entry->indep = indep;
}


