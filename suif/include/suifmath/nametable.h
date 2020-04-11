/* file "nametable.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef NAMETABLE_H
#define NAMETABLE_H


#define NM_LOCATIONS  -1
#define NM_SYMBOLS    -10
#define NM_BOUNDS     -100
#define NM_CONSTANT   -1000



#define TABLESIZE      50
class name_store
{
public:
	char *vars[TABLESIZE];
	char *params[TABLESIZE];
        int varkind[TABLESIZE];
        int paramkind[TABLESIZE];
	int num_vars, num_params;
	int aux;
	name_store() { num_vars = num_params = aux = 0; }

	name_store(name_store & nm) { init(nm); }

	~name_store();

        void init(name_store & nm);

        void print(FILE * fp) {
            fprintf(fp, "Vars: ");
	    int i;
            for (i=0; i<num_vars; i++) 
                fprintf(fp, "%s ", vars[i]);
            fprintf(fp, "\nParams: ");
            for (i=0; i<num_params; i++) 
                fprintf(fp, "%s ", params[i]);
            fprintf(fp, "\n");
        }
};              

#endif
