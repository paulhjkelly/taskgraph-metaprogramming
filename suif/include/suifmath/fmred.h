/* file "fmred.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef FMRED_H
#define FMRED_H

enum red_operation { RO_DONT_REDUCE,
                     RO_REDUCE_SHOW,
                     RO_REDUCE_AWAY
                     };


struct column_stat {
    red_operation cs_op;
    int           cs_rank;
};


class fmred {
public:
    fmred();
    void set(column_stat in[]);
    void set(lin_ineq & in);
    lin_ineq reduce();
};


#endif
