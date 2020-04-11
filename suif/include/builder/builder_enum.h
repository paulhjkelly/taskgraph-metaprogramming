/* file "builder_enum.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

/***************************************************************************
 ***************************************************************************
 *
 *                     S U I F   C o m p i l e r  S y s t e m
 *
 *                                B U I L D E R
 *
 ***************************************************************************
 ***************************************************************************/


enum unary_op {  uop_minus,                     // -a
                 uop_not,                       // ~a
                 uop_lnot,                      // !a
                 uop_abs,                       // abs(a) or |a|
                 uop_addr,                      // address of (&a)
                 uop_dref                       // dereferencing a ptr (*a)
};

enum binary_op { bop_mul,                       // a * b
                 bop_div,                       // a / b
                 bop_mod,                       // a % b  (always positive result)
                 bop_rem,                       // same as a % b (not always positive)
                 bop_add,                       // a + b
                 bop_sub,                       // a - b
                 bop_lshift,                    // a << b
                 bop_rshift,                    // a >> b
                 bop_less,                      // (a < b)
                 bop_gt,                        // (a > b)
                 bop_leq,                       // (a <= b)
                 bop_geq,                       // (a >= b)
                 bop_eq,                        // (a == b)
                 bop_neq,                       // (a != b)
                 bop_and,                       // a & b
                 bop_xor,                       // a ^ b
                 bop_or,                        // a | b
                 bop_land,                      // a && b
                 bop_lor,                       // a || b
                 bop_divfloor,             // (int)floor((double)a/(double)b)
                 bop_divceil,              // (int)ceil((double)a/(double)b)
                 bop_min,                       // min(a, b)
                 bop_max                        // max(a, b)
};

enum assign_op { aop_eq,                        // a = b;
                 aop_mod,                       // a = a % b;
                 aop_add,                       // a = a + b;
                 aop_sub,                       // a = a - b;
                 aop_mul,                       // a = a * b;
                 aop_div,                       // a = a / b;
                 aop_rshift,                    // a = a >> b;
                 aop_lshift,                    // a = a << b;
                 aop_and,                       // a = a & b;
                 aop_xor,                       // a = a ^ b;
                 aop_or                         // a = a | b;
};

