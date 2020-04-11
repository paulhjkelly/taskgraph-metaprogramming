/*  Instruction Opcode Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef OPCODES_H
#define OPCODES_H

RCS_HEADER(opcodes_h,
    "$Id$")

/*
 *  Opcode Enumerated Type.  The io_ prefix stands for "intermediate
 *  form opcode".  The special value io_last is used to represent the last
 *  value in the enumeration in range comparisons.
 */

enum if_ops {
    io_eos,				/* end-of-stream; internal use only */
    io_mrk,				/* mark */
    io_data,				/* -- no longer used -- */
    io_cpy,				/* copy */
    io_nop,				/* no op */
    io_add,				/* add */
    io_sub,				/* subtract */
    io_neg,				/* negate */
    io_mul,				/* multiply */
    io_div,				/* divide */
    io_rem,				/* remainder */
    io_mod,				/* modulus */
    io_min,				/* minimum */
    io_max,				/* maximum */
    io_not,				/* bitwise not */
    io_and,				/* bitwise and */
    io_ior,				/* bitwise inclusive or */
    io_xor,				/* bitwise exclusive or */
    io_abs,				/* absolute value */
    io_asr,				/* arithmetic shift right */
    io_lsl,				/* logical shift left */
    io_lsr,				/* logical shift right */
    io_rot,				/* rotate */
    io_cvt,				/* convert */
    io_ldc,				/* load constant */
    io_lod,				/* load */
    io_str,				/* store */
    io_seq,				/* set equal */
    io_sne,				/* set not equal */
    io_sl,				/* set less than */
    io_sle,				/* set less than or equal */
    io_btrue,				/* branch if true */
    io_bfalse,				/* branch if false */
    io_jmp,				/* unconditional jump */
    io_cal,				/* call */
    io_ret,				/* return */
    io_lab,				/* label */
    io_array,				/* array reference */
    io_mbr,				/* multi-way branch */
    io_divfloor,			/* divide with floor */
    io_divceil,				/* divide with ceiling */
    io_memcpy,				/* memory-to-memory copy */
    io_gen,				/* generic instruction */
    io_last
};


/*
 *  The if_ops_name function may be called to return the string name of
 *  an if_ops value.
 */

extern const char * if_ops_name(if_ops o);


/*
 *  Format tags.  Each opcode refers to an instruction of a certain
 *  format.  For example, the io_add has format inf_rrr.  The function
 *  which_format provide this mapping.  Instructions have several
 *  formats depending on how many and what kind of arguments they have.
 *  They can be discriminated into several groups each represented by a
 *  subtype of instruction.  The subtype being used for a specific
 *  opcode can be determined by calling which_format.
 */

enum inst_format {
    inf_none,
    inf_rrr,
    inf_bj,
    inf_ldc,
    inf_cal,
    inf_array,
    inf_mbr,
    inf_lab,
    inf_gen
};


/*
 *  The which_format function returns the format used by each opcode.
 */

extern inst_format which_format(if_ops o);

#endif /* OPCODES_H */
