/*  Opcodes Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#define RCS_BASE_FILE opcodes_cc

#include "misc.h"
#include "opcodes.h"

RCS_BASE(
    "$Id$")

/*
 *  Opcode names.  Used by the if_ops_name() function.
 */

static const char *if_op_names[] = {
    "eos",
    "mrk",
    "unused",				/* -- no longer used -- */
    "cpy",
    "nop",
    "add",
    "sub",
    "neg",
    "mul",
    "div",
    "rem",
    "mod",
    "min",
    "max",
    "not",
    "and",
    "ior",
    "xor",
    "abs",
    "asr",
    "lsl",
    "lsr",
    "rot",
    "cvt",
    "ldc",
    "lod",
    "str",
    "seq",
    "sne",
    "sl",
    "sle",
    "btrue",
    "bfalse",
    "jmp",
    "cal",
    "ret",
    "lab",
    "array",
    "mbr",
    "divfloor",
    "divceil",
    "memcpy",
    "gen"
};


const char *
if_ops_name (if_ops o)
{
    if (o >= io_last) {
	return "error";
    }
    return if_op_names[o];
}


/*
 *  Opcode formats.  Used by the which_format() function.
 */

static inst_format if_op_format[] = {
    inf_none,	/* eos */
    inf_rrr,	/* mrk */
    inf_none,	/* unused */		/* -- no longer used -- */
    inf_rrr,	/* cpy */
    inf_rrr,	/* nop */
    inf_rrr,	/* add */
    inf_rrr,	/* sub */
    inf_rrr,	/* neg */
    inf_rrr,	/* mul */
    inf_rrr,	/* div */
    inf_rrr,	/* rem */
    inf_rrr,	/* mod */
    inf_rrr,	/* min */
    inf_rrr,	/* max */
    inf_rrr,	/* not */
    inf_rrr,	/* and */
    inf_rrr,	/* ior */
    inf_rrr,	/* xor */
    inf_rrr,	/* abs */
    inf_rrr,	/* asr */
    inf_rrr,	/* lsl */
    inf_rrr,	/* lsr */
    inf_rrr,	/* rot */
    inf_rrr,	/* cvt */
    inf_ldc,	/* ldc */
    inf_rrr,	/* lod */
    inf_rrr,	/* str */
    inf_rrr,	/* seq */
    inf_rrr,	/* sne */
    inf_rrr,	/* sl */
    inf_rrr,	/* sle */
    inf_bj,	/* btrue */
    inf_bj,	/* bfalse */
    inf_bj,	/* jmp */
    inf_cal,	/* cal */
    inf_rrr,	/* ret */
    inf_lab,	/* lab */
    inf_array,	/* array */
    inf_mbr,	/* mbr */
    inf_rrr,	/* divfloor */
    inf_rrr,	/* divceil */
    inf_rrr,	/* memcpy */
    inf_gen	/* gen */
};


inst_format
which_format (if_ops o)
{
    if (o >= io_last) {
	return inf_none;
    }
    return if_op_format[o];
}


