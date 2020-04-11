/*  Symbolic Address Class */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef SYMADDR_H
#define SYMADDR_H

#pragma interface

RCS_HEADER(symaddr_h,
    "$Id$")

class in_stream;
class out_stream;
class base_symtab;
class sym_node;


/*
 *  A sym_addr is used to encode a symbolic address consisting of a symbol
 *  and an unsigned offset (in bits).  They are currently used only in immed
 *  values.  The dataonly part is for inclusion in unions.
 */

class sym_addr_dataonly {
protected:
    sym_node *sym;
    unsigned off;

public:
    boolean operator==(const sym_addr_dataonly& s)
	{ return (sym == s.sym) && (off == s.off); }
    boolean operator!=(const sym_addr_dataonly& s)
	{ return !(*this == s); }
};


class sym_addr : public sym_addr_dataonly {
public:
    sym_addr(sym_node *s, unsigned o)	{ sym = s; off = o; }
    sym_addr(in_stream *is, base_symtab *symtab);
    sym_addr(const sym_addr &s)		{ sym = s.sym; off = s.off; }

    sym_node *symbol()			{ return sym; }
    unsigned offset()			{ return off; }

    sym_addr& operator=(sym_addr& s)	{ sym = s.sym; off = s.off; return *this; }

    void write(out_stream *os);
    void print(FILE *f);
};

#endif /* SYMADDR_H */
