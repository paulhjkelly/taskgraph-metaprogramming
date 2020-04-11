/* file "builder.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libbuilder.a"
#pragma implementation "builder_enum.h"
#pragma implementation "builder_internals.h"
#pragma implementation "builder_def.h"
#define BUILDERLIB


/***************************************************************************
 ***************************************************************************
 *
 *                     S U I F   C o m p i l e r  S y s t e m
 *
 *                                B U I L D E R
 *
 ***************************************************************************
 ***************************************************************************/


#include <suif1.h>
#include <cstdlib>
#include <cstring>
#include "builder.h"
#include "bldtypes.h"


error_type  builder_base::error_level = et_none;
const char* builder_base::error_file = NULL;
int         builder_base::error_line = 0;
tree_proc*  builder_base::proc = NULL;
block*      builder::B_NOOP = new block();


void delete_static_builder_data( void ) {
  delete builder::B_NOOP;
  builder::B_NOOP = NULL;
}

int bdebug0 = 0;

block_symtab * builder_base::BlockSymTab = NULL;


/***************************************************************************
 * copy any operator                                                       *
 ***************************************************************************/
void any_oper::init(const any_oper & a)
{
    ot = a.ot;
    uop = a.uop;
    bop = a.bop;
    aop = a.aop;
}

/***************************************************************************
 * copy block info                                                         *
 ***************************************************************************/
void block_info::init(const block_info & b)
{
    op = b.op;
    ct = b.ct;
    ncpy = NULL;
    switch(b.ct) {
    case ct_none:
        break;
    case ct_sym_node:
        code.sym = b.code.sym;
        break;
    case ct_struct_access:
        code.sta = new struct_access(b.code.sta);
        break;
    case ct_tree_node:
        code.trn = b.code.trn;
        ncpy = b.ncpy;
        break;
    case ct_tree_node_list:
        code.tnl = b.code.tnl;
        ncpy = b.ncpy;
        break;
    case ct_instruction:
        code.ins = b.code.ins;
        break;
    case ct_type_node:
        code.typ = b.code.typ;
        break;
    }
    cst = b.cst;
    switch(b.cst) {
    case cst_none:
        break;
    case cst_int:
        con.icon = b.con.icon;
        break;
    case cst_double:
        con.dcon = b.con.dcon;
        break;
    case cst_float:
        con.fcon = b.con.fcon;
        break;
    case cst_char:
        con.ccon = b.con.ccon;
        break;
    case cst_ext_int:
    case cst_ext_float:
    case cst_str:
        con.scon = b.con.scon;
        break;
    }
}



/* ##################################################
   #####   builder, block                       #####
   ################################################## */


block::~block()
{
    if(child) delete child;
    if(sib) delete sib;
}


/***************************************************************************
 * create an empty block, each other init should call this first.          *
 ***************************************************************************/
void builder::init()
{
    bt = bt_none;
    nt = nt_none;
    child = NULL;
    sib = NULL;
}

/***************************************************************************
 * make a copy of another block into this                                  *
 ***************************************************************************/
void builder::init(const block * B)
{
    assert(B);
    bt = B->bt;
    nt = B->nt;
    info = B->info;
    child = NULL;
    sib = NULL;
    if(B->child) {
        child = new block(B->child);
        const block * chk = B->child;
        block * curr = child;
        while(chk->sib) {
            curr->sib = new block(chk->sib);
            chk = chk->sib;
            curr = curr->sib;
        }
    }
}


void builder::init(const block & B)
{
    init(&B);
}


/***************************************************************************
 * concatinate a list of blocks                                            *
 ***************************************************************************/
block::block(const block & ex1, const block & ex2, const block & ex3,
             const block & ex4, const block & ex5, const block & ex6,
             const block & ex7, const block & ex8, const block & ex9,
             const block & ex10, const block & ex11, const block & ex12)
{
    init(ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12);
}

void builder::init(const block & ex1, const block & ex2, const block & ex3, const block & ex4,
                   const block & ex5, const block & ex6, const block & ex7, const block & ex8,
                   const block & ex9, const block &ex10, const block &ex11, const block &ex12)
{
    init();

    bt = bt_stmt;
    nt = nt_stmt_list;

    block * curr = (block *)this;

    assert(&ex1 != B_NOOP);
    assert(&ex2 != B_NOOP);
    if(check_grammer(bt_stmt, ex1)==0)
        ERROR(et_grammer, ("parameter is not a expr but  %s", get_bt(ex1.bt)));
    curr = add_stmt(curr, ex1);
    curr = add_stmt(curr, ex2);
    assert(curr == (block *)this);
    if(&ex3 != B_NOOP) {
        curr = add_stmt(curr, ex3);
        if(&ex4 != B_NOOP) {
            curr = add_stmt(curr, ex4);
            if(&ex5 != B_NOOP) {
                curr = add_stmt(curr, ex5);
                if(&ex6 != B_NOOP) {
                    curr = add_stmt(curr, ex6);
                    if(&ex7 != B_NOOP) {
                        curr = add_stmt(curr, ex7);
                        if(&ex8 != B_NOOP) {
                            curr = add_stmt(curr, ex8);
                            if(&ex9 != B_NOOP) {
                                curr = add_stmt(curr, ex9);
                                if(&ex10 != B_NOOP) {
                                    curr = add_stmt(curr, ex10);
                                    if(&ex11 != B_NOOP) {
                                        curr = add_stmt(curr, ex11);
                                        if(&ex12 != B_NOOP) {
                                            curr = add_stmt(curr, ex12);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    assert(curr == (block *)this);
}



/***************************************************************************
 * output                                                                  *
 *                                                                         *
 ***************************************************************************/
tree_node * block::make_tree_node(block_symtab * bs)
{
    return get_tree_node(*this, bs);
}

tree_node_list * block::make_tree_node_list(block_symtab * bs)
{
    return get_tree_node_list(*this, bs);
}

instruction * block::make_instruction(block_symtab * bs)
{
    return get_instruction(*this, bs);
}

tree_node * block::make_tree_node()
{
    return get_tree_node(*this, get_scope());
}

tree_node_list * block::make_tree_node_list()
{
    return get_tree_node_list(*this, get_scope());
}

instruction * block::make_instruction()
{
    return get_instruction(*this, get_scope());
}

tree_node * block::make_tree_node(tree_node * t)
{
    return get_tree_node(*this, get_scope(t));
}

tree_node_list * block::make_tree_node_list(tree_node * t)
{
    return get_tree_node_list(*this, get_scope(t));
}

instruction * block::make_instruction(tree_node * t)
{
    return get_instruction(*this, get_scope(t));
}

tree_node * block::make_tree_node(tree_node_list * tl)
{
    return get_tree_node(*this, get_scope(tl));
}

tree_node_list * block::make_tree_node_list(tree_node_list * tl)
{
    return get_tree_node_list(*this, get_scope(tl));
}

instruction * block::make_instruction(tree_node_list * tl)
{
    return get_instruction(*this, get_scope(tl));
}

tree_node * block::make_tree_node(instruction * i)
{
    return get_tree_node(*this, get_scope(i));
}

tree_node_list * block::make_tree_node_list(instruction * i)
{
    return get_tree_node_list(*this, get_scope(i));
}

instruction * block::make_instruction(instruction * i)
{
    return get_instruction(*this, get_scope(i));
}



block_symtab * builder::get_scope()
{
    return get_proc()->symtab();
}

block_symtab * builder::get_scope(tree_node * tn)
{
    assert(tn);
    if(tn->kind() == TREE_BLOCK) {
        tree_block * tb = (tree_block *)tn;
        return tb->symtab();
    }
    assert(tn->parent());
    return get_scope(tn->parent());
}

block_symtab * builder::get_scope(tree_node_list * tnl)
{
    assert(tnl);
    return get_scope(tnl->parent());
}

block_symtab * builder::get_scope(instruction * i)
{
    assert(i);
    return get_scope(i->parent());
}




tree_node * builder::get_tree_node(const block & B, block_symtab * bs)
{
    BlockSymTab = bs;
    return generate_tree_node(B);
}

tree_node_list * builder::get_tree_node_list(const block & B, block_symtab * bs)
{
    BlockSymTab = bs;
    return generate_tree_node_list(B);
}

instruction * builder::get_instruction(const block & B, block_symtab * bs)
{
    BlockSymTab = bs;
    return generate_instruction(B);
}


/***************************************************************************
 * <expr>                                                                  *
 *                                                                         *
 ***************************************************************************/

block * builder::bld_op(char * inop, const block & A)
{
    block * ret;
    any_oper * oper = which_op(inop);

    if((oper->ot != ot_uni)&&(oper->ot != ot_ub))
        ERROR(et_oper, ("Unary operator expected, got %s", inop));

    ret = bld_op(oper->uop, A);

    delete oper;
    return ret;
}



block & block::ARRAY(const block & curr,
                     const block & ex1, const block & ex2, const block & ex3,
                     const block & ex4, const block & ex5, const block & ex6)
{
    block * b = bld_array(curr, ex1, ex2, ex3, ex4, ex5, ex6);
    return *b;
}


block * builder::bld_array(const block & curr,
                           const block & ex1, const block & ex2, const block & ex3,
                           const block & ex4, const block & ex5, const block & ex6)
{
    block * ptr;
    block * ret;
    if(curr.nt == nt_array) {
        ret = new block(curr);
        ptr = ret->child;
        while(ptr->sib)
            ptr = ptr->sib;
    } else {
        ret = new block;
        ret->init();
        ret->bt = bt_expr;
        ret->nt = nt_array;
        ret->child  = new block(curr);
        ptr = ret->child;
    }

    if(&ex1 != B_NOOP) {
        ptr = add_call(ptr, ex1);
        if(&ex2 != B_NOOP) {
            ptr = add_call(ptr, ex2);
            if(&ex3 != B_NOOP) {
                ptr = add_call(ptr, ex3);
                if(&ex4 != B_NOOP) {
                    ptr = add_call(ptr, ex4);
                    if(&ex5 != B_NOOP) {
                        ptr = add_call(ptr, ex5);
                        if(&ex6 != B_NOOP) {
                            ptr = add_call(ptr, ex6);
                        }
                    }
                }
            }
        }
    }
    return ret;
}


block * builder::bld_op(unary_op inop, const block & A)
{
    block * ret = new block();

    if(check_grammer(bt_expr, A)==0)
        ERROR(et_grammer, ("Expression expected with %s got %s", get_op(ot_uni, inop), get_bt(A.bt)));

    if(bdebug0) printf("uop(%s) ", get_op(ot_uni, inop));

    ret->bt = bt_expr;
    ret->nt = nt_oper;
    ret->info.set(ot_uni, inop);
    ret->child =  new block(A);

    return ret;
}


block * builder::bld_op(const block & A, char * inop, const block & B)
{
    block  *ret = NULL;
    any_oper * oper = which_op(inop);

    if((oper->ot == ot_bin) || (oper->ot == ot_ub)) {
        ret = bld_op(A, oper->bop, B);
    } else if(oper->ot == ot_ass)  {
        ret = bld_assign(A, oper->aop, B);
    } else ERROR(et_oper, ("Binary or assignment operator expected, got %s", inop));

    delete oper;
    return ret;
}


block * builder::bld_op(const block & A, binary_op inop, const block & B)
{
    block * ret = new block();

    if(check_grammer(bt_expr, A)==0)
        ERROR(et_grammer, ("1st is not a expr but %s", get_bt(A.bt)));

    if(check_grammer(bt_expr, B)==0)
        ERROR(et_grammer, ("2nd is not a expr but %s", get_bt(B.bt)));

    if(bdebug0) printf("bop(%s) ", get_op(ot_bin, inop));

    ret->bt = bt_expr;
    ret->nt = nt_oper;
    ret->info.set(ot_bin, inop);
    ret->child = new block(A);
    ret->child->sib =  new block(B);

    return ret;
}




block * builder::bld_assign(const block & A, assign_op inop, const block & B)
{
    block * ret = new block();

    if(check_grammer(bt_lval, A)==0)
        ERROR(et_grammer, ("1st is not a lval but %s", get_bt(A.bt)));

    if(check_grammer(bt_expr, B)==0)
        ERROR(et_grammer, ("2nd is not a expr but %s", get_bt(B.bt)));

    if(bdebug0) printf("aop(%s) ", get_op(ot_ass, inop));

    ret->bt = bt_expr;
    ret->nt = nt_oper;
    ret->info.set(ot_ass, inop);
    ret->child = new block(A);
    ret->child->sib =   new block(B);

    return ret;
}


block & block::CALL(const block & s,
                  const block & ex1, const block & ex2, const block & ex3, const block & ex4,
                  const block & ex5, const block & ex6, const block & ex7, const block & ex8,
                  const block & ex9, const block &ex10, const block &ex11, const block &ex12)
{
    block * bk = bld_call(s, ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12);
    return *bk;
}


block * builder::bld_call(const block & s,
                          const block & ex1, const block & ex2, const block & ex3, const block & ex4,
                          const block & ex5, const block & ex6, const block & ex7, const block & ex8,
                          const block & ex9, const block &ex10, const block &ex11, const block &ex12)
{
    block * ret = new block();
    block * curr = 0;

    if(check_grammer(bt_sym, s)==0)
        ERROR(et_grammer, ("Procedure called is not a sym but  %s", get_bt(s.bt)));


    ret->bt = bt_expr;
    ret->nt = nt_call;
    ret->child = new block(s);
    curr =  ret->child;

    if(&ex1 != B_NOOP) {
        curr = add_call(curr, ex1);
        if(&ex2 != B_NOOP) {
            curr = add_call(curr, ex2);
            if(&ex3 != B_NOOP) {
                curr = add_call(curr, ex3);
                if(&ex4 != B_NOOP) {
                    curr = add_call(curr, ex4);
                    if(&ex5 != B_NOOP) {
                        curr = add_call(curr, ex5);
                        if(&ex6 != B_NOOP) {
                            curr = add_call(curr, ex6);
                            if(&ex7 != B_NOOP) {
                                curr = add_call(curr, ex7);
                                if(&ex8 != B_NOOP) {
                                    curr = add_call(curr, ex8);
                                    if(&ex9 != B_NOOP) {
                                        curr = add_call(curr, ex9);
                                        if(&ex10 != B_NOOP) {
                                            curr = add_call(curr, ex10);
                                            if(&ex11 != B_NOOP) {
                                                curr = add_call(curr, ex11);
                                                if(&ex12 != B_NOOP) {
                                                    curr = add_call(curr, ex12);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return ret;
}

block * builder::add_call(block * curr, const block & in)
{
    if(check_grammer(bt_expr, in)==0)
        ERROR(et_grammer, ("parameter is not a expr but  %s", get_bt(in.bt)));

    curr->sib = new block(in);
    return curr->sib;
}


/***************************************************************************
 * <prim>                                                                  *
 *                                                                         *
 ***************************************************************************/

block::block(int i)
{
    init();

    if(bdebug0) printf("(%d) ", i);
    bt = bt_prim;
    nt = nt_const;
    info.set(i);
}

block::block(double d)
{
    init();

    if(bdebug0) printf("(%f) ", d);
    bt = bt_prim;
    nt = nt_const;
    info.set(d);
}

block::block(float f)
{
    init();

    if(bdebug0) printf("(%f) ", f);
    bt = bt_prim;
    nt = nt_const;
    info.set(f);
}

block::block(char * s)
{
    init();
    assert(s);

    if(bdebug0) printf("(%s) ", s);
    bt = bt_prim;
    nt = nt_const;
    info.setch(s);
}


/***************************************************************************
 * <sym>                                                                   *
 *                                                                         *
 ***************************************************************************/

block::block(sym_node * n)
{
    init();
    assert(n);

    if(bdebug0) n->print(stdout);
    bt = bt_sym;
    nt = nt_code;
    info.set(n);
}


block::block(struct_access * sa)
{
    init();
    assert(sa);

    if(bdebug0) sa->print();
    bt = bt_sym;
    nt = nt_code;
    info.set(sa);
}



block & block::field(char * c)
{
    block * b = mk_struct_access(*this, c);
    return *b;
}

block & block::field(int i)
{
    block * b = mk_struct_access(*this, i);
    return *b;
}


block * builder::mk_struct_access(const block & b, int f)
{
    struct_access *sa = NULL;
    if(b.info.ct == ct_sym_node) {
        sym_node * sn = b.info.code.sym;
        if(!sn->is_var()) ERROR(et_type, ("Not a variable symbol"));
        sa = new struct_access((var_sym *)sn, f);
    } else if(b.info.ct == ct_struct_access) {
        sa = new struct_access(b.info.code.sta, f);
    } else
        ERROR(et_type, ("trying to do a structure access of a non-structure"));

    block * ret = new block(sa);
    return ret;
}

block * builder::mk_struct_access(const block & b, char * f)
{
    struct_access *sa = NULL;
    if(b.info.ct == ct_sym_node) {
        sym_node * sn = b.info.code.sym;
        if(!sn->is_var()) ERROR(et_type, ("Not a variable symbol"));
        sa = new struct_access((var_sym *)sn, f);
    } else if(b.info.ct == ct_struct_access) {
        sa = new struct_access(b.info.code.sta, f);
    } else
        ERROR(et_type, ("trying to do structure access of a non-structure"));

    block * ret = new block(sa);
    return ret;
}


block & block::addr()
{
    block * b = bld_op(uop_addr, *this);
    return *b;
}


block & block::dref()
{
    block * b = bld_op(uop_dref, *this);
    return *b;
}


block & block::mk_nr(type_node * tn, char * nm, base_symtab * st)
{
    if(st == NULL)
        st = get_proc()->symtab();

    tn = st->install_type(tn);

    sym_node * sym = st->new_unique_var(tn, nm);

    block * ret = new block(sym);

    return *ret;
}

sym_node * block::get_sym()
{
    assert(bt == bt_sym);
    assert(nt == nt_code);
    assert(info.ct == ct_sym_node);
    return info.code.sym;
}

block & builder::mk_label(char * c)
{
    char * st = new char[strlen(c)+8];
    sprintf(st, "%s%s", LABBASE, c);
    label_sym * ls = get_proc()->proc_syms()->new_label(st);
//    delete st;
    return mk_label(ls);
}


block & builder::mk_label(label_sym * ls)
{
    if(ls == NULL)
        ERROR(et_type, ("no label symbol"));

    block * ret = new block(ls);
    ret->bt = bt_label;
    return *ret;
}

/***************************************************************************
 * <statement>                                                             *
 *                                                                         *
 ***************************************************************************/
block & block::statement(const block & ex1, const block & ex2, const block & ex3,
                         const block & ex4, const block & ex5, const block & ex6,
                         const block & ex7, const block & ex8, const block & ex9,
                         const block & ex10, const block & ex11, const block & ex12)
{
    block * bk = bld_stmt(ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12);
    return *bk;
}

block & block::statement_append(const block & st)
{
    block * bk = add_stmt(this, st);
    return *bk;

}

block & block::statement_insert(const block & st)
{
    block * bk = add_stmt(this, st, TRUE);
    return *bk;

}

block * builder::bld_stmt(const block & ex1, const block & ex2, const block & ex3, const block & ex4,
                                 const block & ex5, const block & ex6, const block & ex7, const block & ex8,
                                 const block & ex9, const block &ex10, const block &ex11, const block &ex12)
{
    block * ret = new block();
    ret->init(ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12);
    return ret;
}


block * builder::add_stmt(block * curr, const block & in, boolean ins)
{
    if(check_grammer(bt_stmt, in)==0)
        ERROR(et_grammer, ("parameter is not a expr but  %s", get_bt(in.bt)));
    if(curr->nt != nt_stmt_list) {
        block * stl = new block;
        stl->init();
        stl->bt = bt_stmt;
        stl->nt = nt_stmt_list;
        stl->child = curr;
        curr = stl;
    }

    block * newin = new block(in);
    if(ins) {
        newin->sib = curr->child;
        curr->child = newin;
    } else {
        if(curr->child) {
            block * ch = curr->child;
            while(ch->sib) ch = ch->sib;
            ch->sib = newin;
        } else
            curr->child = newin;
    }

    return curr;
}



block & block::statement(tree_node * tr, boolean duplicate)
{
    block * bk = bld_stmt(tr, duplicate);
    return *bk;
}


block * builder::bld_stmt(tree_node * tr, boolean duplicate)
{
    block * ret = new block();
    ret->init(tr, duplicate);
    return ret;
}

void builder::init(tree_node * tr, boolean duplicate)
{
    init();
    bt = bt_stmt;
    nt = nt_code;
    if(duplicate)
        tr = tr->clone(tr->scope());
    info.set(tr);
}

block & block::statement(tree_node_list * tl, boolean duplicate)
{
    block * bk = bld_stmt(tl, duplicate);
    return *bk;
}


block * builder::bld_stmt(tree_node_list * tl, boolean duplicate)
{
    block * ret = new block();
    ret->init(tl, duplicate);
    return ret;
}

void builder::init(tree_node_list * tl, boolean duplicate)
{
    init();
    bt = bt_stmt;
    nt = nt_code;
    info.set(tl, duplicate);
}

block & block::statement(instruction * in)
{
    block * bk = bld_stmt(in);
    return *bk;
}


block * builder::bld_stmt(instruction * in)
{
    block * ret = new block();
    ret->init(in);
    return ret;
}

void builder::init(instruction * in)
{
    assert(in);
    init();
    bt = bt_expr;
    nt = nt_code;
    info.set(in);
}


void builder::init(const operand & op)
{
    init();
    switch(op.kind()) {
    case OPER_NULL:
        ERROR(et_suif, ("Null Operand Type"));
        break;
    case OPER_SYM:
        bt = bt_sym;
        nt = nt_code;
        info.set(op.symbol());
        break;
    case OPER_INSTR:
        init(op.instr());
        break;
    default:
	assert(0);
	break;
    }

}


void builder::init(immed im)
{
    init();

    switch(im.kind()) {
    case im_int:
        if(bdebug0) printf("(%d) ", im.integer());
        bt = bt_prim;
        nt = nt_const;
        info.set(im.integer());
        break;

    case im_extended_int:
        if(bdebug0) printf("(%s) ", im.ext_integer());
        bt = bt_prim;
        nt = nt_const;
        info.set_ext_int(im.ext_integer());
        break;

    case im_float:
        if(bdebug0) printf("(%f) ", im.flt());
        bt = bt_prim;
        nt = nt_const;
        info.set(im.flt());
        break;

    case im_extended_float:
        if(bdebug0) printf("(%s) ", im.ext_flt());
        bt = bt_prim;
        nt = nt_const;
        info.set_ext_float(im.ext_flt());
        break;

    case im_string:
        if(bdebug0) printf("(%s) ", im.string());
        bt = bt_prim;
        nt = nt_const;
        info.setch(im.string());
        break;

    case im_symbol: {
        sym_node * sym = im.symbol();
        if(bdebug0) sym->print(stdout);
        bt = bt_sym;
        nt = nt_code;
        info.set(sym);
        break;
        }

    case im_undef:
    case im_type:
    case im_instr:
    case im_op:
        ERROR(et_type, ("invalid immed type"));
    }
}


block & block::FOR(const block & i, const block & l, const block & u, const block & b)
{
    block * st = new block(1);
    block * bk = bld_for(i, l, bop_leq, u, *st, b);
    return *bk;
}

block & block::FOR(const block & i, const block & l, binary_op op, const block & u, const block & b)
{
    block * st = new block(1);
    block * bk = bld_for(i, l, op, u, *st, b);
    return *bk;
}

block & block::FOR(const block & i, const block & l, const block & u, const block & s, const block & b)
{
    block * bk = bld_for(i, l, bop_leq, u, s, b);
    return *bk;
}

block & block::FOR(const block & i, const block & l, binary_op op, const block & u, const block & s, const block & b)
{
    block * bk = bld_for(i, l, op, u, s, b);
    return *bk;
}


block * builder::bld_for(const block & i, const block & l, binary_op op, const block & u, const block & s, const block & b)
{
    block * ret = new block();

    if(check_grammer(bt_sym, i)==0)
        ERROR(et_grammer, ("loop variable is not a sym but  %s", get_bt(i.bt)));

    if(check_grammer(bt_expr, l)==0)
        ERROR(et_grammer, ("lower bound is not a expr but  %s", get_bt(l.bt)));

    if(check_grammer(bt_expr, u)==0)
        ERROR(et_grammer, ("upper bound is not a expr but  %s", get_bt(u.bt)));

    if(check_grammer(bt_expr, s)==0)
        ERROR(et_grammer, ("step bound is not a expr but  %s", get_bt(s.bt)));

    if(check_grammer(bt_stmt, b)==0)
        ERROR(et_grammer, ("loop body is not a stmt but  %s", get_bt(b.bt)));


    ret->bt = bt_stmt;
    ret->nt = nt_for;
    ret->info.op.set(ot_bin, op);
    ret->child = new block(i);
    ret->child->sib = new block(l);
    ret->child->sib->sib = new block(u);
    ret->child->sib->sib->sib = new block(s);
    ret->child->sib->sib->sib->sib = new block(b);
    return ret;
}

block & block::IF(const block & c, const block & t)
{
    block * bk = bld_if(c, t, *B_NOOP);
    return *bk;
}

block & block::IF(const block & c, const block & t, const block & f)
{
    block * bk = bld_if(c, t, f);
    return *bk;
}


block * builder::bld_if(const block & c, const block & t, const block & f)
{
    block * ret = new block();

    if(check_grammer(bt_expr, c)==0)
        ERROR(et_grammer, ("if condition is not a expr but  %s", get_bt(c.bt)));

    if(check_grammer(bt_stmt, t)==0)
        ERROR(et_grammer, ("ture path statement of if not a stmt but  %s", get_bt(t.bt)));

    if(&f != B_NOOP)
        if(check_grammer(bt_stmt, f)==0)
            ERROR(et_grammer, ("false path statement of if not a stmt but  %s", get_bt(f.bt)));

    ret->bt = bt_stmt;
    ret->nt = nt_if;
    ret->child = new block(c);
    ret->child->sib = new block(t);
    if(&f != B_NOOP)
        ret->child->sib->sib = new block(f);
    return ret;
}


block & block::WHILE(const block & c, const block & b)
{
    block * bk = bld_while(c, b);
    return *bk;
}


block * builder::bld_while(const block & c, const block & b)
{
    block * ret = new block();

    if(check_grammer(bt_expr, c)==0)
        ERROR(et_grammer, ("condition is not a expr but  %s", get_bt(c.bt)));

    if(check_grammer(bt_stmt, b)==0)
        ERROR(et_grammer, ("body of while not a stmt but  %s", get_bt(b.bt)));

    ret->bt = bt_stmt;
    ret->nt = nt_while;
    ret->child = new block(c);
    ret->child->sib = new block(b);
    return ret;
}


block & block::RETURN()
{
    block * blk = bld_return(*B_NOOP);
    return *blk;
}

block & block::RETURN(const block & b)
{
    block * blk =  bld_return(b);
    return *blk;
}

block * builder::bld_return(const block & b)
{
    block * ret = new block();
    ret->bt = bt_stmt;
    ret->nt = nt_return;
    if(&b != B_NOOP)
        ret->child = new block(b);

    return ret;
}


block & block::GOTO(const block & b)
{
    block * blk =  bld_goto(b);
    return *blk;
}

block * builder::bld_goto(const block & b)
{
    block * ret = new block();
    ret->bt = bt_stmt;
    ret->nt = nt_goto;
    if(&b == B_NOOP) ERROR(et_grammer, ("no label"));
    if(b.bt != bt_label) ERROR(et_grammer, ("expecting a label, got %s", get_bt(b.bt)));
    if(b.info.ct != ct_sym_node) ERROR(et_grammer, ("internal error, not a sym_node"));
    ret->child = new block(b.info.code.sym);

    return ret;
}




/***************************************************************************
 * type stuff.                                                             *
 ***************************************************************************/

type_node *  builder::get_typ(const_type tp)
{
    switch(tp) {
    case cst_none:
        ERROR(et_type, ("Unknown type"));
        return NULL;
    case cst_int:
        return type_signed;
    case cst_ext_int:
        return type_signed_long;
    case cst_double:
        return type_double;
    case cst_float:
        return type_float;
    case cst_ext_float:
        return type_longdouble;
    case cst_str:
        return type_ptr;
    case cst_char:
        return type_char;
    default:
        ERROR(et_type, ("Unknown type"));
        return NULL;
    }
}


type_node * block::parse_type(char * c,
                              type_node * tn0, type_node * tn1,
                              type_node * tn2, type_node * tn3,
                              type_node * tn4, type_node * tn5,
                              type_node * tn6, type_node * tn7,
                              type_node * tn8, type_node * tn9)
{
    if(c == NULL) ERROR(et_type, ("Empty input to parse"));

    assert(get_proc());
    type_template T(get_proc()->proc_syms());
    type_node * tn = T.parse_type(c,
                                  tn0, tn1, tn2, tn3, tn4,
                                  tn5, tn6, tn7, tn8, tn9);

    return tn;
}


type_node * block::parse_type(base_symtab * st, char * c,
                              type_node * tn0, type_node * tn1,
                              type_node * tn2, type_node * tn3,
                              type_node * tn4, type_node * tn5,
                              type_node * tn6, type_node * tn7,
                              type_node * tn8, type_node * tn9)
{
    if(c == NULL) ERROR(et_type, ("Empty input to parse"));

    assert(st);
    type_template T(st);
    type_node * tn = T.parse_type(c,
                                  tn0, tn1, tn2, tn3, tn4,
                                  tn5, tn6, tn7, tn8, tn9);

    return tn;
}




/***************************************************************************
 * Error handler                                                           *
 ***************************************************************************/

void builder_base::error_internal(const char *fmt, va_list args)
{
    const char * out;

    switch(error_level) {
    case et_none:
        out = "??";
        break;
    case et_type:
        out = "type checking";
        break;
    case et_grammer:
        out = "grammer checking";
        break;
    case et_oper:
        out = "input";
        break;
    case et_suif:
        out = "suif system";
        break;
    case et_gen:
        out = "code generation";
        break;
    default:
        out = "unknown?? ";
    }

    fprintf(stderr, "****Builder Error (in %s)", out);
    fprintf(stderr, ": ");
    vfprintf(stderr, fmt, args);
    if(error_file)
        fprintf(stderr, "  [%s:%d]", error_file, error_line);
    fprintf(stderr, "\n");
    error_file = NULL;
}

void builder_base::error(error_type et, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    error_level = et;
    error_internal(fmt, args);
    va_end(args);
}

void builder_base::_error(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    error_internal(fmt, args);
    va_end(args);
}
