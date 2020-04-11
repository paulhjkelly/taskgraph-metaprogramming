/* file "butil.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libbuilder.a"
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


extern int bdebug;

/***************************************************************************
 * Initialization routine that'll initialize the statics of the library    *
 ***************************************************************************/
void init_builder(int& /* argc */, char * /* argv */ [])
{

}


void exit_builder(void)
{

}


/***************************************************************************
 * Parse a string to find which operator that is                           *
 ***************************************************************************/
any_oper * builder::which_op(char * str)
{
    any_oper * r = new any_oper;

    assert(str);
    if(strlen(str) == 0) ERROR(et_oper, ("Invalid operator %s", str));
    if(strlen(str) > 3) ERROR(et_oper, ("Invalid operator %s", str));
    if(strlen(str) == 1) {
        switch(str[0]) {
        case '-':
            r->ot = ot_ub;
            r->uop = uop_minus;
            r->bop = bop_sub;
            break;

        case '!':
            r->ot = ot_uni;
            r->uop = uop_lnot;
            break;

        case '~':
            r->ot = ot_uni;
            r->uop = uop_not;
            break;

        case '*':
            r->ot = ot_bin;
            r->bop = bop_mul;
            break;

        case '/':
            r->ot = ot_bin;
            r->bop = bop_div;
            break;

        case '%':
            r->ot = ot_bin;
            r->bop = bop_mod;
            break;

        case '+':
            r->ot = ot_bin;
            r->bop = bop_add;
            break;

        case '<':
            r->ot = ot_bin;
            r->bop = bop_less;
            break;

        case '>':
            r->ot = ot_bin;
            r->bop = bop_gt;
            break;

        case '&':
            r->ot = ot_bin;
            r->bop = bop_and;
            break;

        case '^':
            r->ot = ot_bin;
            r->bop = bop_xor;
            break;

        case '|':
            r->ot = ot_bin;
            r->bop = bop_or;
            break;

        case '=':
            r->ot = ot_ass;
            r->aop = aop_eq;
            break;

        default:
            ERROR(et_oper, ("Invalid operator %s", str));
        }
    } else {
        // strlen (str) > 1)

        if(strcmp(str, "<<") == 0) {
            r->ot = ot_bin;
            r->bop = bop_lshift;
        } else if(strcmp(str, ">>") == 0) {
            r->ot = ot_bin;
            r->bop = bop_rshift;
        } else if(strcmp(str, "<=") == 0) {
            r->ot = ot_bin;
            r->bop = bop_leq;
        } else if(strcmp(str, ">=") == 0) {
            r->ot = ot_bin;
            r->bop = bop_geq;
        } else if(strcmp(str, "==") == 0) {
            r->ot = ot_bin;
            r->bop = bop_eq;
        } else if(strcmp(str, "!=") == 0) {
            r->ot = ot_bin;
            r->bop = bop_neq;
        } else if(strcmp(str, "&&") == 0) {
            r->ot = ot_bin;
            r->bop = bop_land;
        } else if(strcmp(str, "||") == 0) {
            r->ot = ot_bin;
            r->bop = bop_lor;
        } else if(strcmp(str, "+=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_add;
        } else if(strcmp(str, "-=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_sub;
        } else if(strcmp(str, "%=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_mod;
        } else if(strcmp(str, "*=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_mul;
        } else if(strcmp(str, "/=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_div;
        } else if(strcmp(str, ">>=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_rshift;
        } else if(strcmp(str, "<<=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_lshift;
        } else if(strcmp(str, "&=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_and;
        } else if(strcmp(str, "|=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_or;
        } else if(strcmp(str, "^=") == 0) {
            r->ot = ot_ass;
            r->aop = aop_xor;
        } else
            ERROR(et_oper, ("Invalid operator %s", str));
    }
    return r;
}


const char * builder::get_op(op_type tp, int op)
{
    any_oper oper(tp, op);
    return get_op(oper);
}


/***************************************************************************
 * Set the operator in any_oper structure                                  *
 ***************************************************************************/
void any_oper::set(op_type tp, int op)
{
    ot = tp;
    switch(tp) {
    case ot_none:
    case ot_ub:
        assert(0);

    case ot_uni:
        uop = (unary_op)op;
        break;

    case ot_bin:
        bop = (binary_op)op;
        break;

    case ot_ass:
        aop = (assign_op)op;
        break;
    };

}


/***************************************************************************
 * return the ascii representation for the operator                        *
 ***************************************************************************/
const char * any_oper::get() const
{
    switch(ot) {
    case ot_none:
    case ot_ub:
        assert(0);

    case ot_uni:
        switch(uop) {
        case uop_minus:
            return "-";
        case uop_not:
            return "~";
        case uop_lnot:
            return "!";
        case uop_abs:
            return "|";
        case uop_addr:
            return "&";
        case uop_dref:
            return "*";
        }
        assert(0);

    case ot_bin:
        switch(bop) {
        case bop_mul:
            return "*";
        case bop_div:
            return "/";
        case bop_mod:
            return "%";
        case bop_rem:
            return "%%";
        case bop_add:
            return "+";
        case bop_sub:
            return "-";
        case bop_lshift:
            return "<<";
        case bop_rshift:
            return ">>";
        case bop_less:
            return "<";
        case bop_gt:
            return ">";
        case bop_leq:
            return "<=";
        case bop_geq:
            return ">=";
        case bop_eq:
            return "==";
        case bop_neq:
            return "!=";
        case bop_and:
            return "&";
        case bop_xor:
            return "^";
        case bop_or:
            return "|";
        case bop_land:
            return "&&";
        case bop_lor:
            return "||";
        case bop_divfloor:
            return "F";
        case bop_divceil:
            return "C";
        case bop_min:
            return "N";
        case bop_max:
            return "X";
        }

    case ot_ass:
        switch(aop) {
        case aop_eq:
            return "=";
        case aop_mod:
            return "%=";
        case aop_add:
            return "+=";
        case aop_sub:
            return "-=";
        case aop_mul:
            return "*=";
        case aop_div:
            return "/=";
        case aop_rshift:
            return ">>=";
        case aop_lshift:
            return "<<=";
        case aop_and:
            return "&=";
        case aop_xor:
            return "^=";
        case aop_or:
            return "|=";
        }
        assert(0);
    }
    assert(0);
    return "Major foobar";
}



/***************************************************************************
 * return the suif instruction type for a given builder operator           *
 ***************************************************************************/
if_ops  any_oper::get_ifops() const
{
    switch(ot) {
    case ot_none:
    case ot_ub:
        assert(0);

    case ot_uni:
        switch(uop) {
        case uop_minus:
            return io_neg;
        case uop_not:
            return io_not;
        case uop_lnot:
            return io_not;
        case uop_abs:
            return io_abs;
        case uop_addr:
        case uop_dref:
            assert(0);
        }
        assert(0);

    case ot_bin:
        switch(bop) {
        case bop_mul:
            return io_mul;
        case bop_div:
            return io_div;
        case bop_mod:
            return io_mod;
        case bop_rem:
            return io_rem;
        case bop_add:
            return io_add;
        case bop_sub:
            return io_sub;
        case bop_lshift:
            return io_lsl;
        case bop_rshift:
            return io_lsr;
        case bop_less:
            return io_sl;
        case bop_gt:
            return io_sl;
        case bop_leq:
            return io_sle;
        case bop_geq:
            return io_sle;
        case bop_eq:
            return io_seq;
        case bop_neq:
            return io_sne;
        case bop_and:
            return io_and;
        case bop_xor:
            return io_xor;
        case bop_or:
            return io_ior;
        case bop_land:
            error(et_gen, "Logical and not implemented, will convert to 'and' instruction");
            return io_and;
        case bop_lor:
            error(et_gen, "Logical or not implemented, will convert to 'or' instruction");
            return io_ior;
        case bop_divfloor:
            return io_divfloor;
        case bop_divceil:
            return io_divceil;
        case bop_min:
            return io_min;
        case bop_max:
            return io_max;
        };

    case ot_ass:
        switch(aop) {
        case aop_eq:
            return io_cpy;
        case aop_mod:
            error(et_gen, "To be implemented, be cool");
            return io_nop;
        case aop_add:
            return io_add;
        case aop_sub:
            return io_sub;
        case aop_mul:
            return io_mul;
        case aop_div:
            return io_div;
        case aop_rshift:
            return io_lsr;
        case aop_lshift:
            return io_lsl;
        case aop_and:
            return io_and;
        case aop_xor:
            return io_xor;
        case aop_or:
            return io_ior;
        }
        assert(0);
    }
    assert(0);
    return io_nop;
}



/***************************************************************************
 * pretty display of enum block_type                                       *
 ***************************************************************************/
const char * builder::get_bt(block_type bt)
{
    switch(bt) {
    case bt_none:
        return "unknown??";
    case bt_expr:
        return "expression";
    case bt_prim:
        return "primary";
    case bt_lval:
        return "l-value";
    case bt_sym:
        return "symbol";
    case bt_stmt:
        return "statement";
    case bt_param:
        return "parameter";
    case bt_parmlist:
        return "parameter list";
    case bt_proc:
        return "procedure";
    case bt_label:
        return "label";
    }
    assert_msg(0, ("%d is not a block_type",bt));
    return "Yo dood, this should not be printed";
}


/***************************************************************************
 * BUGBUG: Semantic checking should be done.                               *
 ***************************************************************************/
int builder::check_grammer(block_type /* match */, const block & /* in */)
{
//    int ok = 0;

    return 1;
}


/***************************************************************************
 * Print the block structures                                              *
 ***************************************************************************/
void builder::print(const block * b, FILE * fp, int debug)
{
    if(b == 0) {
        fprintf(fp, "NULL");
        return;
    };

//    if(b->sib) printf("S");
//    if(b->child) printf("C");

    if(debug) {
        fprintf(fp, "\n%s-------\nblock_info:", get_bt(b->bt));
        b->info.print(fp); fprintf(fp, "\n");
    }


    if(b->nt == nt_none) {
        fprintf(fp, "nt_none");
    }

    else if(b->nt == nt_oper) {
        switch(b->info.op.ot) {
        case ot_none:
            fprintf(fp, "ot_none");
            break;

        case ot_ub:
            fprintf(fp, "ot_ub");
            break;

        case ot_uni:
            fprintf(fp, "%s", b->info.op.get());
            print(b->child, fp, debug);
            break;

        case ot_bin:
            fprintf(fp, "(%s ", b->info.op.get());
            print(b->child, fp, debug);
            fprintf(fp, " ");
            if(b->child == NULL)
                fprintf(fp, "NULL");
            else
                print((b->child->sib), fp, debug);
            fprintf(fp, ")");
            break;

        case ot_ass:
            fprintf(fp, "(");
            print(b->child, fp, debug);
            fprintf(fp, " %s ", b->info.op.get());
            if(b->child == NULL)
                fprintf(fp, "NULL");
            else
                print((b->child->sib), fp, debug);
            fprintf(fp, ")");
            break;

        default:
            fprintf(fp, "????");
            break;
        }
    }

    else if(b->nt == nt_const) {
        switch(b->info.cst)  {
        case cst_none:
            fprintf(fp, "cst_none");
            break;

        case cst_int:
            fprintf(fp, "%d", b->info.con.icon);
            break;

        case cst_ext_int:
            fprintf(fp, "%s", b->info.con.scon);
            break;

        case cst_double:
            fprintf(fp, "%f", b->info.con.dcon);
            break;

        case cst_float:
            fprintf(fp, "%f", b->info.con.fcon);
            break;

        case cst_ext_float:
            fprintf(fp, "%s", b->info.con.scon);
            break;

        case cst_char:
            fprintf(fp, "`%c'", b->info.con.ccon);
            break;

        case cst_str:
            fprintf(fp, "\"%s\"", b->info.con.scon);
            break;

        default:
            fprintf(fp, "< ???? >");
            break;
        }
    }

    else if(b->nt == nt_code) {
        switch(b->info.ct)  {
        case ct_none:
            fprintf(fp, "ct_none");
            break;

        case ct_sym_node:
            b->info.code.sym->print(fp);
            break;

        case ct_struct_access:
            b->info.code.sta->print();
            break;

        case ct_tree_node:
            fprintf(fp, "<tree_node>");
            break;

        case ct_tree_node_list:
            fprintf(fp, "<tree_node_list>");
            break;

        case ct_instruction:
            fprintf(fp, "<instruction>");
            break;

        case ct_type_node:
            fprintf(fp, "<type_node>");
            break;

        default:
            fprintf(fp, "< ???? >");
            break;
        }
    }


    else if((b->nt == nt_call)||(b->nt == nt_for)||(b->nt == nt_goto)||
            (b->nt == nt_if)||(b->nt == nt_stmt_list)) {
        if(b->nt == nt_call)
            fprintf(fp, "(CALL ");
        else if(b->nt == nt_for)
            fprintf(fp, "(FOR%s ", b->info.op.get());
        else if(b->nt == nt_if)
            fprintf(fp, "(IF ");
        else if(b->nt == nt_goto)
            fprintf(fp, "(GOTO ");
        else if(b->nt == nt_stmt_list)
            fprintf(fp, "( ");

        block * curr = b->child;
        while(curr) {
            print(curr, fp, debug);
            curr = curr->sib;
            if(curr) fprintf(fp, ", ");
        }
        fprintf(fp, ")");
    } else
        fprintf(fp, "nt_unknown");
}


void builder::print(const block & b, FILE * fp, int debug)
{
    print(&b, fp, debug);
}


/***************************************************************************
 * A sanity check. (For debugging)                                         *
 ***************************************************************************/
void block::check()
{
    int w = FALSE;
    switch(bt) {
    case bt_none:
        printf("bt_none");
        w = TRUE;
        break;

    case bt_expr:
    case bt_prim:
    case bt_lval:
    case bt_sym:
    case bt_stmt:
    case bt_param:
    case bt_parmlist:
    case bt_proc:
    case bt_label:
        w = FALSE;
        break;

    default:
        assert_msg(0, ("bt=%d\n", bt));
        break;
    }

    switch(nt) {
    case nt_none:
        printf("nt_none");
        w = TRUE;
        break;

    case nt_oper:
    case nt_code:
    case nt_const:
    case nt_array:
    case nt_call:
    case nt_goto:
    case nt_stmt_list:
    case nt_for:
    case nt_if:
    case nt_while:
    case nt_do:
    case nt_return:
        w = FALSE;
        break;

    default:
        assert_msg(0, ("nt=%d\n", nt));
        break;
    }

    if(child) child->check();
    if(sib) sib->check();

}


/***************************************************************************
 * print the block_info structure                                          *
 ***************************************************************************/
void block_info::print(FILE * fp) const
{
    switch(ct) {
    case ct_sym_node:
        code.sym->print(fp);
        break;

    case ct_tree_node:
        code.trn->print(fp);
        break;

    case ct_tree_node_list: {
        tree_node_list_iter it(code.tnl);
        while(!it.is_empty()) {
            tree_node * tn = it.step();
            tn->print(fp);
        }
        break;
        }

    case ct_instruction:
        code.ins->print(fp);
        break;

    case ct_type_node:
        code.typ->print(fp);
        break;

    default:
        fprintf(fp, "???");
        break;
    }

    switch(cst) {
    case cst_int:
        fprintf(fp, "%d", con.icon);
        break;

    case cst_ext_int:
        fprintf(fp, "%s", con.scon);
        break;

    case cst_double:
        fprintf(fp, "%f", con.dcon);
        break;

    case cst_float:
        fprintf(fp, "%f", con.fcon);
        break;

    case cst_ext_float:
        fprintf(fp, "%s", con.scon);
        break;

    case cst_char:
        fprintf(fp, "`%c'", con.ccon);
        break;

    case cst_str:
        fprintf(fp, "\"%s\"", con.scon);
        break;

    case cst_none:
        fprintf(fp, "???");
        break;
    }
}


/***************************************************************************
 * find the appropriate tree_for_test given the operator.                  *
 ***************************************************************************/
tree_for_test builder::get_tft(const any_oper & op, type_node *comp_type)
{
    tree_for_test tft = FOR_SLTE;
    if(op.ot != ot_bin)
        ERROR(et_gen, ("Cannot make a FOR test from %s", op.get()));

    type_node *comp_unqual = comp_type->unqual();
    boolean is_signed;
    switch (comp_unqual->op())
      {
        case TYPE_INT:
        case TYPE_ENUM:
          {
            base_type *the_base_type = (base_type *)comp_unqual;
            is_signed = the_base_type->is_signed();
            break;
          }
        case TYPE_FLOAT:
        case TYPE_PTR:
            is_signed = TRUE;
            break;
        default:
            ERROR(et_gen, ("Illegal type for FOR test"));
            is_signed = TRUE;
      }

    switch(op.bop) {
    case bop_eq:
        tft = FOR_EQ;
        break;
    case bop_neq:
        tft = FOR_NEQ;
        break;
    case bop_less:
        tft = (is_signed ? FOR_SLT : FOR_ULT);
        break;
    case bop_leq:
        tft = (is_signed ? FOR_SLTE : FOR_ULTE);
        break;
    case bop_gt:
        tft = (is_signed ? FOR_SGT : FOR_UGT);
        break;
    case bop_geq:
        tft = (is_signed ? FOR_SGTE : FOR_UGTE);
        break;
    default:
        ERROR(et_gen, ("Cannot make a FOR test from %s", op.get()));
    };

    return tft;
}





