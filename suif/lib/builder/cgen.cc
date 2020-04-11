/* file "cgen.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libbuilder.a"
#pragma implementation "cgen.h"
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


#include <cstdlib>
#include <suif1.h>
#include <useful.h>
#include "builder.h"
#include "cgen.h"
#include "bldtypes.h"


/***************************************************************************
 * Print the suif tree that is generated.                                  *
 ***************************************************************************/
void gen_tree::print_suif(FILE * fp) const
{
    switch(gtt) {
    case gtt_none:
        fprintf(fp, "<none>");
        break;
    case gtt_sym: {
        sym_node * sym = gen.sym;
        fprintf(fp, "sym_node: ");
        sym->print(fp);
        break;
        }
    case gtt_trn: {
        tree_node * trn = gen.trn;
        fprintf(fp, "tree_node: ");
        trn->print(fp);
        break;
        }
    case gtt_tnl: {
        tree_node_list * tnl = gen.tnl;
        fprintf(fp, "tree_node_list:\n");
        tree_node_list_iter iter(tnl);
        while(!iter.is_empty()) {
            tree_node * trn = iter.step();
            trn->print(fp);
            if(!iter.is_empty()) fprintf(fp, "\n");
        }
        break;
        }
    case gtt_ins: {
        instruction * ins = gen.ins;
        fprintf(fp, "instruction: ");
        ins->print(fp);
        break;
        }
    }
    fprintf(fp, "\n");
}

/***************************************************************************
 * Interface routines that construct suif structures from given blocks     *
 ***************************************************************************/
tree_node * builder::generate_tree_node(const block & b)
{
    generator gen;
    return gen.return_tree_node(b);
}

tree_node_list * builder::generate_tree_node_list(const block & b)
{
    generator gen;
    return gen.return_tree_node_list(b);
}

instruction * builder::generate_instruction(const block & b)
{
    generator gen;
    return gen.return_instruction(b);
}


/***************************************************************************
 * The parent tree_node_list for the suif tree                             *
 ***************************************************************************/
tree_node_list * gen_tree::get_list()
{
    if(gtt == gtt_tnl) return gen.tnl;

    if(par == NULL) return NULL;
    return par->get_list();

}


/***************************************************************************
 * The parent tree_node for the suif tree                                  *
 ***************************************************************************/
tree_node * gen_tree::get_node()
{
    if(gtt == gtt_trn) return gen.trn;

    if(par == NULL) return NULL;
    return par->get_node();
}


/***************************************************************************
 * Create an operand out of a generated suif (symbol or instrucition)      *
 ***************************************************************************/
operand gen_tree::get_oper()
{
    operand * op = NULL;
    switch(gtt) {
    case gtt_sym:
        assert(gen.sym->is_var());
        op = new operand((var_sym *)gen.sym);
        break;
    case gtt_ins:
        op = new operand(gen.ins);
        break;
    case gtt_none:
    case gtt_trn:
    case gtt_tnl:
        op = NULL;
        assert(0);
        break;
    default:
        assert(0);
    }

    return *op;
}

/***************************************************************************
 *
 ***************************************************************************/
tree_node * generator::return_tree_node(const block & b)
{
    gen_tree * par = new gen_tree;
    par->gtt = gtt_tnl;
    par->gen.tnl = new tree_node_list();

    gen_tree * g = gen_any(b, par);

    if(g->gtt == gtt_ins) {
        tree_instr * ti = new tree_instr(g->gen.ins);
        return ti;
    } else if(g->gtt == gtt_trn)
        return g->gen.trn;
    else if(g->gtt == gtt_tnl) {
	tree_block * tb = new tree_block(g->gen.tnl, new block_symtab("_builder_treenodelist2treenode"));
	return tb;
    } else
        ERROR(et_gen, ("Cannot create tree_node from %d", g->gtt));


    return NULL;  // To satisfy the compiler
}

tree_node_list * generator::return_tree_node_list(const block & b)
{
    gen_tree * g = gen_stmt_list(b);
    assert(g->gtt == gtt_tnl);
    return g->gen.tnl;
}

instruction * generator::return_instruction(const block & b)
{
    gen_tree * g = gen_any(b, NULL);
    if(g->gtt == gtt_sym) {
        assert(g->gen.sym->is_var());
        g = gen_cpy((var_sym *)g->gen.sym, NULL);
    }
    assert(g->gtt == gtt_ins);
    return g->gen.ins;
}


gen_tree * generator::gen_lod(const block & b, gen_tree * parent)
{
    gen_tree * g = gen_any(b, parent);

    if(g->gtt == gtt_sym) {
        g = gen_ldc(immed(g->gen.sym), parent);
    }
    return g;
}


gen_tree * generator::gen_any(const block & b, gen_tree * parent)
{
    gen_tree * g = NULL;
    switch(b.nt) {
    case nt_none:
        ERROR(et_gen, ("Cannot create code for node nt_none"));
        break;

    case nt_oper:
        g = gen_op(b, parent);
        break;

    case nt_code:
        switch(b.info.ct) {
        case ct_none:
            ERROR(et_gen, ("Cannot create code for node ct_none"));
            break;

        case ct_sym_node: {
            sym_node * sym = b.info.code.sym;
            switch(sym->kind()) {
            case SYM_VAR: {
                var_sym * vs = (var_sym *)sym;
                g = new gen_tree(parent);
                type_node * tp = vs->type();

                // Arrays always need an ldc of the address
                if(tp->unqual()->is_array()) {
                    in_ldc * ld = new in_ldc;
                    ld->set_value(immed(vs));
                    vs->set_addr_taken();

                    ld->set_result_type(tp->ptr_to());

                    g->gtt = gtt_ins;
                    g->gen.ins = ld;
                } else {
                    g->gtt = gtt_sym;
                    g->gen.sym = vs;
                }
            }
                break;

            case SYM_LABEL: {
                label_sym * ls = (label_sym *)sym;
                g = new gen_tree(parent);
                g->gtt = gtt_ins;
                instruction * inst = new in_lab(ls);
                g->gen.ins  = inst;
                break;
                }

            case SYM_PROC: {
                proc_sym * ps = (proc_sym *)sym;

                in_ldc * ld = new in_ldc;
                ld->set_value(immed(ps));
                ld->set_result_type(ps->type()->ptr_to());

                g = new gen_tree(parent);
                g->gtt = gtt_ins;
                g->gen.ins = ld;
                break;
                }

            default:
                ERROR(et_gen, ("Cannot create code for this kind"));
                break;
            }
            break;
            }

        case ct_struct_access: {
            struct_access * sa = b.info.code.sta;
            g = new gen_tree(parent);
            g->gtt = gtt_ins;

	    type_node * tp = sa->type;
            boolean do_load = (tp->is_array() == FALSE);

	    gen_tree * gld2;
	    if(sa->dref_ptr) {
	      in_rrr * ir = new in_rrr;
	      gld2 = new gen_tree;
	      gld2->gen.ins = ir;
	      gld2->gtt = gtt_ins;
	      ir->set_opcode(io_add);
	      ir->set_src1(operand(sa->var));
	      gen_tree * goff = gen_ldc(immed(sa->offset/8), gld2);
	      ir->set_src2(operand(goff->gen.ins));
	    } else
	      gld2 = gen_ldc(immed(sa->var, sa->offset), g);

            assert(gld2->gtt == gtt_ins);
            instruction * ildc = gld2->gen.ins;
            ildc->set_result_type(tp->ptr_to());
            immed_list * iml = new immed_list;
            iml->copy(&sa->field_list);
            ildc->append_annote(k_fields, iml);

            if(do_load) {
                in_rrr * ilod = new in_rrr;
                ilod->set_opcode(io_lod);
                ilod->set_result_type(tp->unqual());
                ilod->set_src_addr_op(operand(ildc));
                g->gtt = gtt_ins;
                g->gen.ins = ilod;
            } else {
                g->gtt = gtt_ins;
                g->gen.ins = ildc;
            }
            break;
            }

        case ct_tree_node:
            g = new gen_tree(parent);
            g->gtt = gtt_trn;
            assert(b.info.ncpy);
            if(b.info.ncpy->num > 0)
                g->gen.trn = b.info.code.trn->clone(BlockSymTab);
            else
                g->gen.trn = b.info.code.trn;
            b.info.ncpy->num++;

            break;

        case ct_tree_node_list:
            g = new gen_tree(parent);
            g->gtt = gtt_tnl;
            assert(b.info.ncpy);
            if(b.info.ncpy->num > 0)
                g->gen.tnl = b.info.code.tnl->clone(BlockSymTab);
            else
                g->gen.tnl = b.info.code.tnl;
            b.info.ncpy->num++;
            break;

        case ct_instruction:
            g = new gen_tree(parent);
            g->gtt = gtt_ins;
            g->gen.ins = b.info.code.ins->clone(BlockSymTab);
            break;

        default:
            ERROR(et_gen, ("Cannot create code for this code_type"));
            break;
        }
        break;

    case nt_const:
        switch(b.info.cst) {
        case cst_none:
            ERROR(et_gen, ("Cannot create code for node cst_none"));
            break;

        case cst_int:
            g = gen_ldc(immed(b.info.con.icon), parent);
            break;

        case cst_double:
            g = gen_ldc(immed(b.info.con.dcon), parent);
            break;

        case cst_float:
            g = gen_ldc(immed(b.info.con.fcon), parent);
            break;

        case cst_char:
            g = gen_ldc(immed(b.info.con.ccon), parent);
            break;

        case cst_str: {
            g = new gen_tree;
            g->gtt = gtt_ins;
            operand literal_op =
                    string_literal_op(b.info.con.scon, proc->proc()->file());
            assert(literal_op.is_instr());
            g->gen.ins = literal_op.instr();
            break;
          }

        default:
            ERROR(et_gen, ("Cannot create code for this const_type"));
            break;
        }
        break;


    case nt_for:
        g = gen_for_node(b, parent, BlockSymTab);
        break;

    case nt_while:
    case nt_do:
        g = gen_loop_node(b, parent, BlockSymTab);
        break;


    case nt_if:
        g = gen_if_node(b, parent, BlockSymTab);
        break;

    case nt_stmt_list:
        g = gen_stmt_list(b, parent);
        break;

    case nt_call:
        g = gen_call(b, parent);
        break;

    case nt_goto: {
        g = new gen_tree(parent);
        g->gtt = gtt_ins;
        in_bj * inst = new in_bj;
        g->gen.ins  = inst;
        inst->set_opcode(io_jmp);
        assert(b.child);
        assert(b.child->info.code.sym->is_label());
        inst->set_target((label_sym *)b.child->info.code.sym);
        break;
        }

    case nt_return:
        g = gen_return_inst(b, parent);
        break;

    case nt_array:
        g = gen_array_access(b, parent);
        break;

    default:
        ERROR(et_gen, ("Cannot create code for this nod_type"));
        break;

    }
    return g;
}






gen_tree * generator::gen_ldc(immed ic, gen_tree * /* parent */)
{
    gen_tree * g = new gen_tree;

    in_ldc * ld = new in_ldc;

    type_node * tp = get_type(ic);

    if(ic.is_symbol()) {
        if (ic.symbol()->is_var())
            ((var_sym *)(ic.symbol()))->set_addr_taken();
    }

    ld->set_result_type(tp);

    ld->set_value(ic);
    g->gen.ins = ld;
    g->gtt = gtt_ins;
    return g;
}


gen_tree * generator::gen_cpy(var_sym * vs, gen_tree * /* parent */)
{
    gen_tree * g = new gen_tree;

    in_rrr * cp = new in_rrr;
    cp->set_opcode(io_cpy);
    cp->set_result_type(vs->type()->unqual());
    cp->set_src1(operand(vs));

    g->gen.ins = cp;
    g->gtt = gtt_ins;
    return g;
}


static void remove_dst(operand & op)
{
    if(!op.is_instr()) return;

    instruction * inst = op.instr();
    if(!inst->dst_op().is_instr()) return;
    op.remove();
}


gen_tree * generator::gen_op(const block & b, gen_tree * parent)
{
    assert(b.nt == nt_oper);

    type_node * rtype = type_signed;
    type_node * rt1 = NULL;
    type_node * rt2 = NULL;

    gen_tree * g = new gen_tree(parent);
    g->gtt = gtt_ins;

    if(b.info.op.ot == ot_uni) {
        // address of operation
        if(b.info.op.uop == uop_addr) {
            gen_tree * child = gen_any(*(b.child), g);
            if(child->gtt == gtt_sym) {
                immed im(child->gen.sym);
                if (child->gen.sym->is_var())
                    ((var_sym *)(child->gen.sym))->set_addr_taken();
                in_ldc * ld = new in_ldc;
                g->gen.ins = ld;
                type_node * tp = get_type(im);
                ld->set_result_type(tp);
                ld->set_value(im);

            } else if(child->gtt == gtt_ins) {
                if(child->gen.ins->opcode() != io_lod)
                    ERROR(et_gen, ("Not a load instruction, cannot dereference"));
                in_rrr * rrr = (in_rrr *)child->gen.ins;

                operand oper(rrr->src_addr_op());
                remove_dst(oper);
                delete child;
                if(oper.is_symbol()) {
                    g->gtt = gtt_sym;
                    g->gen.sym = oper.symbol();
                } else
                    g->gen.ins = oper.instr();
            } else
                assert(0);
            return g;

        } else if(b.info.op.uop == uop_dref) {
            gen_tree * child = gen_any(*(b.child), g);

            operand oper(child->get_oper());

            type_node * tp = oper.type();
            if(!tp->is_ptr())
                ERROR(et_gen, ("Dereferencing a non-pointer type"));

            in_rrr * rrr = new in_rrr;
            g->gen.ins = rrr;
            g->gtt = gtt_ins;

            rrr->set_opcode(io_lod);
            rrr->set_result_type(((ptr_type *)tp)->ref_type()->unqual());
            rrr->set_src_addr_op(oper);

            return g;
        }
    }


    in_rrr * inst = new in_rrr;
    g->gen.ins  = inst;

    inst->set_opcode(b.info.op.get_ifops());

    if(b.info.op.ot == ot_none)
        ERROR(et_gen, ("Cannot make an op from ot_none"));
    else if(b.info.op.ot == ot_ub)
        ERROR(et_gen, ("Ambigious operator"));

    else if(b.info.op.ot == ot_uni) {
        assert(b.child != 0);
        gen_tree * child = gen_any(*(b.child), g);

        operand oper1(child->get_oper());
        rtype = oper1.type();
        inst->set_src1(oper1);

    } else if(b.info.op.ot == ot_bin) {
        assert(b.child != 0);
        assert(b.child->sib != 0);
        gen_tree * child1 = gen_any(*(b.child), g);
        gen_tree * child2 = gen_any(*(b.child->sib), g);
        assert(child1);
        assert(child2);

        if((b.info.op.bop == bop_gt)||
           (b.info.op.bop == bop_geq)) {       // switch operands
            gen_tree * tc = child1;
            child1 = child2;
            child2 = tc;
        }

        operand oper1(child1->get_oper());
        rt1 = oper1.type();
        inst->set_src1(oper1);

        operand oper2(child2->get_oper());
        rt2 = oper2.type();
        inst->set_src2(oper2);

        rtype = match_type(inst, rt1, rt2);
    } else { // b.info.op.ot == ot_ass
        gen_tree * child1 = gen_any(*(b.child), g);
        gen_tree * child2 = gen_any(*(b.child->sib), g);

        if(b.info.op.aop == aop_eq) {                   // a = b
            if(child1->gtt == gtt_sym) {
                if(child2->gtt == gtt_sym) {            // cpy pr#y, pr#x
                                                        assert(child1->gen.sym->is_var());
                                                        assert(child2->gen.sym->is_var());
                                                        rt1 = get_type(operand((var_sym *)child1->gen.sym));
                                                        rt2 = get_type(operand((var_sym *)child2->gen.sym));
                                                        inst->set_dst((var_sym *)child1->gen.sym);
                                                        if(rt1 != rt2) {
                                                            instruction * in = mk_cvt(operand((var_sym *)child2->gen.sym), rt1);
                                                            inst->set_src1(operand(in));
                                                        } else
                                                            inst->set_src1((var_sym *)child2->gen.sym);
                                                        rtype = rt1;
                                                    } else {    // oper pr#y, ...
                                                        delete g;
                                                        g = child2;
                                                        assert(child1->gen.sym->is_var());
                                                        operand  opx((var_sym *)child1->gen.sym);
                                                        rt1 = get_type(opx);
                                                        rt2 = get_type(operand(g->gen.ins));
                                                        if(rt1 != rt2)
                                                            g->gen.ins = mk_cvt(operand(g->gen.ins), rt1);
                                                        g->gen.ins->set_dst(opx);
                                                        rtype = rt1;
                                                    }
            } else {                                    // str <>, ...
                in_rrr * old_lod = (in_rrr *)child1->gen.ins;

                if(old_lod->opcode() != io_lod)
                    ERROR(et_gen, ("lhs has to be an address"));

                in_rrr * new_str = new in_rrr;
                new_str->set_opcode(io_str);
                g->gen.ins = new_str;

                operand  op_sr(child2->get_oper());
                remove_dst(op_sr);

                operand  op_ad(old_lod->src_addr_op());
                remove_dst(op_ad);
                new_str->set_dst_addr_op(op_ad);

                type_node * rts = get_type(op_sr);
                type_node * rta = get_type(op_ad);
                if(!rta->unqual()->is_ptr())
                    ERROR(et_gen, ("Result of store is not a pointer"));
                type_node * drta = ((ptr_type *)rta->unqual())->ref_type();
                if(rts != drta)
                    op_sr.set_instr(mk_cvt(op_sr, drta));

                new_str->set_src(op_sr);

                rtype = rta;
            }
        } else {                                        // a op= b
            ERROR(et_gen, ("To be implemented, be cool"));
        }
    }

    inst->set_result_type(rtype->unqual());
    return g;
}


type_node * generator::cast_to(type_node * c1, type_node * c2)
{
    assert(c1);
    assert(c2);

    if(c1->size() < c2->size()) {
        type_node * t = c1;
        c1 = c2;
        c2 = t;
    }

    if(c1->op() == c2->op()) return c1;

    if(c1->op() == TYPE_FLOAT)
        return c1;
    if(c2->op() == TYPE_FLOAT)
        return c2;

    if(c1->op() == TYPE_PTR)
        return c1;
    if(c2->op() == TYPE_PTR)
        return c2;

    if(c1->op() == TYPE_INT)
        return c1;
    if(c2->op() == TYPE_INT)
        return c2;

    if(c1->op() == TYPE_VOID)
        return c1;
    if(c2->op() == TYPE_VOID)
        return c2;

    return c1;
}


void generator::create_mk_cvt(in_rrr * inst, type_node * c, boolean first)
{
    if(first) {
        operand op1(inst->src1_op());
        if(op1.is_instr()) op1.instr()->remove();
        instruction * in = mk_cvt(op1, c);
        inst->set_src1(operand(in));
    } else {
        operand op2(inst->src2_op());
        if(op2.is_instr()) op2.instr()->remove();
        instruction * in = mk_cvt(op2, c);
        inst->set_src2(operand(in));
    }
}

type_node * generator::create_mk_cvt(in_rrr * inst, type_node * c1, type_node * c2)
{
    type_node * cst = cast_to(c1, c2);
    create_mk_cvt(inst, cst, (boolean)(cst==c2));
    return cst;
}


type_node * generator::match_type(in_rrr * inst,
                                  type_node * c1,
                                  type_node * c2)
{
    // Cannot add two pointers
    switch(inst->opcode()) {
    case io_add:
    case io_sub:
    case io_mul:
    case io_div:
    case io_rem:
    case io_mod:
    case io_min:
    case io_max:
    case io_and:
    case io_ior:
    case io_xor:
    case io_divfloor:
    case io_divceil:

        if (inst->opcode() == io_add)
          {
            if (c1->is_ptr())
              {
                if (c2->op() != TYPE_INT)
                    create_mk_cvt(inst, type_ptr_diff, FALSE);
                return c1;
              }
            if (c2->is_ptr())
              {
                if (c1->op() != TYPE_INT)
                    create_mk_cvt(inst, type_ptr_diff, TRUE);
                return c2;
              }
          }
        else if (inst->opcode() == io_sub)
          {
            if (c1->is_ptr())
              {
                if (c2->is_ptr())
                  {
                    create_mk_cvt(inst, c1, FALSE);
                    return type_ptr_diff;
                  }
                if (c2->op() != TYPE_INT)
                    create_mk_cvt(inst, type_ptr_diff, FALSE);
                return c1;
              }
          }

        if(!c1->compatible(c2))
            return create_mk_cvt(inst, c1, c2);
        else
            return c1;

    case io_asr:
    case io_lsl:
    case io_lsr:
    case io_rot:
        if(c2->op() != TYPE_INT)
            create_mk_cvt(inst, type_signed, FALSE);
        return c1;

    case io_str:
        if(c1 != c2->ptr_to())
            create_mk_cvt(inst, c2->ptr_to(), TRUE);
        return type_void;

    case io_memcpy:
        if (!c1->is_ptr())
            ERROR(et_gen, ("Memcopy with non-pointer type is meaningless"));
        if (!c2->is_ptr())
            ERROR(et_gen, ("Memcopy with non-pointer type is meaningless"));
        return type_void;

    case io_seq:
    case io_sne:
    case io_sle:
    case io_sl:
        if(!c1->compatible(c2))
            create_mk_cvt(inst, c1, c2);
        return type_signed;

    default:
        ERROR(et_gen, ("Should not get a match called with the instr"));
        return NULL; // to make the compiler shut-up
    }
}


gen_tree * generator::gen_for_node(const block & b, gen_tree * parent, block_symtab * bs)
{
    gen_tree * g = new gen_tree(parent);
    g->gtt = gtt_trn;
    assert(b.nt == nt_for);

    if(b.child->info.ct != ct_sym_node)
        ERROR(et_gen, ("Index is not a sym_node but %d", b.child->info.ct));

    sym_node * index = b.child->info.code.sym;
    if(!index->is_var())
        ERROR(et_gen, ("Index is not a var_sym"));

    gen_tree * gbody = gen_stmt_list(*(b.child->sib->sib->sib->sib), g);

//    gen_lod(b.child->sib, g)->get_oper().print(); printf(" ");
//    gen_lod(b.child->sib->sib, g)->get_oper().print(); printf(" ");
//    gen_lod(b.child->sib->sib->sib, g)->get_oper().print(); printf("----\n");

    var_sym *index_var = (var_sym *)index;
    if(bs == NULL) bs = get_proc()->proc_syms();
    tree_for *fn = new tree_for(index_var,
                                builder::get_tft(b.info.op, index_var->type()),
                                bs->new_unique_label(LABBASE),
                                bs->new_unique_label(LABBASE),
                                gbody->gen.tnl,
                                gen_any(*(b.child->sib), g)->get_oper(),
                                gen_any(*(b.child->sib->sib), g)->get_oper(),
                                gen_any(*(b.child->sib->sib->sib), g)->get_oper(),
                                new tree_node_list);


    g->gen.trn = fn;

    return g;
}


gen_tree * generator::gen_if_node(const block & b, gen_tree * parent, block_symtab * bs)
{
    gen_tree * g = new gen_tree(parent);
    g->gtt = gtt_trn;
    assert(b.nt == nt_if);

    gen_tree * gt = gen_stmt_list(*(b.child->sib), g);
    gen_tree * gf = NULL;
    if(b.child->sib->sib)
        gf = gen_stmt_list(*(b.child->sib->sib), g);
    else {
        gf = new gen_tree();
        gf->gtt = gtt_tnl;
        gf->gen.tnl = new tree_node_list();
    }

    if(bs == NULL) bs = get_proc()->proc_syms();
    label_sym * jmpto = bs->new_unique_label(LABBASE);

    in_bj * inst = new in_bj(io_bfalse, jmpto, gen_any(*(b.child), NULL)->get_oper());
    inst->set_result_type(type_void);

    tree_node_list * tnl = new tree_node_list();
    tree_instr * ti = new tree_instr(inst);
    tnl->append(ti);

    tree_if *in = new tree_if(jmpto, tnl, gt->gen.tnl, gf->gen.tnl);

    g->gen.trn = in;

    return g;
}




gen_tree * generator::gen_loop_node(const block & b, gen_tree * parent, block_symtab * bs)
{
    gen_tree * g = new gen_tree(parent);
    g->gtt = gtt_trn;
    assert((b.nt == nt_while)||(b.nt == nt_do));

    gen_tree * gb = gen_stmt_list(*(b.child->sib), g);

    if(bs == NULL) bs = get_proc()->proc_syms();

    label_sym * l_cnt = bs->new_unique_label(LABBASE);
    label_sym * l_brk = bs->new_unique_label(LABBASE);
    label_sym * l_top = bs->new_unique_label(LABBASE);

    in_bj * inst = new in_bj(io_bfalse, l_cnt, gen_any(*(b.child), NULL)->get_oper());
    inst->set_result_type(type_void);

    tree_node_list * tnl = new tree_node_list();
    tree_instr * ti = new tree_instr(inst);
    tnl->append(ti);

    tree_loop *in = new tree_loop(gb->gen.tnl, tnl, l_cnt, l_brk, l_top);

    g->gen.trn = in;

    return g;
}



gen_tree * generator::gen_array_access(const block & b, gen_tree * /* parent */)
{
    in_rrr * alod = new in_rrr;         // ldc of the array access

    alod->set_opcode(io_lod);
    gen_tree * g = new gen_tree;
    g->gtt = gtt_ins;
    g->gen.ins = alod;

    in_array * arr = new in_array;      // array instr. itself
    gen_tree * ga = new gen_tree(g);
    ga->gtt = gtt_ins;
    ga->gen.ins = arr;


    assert(b.child);
    gen_tree * bs_g = gen_any(*(b.child), ga);
    operand bs_op(bs_g->get_oper());
    if(!bs_op.type()->unqual()->is_ptr())
        ERROR(et_gen, ("Base of the array operation is not a ptr to array"));
    ptr_type * bpt = (ptr_type *)bs_op.type()->unqual();
    if(!bpt->ref_type()->unqual()->is_array())
        ERROR(et_gen, ("Base of the array operation is not of array type"));
    array_type * bs_type = (array_type *)bpt->ref_type()->unqual();


    // find the Number of dimensions
    // check the array type defind against dimensions specified.
    // find the element type
    block * curr = b.child;
    array_type * ct = bs_type;
    type_node * e_type = NULL;
    int ndim = 0;
    while(curr->sib) {
        ndim++;
        curr = curr->sib;

        if(e_type)
            ERROR(et_gen, ("Number of array dimensions does not match definition"));
        if(ct->elem_type()->is_array())
            ct = (array_type *)ct->elem_type();
        else
            e_type = ct->elem_type();
    }
    if(e_type == NULL)
        ERROR(et_gen, ("Number of array dimensions does not match definition"));


    alod->set_result_type(e_type->unqual());
                                         // element type is the result of ldc
    alod->set_src_addr_op(operand(arr)); // src of ldc is the array instr

    arr->set_result_type(e_type->ptr_to());
                                        // res type of array is ptr to array
    arr->set_base_op(bs_op);            // set the base
    arr->set_elem_size(e_type->size()); // set the size of an element
    arr->set_dims(ndim);                // num dim

//    printf("Array: "); arr->result_type()->print_full();
//    printf("\nbase: "); bs_op.type()->print_full();
//    printf("\n");

    int * lb = new int[ndim];
    int * ub = new int[ndim];
    pblock * indlist = new pblock[ndim];
    ct = bs_type;
    curr = b.child->sib;
    int i;
    for(i=0; i<ndim; i++) {
        // currently we handle only constant bounds
        if(!ct->lower_bound().is_constant())
            ERROR(et_gen, ("Lower bound is not a constant"));
        if(!ct->upper_bound().is_constant())
            ERROR(et_gen, ("Lower bound is not a constant"));
        lb[i] = ct->lower_bound().constant();
        ub[i] = ct->upper_bound().constant();
        if(ub[i] < lb[i])
            ERROR(et_gen, ("No elements in the dimension"));

        indlist[i] = curr;

        ct = (array_type *)ct->elem_type();
        curr = curr->sib;
    }


    int offset = 0;
    int stride = 1;
    for(i=ndim-1; i>=0; i--) {                  // for each dim
        in_ldc * ld_bound = new in_ldc;         // load the # elements in dim
        ld_bound->set_result_type(type_signed);
        ld_bound->set_value(immed(ub[i]-lb[i]+1));
        arr->set_bound(i, operand(ld_bound));

        gen_tree * ind_g = gen_any(*(indlist[i]), ga);  // load the index expr.
        operand ind_op(ind_g->get_oper());
        arr->set_index(i, ind_op);

        offset += lb[i]*stride;                // find the offset and stride
        stride *= (ub[i]-lb[i]+1);

    }

    delete[] indlist;
    delete[] lb;
    delete[] ub;

    // set the offset
    if(get_proc()->proc()->src_lang() == src_fortran) {
        in_ldc * ld_offset = new in_ldc;
        ld_offset->set_result_type(type_signed);
        ld_offset->set_value(immed(offset));
        arr->set_offset_op(operand(ld_offset));
    } else
        arr->set_offset(offset);

    return g;
}



gen_tree * generator::gen_return_inst(const block & b, gen_tree * /* parent */)
{
    gen_tree * g = new gen_tree;
    g->gtt = gtt_ins;
    in_rrr * ret = new in_rrr;
    ret->set_opcode(io_ret);
    g->gen.ins = ret;

    if(b.child) {
        gen_tree * child = gen_any(*(b.child), g);
        operand oper(child->get_oper());
        ret->set_src1(oper);
    }
    ret->set_result_type(type_void);

    return g;
}



gen_tree * generator::gen_call(const block & b, gen_tree * /* parent */)
{
    gen_tree * g = new gen_tree;
    g->gtt = gtt_ins;
    in_cal * ic = new in_cal;
    g->gen.ins = ic;

    assert(b.child);
    assert(b.child->info.ct == ct_sym_node);
    if(b.child->info.code.sym->kind() != SYM_PROC) {
        ERROR(et_gen, ("calling procedure symbol is not correct"));
        return NULL;
    }
    proc_sym * sym = (proc_sym *)b.child->info.code.sym;
    func_type * tp = sym->type();

    type_node * rettp = tp->return_type();

    in_ldc * ld = new in_ldc;
    ld->set_value(immed(sym));
    ld->set_result_type(tp->ptr_to());

    ic->set_addr_op(operand(ld));
    ic->set_result_type(rettp);

    if(!sym->type()->is_func()) {
        ERROR(et_gen, ("calling symbol is not a function call"));
        return NULL;
    }

    func_type * ft = (func_type *)sym->type();

    unsigned cnt;
    block * curr = b.child->sib;
    for(cnt = 0; curr; cnt++)
        curr = curr->sib;

    if (ft->args_known()) {
        if (ft->has_varargs()) {
            if (ft->num_args() > cnt) {
                ERROR(et_gen, ("call has %u arguments(at least %u expected)",
                               cnt, ft->num_args()));
                return NULL;
            }
        } else {
            if (ft->num_args() != cnt) {
                ERROR(et_gen, ("call has %u arguments(%u expected)", cnt,
                               ft->num_args()));
                return NULL;
            }
        }
    }

    ic->set_num_args(cnt);

    curr = b.child->sib;
    for(unsigned i=0; i<cnt; i++, curr = curr->sib) {
        assert(curr);
        gen_tree * curr_t = gen_any(*curr, g);
        assert(curr_t);
        ic->set_argument(i, curr_t->get_oper());
    }

    return g;
}


gen_tree * generator::gen_stmt_list(const block & b, gen_tree * /* parent */)
{
    gen_tree * g = new gen_tree;

    g->gtt = gtt_tnl;
    g->gen.tnl = new tree_node_list();

    const block * curr;
    boolean stmt_list = (b.nt == nt_stmt_list);
    curr = (stmt_list)?b.child:&b;

    while(curr) {
        gen_tree * cg = gen_any(*curr, g);
        switch(cg->gtt) {
        case gtt_none:
            ERROR(et_gen, ("Cannot create an ast_node from %d", cg->gtt));
            break;

        case gtt_sym:
            ERROR(et_gen, ("To be implemented, be cool"));
            break;

        case gtt_ins: {
            tree_instr * ti = new tree_instr(cg->gen.ins);
            g->gen.tnl->append(ti);
            break;
            }

        case gtt_trn:
            g->gen.tnl->append(cg->gen.trn);
            break;

        case gtt_tnl: {
            tree_node_list_iter iter(cg->gen.tnl);
            while(!iter.is_empty()) {
                tree_node * tn = iter.step();
                g->gen.tnl->append(tn);
            }
            break;
            }

        default:
            ERROR(et_gen, ("Cannot generate stmt list for this gen_tree_type"));
        }
        curr = (stmt_list)?curr->sib:NULL;
    }

    return g;
}



in_rrr * generator::mk_cvt(operand op, type_node * out)
{
    assert(out);
    in_rrr * inst = new in_rrr(io_cvt, out, operand(), op, operand());
    return inst;
}

type_node * generator::get_type(operand op)
{
    switch(op.kind()) {
    case OPER_SYM: {
        sym_node * sym = op.symbol();
        if(!sym->is_var()) {
            ERROR(et_gen, ("Cannot find type for non-variable symbol"));
            return type_void;
        }
        type_node * tp = ((var_sym *)sym)->type();
        return tp;
        }

    case OPER_INSTR: {
        instruction * ins = op.instr();
        return ins->result_type();
        }
    case OPER_NULL:
    default:
        ERROR(et_gen, ("Cannot find type for register or null"));
        return type_void;
    }
    return type_void;  // to make the compiler happy
}


type_node * generator::get_type(immed ic)
{
    switch(ic.kind()) {
    case im_int:
    case im_extended_int:
        return type_signed;
    case im_string:
        return type_char->ptr_to();
    case im_float:              // actually a double
    case im_extended_float:
        return type_f64;
    case im_symbol: {
        sym_node * sym = ic.symbol();
        if (sym->is_var())
            return ((var_sym *)sym)->type()->ptr_to();
        else if (sym->is_proc())
            return ((proc_sym *)sym)->type()->ptr_to();
        else if (sym->is_label())
            return type_ptr;
        else ERROR(et_gen, ("Cannot find type of an undef"));
        return type_void;
        }
    case im_type:
        return ic.type();
    case im_op:
        return ic.op().type();
    case im_instr:
        return ic.instr()->result_type();
    case im_undef:
        ERROR(et_gen, ("Cannot find type for undef"));
        return type_void;
    }
    return type_void;  // to make the compiler happy
}

type_node * generator::get_type()
{
    return type_signed;

}
