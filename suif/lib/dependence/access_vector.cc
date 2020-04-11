/* file "access_vector.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "access_vector.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "include.h"

#include <cstdlib>

// allow a[i] where i is a global
#define INDEX_IS_FUNKY_PATH

int forget_about_mod_this = 0;

/************************************************************************
 *                                                                      *
 * Add value i if entry for 'v' exists. otherwise create an entry for v *
 * with value i.                                                        *
 *                                                                      *
 ************************************************************************/
void access_list::enter(void *v,int i)	// compiler bug, couldn\'t inline
{
    access_list_e *e=search(v);
    if(e) {
        int newval = ((long) e->info + i);
        e->info = (void *) newval;
        if(newval == 0) {
            remove(e);
            delete e;
        }
    }
    else if(i)
        alist::enter(v, (void *)i);
}

int av_compare_access_lists(const access_list *av,  const access_list *av2)
{
    // first check that all values on one list are on other
    access_list_iter ai(av2);
    while(!ai.is_empty()) {
        access_list_e *e = ai.step();
        access_list_e *ave = av->search(e->var());
        assert(e->val() != 0);
        if(!ave) {
            return 0;
        } else {
            assert(ave->val() != 0);
            if(e->val() != ave->val()) return 0;
        }
    }

    // now check that any values on av but not av2 are zero
    access_list_iter avi(av);
    while(!avi.is_empty()) {
        access_list_e *e = avi.step();
        if(av2->search(e->key) == 0) {
            assert(e->val() != 0);
            return 0;
        }
    }

    return 1;
}

av_compare_info::av_compare_info(const access_vector *av,
				 const access_vector *av2)
{
//    flag = 0;
    flag = 8;
    assert(!av->too_messy && !av2->too_messy);
    if(av_compare_access_lists(&av->elts, &av2->elts)) flag |= 1;
    if(av_compare_access_lists(&av->conregs, &av2->conregs)) flag |= 2;
    if(av_compare_access_lists(&av->memregs, &av2->memregs)) flag |= 4;
    if(av->con == av2->con) flag |= 16;
}

void print_prefix(FILE *f,int i,int *first)
{
    assert(i != 0);
    if(i > 0 && !*first)
        fprintf(f,"+");
    *first = 0;

    if(i == -1)
        fprintf(f,"-");
    else if(i != 1)
        fprintf(f,"%d*",i);
}

void access_vector::print(FILE *f)
{
    if(too_messy) {
        fprintf(f,"<too messy>");
        return;
    }
    int first = 1;

    access_list_iter ai(&elts);
    while(!ai.is_empty()) {
        access_list_e *e = ai.step();
        tree_node *n = (tree_node *) e->var();
        int i = e->val();
        var_sym * index;
        switch(n->kind()) {
        case TREE_FOR:
            print_prefix(f,i,&first);
            index = ((tree_for *)n)->index();
            fprintf(f,"%s",index->name());
            break;
        default:
            assert(0);
        }
    }
    ai.reset(&conregs);
    while(!ai.is_empty()) {
        access_list_e *e = ai.step();
        var_sym * v = (var_sym *) e->var();
        print_prefix(f,e->val(),&first);
        fprintf(f,"%s",v->name());
    }
    ai.reset(&memregs);
    while(!ai.is_empty()) {
        access_list_e *e = ai.step();
        tree_node *n = (tree_node *) e->var();
        int i = e->val();
        var_sym * index;
        switch(n->kind()) {
        case TREE_FOR:
            print_prefix(f,i,&first);
            index = ((tree_for *)n)->index();
            fprintf(f,"%s",index->name());
            break;
        default:
            assert(0);
        }
    }
    if(first || con < 0)
        fprintf(f,"%d",con);
    else if(con>0)
        fprintf(f,"+%d",con);
    if(mod_this) {
        assert(mod_this->kind() == TREE_FOR);
        fprintf(f,"`mod by: %s'",mod_this->index()->name());
    }
}

access_vector::~access_vector()
{
    if (min) delete min;
    if (max) delete max;
    while(!elts.is_empty()) delete elts.pop();
    while(!conregs.is_empty()) delete conregs.pop();
    while(!memregs.is_empty()) delete memregs.pop();
}

int is_const(const operand & op, int *val)
{
    if(!op.is_instr()) return 0;
    instruction * in = op.instr();
    if(!in) return 0;
    if(in->opcode() == io_cvt)
        return is_const(((in_rrr *)in)->src1_op(), val);
    if(in->opcode() != io_ldc) return 0;
    immed ic = ((in_ldc *) in)->value();
    if(ic.kind() == im_int) *val = ic.integer();
    return (ic.kind() == im_int);
}

tree_for * is_index(var_sym * v, tree_node *tn)
{
    assert(v);
    for(tree_node *n = tn; n; n = n->parent()->parent()) {
        if(n->kind() == TREE_FOR &&
           ((tree_for *)n)->index() == v)
            return (tree_for *) n;
        if(n->parent()==NULL) break;
    }
    return NULL;
}

static void put_index_info(tree_for * fn,	        /* deepest for node */
			   const operand & op,	        /* expression */
			   access_vector *av,
			   int factor)
{
    assert(av);

    tree_for *nf=NULL;
    if(op.kind() == OPER_SYM) {
        if(op.symbol()->is_var() == 0) {
            av->too_messy++;
            return;
        }
        if(fn) {
            nf = is_index((var_sym *)op.symbol(), fn);
            if(nf)
                av->add(nf, factor);
            else
                av->add((var_sym *)op.symbol(), 0, factor);
        } else
            av->add((var_sym *)op.symbol(), 0, factor);
        return;
    }

    if(op.kind() != OPER_INSTR) {
        av->too_messy++;
        return;
    }
    instruction * ins = op.instr();
    in_rrr *i3r = (in_rrr *) ins;
    if(ins->format() == inf_rrr) {
        int val;
	if(is_const(operand(ins), &val)) {
            av->con += val*factor;
            return;
	}
    } else if(ins->format() == inf_ldc) {
        in_ldc * ld = (in_ldc *)ins;
        if(ld->value().is_int_const())
            av->con += ld->value().integer() * factor;
        else
            av->too_messy++;
        return;
    }
    switch(ins->opcode()) {
        // not that io_cvt is too messy.  That way we can assume that
        // lods have the correct type, and in general it allows us
        // to rebuild from the access vectors.
	// Saman 12/1/94:
	// io_cvt is needed in SGI64 format, so here we go.
    case io_cvt: {
	put_index_info(fn, i3r->src1_op(), av, factor);
	break;
    }

    default:
        av->too_messy++;
        break;
    case io_lod: {
        // could be a constant path
        operand  s1 = i3r->src1_op();
        if(s1.is_instr() && s1.instr()->opcode() == io_array) {
            //BUGBUG:10/22/93
            // For a bound DO i=1, U(A(0))
            // returning upper bound to be 0 is incorrect.
            av->too_messy++;
            // all indexes had better be zero or forget it
            in_array *ia = (in_array *) s1.instr();
            s1 = ia->base_op();
	    int i;
            for(i = 0; ((unsigned)i) < ia->dims(); i++) {
                int val;
                if(!is_const(ia->index(i), &val) || val)
                    break;
            }
            if(((unsigned)i) != ia->dims()) {
                av->too_messy++;
                break;
            }
            ins = s1.instr();	// look at the base of the array.
        }
        av->too_messy++;
        break;
    }
    case io_neg:
        put_index_info(fn, i3r->src1_op(), av, -factor);
        break;
    case io_add:
        put_index_info(fn, i3r->src1_op(), av, factor);
        put_index_info(fn, i3r->src2_op(), av, factor);
        break;
    case io_sub:
        put_index_info(fn, i3r->src1_op(), av, factor);
        put_index_info(fn, i3r->src2_op(), av, -factor);
        break;
    case io_mul: {
        int val;
        // var*const or const*var
        if(is_const(i3r->src1_op(), &val))
            put_index_info(fn, i3r->src2_op(), av, factor * val);
        else if(is_const(i3r->src2_op(), &val))
            put_index_info(fn, i3r->src1_op(), av, factor * val);
        else
            av->too_messy++;
    }
        break;
    case io_div:
    case io_divfloor:
    case io_divceil: {
        int val;
        // var/const
        if(!is_const(i3r->src2_op(),&val) || (val == 0) || factor % val)
            av->too_messy++;
        else
            put_index_info(fn, i3r->src1_op(), av, factor / val);
    }
        break;
    case io_lsl: {
        int val;
        if(!is_const(i3r->src2_op(), &val) || val < 0)
            av->too_messy++;
        else
            put_index_info(fn, i3r->src1_op(), av, factor<<val);
    };
        break;
    case io_cpy:
        put_index_info(fn, i3r->src1_op(), av, factor);
        break;
    }
}

void access_vector::enter_als(const access_vector *a)
{
    access_list_iter ai(&a->elts);
    while (!ai.is_empty()){
        access_list_e * ae = ai.step();
        elts.enter(ae->var(),ae->val());
    }

    ai.reset(&a->conregs);
    while (!ai.is_empty()){
        access_list_e * ae = ai.step();
        conregs.enter(ae->var(),ae->val());
    }

    ai.reset(&a->memregs);
    while (!ai.is_empty()){
        access_list_e * ae = ai.step();
        memregs.enter(ae->var(),ae->val());
    }
}


access_vector::access_vector(const access_vector *a): glist_e()
{
    too_messy = a -> too_messy;
    mod_this = forget_about_mod_this ? 0 : a->mod_this;

    con = a->con;
    if (a->min) {
        min = new int;
        *min = *a->min;
    } else min = NULL;
    if (a->max) {
        max = new int;
        *max = *a->max;
    } else max = NULL;

    enter_als(a);
}


access_vector::access_vector(const access_vector &a): glist_e()
{
    too_messy = a.too_messy;
    mod_this = forget_about_mod_this ? 0 : a.mod_this;

    con = a.con;
    if (a.min) {
        min = new int;
        *min = *a.min;
    } else min = NULL;
    if (a.max) {
        max = new int;
        *max = *a.max;
    } else max = NULL;

    enter_als(&a);
}


access_vector::access_vector(tree_instr *n, int fancy):
    glist_e(), too_messy(0), con(0)
{
    min = max = 0;
    mod_this = 0;
    tree_for * tf = (tree_for *)next_further_out(n, TREE_FOR);
    put_index_info(tf, operand(n->instr()), this, 1);

    if(fancy == 0 &&
       !(conregs.is_empty() && memregs.is_empty()))
        too_messy++;
    else if(fancy == 1 &&
            !(memregs.is_empty()))
        too_messy++;
    if(!forget_about_mod_this)
        set_mod_this(n);
    //set_min_max();  // gets messed up by normalization
}

access_vector::access_vector(operand op, tree_node * tn, int fancy):
    glist_e(), too_messy(0), con(0)
{
    min = max = 0;
    mod_this = 0;


    tree_for * tf = (tree_for *)next_further_out(tn, TREE_FOR, TRUE);
    put_index_info(tf, op, this, 1);

    if(!fancy &&
       !(conregs.is_empty() && memregs.is_empty()))
        too_messy++;
    else if(fancy == 1 &&
            !(memregs.is_empty()))
        too_messy++;
    if(!forget_about_mod_this)
        if(tf)
            set_mod_this(tf);
    //set_min_max();  /* gets messed up by normalization */
}

access_vector::access_vector(instruction  * inst, int fancy):
    glist_e(), too_messy(0), con(0)
{
    min = max = 0;
    mod_this = 0;

    assert(inst);

    tree_for * tf = (tree_for *)next_further_out(inst->parent(), TREE_FOR, TRUE);
    put_index_info(tf, operand(inst), this, 1);

    if(!fancy &&
       !(conregs.is_empty() && memregs.is_empty()))
        too_messy++;
    else if(fancy == 1 &&
            !(memregs.is_empty()))
        too_messy++;
    if(!forget_about_mod_this)
        set_mod_this(tf);
    //set_min_max();  /* gets messed up by normalization */
}


void array_info::print(FILE *f)
{
    int print_comma = 0;
    fprintf(f,"[");
    array_info_iter ai(this);
    while(!ai.is_empty()) {
        if(print_comma++) fprintf(f,",");
        ai.step()->print(f);
    }
    fprintf(f,"]");
}

array_info::~array_info()
{
    while(!is_empty()) delete pop();
}

array_info::array_info(instruction * ins, int fancy)
{
    assert(ins->opcode() == io_array);
    in_array *i = (in_array *) ins;

    for(int cnt = 0; ((unsigned)cnt) < i->dims(); cnt++) {
        access_vector *av;
        av = new access_vector(i->index(cnt), ins->parent(), fancy);
        append(av);
    }
}


array_info::array_info(array_info * ainfo)
{
    array_info_iter iter(ainfo);

    while(!iter.is_empty()) {
        access_vector * av = iter.step();
        append(new access_vector(av));
    }
}

static void build_block(block &B, boolean &block_empty, binary_op bop, block &A)
{
    if (block_empty) {
	switch (bop) {
	case bop_add: B.set(A);  break;
	case bop_sub: B.set(-A); break;
	default: assert(0);
	}
    }
    else B.set(B.dobinop(bop,A));
    block_empty = FALSE;
}


operand access_vector::generate_code(tree_proc *p, block_symtab * sym)
{
    // ignores the step, so if the step\'s not one, be careful!

    assert(!too_messy);

    block::set_proc(p); // block::set_proc(p) didn't compile
    block B;
    boolean block_empty = TRUE;

    // Only set the block to the constant value if the constant is non-zero.
    if (con) {
	B.set(block(con));
	block_empty = FALSE;
    }

    access_list_iter ai(&elts);
    while (!ai.is_empty()) {
	access_list_e *ae = ai.step();
	tree_for *a = (tree_for *) ae->var();
	int i = ae->val();
	block var(a->index());
	if(i == 0) continue;
	else if(i == 1) build_block(B,block_empty,bop_add,var);
	else if(i == -1) build_block(B,block_empty,bop_sub,var);
	else {
	    block multi(i);
	    build_block(B,block_empty,bop_add,multi*var);
	}
    }

    ai.reset(&conregs);
    while (!ai.is_empty()) {
	access_list_e *ae = ai.step();
	var_sym * v = (var_sym *) ae->var();
	int i = ae->val();
	block var(v);
	if(i == 0) continue;
	else if(i == 1) build_block(B,block_empty,bop_add,var);
	else if(i == -1) build_block(B,block_empty,bop_sub,var);
	else {
	    block multi(i);
	    build_block(B,block_empty,bop_add,multi*var);
	}
    }

    ai.reset(&memregs);
    while (!ai.is_empty()) {
	access_list_e *ae = ai.step();
	var_sym * v = (var_sym *) ae->var();
	int i = ae->val();
	block var(v);
	if(i == 0) continue;
	else if(i == 1) build_block(B,block_empty,bop_add,var);
	else if(i == -1) build_block(B,block_empty,bop_sub,var);
	else {
	    block multi(i);
	    build_block(B,block_empty,bop_add,multi*var);
	}
    }

    // If the block is still empty, then add the zero constant.
    if (block_empty)
	B.set(block(con));

    instruction * inst;
    if(sym)
	inst = B.make_instruction();  // B.make_instruction(sym)
    else
	inst = B.make_instruction();

    return operand(inst);
}



void access_vector::operator = (const access_vector &a)
{
	too_messy = a.too_messy;
	con = a.con;
	if (a.min) {
		min = new int;
		*min = *a.min;
        } else min = 0;
	if (a.max) {
		max = new int;
		*max = *a.max;
	} else max = 0;

	while(!elts.is_empty()) delete elts.pop();
	while(!conregs.is_empty()) delete conregs.pop();
	while(!memregs.is_empty()) delete memregs.pop();

	enter_als(&a);
}

void access_vector::operator += (const access_vector &a)
{
	if (min && a.min) {
            *min += *a.min;
	} else {
            if(min) delete min;
            min = 0;
        }

	if (max && a.max) {
            *max += *a.max;
	} else {
            if(max) delete max;
            max = 0;
        }

	if(a.too_messy)
		too_messy++;
	if(too_messy)
		return;

	con += a.con;

	enter_als(&a);
}

void access_vector::operator -= (const access_vector &a)
{
    if (min && a.min) {
        *min -= *a.min;
    } else {
        if(min) delete min; min = 0;
    }

    if (max && a.max) {
        *max -= *a.max;
    } else {
        if(max) delete max; max = 0;
    }

	if(a.too_messy)
		too_messy++;
	if(too_messy)
		return;

	con -= a.con;

	access_list_iter ai(&a.elts);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    elts.enter(ae->var(),-ae->val());
	}

	ai.reset(&a.conregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    conregs.enter(ae->var(),-ae->val());
	}

	ai.reset(&a.memregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    memregs.enter(ae->var(),-ae->val());
	}

}

void access_vector::operator *= (int f)
{
	if (min) {
		*min *= f;
	}

	if (max) {
		*max *= f;
	}

	if (f < 0) {
		int *temp = min;
		min = max;
		max = temp;
	}

	if(too_messy || f == 1)
		return;

	if(f == 0) {
		access_vector av;
		*this = av;
		return;
	}

	con *= f;

	access_list_iter ai(&elts);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    ae->info = (void *) ((long) ae->info * f);
	}

	ai.reset(&conregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    ae->info = (void *) ((long) ae->info * f);
	}

	ai.reset(&memregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    ae->info = (void *) ((long) ae->info * f);
	}

}

void access_vector::operator /= (int f)
{
	assert(f != 0);

	int f2=f;
	if (f < 0) {
		f2 = -f;
		int *temp = min;
		min = max;
		max = temp;
		if (min) *min = -*min;
		if (max) *max = -*max;
	}

	if (min) {
		if (*min % f2 == 0) *min /= f2;
		else if (*min > 0) *min /= f2;
		else *min = *min / f2 - 1;
	}
	if (max) {
		if (*max % f2 == 0) *max /= f2;
		else if (*max > 0) *max = *max/f2 + 1;
		else *max = *max / f2 ;
	}


	if(too_messy || f == 1)
		return;

	if(con % f) too_messy++;
	else con /= f;

	access_list_iter ai(&elts);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    if((long) ae->info % f) too_messy++;
	    else ae->info = (void *) ((long) ae->info / f);
	}

	ai.reset(&conregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    if((long) ae->info % f) too_messy++;
	    else ae->info = (void *) ((long) ae->info / f);
	}

	ai.reset(&memregs);
	while (!ai.is_empty()){
	    access_list_e * ae = ai.step();
	    if((long) ae->info % f) too_messy++;
	    else ae->info = (void *) ((long) ae->info / f);
	}

}



//
// find the innermost for loop which might assign this acceess vector
//
void access_vector::set_mod_this(tree_node * ti)
{
    assert(forget_about_mod_this == 0);

    mod_this = 0;

    tree_for *find_inner(tree_node *, var_sym *);
    int is_ancestor(tree_node *, tree_node *);

    // first find innermost for loop
    tree_node *inner = (tree_node *) ti;
    tree_node *prev = inner;
    while(inner->kind() != TREE_FOR) {
        assert(inner->parent());
        inner = inner->parent()->parent();
        if (!inner) {                           // not inside a for loop
            mod_this = NULL;
            return;
        }
        prev = inner;
    }

    // go through all the registers
    access_list_iter ali(&conregs);
    while (!ali.is_empty()) {
        access_list_e *e = ali.step();
        var_sym * sym = (var_sym *)(long)e->var();
        tree_for *temp = find_inner(inner, sym);
        if (temp) {
            if(!mod_this || is_ancestor(mod_this, temp)) {
                mod_this = temp;
            }
        }
    }

    // go through all the memory registers
    ali.reset(&memregs);
    while (!ali.is_empty()) {
        access_list_e *e = ali.step();
        var_sym * sym = (var_sym *)(long)e->var();
        tree_for *temp = find_inner(inner, sym);
        if (temp) {
            if(!mod_this || is_ancestor(mod_this,temp)) {
                mod_this = temp;
            }
        }
    }

    // copout.  We need to handle this case eventually ...
    if (!memregs.is_empty()) {
        assert(inner && inner->kind() == TREE_FOR);
        mod_this = (tree_for *) inner;
        return;
    }

}


// is node 1 an ancestor of node 2
int is_ancestor(tree_node *node1, tree_node *node2)
{
    if(node2->parent() == NULL) return 0;
    node2 = node2->parent()->parent();
    while (node2) {
        if (node2 == node1) return 1;
        if(node2->parent() == NULL) return 0;
        node2 = node2->parent()->parent();
    }
    return 0;
}


// find the innermost for loop which might assign a register
tree_for *find_inner(tree_node *n, var_sym * v)
{
    tree_for *prev = 0;

    while (n) {
        if (n->kind() == TREE_FOR) {
            tree_for *tnf = (tree_for *)n;
            if (sym_modified(v, tnf->body(), prev)) {
                return tnf;
            } else if (sym_modified(v, tnf->ub_list(), prev)) {
                return tnf;
            } else if (sym_modified(v, tnf->step_list(), prev)) {
                return tnf;
            } else if (sym_modified(v, tnf->lb_list(), prev)) {
                return tnf;
            }
            prev = tnf;
        }

        n = (n->parent())?n->parent()->parent():NULL;
    }
    return 0;
}



// routines to manipulate access lists

// set the union of two lists
void access_list::unite(access_list *a1, access_list *a2)
{
	access_list_iter ai(a1);
	while(!ai.is_empty()) {
		access_list_e *e = ai.step();
		enter(e->var(),e->val());
	}
	access_list_iter ai2(a2);
	while(!ai2.is_empty()) {
		access_list_e *e = ai2.step();
		if (!this->search(e->var())) {
			enter(e->var(),e->val());
		}
	}
}

// set the union of this to another list
void access_list::unite(access_list *a1)
{
	access_list_iter ai(a1);
	while(!ai.is_empty()) {
		access_list_e *e = ai.step();
		if (!this->search(e->var())) {
			enter(e->var(),e->val());
		}
	}
}

// set the intersection of two lists
void access_list::intersect(access_list *a1, access_list *a2)
{
	access_list_iter ai(a1);
	while(!ai.is_empty()) {
		access_list_e *e = ai.step();
		if (a2->search(e->var())){
			enter(e->var(),e->val());
		}
	}
}

// set the intersection of this to another list
void access_list::intersect(access_list *a1)
{
	access_list_iter ai(this);
	while(!ai.is_empty()) {
		access_list_e *e = ai.step();
		if (!a1->search(e->var())){
			remove(e);
		}
	}
}

// how many elements
int access_list::count()
{
	int cnt;
	access_list_iter ai(this);
	for (cnt=0; !ai.is_empty(); ai.step()) cnt++;
	return cnt;
}

// find the min and max of this access_vector
void access_vector::set_min_max()
{
	if (max) {
		delete max;
		max = 0;
	}
	if (min) {
		delete min;
		min = 0;
	}

	if (too_messy || !conregs.is_empty() ||
	    !memregs.is_empty()) {
                max = 0; min = 0;
		return;
	}

	min = new int;
	max = new int;
	*max = *min = con;

	access_list_iter ai(&elts);
	while(!ai.is_empty()) {
	    access_list_e *induct = ai.step();
	    int value = induct->val();
            if(value==0)
                continue;
            dep_for_annote *dfa = NULL;  // = (tree_for *) induct->var();
            assert(dfa);

	    int *formin = 0;
            if(dfa->lb) {
                array_info_iter ai(dfa->lb);
                while (!ai.is_empty()) {
                    access_vector *av = ai.step();
                    if (!av->too_messy && av->min) {
			if (!formin || (*av->min < *formin)) {
                            formin = av->min;
			}
                    }
		}
	    }
	    int *formax = 0;
            if(dfa->ub) {
                array_info_iter ai2(dfa->ub);
                while (!ai2.is_empty()) {
                    access_vector *av = ai2.step();
                    if (!av->too_messy && av->max) {
			if (!formax || (*av->max > *formax)) {
                            formax = av->max;
			}
                    }
		}
	    }

            int *fmin = value > 0 ? formin : formax;
            int *fmax = value > 0 ? formax : formin;

            if (min && fmin) {
                *min = *min + value * *fmin;
            } else {
                if(min) delete min;
                min = 0;
            }
            if (max && fmax) {
                *max = *max + value * *fmax;
            } else {
                if(max) delete max;
                max = 0;
            }
	}
}




