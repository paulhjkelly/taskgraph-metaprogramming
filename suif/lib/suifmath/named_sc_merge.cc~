#define _MODULE_ "libsuifmath.a"
#pragma implementation "named_sc_merge.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include "suifmath.h"



const char * k_named_sc_merge_mark_annote = NULL;

name_table * named_sc_merge::cnt = NULL;
name_table * named_sc_merge::pnt = NULL;


static boolean is_identical(named_symcoeff_ineq & v1,
                            named_symcoeff_ineq & v2)
{
    named_symcoeff_ineq nsi1(v1);
    named_symcoeff_ineq nsi2(v2);

    nsi1 || nsi2;

    if(nsi1.p() != nsi2.p()) return FALSE;
    if(nsi1.m() != nsi2.m()) return FALSE;
    if(nsi1.n() != nsi2.n()) return FALSE;

    for(int ip=0; ip < nsi1.p(); ip++)
        for(int im=0; im < nsi1.m(); im++)
            for(int in=0; in < nsi1.n(); in++)
                if(nsi1[ip][im][in] != nsi2[ip][im][in])
                    return FALSE;
    return TRUE;
}

/* ##################################################
   #####   named_sc_merge                       #####
   ################################################## */

named_sc_merge::named_sc_merge(named_symcoeff_ineq_list * nsil, 
                               sc_merge_type smt)
{
    assert(smt == smt_full);    // Currently the only implementation
    assert(nsil);
    init();
    int i = 0;
    named_symcoeff_ineq_list_iter iter(nsil);
    while(!iter.is_empty()) {
        named_symcoeff_ineq * tnsi = iter.step();
        marked_nsi * mn = new marked_nsi(*tnsi, i++);
        mn_list.append(mn);
    }
}


named_sc_merge::named_sc_merge(marked_nsi_list * mnl,
                               sc_merge_type smt)
{
    assert(smt == smt_full);    // Currently the only implementation
    assert(mnl);
    init();
    marked_nsi_list_iter iter(mnl);
    while(!iter.is_empty()) {
        marked_nsi * mn = iter.step();
        marked_nsi * nmn = new marked_nsi(mn);
        mn_list.append(nmn);
    }
}


void named_sc_merge::init()
{
    cond_num = -1;
    excess_num = -1;

    mn_list.clear();

    assert_msg(k_named_sc_merge_mark_annote, ("suifmath library is not initialized"));
}


named_sc_merge::~named_sc_merge()
{
    if(cnt) { 
        delete cnt;
        cnt = NULL;
    }

    if(pnt) { 
        delete pnt;
        pnt = NULL;
    }

    if(hdr) delete hdr;
}


void named_sc_merge::merge()
{
    do_align(mn_list);

    do_merge_full(mn_list);
#ifdef DEBUG_PRINT
    fprintf(stderr,"-before delete----------\n");
    print(stderr);
#endif
    do_delete_empty();
}

void named_sc_merge::do_mark_cond()
{
    if(cond_num>0) {
        assert(cond_num<=names().n());
        
        do_mark_cond(names(), cond_num);
        
        marked_nsi_list_iter iter(&mn_list);
        while(!iter.is_empty()) {
            marked_nsi * mn = iter.step();
            do_mark_cond(mn->nsi.cols(), cond_num);
        }
    }
}


void named_sc_merge::do_mark_excess()
{
    if(excess_num>0) {
        assert(excess_num <=  names().n());
        constraint filt(names().n());
        filt = 0;
        for(int i=excess_num; i<names().n(); i++)
            filt[i] = 1;

        marked_nsi_list_iter iter(&mn_list);
        while(!iter.is_empty()) {
            marked_nsi * mn = iter.step();
            mn->nsi = mn->nsi.filter_away(&filt, NULL,  0);
            mn->nsi.del_col(filt);
        }

        names().remove(excess_num, names().n()-1);
    }
}


void named_sc_merge::do_mark_cond(name_table & nt, int i)
{
    int x;
    for(x=1; x<=i; x++)
        nt[x].mark_cond();
    for(; x<nt.n(); x++)
        if(nt[x].kind() != nte_aux)
            nt[x].mark_loop();
}


void named_sc_merge::do_align(marked_nsi_list & mnl)
{
    marked_nsi_list_iter iter1(&mnl);
    cnt = NULL;
    pnt = NULL;
    while(!iter1.is_empty()) {
        named_symcoeff_ineq & nsi = iter1.step()->nsi;
        if(cnt) {
            name_table * tcnt = name_table::mk_align(cnt, nsi.cols());
            name_table * tpnt = name_table::mk_align(pnt, nsi.planes());
            delete cnt;
            delete pnt;
            cnt = tcnt;
            pnt = tpnt;
        } else {
            cnt = new name_table(nsi.cols());
            pnt = new name_table(nsi.planes());
        } 
    }

    assert(cnt);
    assert(pnt);
}


void named_sc_merge::do_merge_full(marked_nsi_list & mnl)
{
    marked_nsi_list_iter iter1(&mnl);
    while(!iter1.is_empty()) {
        marked_nsi * mn = iter1.step();
        
        mn->nsi.nt_align(cnt, pnt);

        named_sc_fm FM(mn->nsi);
        FM.fm_bounds();
        FM.get(&mn->nsi);
    }

    do_mark_cond();
    do_mark_excess();
    
    name_table_entry nte("Root");
    hdr = new level_elem(NULL, nte, NULL);

    named_symcoeff_ineq nsi_null;
    hdr->merge_children(mnl, nsi_null, 1);
}


void named_sc_merge::do_delete_empty()
{
    assert(hdr);
    hdr->delete_empty();
}


tree_node_list * named_sc_merge::generate_code(base_symtab * bs)
{       
    if(bs == NULL) {
        tree_proc * tp = block::get_proc();
        assert_msg(tp, ("No Tree Proc given"));
        bs = tp->symtab();
    }

    assert(bs->is_block());
    assert_msg(hdr, ("Need to perform merge before doing this"));

    return hdr->generate_tree_node_list((block_symtab *)bs);
}


void named_sc_merge::print(int knd, FILE *fp)
{
    assert(hdr);
    fprintf(fp,"\n");
    if(hdr) 
        hdr->print(knd,fp);
    else
        fprintf(fp,"Empty tree\n");
}


/* ##################################################
   #####   value_tree                           #####
   ################################################## */

value_tree::value_tree(value_tree * par, named_symcoeff_ineq & v)
{
    pp = par;
    val = v;
    tp = NULL;
    fp = NULL;
}


value_tree::value_tree(value_tree * par)
{
    pp = par;
    tp = NULL;
    fp = NULL;
}


value_tree::~value_tree()
{
    if(true_branch()) delete true_branch();
    if(false_branch()) delete false_branch();
}



boolean value_tree::is_true_path()
{
    assert(parent());
    if(parent()->true_branch() == this) return TRUE;
    if(parent()->false_branch() == this) return FALSE;
    assert(0);
    return FALSE; // just to pacify the compiler
}


value_tree * value_tree::duplicate()
{
    value_tree * nvt = new value_tree(NULL, value());

    if(true_branch()) {
        nvt->tp = true_branch()->duplicate();
        nvt->tp->pp = nvt;
    }

    if(false_branch()) {
        nvt->fp = false_branch()->duplicate();
        nvt->fp->pp = nvt;
    }
    
    return nvt;
}


void value_tree::insert_tree(named_symcoeff_ineq & v)
{
    assert(v.m() == 1);
    assert(parent() == NULL);
    insert_tree(v, v);
}


void value_tree::insert_tree(named_symcoeff_ineq & v, 
                             named_symcoeff_ineq & curr)
{

    if(is_identical(value(), v)) return; 
    
    if(value().m() == 0) {
        assert((true_branch() == FALSE)&&(false_branch() == FALSE)); 
        val = v;
        return;
    }

    named_symcoeff_ineq t_tmp(curr);
    named_symcoeff_ineq f_tmp(curr);
    t_tmp &= value();
    f_tmp &= value().inverse_all();
    boolean t_possible = (~t_tmp == FALSE);
    boolean f_possible = (~f_tmp == FALSE);
    assert(t_possible || f_possible);
    if(t_possible) {
        if(true_branch()) 
            true_branch()->insert_tree(v, t_tmp);
        else
            tp = new value_tree(this, v);
    }

    if(f_possible) {  
        if(false_branch()) 
            false_branch()->insert_tree(v, f_tmp);
        else 
            fp = new value_tree(this, v);
    }
}


boolean value_tree::is_leaf()
{
    return FALSE;
}


void value_tree::insert_leaves()
{
    assert(!is_leaf());
    if(true_branch())   
            true_branch()->insert_leaves();
        else
            tp = new tree_leaf(this);

    if(false_branch())   
        false_branch()->insert_leaves();
    else
        fp = new tree_leaf(this);
}


int value_tree::number_leaves(int n)
{
    if(is_leaf()) {
        assert(true_branch() == NULL);
        assert(false_branch() == NULL);
        tree_leaf * tl = (tree_leaf *)this;
        tl->num = n;
        return n+1;
    }
    if(false_branch())
        n = false_branch()->number_leaves(n);
    if(true_branch())
        n = true_branch()->number_leaves(n);
    return n;
}


void value_tree::mark_leaves(marked_nsi & curr, named_symcoeff_ineq * bounds)
{
    named_symcoeff_ineq t_tmp(value() & curr.nsi);
    named_symcoeff_ineq f_tmp(value().inverse_all() & curr.nsi);

    if(bounds) {
        t_tmp &= *bounds;
        f_tmp &= *bounds;
    }

    named_symcoeff_ineq t_tmp2(t_tmp & curr.nsi);
    named_symcoeff_ineq f_tmp2(f_tmp & curr.nsi);
    boolean t_possible = (~t_tmp2 == FALSE);
    boolean f_possible = (~f_tmp2 == FALSE);
    if(t_possible) {
        assert(true_branch());
        true_branch()->mark_leaves(curr, &t_tmp);
    }
    if(f_possible) {
        assert(false_branch());
        false_branch()->mark_leaves(curr, &f_tmp);
    }
}


void value_tree::create_levels(level_elem * par, 
                               named_symcoeff_ineq * bound,
                               named_symcoeff_ineq * outer_bound,
                               name_table_entry & nte)
{
    named_symcoeff_ineq t_tmp(value());
    named_symcoeff_ineq f_tmp(value().inverse_all());
    if(bound) {
        t_tmp &= *bound;
        f_tmp &= *bound;
    }

    boolean t_possible = (~t_tmp == FALSE);
    boolean f_possible = (~f_tmp == FALSE);

    if(f_possible) {
        assert(false_branch());
        false_branch()->create_levels(par, &f_tmp, outer_bound, nte);
    }
    if(t_possible) {
        assert(true_branch());
        true_branch()->create_levels(par, &t_tmp, outer_bound, nte);
    }
}


int dbg_count = 0;
void  value_tree::debug_print(named_symcoeff_ineq & curr,
                              name_table_entry & nte)
{
    if(is_leaf()) {
        tree_leaf * tl = (tree_leaf *)this;
        if(~curr == FALSE) {
            fprintf(stderr,"\n%d.%d---", tl->num, dbg_count++);
            
            named_symcoeff_ineq tmp(curr);
            int i = tmp.cols().find(nte);
            assert(i>0);
            if(i < tmp.n()-1)
                tmp.swap_col(i, tmp.n()-1);

            named_sc_fm FM(tmp);
            FM.fm_bounds();
            FM.get(&tmp);
            tmp.print_code(TRUE, tmp.n()-1, tmp.n()-1, stderr);
        }
    } else {
        named_symcoeff_ineq t_tmp(curr);
        named_symcoeff_ineq f_tmp(curr);
        t_tmp &= value();
        f_tmp &= value().inverse_all();
        if(false_branch())
            false_branch()->debug_print(f_tmp, nte);
        if(true_branch())
            true_branch()->debug_print(t_tmp, nte);
    }
}


void  value_tree::print(int level, FILE *fp)
{
    if(is_leaf()) {
        tree_leaf * tl = (tree_leaf *)this;
        assert(true_branch() == NULL);
        assert(false_branch() == NULL);
        for(int t=0; t<level; t++) fprintf(fp,"   ");        
        fprintf(fp,"LEAF %d\n", tl->num);
    } else {
        assert(value().m() == 1);

	int t;
        for(t=0; t<level; t++) fprintf(fp,"   ");
        fprintf(fp,"if("); value().print_exp(0,fp); fprintf(fp,">=0) then\n");
        if(true_branch())
            true_branch()->print(level+1, fp);
        else {
            for(t=0; t<level+1; t++) fprintf(fp,"   ");
            fprintf(fp,"NULL\n");
        }

        for(t=0; t<level; t++) fprintf(fp,"   ");
        fprintf(fp,"else // "); value().inverse_all().print_exp(0,fp); fprintf(fp,">=0 \n");
        if(false_branch())
            false_branch()->print(level+1, fp);
        else {
            for(t=0; t<level+1; t++) fprintf(fp,"   ");
            fprintf(fp,"NULL\n");
        }

    }
}

/* ##################################################
   #####   tree_leaf                            #####
   ################################################## */
tree_leaf::tree_leaf(value_tree * par) : value_tree(par)
{
    num = -1;
}


tree_leaf::~tree_leaf()
{
    marked_nsi_list_iter iter(&valid_list);
    while(!iter.is_empty()) {
        marked_nsi * mn = iter.step();
        delete mn;
    }
    valid_list.clear();
}
        

boolean tree_leaf::is_leaf()
{
    return TRUE;
}


void tree_leaf::mark_leaves(marked_nsi & curr, named_symcoeff_ineq * /* bounds */)
{
    // BUGBUG: parameter bounds is not used
    marked_nsi * nmn = new marked_nsi(curr);
    valid_list.append(nmn);
}


void tree_leaf::create_levels(level_elem * par, 
                              named_symcoeff_ineq * bound,
                              named_symcoeff_ineq * outer_bound,
                              name_table_entry & nte)
{
    if(bound == NULL) return;

#ifdef DEBUG_PRINT
    fprintf(stderr,"-----------\n");
    if(bound) {
        boolean possible = (~*bound == FALSE);
        fprintf(stderr,"    bound %d", possible);
        bound->print_code(stderr);
        named_sc_fm FM(bound);
        FM.fm_bounds();
        fprintf(stderr,"Reduced");
        FM.get()->print_code(stderr);
    }
    if(outer_bound) {
        boolean possible = (~*outer_bound == FALSE);
        fprintf(stderr,"    outer bound %d", possible);
        outer_bound->print_code(stderr);
    }
#endif

    
    level_elem * curr = new level_elem(par, nte, bound, outer_bound);
    par->ch.append(curr);
    int pos = named_sc_merge::names().find(nte);
    assert(pos>0);
    
#ifdef DEBUG_PRINT
    fprintf(stderr,"%d.%d", num, curr->uid());
#endif
    
    if(pos < named_sc_merge::names().n() - 1) {
#ifdef DEBUG_PRINT
        fprintf(stderr,"[");
#endif
        marked_nsi_list_iter iter(&valid_list);
        marked_nsi_list newlist;
        while(!iter.is_empty()) {
            marked_nsi * mn = iter.step();
            named_symcoeff_ineq nsi(mn->nsi);
            if(bound) 
                nsi &= *bound;
            
            boolean possible = (~nsi == FALSE);
            if(possible) {
                // restrict the bounds
                named_sc_fm FM(nsi);
                FM.fm_bounds();
                FM.get(&nsi);
                fprintf(stderr,"%d ", mn->mark);
                
                marked_nsi * nmn = new marked_nsi(nsi, mn->mark);
                newlist.append(nmn);
            }
        }
#ifdef DEBUG_PRINT
        fprintf(stderr,"]\n");
#endif
        curr->merge_children(newlist, *bound, pos+1);
    } else {
#ifdef DEBUG_PRINT
        fprintf(stderr,"[...]\n");
#endif
        curr->add_orig_mark(valid_list);
    }
}



/* ##################################################
   #####   level_elem                           #####
   ################################################## */

#ifdef DEBUG_COUNTER
int level_elem::le_counter = 0;
int level_elem::uid()           { return unum; }
#else 
int level_elem::uid()           { return 0; }
#endif



level_elem::level_elem(level_elem * p,
                       name_table_entry & nte,
                       named_symcoeff_ineq * iq,
                       named_symcoeff_ineq * outer)

{
#ifdef DEBUG_COUNTER
    unum = le_counter++;
#endif
    par = p;
    ind.init(nte);
    if(iq) {
        ineq = *iq;

        int pos = named_sc_merge::names().find(nte);
        assert(pos>0);

        constraint filt(ineq.n());
        filt = 0;
        filt[pos] = 1;
        ineq = ineq.filter_thru(&filt, NULL,  0);

        if(outer) ineq &= *outer;
#ifdef DEBUG_PRINT
        boolean possible = (~ineq == FALSE);
#endif
        named_sc_fm FM(ineq);
        FM.fm_bounds(pos, pos+1);
        FM.get(&ineq);
#ifdef DEBUG_PRINT
        fprintf(stderr,"    level_elem %d", possible);
        ineq.print_code(stderr);
#endif
    }
    ch_vtree = NULL;
}


level_elem::~level_elem()
{
    level_elem_list_iter iter(&ch);
    while(!iter.is_empty()) {
        level_elem * le = iter.step();
        delete le;
    }
    ch.clear();
    
    if(ch_vtree) delete ch_vtree;
}


void level_elem::merge_children(marked_nsi_list & inlist, 
                                named_symcoeff_ineq & outer_bounds,
                                int pos)
{
    // create the value tree
    marked_nsi_list_iter iter1(&inlist);
    while(!iter1.is_empty()) {
        named_symcoeff_ineq nsi(iter1.step()->nsi);
        merge_to_value_tree(nsi, pos);
    }

    if(ch_vtree) 
        ch_vtree->insert_leaves();
    else
        ch_vtree = new tree_leaf(NULL);
    ch_vtree->number_leaves();


    // mark the leaves
    marked_nsi_list_iter iter2(&inlist);
    while(!iter2.is_empty()) {
        marked_nsi * mn = iter2.step();
        ch_vtree->mark_leaves(*mn, NULL);
    }

    assert(pos<named_sc_merge::names().n());
    named_symcoeff_ineq * bpt = (outer_bounds.m())?&outer_bounds:NULL;
    ch_vtree->create_levels(this, bpt, bpt, named_sc_merge::names()[pos]);
}



void level_elem::merge_to_value_tree(named_symcoeff_ineq & ineq, int pos)
{
    named_symcoeff_ineq tmp(ineq);
//    fprintf(stderr,"before inserting %s\n", ineq.cols()[pos].string());

    constraint filt(tmp.n());
    filt = 0;
    int j;
    for(j=pos+1; j<tmp.n(); j++) filt[j] = 1;
    named_symcoeff_ineq curr = tmp.filter_away(&filt, NULL,  0);
    
    filt = 0;
    filt[pos] = 1;
    named_symcoeff_ineq lb = curr.filter_thru(&filt, NULL,  1);
    named_symcoeff_ineq ub = curr.filter_thru(&filt, NULL, -1);

    for(j=0; j<lb.m(); j++) {
        named_symcoeff_ineq tmp(lb);
        tmp.set_n_ineq(1);
        lin_ineq * lim = lb.get_m(j);
        tmp.set_m(0, lim);
        add_vtree(tmp);
        delete lim;
    }
    named_symcoeff_ineq ubinv(ub.inverse_all());
    for(j=0; j<ub.m(); j++) {
        named_symcoeff_ineq tmp(ubinv);
        tmp.set_n_ineq(1);
        lin_ineq * lim = ubinv.get_m(j);
        tmp.set_m(0, lim);
        add_vtree(tmp);
        delete lim;
    }
}


void level_elem::add_vtree(named_symcoeff_ineq &v)
{
//    fprintf(stderr," Inserting: "); v.print_exp(stderr);
    if(ch_vtree) 
        ch_vtree->insert_tree(v);
    else
        ch_vtree = new value_tree(NULL, v);
}


void level_elem::add_orig_mark(marked_nsi_list & lst)
{
    marked_nsi_list_iter iter(&lst);
    while(!iter.is_empty()) {
        marked_nsi * mn = iter.step();
        marked_nsi * nmn = new marked_nsi(mn->mark);
        orig_loop.append(nmn);
    }
}


boolean level_elem::delete_empty() 
{
    if(ch.count() == 0)
        return (orig_loop.count() > 0);

    boolean found = FALSE;
    level_elem_list_iter iter(&ch);
    while(!iter.is_empty()) {
        level_elem * c = iter.step();
        boolean res = c->delete_empty();
        found |= res;
        if(res == FALSE) {
            level_elem_list_e * le = iter.cur_elem();
            ch.remove(le);
        }
    }

    return found;
}


tree_node_list * level_elem::generate_tree_node_list(block_symtab * bs)
{
    tree_node_list * ch_code = new tree_node_list;
    if(ch.count()) {
        level_elem_list_iter iter1(&ch);
        while(!iter1.is_empty()) {
            level_elem * le = iter1.step();
            tree_node * tn = le->generate_tree_node(bs);
            ch_code->append(tn);
        }
    } else {
        immed_list * iml = new immed_list;
        marked_nsi_list_iter iter2(&orig_loop);
        while(!iter2.is_empty()) {
            marked_nsi * mn = iter2.step();
            iml->append(immed(mn->mark));
        }
	block_symtab * nbs = new block_symtab("named_sc_merge");
	bs->add_child(nbs);
        tree_block * tb = new tree_block(new tree_node_list, nbs);
        tb->append_annote(k_named_sc_merge_mark_annote, iml);
        ch_code->append(tb);
    }

    return ch_code;
}

tree_node * level_elem::generate_tree_node(block_symtab * bs)
{
    block ind(index().var());
    block blb;
    block bub;
    block code;
    
    named_symcoeff_ineq tmp(ineq);
    tmp.cleanup();
    
    int i = tmp.cols().find(index());
    constraint filt(tmp.n());
    filt = 0;
    for(int j=i+1; j<tmp.n(); j++) filt[j] = 1;
    named_symcoeff_ineq curr = tmp.filter_away(&filt, NULL,  0);
    filt = 0;
    filt[i] = 1;
    named_symcoeff_ineq lb = curr.filter_thru(&filt, NULL,  1);
    if(lb.m()) {
        instruction * ilb = lb.create_expression(index().name(), FALSE, bs);
        blb.set(ilb);
    }
    named_symcoeff_ineq ub = curr.filter_thru(&filt, NULL, -1);
    if(lb.m()) {
        instruction * iub = ub.create_expression(index().name(), TRUE,  bs);
        bub.set(iub);
    }
    
    tree_block * tb = NULL;
    block_symtab * new_bs = bs;
    if((lb.m()==0)&&(ub.m()==0)) {
        tb = new tree_block(new tree_node_list, bs);
        new_bs = tb->symtab();
    }
    
    tree_node_list * ch_code = generate_tree_node_list(bs);
    block body(ch_code);
    
    if((lb.m()==0)&&(ub.m()==0)) {
        tb->set_body(ch_code);
        code.set(tb);
    } else if(tmp.cols()[i].kind() == nte_cond) {
        block cond;
        if(lb.m() && ub.m()) {
            cond.set((blb <= ind)&&(ind <= bub));
        } else if(lb.m()) {
            cond.set(blb <= ind);
        } else if(ub.m()){
            cond.set(ind <= bub);                   
        }
        code.set(block::IF(cond, body));
    } else {
        assert(lb.m() && ub.m());
        code.set(block::FOR(ind, blb, bub, body));
    }

    tree_node * tn = code.make_tree_node(bs);

    return tn;
}


void level_elem::print(int tab, int knd, FILE *fp)
{
    if(knd) for(int t=0; t<tab; t++) fprintf(fp,"   ");
    
    named_symcoeff_ineq tmp(ineq);
    tmp.cleanup();
    
    if(tab >= 0) {
        if(tmp.m() > 0) {
            int i = tmp.cols().find(index());
	    if(i == -1) {
		fprintf(fp,"BUGBUG: %s is not in the system\n", index().string());
	    } else {
		constraint filt(tmp.n());
		filt = 0;
		for(int j=i+1; j<tmp.n(); j++) filt[j] = 1;
		named_symcoeff_ineq curr = tmp.filter_away(&filt, NULL,  0);
		
		filt = 0;
		filt[i] = 1;
		named_symcoeff_ineq lb = curr.filter_thru(&filt, NULL,  1);
		named_symcoeff_ineq ub = curr.filter_thru(&filt, NULL, -1);
		
#ifdef DEBUG_PRINT
		fprintf(fd,"%d: ", uid());
#endif
		if(tmp.cols()[i].kind() == nte_cond) {
		    const char * nms = index().string();
		    if(lb.m() && ub.m()) {
			fprintf(fp,"if ((");
			lb.print_exp(pet_max, i, fp);
			fprintf(fp," <= %s) and (%s <= ", nms, nms);
			ub.print_exp(pet_min, i, fp);
			fprintf(fp,")) then %s\n", (knd==2)?"{":"");
		    } else if(lb.m()) {
			fprintf(fp,"if (");
			lb.print_exp(pet_max, i, fp);
			fprintf(fp," <= %s) then %s\n", nms, (knd==2)?"{":"");
		    } else if(ub.m()){
			fprintf(fp,"if (%s <= ", nms);
			ub.print_exp(pet_min, i, fp);
			fprintf(fp,") then %s\n", (knd==2)?"{":"");
		    } else 
			fprintf(fp,"empty then %s\n", (knd==2)?"{":"");
		} else {
		    fprintf(fp,"for %s = ", index().string());
		    
		    if(lb.m() == 0) 
			fprintf(fp,"-infinity");
		    else 
			lb.print_exp(pet_max, i, fp);
		    
		    fprintf(fp," to ");
		    
		    if(ub.m() == 0) 
			fprintf(fp,"infinity");
		    else 
			ub.print_exp(pet_min, i, fp);
		    
		    fprintf(fp," do %s\n", (knd==2)?"{":"");
		}
	    }
	} else
	    fprintf(fp,"%s is Empty %s\n", index().string(), (knd==2)?"{":"");
    }
    
    
    if(knd == 0) {
        if(ch_vtree) {
            fprintf(fp,"value tree:\n");
            ch_vtree->print(fp);
            
            dbg_count = 0;
//            named_symcoeff_ineq cx;
//            if(ch.head())
//                ch_vtree->debug_print(cx, ch.head()->contents->index());
        }
        fprintf(fp,"\n");
    }
    
    level_elem_list_iter iter1(&ch);
    while(!iter1.is_empty()) {
        level_elem * le = iter1.step();
        le->print(tab+1, knd, fp);
    }
    
    if(tab >= 0)
        if(knd) {
            if(orig_loop.count() > 0) {
                assert(ch.count() == 0);
                for(int t=0; t<tab+1; t++) fprintf(fp,"   ");
                boolean comma = FALSE;
                fprintf(fp,"[");
                marked_nsi_list_iter iter3(&orig_loop);
                while(!iter3.is_empty()) {
                    marked_nsi * mn = iter3.step();
                    fprintf(fp,"%s %d", (comma)?",":"", mn->mark);
                    comma = TRUE;
                }
                fprintf(fp,"]\n");
            }
            
            if(knd == 2) {
                for(int t=0; t<tab; t++) fprintf(fp,"   ");
                fprintf(fp,"}\n");
            }
        }
}





