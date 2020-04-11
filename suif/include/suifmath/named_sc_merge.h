#pragma interface

#ifndef NAMED_SC_MERGE_H
#define NAMED_SC_MERGE_H

/************************************************************************
 ************************************************************************
 ****                                                                ****
 ****                                                                ****
 ****                                                                ****
 ************************************************************************
 ************************************************************************/

extern const char * k_named_sc_merge_mark_annote;

//#define DEBUG_COUNTER
//#define DEBUG_PRINT

#ifdef DEBUG_PRINT
#define DEBUG_COUNTER
#endif

class level_elem;
class value_tree;
struct marked_nsi;

DECLARE_DLIST_CLASSES(level_elem_list, level_elem_list_e,
		      level_elem_list_iter, level_elem*);

DECLARE_DLIST_CLASSES(marked_nsi_list, marked_nsi_list_e,
		      marked_nsi_list_iter, marked_nsi*);



enum sc_merge_type { smt_none, smt_simple, smt_runtime, smt_full};


/**************************************************************************
 ***                                                                    ***
 *** Merging of multiple loop nests such that the iterations are        ***
 *** executed in the original lexicographical ordering and multiple     ***
 *** nests refering to the same iteration gets executed toghter.        ***
 ***                                                                    ***
 *** The current implementation handles the full merging method         ***
 ***                                                                    ***
 *** The loop nests are provided by a list of named_symcoeff_ineq's.    ***
 *** All the columns of the ineq's are aligned.                        ***
 *** The columns that are not loops (first few) should be designated    ***
 *** using marked_cond().   The columns refering to non-merged loops    ***
 *** (last few) should be designated using mark_excess().               ***
 ***                                                                    ***
 **************************************************************************/
class named_sc_merge {
friend class level_elem;
    level_elem * hdr;                           // root of the loop structure
    static name_table * cnt;                    // align the columns
    static name_table * pnt;                    // align the planes
    marked_nsi_list mn_list;                    // input list
    int cond_num;                               // # of `if' levels (beginning)
    int excess_num;                             // # of not merged (end)
    
public:
    named_sc_merge(named_symcoeff_ineq_list *, sc_merge_type);
    named_sc_merge(marked_nsi_list *, sc_merge_type);
    ~named_sc_merge();

    void mark_cond(int i)                       { cond_num = i; }
    void mark_excess(int i)                     { excess_num = i; }

    void merge();

    tree_node_list * generate_code(base_symtab * bs = NULL);
    void print(int knd=1, FILE *fp=stdout);

    static name_table & names()                 { return *cnt; }
private:
    void init();
    void do_align(marked_nsi_list &);
    void do_merge_full(marked_nsi_list &);
    void do_mark_cond();
    void do_mark_excess();
    void do_delete_empty();
    static void do_mark_cond(name_table & nt, int i);
};



/**************************************************************************
 ***                                                                    ***
 *** Internal representation for a single loop.                         ***
 ***                                                                    ***
 **************************************************************************/
class level_elem {
friend class named_sc_merge;
friend class value_tree;
friend class tree_leaf;
#ifdef DEBUG_COUNTER
    static int le_counter;
    int unum;
#endif
    named_symcoeff_ineq ineq;                   // bounds of the current loop
    level_elem * par;                           // parent loop
    level_elem_list ch;                         // children loops
    value_tree * ch_vtree;                      // tree (to construct children)
    name_table_entry ind;                       // loop index
    marked_nsi_list orig_loop;                  // valid original nests
public:
    level_elem(level_elem * par, 
               name_table_entry & i,
               named_symcoeff_ineq * ineq,
               named_symcoeff_ineq * outer = NULL);
    ~level_elem();

    int uid();

    level_elem * top_elem();
    level_elem * parent()                       { return par; }
    name_table_entry & index()                  { return ind; }


    void merge_children(marked_nsi_list & inlist, 
                        named_symcoeff_ineq & outer_bounds,
                        int pos);
    boolean delete_empty();

    void print(int knd=1, FILE *fp=stdout)       { print(-1, knd, fp); }
    tree_node * generate_tree_node(block_symtab *);
    tree_node_list * generate_tree_node_list(block_symtab *);
private:
    void merge_to_value_tree(named_symcoeff_ineq &, int pos);
    void add_vtree(named_symcoeff_ineq &v);
    void add_orig_mark(marked_nsi_list &);
    void print(int tab, int knd, FILE *fp=stdout);
};


/**************************************************************************
 ***                                                                    ***
 *** The tree structure that builds all the combinations of loop nests  ***
 ***                                                                    ***
 **************************************************************************/
class value_tree {
    value_tree * tp;                            // Branch when cond is true
    value_tree * fp;                            // Branch when cond is false
    value_tree * pp;                            // Parent
    named_symcoeff_ineq val;                    // Condition
protected:
    value_tree(value_tree * par);
public:
    value_tree(value_tree * par, named_symcoeff_ineq & v);
    virtual ~value_tree();
    
    value_tree * true_branch()                  { return tp; }
    value_tree * false_branch()                 { return fp; }
    value_tree * parent()                       { return pp; }
    
    boolean is_true_path();
    virtual boolean is_leaf();
    named_symcoeff_ineq & value()               { return val; }

    value_tree * duplicate();
    void insert_tree(named_symcoeff_ineq &);
    void insert_leaves();
    void number_leaves()                        { number_leaves(0); }
    virtual void mark_leaves(marked_nsi & c, named_symcoeff_ineq * bound);
    virtual void create_levels(level_elem * par, 
                               named_symcoeff_ineq * bound, 
                               named_symcoeff_ineq * prev_bound, 
                               name_table_entry & nte);


    void print(FILE *fp=stdout)                 { print(0,fp); }
    void debug_print(named_symcoeff_ineq &, name_table_entry & nte);
private:
    void print(int level, FILE *fp=stdout);
    void insert_tree(named_symcoeff_ineq & val, named_symcoeff_ineq & curr);
    int number_leaves(int);
};



/**************************************************************************
 ***                                                                    ***
 *** 
 ***                                                                    ***
 **************************************************************************/
class tree_leaf : public value_tree {
friend class value_tree;
    int num;                                    // Unique ID
    marked_nsi_list valid_list;                 // Valid original nests
public:
    tree_leaf(value_tree * par);
    ~tree_leaf();

    boolean is_leaf();

    void mark_leaves(marked_nsi & c, named_symcoeff_ineq * bound);
    void create_levels(level_elem * par, 
                       named_symcoeff_ineq  * bound,
                       named_symcoeff_ineq * prev_bound, 
                       name_table_entry & nte);
};
    


struct marked_nsi {
    named_symcoeff_ineq nsi;
    int mark;

    marked_nsi(named_symcoeff_ineq & i, int m) : nsi(i), mark(m)        { }
    marked_nsi(int m) : mark(m)                                         { }
    marked_nsi(marked_nsi & mn) : nsi(mn.nsi), mark(mn.mark)            { }
    marked_nsi(marked_nsi * mn) : nsi(mn->nsi), mark(mn->mark)          { }
};

#endif


