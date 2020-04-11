/* file "ctree.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * Definition of the "ctree" class.  This class attempts to emulate a
 * C abstract syntax tree.  A ctree node consists of an operator, a
 * type, some random extra data, and possibly some children.
 */


#ifndef CTREE_H
#define CTREE_H

#pragma interface

class ctree;
class ctree_list;
class ctree_list_iter;


/*
 * These are the ctree operators, along with comments about their use
 * and precedence level.
 */

enum ctree_op {
    ctree_funcdef,	/* 16 top-level func; lv.y name, child(0..N-2)
			 * vardecl for params, child(N-1) func body */
    ctree_intconst,	/* 15 i */
    ctree_charconst,	/* 15 i */
    ctree_floatconst,	/* 15 f */
    ctree_enumconst,	/* 15 s */
    ctree_strconst,	/* 15 s */
    ctree_symconst,	/* 15 y */
    ctree_funcdecl,	/* 15 lv.y is symbol */
    ctree_vardecl,	/* 15 lv.y is symbol */
    ctree_vardef,	/* 15 lv.y is symbol */
    ctree_typedecl,	/* 15 lv.t is type */
    ctree_typeforward,	/* 15 lv.t is type */
    ctree_goto,		/* 15 lv.y is symbol */
    ctree_break,	/* 15 */
    ctree_continue,	/* 15 */
    ctree_return,	/* 15 return */
    ctree_label,	/* 15 lv.y is symbol */
    ctree_case,		/* 15 lv.i is case value */
    ctree_default,	/* 15 default label */
    ctree_for,		/* 15 for */
    ctree_if,		/* 15 if */
    ctree_do,		/* 15 do...while */
    ctree_switch,	/* 15 switch */
    ctree_block,	/* 15 nested block */
    ctree_strptr,	/* 14 ->  lv.s is name */
    ctree_strsel,	/* 14 .   lv.s is name */
    ctree_subscr,	/* 14 [] */
    ctree_funcall,	/* 14 () */
    ctree_macro,	/* 14 macro */
    ctree_postinc,	/* 13 ++ */
    ctree_preinc,	/* 13 ++ */
    ctree_postdec,	/* 13 -- */
    ctree_predec,	/* 13 -- */
    ctree_compl,	/* 13 ~ */
    ctree_not,		/* 13 ! */
    ctree_neg,		/* 13 - */
    ctree_addrof,	/* 13 & */
    ctree_deref,	/* 13 * */
    ctree_conv,		/* 13 () */
    ctree_mult,		/* 12 * */
    ctree_div,		/* 12 / */
    ctree_mod,		/* 12 % */
    ctree_add,		/* 11 + */
    ctree_sub,		/* 11 - */
    ctree_lshift,	/* 10 << */
    ctree_rshift,	/* 10 >> */
    ctree_lt,		/* 9 < */
    ctree_lte,		/* 9 <= */
    ctree_gt,		/* 9 > */
    ctree_gte,		/* 9 >= */
    ctree_eq,		/* 8 == */
    ctree_neq,		/* 8 != */
    ctree_bitand,	/* 7 & */
    ctree_bitxor,	/* 6 ^ */
    ctree_bitor,	/* 5 | */
    ctree_logand,	/* 4 && */
    ctree_logor,	/* 3 || */
    ctree_ternary,	/* 2 ?: */
    ctree_assign,	/* 1 = */
    ctree_addassign,	/* 1 += */
    ctree_subassign,	/* 1 -= */
    ctree_comma,	/* 0 , */
    ctree_semi,		/* 0 ; */
    ctree_blank_line,
    ctree_pound_line,
    ctree_type_ref
};


/*
 * This is the table of ctree operators including precedence,
 * commutivity, and assocativity info.
 */

#define ctree_leftassoc 0
#define ctree_rightassoc 1

extern struct _ctree_oper_attr {
    int prec;
    int assoc;
    int commutes;
    const char *sym;
} ctree_oper_attr[];



/*
 * This is the ctree class itself.
 */

DECLARE_DLIST_CLASS(ctree_list, ctree*);

class ctree {
private:
    ctree_op op;
    type_node *type;
    ctree_list *children;
    union {
	struct {
	    boolean is_c_int;
	    union {
		int i;
		const char *str;
	    } u;
	} i;
	struct {
	    boolean is_c_double;
	    union {
		double f;
		const char *str;
	    } u;
	} f;
	const char *s;
	sym_node *y;
	type_node *t;
    } leafval;
    boolean foldable;  /* TRUE iff we're alowed to optimize this away */

    const char **comments;
    int comment_space;

    /* child cache */
    ctree_list_e *cached_child;
    int cached_child_num;
    int cached_size;
    
    void print_with_prec(io_class *, int, int, boolean is_left);
    void print_as_body(io_class *out, int nindent);

    void full_line_output_comments(io_class *out, int nindent);
    void in_line_output_comments(io_class *out, int prefix_space,
                                 int suffix_space);
    void output_a_comment(io_class *out, const char *comment);

    /* Truncate all strings with trailing zeros */
    void truncate_all_zeros(void);

    boolean remainder_is_blanks(int starting_point);
    
    boolean operator==(ctree &c);
    boolean operator!=(ctree &c)	{ return !(*this == c); }

    void operator=(ctree &)		{ assert(FALSE); }

    void swap(ctree *other);
    
public:
    static ctree_op ctree_io_to_op(if_ops);

    ctree(void);
    ctree(ctree_op);
    ctree(ctree_op, boolean is_foldable, type_node *);
    ctree(ctree_op, type_node *, const i_integer &);
    ctree(type_node *, immed float_immed);
    ctree(ctree_op, const char *);
    ctree(ctree_op, const char *, type_node *);
    ctree(ctree_op, sym_node *, type_node * = NULL);
    ctree(ctree_op, type_node *);
    ~ctree();
    
    void flush_child_cache(void)	{ cached_child = NULL;
					  cached_size = 0; }

    int N();
    ctree *child(int n);
    ctree_op getop()			{ return op; }
    type_node *gettype()		{ return type; }
    const char *string_val()		{ return leafval.s; }
    i_integer integer_val() const;
    void set_integer_val(const i_integer &new_value);
    immed float_val() const;
    void set_float_val(immed new_value);
    
    void reset_children()		{ flush_child_cache();
					  delete children;
					  children = new ctree_list; }
    void addchild(ctree *c)		{ flush_child_cache();
					  children->append(c); }
    void replace_child(int child_num, ctree *new_child);
    void add_comment(const char *comment);
    void add_object_comments(suif_object *the_object);
    void copy_comments_from(ctree *other_ctree);

    boolean is_int_const(i_integer *value);
    type_node *expr_type(void);

    /* Flatten commutative operators, copies, and blocks */
    void flatten(boolean in_initializer = FALSE);

    /* Convert initialized data to children of the vardef. */
    void extract_data();
    
    /* Fixup array and ptr expressions */
    void do_basic_folding();

    /* Fold away type casts where possible */
    void fold_converts();

    /* Simplify logical (cond) exps so they look nicer */
    void log_simp();

    /* Simplify conditional exps into ternary ops, etc. */
    void cond_simp();

    /* Truncate strings with trailing zeros in static initializers */
    void truncate_zeros_in_static_inits(void);

    /* Print in list form */
    void print(FILE *, int nindent=0);

    /* Print in C form */
    void print_as_c(io_class *, int nindent=0);

    /* Attempt to fold in division by amount */
    void try_const_div(int amount, boolean *folded);
};

#endif /* CTREE_H */
