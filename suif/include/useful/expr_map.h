/* file "expr_map.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  Header for expr_map class: expression mapping for libuseful, the
 *  SUIF library of miscellaneous useful routines
 */

#ifndef EXPR_MAP_H
#define EXPR_MAP_H

#ifndef BASIC_H
#error "<useful.h> must be included before this point"
#endif


/*
 *  The expr_map class provides a general way of representing certain
 *  kinds of maps between expressions.  Logically it consists of a
 *  finite number of ordered pairs of expressions.  Application of the
 *  map to an expression consists of first looking to see if the
 *  expression is exactly the same as any of the left-hand sides of
 *  the expression pairs.  If it matches, the result of the mapping is
 *  the right-hand side of one of the matching ordered pairs
 *  (left-hand sides are expected to be unique, so if they are not and
 *  the expression matches more than one, it is undefined which of the
 *  right-hand sides is the result).  If there is no match, and the
 *  source expression is a symbol, the result is the same expression
 *  unchanged.  If the source expression is an instruction tree, the
 *  result is a duplicate of the base instruction with the expr_map
 *  applied recursively to its sub-expressions, if any.
 *
 *          Methods to change the expr_map
 *
 *  void init(expr_map *other_map)
 *  void init(expr_map &other_map)
 *      Initialize this expr_map to be identical to the other_map.
 *
 *  void clear(void)
 *      Clear this expr_map to the identity map.
 *
 *  void add(operand source_op, operand target_op)
 *      Add the ordered pair (source_op, target_op) to the list of
 *      logical ordered pairs in this expr_map.  Hence after this
 *      method has been executed, applying the mapping to source_op
 *      would give target_op.  If either expression is an expression
 *      tree, it is duplicated and stored by this class, and the
 *      original is left unchanged.
 *
 *  void kill_source(operand source_op)
 *      Remove an ordered pair with an expression matching source_op
 *      as the left-hand side.  If no left-hand side matches, there is
 *      no effect.  If more than one matches, it is undefined which is
 *      removed from the expr_map.
 *
 *  void kill_target(operand target_op)
 *      Remove an ordered pair with an expression matching source_op
 *      as the right-hand side.  As with kill_source, no match implies
 *      no effect and multiple matches leave it undefined which will
 *      be removed.
 *
 *  void invert(void)
 *      Replace this expr_map with its inverse; that is, reverse all
 *      the ordered pairs.  If this is called, it is assumed that the
 *      right-hand sides are unique; otherwise, the result has
 *      non-unique left-hand sides and hence its result is not always
 *      well-defined.
 *
 *          Methods to use the expr_map
 *
 *  operand map(operand source_op)
 *  operand map(var_sym *source_var)
 *      Apply the map to the given expression.  If the result is an
 *      expression tree, it is a newly created one that can be changed
 *      and de-allocated as appropriate, even if the map returns an
 *      expression equivalent to the source expression.  That is,
 *      applying the identity expr_map is equivalent to cloning the
 *      expression.
 *
 *  operand inverse_map(operand source_op)
 *  operand inverse_map(var_sym *source_var)
 *      Apply the inverse of this expr_map.  The resulting expression
 *      is what would have been returned by calling invert() and then
 *      inverse_map() (but the expr_map itself is unchanged).
 *
 *  expr_map *compose(expr_map *first_map)
 *      Return a pointer to a new expr_map, allocated with ``new'',
 *      that is the composition map of the first_map with this map.
 *      That is, this_map->compose(that_map)->map(op) should give the
 *      same expression as this_map->map(that_map->map(op)).
 *
 *  operand_list *inverse_image(operand target_op)
 *      Return the inverse image of a given expression.  That is,
 *      return a list containing all of the expressions that would
 *      give target_op if this expr_map were applied to them.
 *      Anything that would give an operand equivalent to target_op
 *      must have started out equivalent to something on the list.
 *      For each operand on the resulting list, if it is an expression
 *      tree, it will be newly allocated so that it can be changed or
 *      deallocated as appropriate.  The result list itself is also
 *      allocated this way.
 *
 *          Methods to access the pairs
 *
 *  For the purpose of allowing other code to look at the contents of
 *  an expr_map, the pairs are ordered and numbered, and can be
 *  examined by number.  This ordering and numbering does not affect
 *  how the expr_map is applied and can be ignored if the contents are
 *  not to be examined.
 *
 *  Pairs are ordered by the times they were added.  If one is
 *  removed, the ordering of the rest is unaffected.  This ordering
 *  defines the numbering of pairs: the first pair at any time is
 *  number zero and each other pair has a number one greater than the
 *  previous pair.  Hence removing a given pair leaves the numbers of
 *  preceeding pairs unaffected and decrements by one the numbers of
 *  all succeeding pairs.
 *
 *  unsigned num_pairs(void)
 *      Return the number of pairs.
 *
 *  void get_pair(unsigned pair_num, operand *left, operand *right)
 *      Set *left to the left-hand side of pair number pair_num, and
 *      set *right to that pair's right-hand side.  If either or both
 *      expressions are expression trees of instructions, the
 *      expressions put in *left and *right are newly allocated
 *      expression trees and hence can be changed or deallocated as
 *      appropriate without affecting the expr_map or anything else.
 *
 *  void print(FILE *fp)
 *      Print out the map in some form to the given file.  This is
 *      intended for development and debugging of code that uses
 *      expr_maps.
 *
 */

class expr_map
  {
private:
    operand_dlist left_list;
    operand_dlist right_list;

    operand_dlist_e *left_cache;
    operand_dlist_e *right_cache;
    unsigned cache_num;
    unsigned pair_count;

    void kill_cached(void);
    void cache_match(operand to_match, boolean use_left);
    operand map_internal(operand source_op, boolean use_left);

public:
    expr_map(void);
    ~expr_map(void);

    void init(expr_map *other_map) { init(*other_map); }
    void init(expr_map &other_map);
    void clear(void);
    void add(operand source_op, operand target_op);
    void kill_source(operand source_op);
    void kill_target(operand target_op);
    void invert(void);

    operand map(operand source_op);
    operand map(var_sym *source_var) { return map(operand(source_var)); }
    operand inverse_map(operand source_op);
    operand inverse_map(var_sym *source_var)
      {
        return inverse_map(operand(source_var));
      }
    expr_map *compose(expr_map *first_map);
    operand_list *inverse_image(operand target_op);

    unsigned num_pairs(void) { return pair_count; }
    void get_pair(unsigned pair_num, operand *left, operand *right);

    void print(FILE *fp = stdout);
  };

#endif /* EXPR_MAP_H */
