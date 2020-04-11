/* file "basic.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Header for SUIF library of miscellaneous useful routines */

#ifndef BASIC_H
#define BASIC_H

#include <suif1.h>

/*----------------------------------------------------------------------*
    Beginning of Initialization Routines
 *----------------------------------------------------------------------*/

/*
 *  Initialize this library.  This must be called before anything else
 *  in this library is used, but usually it's called automatically by
 *  start_suif() if necessary, so the user needn't think about
 *  it.
 */
extern void init_useful(int &argc, char *argv[]);

/*
 *  Cleanup after the last use of this library.  This is called before
 *  the program exits to do cleanup such as de-allocating
 *  datastructures.
 */
extern void exit_useful(void);

/*----------------------------------------------------------------------*
    End of Initialization Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Annotation Names
 *----------------------------------------------------------------------*/

/*
 *  These are all registered annotation names.
 */

/*
 *  This annotation on a proc_sym asserts that the given procedure has
 *  no side-effects and if it accesses memory at all, it only does so
 *  directly through parameters to the function that have pointer
 *  type.  Since it has no side-effects, the function may only read
 *  from memory in any case.
 */
EXPORTED_BY_USEFUL const char *k_pure_function;

/*
 *  This annotation on an io_mrk instruction asserts that all the
 *  var_syms in the data list of the immed are ``dead'' at that point
 *  -- that, is that when this point in the code is reached
 *  dynamically, there will be no further dynamic reads of that
 *  variable that depend on the current value.  So, for example, the
 *  value could be set to anything at this point and it would not
 *  affect anything else.
 */
EXPORTED_BY_USEFUL const char *k_dead;

/*
 *  This annotation on a var_sym asserts that either its address is
 *  not taken at all, or everywhere it is taken, it is used directly
 *  in the same expression tree in the computation of an address used
 *  to access memory.  In the case of sub-variables, it further
 *  asserts that the same property applies recursively to its parent
 *  variable, if any, and all sub-variables.  That guarantees that the
 *  address cannot be stored in a pointer variable of any kind, and
 *  neither can anything that could be used to generate the address be
 *  stored.  This guarantees that if a memory address expression is
 *  found that does not include an ldc of this variable, or something
 *  that has the same root parent in the case of sub-variables, that
 *  memory address cannot point to any part of that variable.  More
 *  precisely, it is guaranteed to be undefined whether or not the
 *  address can point to any part of that variable (since it is
 *  undefined where in memory the compiler keeps a variable), so any
 *  program that can tell whether or not it does point to the variable
 *  has undefined behavior.  Hence it is safe for the compiler to
 *  assume that it does not point to the variable.
 */
EXPORTED_BY_USEFUL const char *k_addr_not_stored;

/*
 *  This annotation on a tree_for asserts that if that for loop is
 *  executed, it's body will be executed at least once -- that is, the
 *  lower bound is guaranteed to meet the loop-terminating test
 *  condition.  This implies that the landing_pad is always executed.
 *  This annotation is typically put on a tree_for around which a
 *  guarding tree_if has been placed to test the bounds of the loop.
 */
EXPORTED_BY_USEFUL const char *k_guarded;

/*
 *  These two annotations are used to communicate with the rescope
 *  pass.  They are to be put on symbols or types in file or
 *  inter-file global symbol tables.  See the man page for rescope for
 *  details.
 */
EXPORTED_BY_USEFUL const char *k_globalize;
EXPORTED_BY_USEFUL const char *k_filize;

/*
 *  This annotation is used to communicate with the do_replacement()
 *  function; see the comments for that function for more details.
 */
EXPORTED_BY_USEFUL const char *k_replacement;

/*
 *  This annotation is used to communicate with the fortsplit pass.
 *  It is used to indicate why a procedure is not eligible to be
 *  converted to Fortran.  See the man page for fortsplit for details.
 */
EXPORTED_BY_USEFUL const char *k_not_fortran_reason;

/*
 *  This annotation is put on a tree_proc to indicate that there is no
 *  possible aliasing in this procedures between parameters that are
 *  pointers and each other or global variables that are accessed in
 *  this procedure.  Fortran 77 specifies that this is exactly the
 *  assumption that can be made about Fortran subroutines.
 */
EXPORTED_BY_USEFUL const char *k_no_parameter_aliasing;

/*
 *  This annotation is put on a proc_sym to indicate that the address
 *  of the symbol is used only in the currect fileset and that it is
 *  used only directly in call instructions, never stored into a
 *  pointer for a possible indirect call.  This annotation is only
 *  valid for symbols for which unreferenced_outside_fileset() returns
 *  TRUE.
 */
EXPORTED_BY_USEFUL const char *k_direct_calls_only;

/*
 *  This annotation is put on a proc_sym to indicate that the
 *  procedure implements a particular Fortran intrinsic function.  The
 *  data should contain a single string immed which is the name of the
 *  intrinsic in Fortran.
 */
EXPORTED_BY_USEFUL const char *k_fortran_intrinsic;

/*
 *  This annotation is a general-purpose marker annotation.  It is
 *  used to show which of a set of objects are interesting for some
 *  circumstance.  Having a general annotation name for such things
 *  eliminates the need to come up with different annotation names and
 *  allows different kinds of things to be done with the same sub-set
 *  of objects without adding multiple annotations.  It also requires,
 *  however, that the k_fred annotations be removed before something
 *  else is run that might use annotations with this name for a
 *  different purpose.
 */
EXPORTED_BY_USEFUL const char *k_fred;

/*
 *  This annotation is used to communicate with the fortsplit pass.
 *  It lists procedures that are called by a given procedure which
 *  prevent convertion to Fortran in an unfixable way.  See the man
 *  page for fortsplit for details.
 */
EXPORTED_BY_USEFUL const char *k_fortran_unfixables_called;

/*
 *  This annotation is put on proc_syms to assert that the procedure
 *  in question is not ever called recursively, either directly or
 *  through any chain of other procedure calls.
 */
EXPORTED_BY_USEFUL const char *k_no_recursion;

/*
 *  This annotation is put on a proc_sym to indicate that the
 *  procedure implements the Fortran ``**'' exponentiation operator
 *  for some combination of operator types.
 */
EXPORTED_BY_USEFUL const char *k_fortran_power_op;

/*
 *  This annotation is put on a symbol table.  It indicates that the
 *  variables listed in its data list should be combined to form a
 *  single array variable.  This annotation should be placed on the
 *  symbol table in which the variables reside.  Any number of
 *  variables may be in the list, but all must have exactly the same
 *  types.  The first variable in the list will become element 0 in
 *  the outer dimension, the second will become element 1 and so on.
 *  All variables in a single k_form_array annotation should be in the
 *  same symbol table and none should be sub-variables or parameters.
 *  They must all be either static or automatic; no mixing of
 *  automatic and static is allowed.
 */
EXPORTED_BY_USEFUL const char *k_form_array;

/*
 *  This annotation is put on a var_sym to indicate that it is never
 *  assigned, either directly or indirectly, so it's staticly
 *  initialized value remains its value through the whole execution of
 *  the program.
 */
extern const char *k_is_constant;

/*
 *  This annotation is put on a tree_for node to indicate that there
 *  is no use of the break label of the tree_for as target of any kind
 *  of branch or jump.  This guarantees that after the loop ends, it
 *  will have exited only because the ub_op() has been passed.
 */
extern const char *k_no_break;

/*
 *  This annotation is put on a tree_for node to indicate that there
 *  is a use of the break label of the tree_for as target of some sort
 *  of branch or jump within the tree_for body.  Note that for every
 *  tree_for node, either k_no_break or k_potential_break applies, but
 *  never can both apply, and a local analysis of the loop body is all
 *  that is needed to determine which applies.
 */
extern const char *k_potential_break;

/*----------------------------------------------------------------------*
    End of Annotations Names
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Miscellaneous Type Routines
 *----------------------------------------------------------------------*/

/*
 *  Return the least offset greater than or equal to the given offset
 *  which also meets the alignment requirement of the given type.
 *  This is typically used to calculate the offset to use for the
 *  next field of a structure.
 */
extern int align_up(int offset, type_node *the_type);

/*
 *  Return the alignment requirement in bits of the given type.  It
 *  uses the information about the target machine from the main SUIF
 *  library's global ``target'' structure.  If the result is zero,
 *  there is no alignment restriction for that type.
 */
extern int get_alignment(type_node *the_type);

/*
 *  Return the C type corresponding to the given integral or
 *  enumerated type.  If the given type doesn't correspond to any
 *  integral type on the target machine, num_C_types is returned.
 */
extern enum C_types c_int_type(type_node *the_type);

/*
 *  Return the C type corresponding to the given floating point type.
 *  If the given type doesn't correspond to any floating point type on
 *  the target machine, num_C_types is returned.
 */
extern enum C_types c_float_type(type_node *the_type);

/*
 *  Return the installed SUIF type corresponding to the given C type.
 */
extern base_type *c_type_to_suif(enum C_types c_type,
                                 boolean is_signed = TRUE);

/*
 *  Return TRUE for arithmetic types that cannot represent any
 *  negative numbers (i.e. pointers and unsigned integers), FALSE for
 *  everything else.
 */
extern boolean non_negative(type_node *the_type);

/*
 *  Return an operand that evaluates to the given array bound.
 */
extern operand operand_from_array_bound(array_bound the_bound);

/*
 *  Return TRUE iff the given type is volatile or any of its component
 *  types are volatile.  Note that component types are only those such
 *  that an object of that type contains an object of the component
 *  type.  Referenced types are _not_ included.  For example, an array
 *  with volatile elements or structure with a volatile field would
 *  return TRUE, put a pointer to a volatile type would not.  Hence it
 *  returns TRUE for those types for which a load of an object of that
 *  type would access something volatile.
 */
extern boolean any_part_volatile(type_node *the_type);

/*
 *  If it is possible to convert legally from an expression of type
 *  ``source'' to one of type ``target'' with a series of io_cvt
 *  instructions, a list is returned representing one legal series of
 *  such io_cvt instructions.  Otherwise, NULL is returned.  If a list
 *  of types is returned, it will be as short as any legal list for
 *  the given types and lose no more information in the conversion
 *  than necessary.  The conversions will also preserve the ANSI C
 *  semantics for converting between the two types, if it is legal to
 *  cast from ``source'' to ``target'' in ANSI C.  The type ``target''
 *  will always be the last type on the list, if a list is returned.
 *  Any intermediate types on the list will have global scope.  The
 *  list returned will be newly allocated and the caller should see to
 *  it that it is properly deallocated when appropriate.
 */
extern type_node_list *cast_sequence(type_node *source_type,
                                     type_node *target_type);

/*
 *  Reorder the fields of the given structure in increasing order of
 *  offset.  Fields with the same offset maintain their relative
 *  ordering.
 */
extern void sort_fields_by_offset(struct_type *the_struct);

/*
 *  Return TRUE iff it is ok to use ``double'' floating point
 *  arithmetic (the arithmetic on the compiling machine) for the given
 *  floating point type.
 */
extern boolean native_floating_arithmetic_ok(base_type *the_type);

/*
 *  Return the type that would normally be returned by taking the
 *  difference between two objects of type original_type.  It is
 *  illegal to call this function if it is not legal to subtract two
 *  objects of type original_type.
 */
extern type_node *diff_type(type_node *original_type);

/*
 *  Given two types, each of which is an integer, floating-point, or
 *  enumerated type, this function returns a type that would be the
 *  most natural to use as a common type for arithmetic combining
 *  them.  If one is floating-point and the other is not, the result
 *  is the unqualified version of the floating-point type.  If both
 *  are floating-point, the unqualified version of the one with the
 *  larger size, or simply the unqualified version of type1 if the
 *  sizes are equal, is returned.  Otherwise, both are integer or
 *  enumerated types.  If either both are signed or both are unsigned,
 *  an integer type is returned that is either signed or unsigned
 *  according to whether the two types are with size equal to that of
 *  the larger of the two.  Otherwise one is signed and the other is
 *  unsigned.  In that case, if one of the sizes of C integer types on
 *  the target machine is greater than the sizes of both types, a
 *  signed type of the least such size is returned.  Otherwise, an
 *  unsigned type is returned of size the larger of the sizes of the
 *  two types.
 */
extern type_node *cast_up(type_node *type1, type_node *type2);

/*----------------------------------------------------------------------*
    End of Miscellaneous Type Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Instruction/Operand Analysis Routines
 *----------------------------------------------------------------------*/

DECLARE_DLIST_CLASS(operand_dlist, operand);

/*
 *  Return TRUE iff the expression tree specified by the given
 *  instruction or operand respectively contains any direct use of the
 *  given variable.  Note that this only includes uses of the
 *  variable, or one which overlaps it in the case of sub-variables,
 *  directly as operands, not loads through its address in memory; in
 *  general we cannot know whether a memory operation uses a
 *  particular location.  For a conservative check that a variable
 *  cannot possibly be referenced, use instr_may_reference_var() or
 *  operand_may_reference_var().
 */
extern boolean instr_references_var(instruction *the_instr, var_sym *the_var);
extern boolean operand_references_var(operand the_operand, var_sym *the_var);

/*
 *  Return FALSE if the expression tree specified by the given
 *  instruction or operand respectively cannot under any circumstances
 *  reference any part of the given variable; otherwise return TRUE.
 *  If the address of the_var has been taken and the expression
 *  contains a load that cannot be determined to be based on another
 *  symbol, the result will be TRUE.  If the address has been taken
 *  and there is a function call that does not have a k_pure_function
 *  annotation or that takes a pointer as an argument which is not
 *  known to be based on some other variable, the result will also be
 *  TRUE.
 */
extern boolean instr_may_reference_var(instruction *the_instr,
                                       var_sym *the_var);
extern boolean operand_may_reference_var(operand the_operand,
                                         var_sym *the_var);

/*
 *  This is the same as instr_may_reference_var(), except that it
 *  applies to an arbitrary node or list of nodes, respectively, not
 *  just a single expression tree.
 */
extern boolean node_may_reference_var(tree_node *the_node, var_sym *the_var);
extern boolean node_list_may_reference_var(tree_node_list *node_list,
                                           var_sym *the_var);

/*
 *  Return FALSE if the expression tree specified by the given
 *  instruction or operand respectively cannot under any circumstances
 *  reference any part of the location pointed to by ``address'';
 *  otherwise, return TRUE.  This will only do very simple array
 *  dependence analysis.
 */
extern boolean instr_may_reference_location(instruction *the_instr,
                                            operand address);
extern boolean operand_may_reference_location(operand the_operand,
                                              operand address);

/*
 *  This is the same as instr_may_reference_location(), except that it
 *  applies to an arbitrary node or list of nodes, respectively, not
 *  just a single expression tree.
 */
extern boolean node_may_reference_location(tree_node *the_node,
                                           operand address);
extern boolean node_list_may_reference_location(tree_node_list *node_list,
                                                operand address);

/*
 *  Return TRUE iff the expression explicitly uses as an operand any
 *  variable with its address taken flag set.  Note that loads and
 *  stores directly through memory are not considered, whether or not
 *  they might access memory used for variables with their address
 *  taken.  Operations done by called procedures for expressions
 *  containing calls are also not considered by these functions.
 */
extern boolean instr_reads_addressed_var(instruction *the_instr);
extern boolean operand_reads_addressed_var(operand the_operand);

/*
 *  Return TRUE iff the given expression tree explicitly uses a load
 *  instruction.  Note that operations done by called procedures for
 *  expressions containing calls are not considered by these
 *  functions.
 */
extern boolean instr_contains_load(instruction *the_instr);
extern boolean operand_contains_load(operand the_operand);

/*
 *  Return TRUE iff the given expression tree might read any global
 *  variable.
 */
extern boolean instr_may_read_global(instruction *the_instr);
extern boolean operand_may_read_global(operand the_operand);

/*
 *  Return TRUE iff the given expression tree might read any
 *  statically allocated variable (global or statically allocated
 *  local).
 */
extern boolean instr_may_read_statically_allocated_var(instruction *the_instr);
extern boolean operand_may_read_statically_allocated_var(operand the_operand);

/*
 *  Return TRUE iff the given expression tree directly references a
 *  symbol in the given scope.  This includes only operands that are
 *  symbols and ldc instructions with immeds that are symbols.  Types
 *  are not considered at all.
 */
extern boolean instr_uses_scope(instruction *the_instr, base_symtab *scope);
extern boolean operand_uses_scope(operand the_operand, base_symtab *scope);

/*
 *  Given an expression tree for an address specified by the given
 *  instruction or operand respectively, if the address is formed by
 *  the address of some known symbol plus or minus some known or
 *  unknown offset, return that symbol, otherwise return NULL.  If the
 *  address of a sub-variable is used, the symbol returned will be the
 *  root symbol for that sub-variable.  Hence if a non-NULL value is
 *  returned, the address is guaranteed to point either to some part
 *  of the given symbol or to undefined memory, not to another symbol
 *  that is not a sub-variable of that symbol.
 */
extern sym_node *instr_address_root_symbol(instruction *the_instr);
extern sym_node *operand_address_root_symbol(operand the_operand);

/*
 *  Return TRUE only if the two expressions are structurally
 *  equivalent.  If so, then evaluating one is equivalent to
 *  evaluating the other, both in terms of the result value and side
 *  effects.
 */
extern boolean operands_are_same_expr(operand op_1, operand op_2);
extern boolean instrs_are_same_expr(instruction *instr_1,
                                    instruction *instr_2);

/*
 *  Return TRUE iff the given expression tree has no side effects and
 *  evaluating it a second time is guaranteed to give the same result
 *  as evaluating it the first time.
 */
extern boolean instr_reevaluation_ok(instruction *the_instr);
extern boolean operand_reevaluation_ok(operand the_operand);

/*
 *  Return TRUE iff the given expression tree contains a call to a
 *  function that might be impure, as determined by
 *  instr_is_impure_call().
 */
extern boolean instr_contains_impure_call(instruction *the_instr);
extern boolean operand_contains_impure_call(operand the_operand);

/*
 *  Return TRUE iff the given instruction is a call to a function that
 *  might be impure.  ``Impure'' in this sense means that it is not a
 *  pure function of its arguments, i.e. that it has side effects or
 *  reads from a global.  The only functions that are assumed to be
 *  pure are those with k_pure_function annotations.
 */
extern boolean instr_is_impure_call(instruction *the_instr);

/*
 *  Return the symbol for the procedure being called, if it can be
 *  determined, otherwise return NULL.
 */
extern proc_sym *proc_for_call(in_cal *the_call);

/*
 *  Given a symbolic address, return a symbolic address that is to the
 *  same location but without use of sub-variables.  If the symbol
 *  isn't a variable symbol there is no effect, and the same sym_addr
 *  is returned.
 */
extern sym_addr root_address(sym_addr old_addr);

/*
 *  Return TRUE iff this function can tell that the objects pointed
 *  to by the two addresses cannot possibly overlap.  Both addresses
 *  must have pointer type.
 */
extern boolean locations_dont_overlap(operand addr1, operand addr2);

/*
 *  Return an integer such that the two addresses are guaranteed to be
 *  at least that many bits different.  Zero is returned if no better
 *  information can be found.  This function will do simple array
 *  analysis to figure out when the two addresses must point to
 *  different parts of the same array.  Both addresses must have
 *  pointer type.
 */
extern int min_addr_separation(operand addr1, operand addr2);

/*
 *  Return TRUE iff this function can figure out that the two operands
 *  give results that always differ by exactly a constant amount.  If
 *  this is the case, that constant amount is put in *diff.
 */
extern boolean constant_difference(operand op1, operand op2,
                                   immed *diff);

/*
 *  Return TRUE iff this function can tell that the operand is exactly
 *  divisible by the divisor.
 */
extern boolean op_divisible_by(operand the_op, i_integer divisor);

/*
 *  Return the coefficient of the_var in the_op assuming that the_op
 *  is a linear expression in the_var.  It is an error to call this
 *  function where the_op is not a linear expression of the_var.  The
 *  original operand, the_op, is left unchanged and a newly allocated
 *  expression tree is returned.
 */
extern operand coefficient(operand the_op, var_sym *the_var);

/*
 *  Return an operand with the given constant value and type.
 */
extern operand const_op(immed the_const, type_node *the_type);

/*
 *  Return an operand with constant value of the address of a symbol.
 *  If the_sym is a variable or procedure, the operand type will be a
 *  pointer to the type of the variable or procedure; if the_sym is a
 *  label symbol, the operand type will be pointer to void.
 */
extern operand addr_op(sym_node *the_sym);

/*
 *  Return FALSE iff the address of the_var cannot be stored -- either
 *  the address is never taken, or every time it is taken it is just
 *  used directly in a memory operation.  This function simply reads
 *  this information from the is_addr_taken() flag and the presence or
 *  absence of the k_addr_not_stored annotation.
 */
extern boolean addr_might_be_stored(var_sym *the_var);

/*
 *  If the_op is an lvalue, return the address of that lvalue,
 *  otherwise return a null operand.  That is, if the_op is a symbol,
 *  return an expression for the address of the symbol; if the_op is
 *  an expression that is a load instruction, return a clone of the
 *  address operand of the load; and in all other cases return a null
 *  operand.
 */
extern operand get_address(operand the_op);

/*----------------------------------------------------------------------*
    End of Instruction/Operand Analysis Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of tree_node Analysis Routines
 *----------------------------------------------------------------------*/

/*
 *  Return TRUE iff executing the node list has no effect at all and
 *  control flow falls out of the bottom of the list.
 */
extern boolean no_effects(tree_node_list *node_list);

/*
 *  If the node list contains only a single node that can possibly
 *  have any effect, return a pointer to that node; otherwise, return
 *  NULL.
 */
extern tree_node *single_effect(tree_node_list *node_list);

/*
 *  Return an expression that computes the final value of the index
 *  variable, assuming the_for executes at least one iteration and
 *  that no jumps to the break label or return statements are
 *  executed.  The value computed will be the value immediately after
 *  the tree_for is done executing.  The resulting value is undefined
 *  for any situations under which the_for will not execute any
 *  iterations of the body at all, or if there is no bound on the
 *  number of iterations assuming no early exits (i.e. if the step
 *  size is zero).  For example, a step size of zero can mean division
 *  by zero in this computation.
 */
extern operand final_index_value(tree_for *the_for);

/*
 *  Return an expression that computes the total number of iterations
 *  executed by the_for, assuming that the_for executes at least one
 *  iteration and that there is no early exit (i.e. no jumps to the
 *  break label of the_for and no return statements are executed).  If
 *  the_for doesn't execute any iterations, or if there is no bound on
 *  the number of iterations assuming no early exit, the result is
 *  undefined.  For example, a step size of zero can result in a
 *  division by zero in this computation.
 */
extern operand iteration_count(tree_for *the_for);

/*
 *  This function returns TRUE iff there is a use of the break label
 *  of the_for as the target of a branch or jump somewhere within the
 *  body of the_for.  If there is either a k_no_break or
 *  k_potential_break annotation on the_for, the information from that
 *  annotation will be used.  Otherwise, the loop body will be
 *  analyzed and either a k_no_break or k_potential_break annotatin
 *  will be added to the_for as appropriate before the result is
 *  returned.
 */
extern boolean potential_break(tree_for *the_for);

/*----------------------------------------------------------------------*
    End of tree_node Analysis Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Symbol Table Related Analysis Routines
 *----------------------------------------------------------------------*/

/*
 *  The following return TRUE iff the specified things are OK for a
 *  scope -- that is, every referenced symbol table object is in a
 *  symbol table that is visible in the given scope.  The first
 *  function deals with all the annotations on an object and
 *  everything those annotations contain, the second concerns an
 *  operand and everything it contains, and the third concerns an
 *  instruction and everything it contains.
 */
extern boolean annotes_scope_ok(suif_object *the_object, base_symtab *scope);
extern boolean op_scope_ok(operand the_op, base_symtab *scope);
extern boolean instr_scope_ok(instruction *the_instr, base_symtab *scope);

/*
 *  The following returns a name that is as close as possible to
 *  original_name without conflicting with the name of any symbol of
 *  kind() the_kind in the_symtab.  If original_name doesn't work, the
 *  result is original_name concatenated with the decimal
 *  representation of the first non-negative integer for which the
 *  concatenation gives an acceptable name.  If the result is not
 *  original_name, it will be a string installed in the lexicon.
 */
extern const char *deconflict_sym_name(const char *original_name, base_symtab *the_symtab,
                                 sym_kinds the_kind);

/*----------------------------------------------------------------------*
    End of Symbol Table Related Analysis Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Constant Folding and Evaluation Routines
 *----------------------------------------------------------------------*/

/*
 *  The following two functions convert i_integers to immeds and
 *  immeds to i_integers.  In the latter case, the immed must be
 *  either a basic integer or extended integer immed (is_int_const()
 *  must return TRUE).
 */
extern immed ii_to_immed(const i_integer &the_ii);
extern i_integer immed_to_ii(const immed &the_immed);

/*
 *  This is set to TRUE whenever one of the constant folding functions
 *  makes a change to the code, to allow the library user to tell when
 *  something has been changed.  Note that the library never resets it
 *  to FALSE; the user must do that, if desired.  That way the user
 *  can run a bunch of folding operations and then check the variable
 *  once to see if any of them had any effect.
 */
EXPORTED_BY_USEFUL boolean fold_had_effect;

/*
 *  This is set to TRUE whenever one of the constant folding functions
 *  finds an overflow in a constant expression it is simplifying.  As
 *  with the fold_had_effect flag, the library never resets it to
 *  FALSE.
 */
EXPORTED_BY_USEFUL boolean overflow_in_folding;

/*
 *  If this flag is set, all folding will leave array reference
 *  instructions alone.  It defaults to being set and this library
 *  never changes it, so this is a way for the user program to specify
 *  to this library whether or not to preserve array reference
 *  instructions.  This is often desirable behavior because many
 *  passes want to see the program preserving the high-level array
 *  access paterns.  The standard dependence analysis library, for
 *  example, can only handle memory references with array reference
 *  instructions.
 */
EXPORTED_BY_USEFUL boolean suppress_array_folding;

/*
 *  This enumerated type is used to reflect the result status of an
 *  attempted evaluation of an expression to a constant.
 */
typedef enum
  {
    EVAL_OK, EVAL_OVERFLOW, EVAL_NOT_CONST, EVAL_DIV_BY_ZERO, EVAL_UNDEFINED,
    EVAL_UNKNOWN_AT_LINK, EVAL_ERROR
  } eval_status;

/*
 *  Attempt to evaluate the given operand as a constant expression.
 *  The return value reflects the success of the operation and if it
 *  succeeds, the resulting immed value is put in the location pointed
 *  to by ``result''.
 */
extern eval_status evaluate_const_expr(operand the_operand, immed *result);

/*
 *  Attempt to evaluate the result of the given instruction.  The
 *  return value reflects the success of the operation and if it
 *  succeeds, the resulting immed value is put in the location pointed
 *  to by ``result''.
 */
extern eval_status evaluate_const_instr(instruction *the_instr, immed *result);

/*
 *  Attempt to evaluate the given operand as a constant integer
 *  expression.  The return value reflects the success of the
 *  operation and if it succeeds, the resulting integer value is put
 *  in the location pointed to by ``result''.
 */
extern eval_status evaluate_const_int_expr(operand the_operand, int *result);

/*
 *  Attempt to evaluate the result of the given instruction as an
 *  integer constant.  The return value reflects the success of the
 *  operation and if it succeeds, the resulting integer value is put
 *  in the location pointed to by ``result''.
 */
extern eval_status evaluate_const_int_instr(instruction *the_instr,
                                            int *result);

/*
 *  Try to fit the given immediate value into the given type.  If it
 *  doesn't fit, the value is changed to fit and EVAL_OVERFLOW is
 *  returned.  Note that since we only know the target representation
 *  of integers, this only checks and changes integers; floating point
 *  values are always assumed to fit.
 */
extern eval_status fit_immed(immed *value, type_node *the_type);

/*
 *  Return TRUE iff the given value fits in the given type.  If an
 *  integer is too large to be represented in the given type, FALSE is
 *  returned.
 */
extern boolean immed_fits(immed value, type_node *the_type);

/*
 *  Return TRUE iff the operand evaluates to an integer constant known
 *  at compile time.  If so, the integer is put in the location
 *  pointed to by ``result''.
 */
extern boolean operand_is_int_const(operand the_operand, int *result);

/*
 *  Return TRUE iff the_op is known to always evaluatesto the constant
 *  the_const.
 */
extern boolean matches_const(operand the_op, immed the_const);

/*
 *  Return TRUE iff the opcode is a ``real'' two operand rrr format
 *  instruction.  That is, both source operands are used and memory is
 *  not accessed and there are no side effects.  The opcodes for which
 *  this is true are the following:
 *
 *      io_add, io_sub, io_mul, io_div, io_rem, io_mod, io_min,
 *      io_max, io_and, io_ior, io_xor, io_asr, io_lsl, io_lsr,
 *      io_rot, io_seq, io_sne, io_sl, io_sle, io_divfloor, and
 *      io_divceil
 */
extern boolean is_real_2op_rrr(if_ops opcode);

/*
 *  Return TRUE iff the opcode is a ``real'' one operand rrr format
 *  instruction.  That is, only one source operand is used and memory
 *  is not accessed and there are no side effects.  The opcodes for
 *  which this is true are the following:
 *
 *      io_cpy, io_neg, io_not, io_abs, and io_cvt
 */
extern boolean is_real_1op_rrr(if_ops opcode);

/*
 *  Calculate the result of a ``real'' two operand rrr format
 *  instruction (i.e. it uses both operands and doesn't access
 *  memory or have side effects).  The opcode must be one for which
 *  is_real_2op_rrr() returns TRUE.
 */
extern eval_status calc_real_2op_rrr(if_ops opcode, immed *result_val,
                                     type_node *result_type, immed src1_val,
                                     immed src2_val);

/*
 *  Calculate the result of a ``real'' one operand rrr format
 *  instruction (i.e. it uses one operand and doesn't access
 *  memory or have side effects).  The opcode must be one for which
 *  is_real_1op_rrr() returns TRUE.
 */
extern eval_status calc_real_1op_rrr(if_ops opcode, immed *result_val,
                                     type_node *result_type, immed src_val);

/*
 *  Fold away constants to simplify the given expression.  There are
 *  two forms to make it more convenient to handle the two cases of
 *  operands that aren't yet attached to anything and instructions
 *  that are already in the SUIF code.  In the former case the caller
 *  needs to know where to look for the new operand and in the later
 *  we need to handle changing destinations and the owner tree_nodes
 *  or other instructions.  Also, if we know that the code is
 *  situated, we can pull out code that needs to be executed only for
 *  side effects, for example when multiplied by zero.
 */
extern operand fold_constants(operand the_operand);
extern void fold_constants(instruction *the_instr);

/*
 *  Return an expression that calculates the result of an rrr
 *  operation, folding away that operation, if possible.  No constant
 *  folding is done within the operands.  The opcode for the first
 *  function must be one for which is_real_2op_rrr() returns TRUE, and
 *  the opcode for the second function must be one for which
 *  is_real_1op_rrr() returns TRUE.
 */
extern operand fold_real_2op_rrr(if_ops opcode, type_node *result_type,
                                 operand src1_op, operand src2_op);
extern operand fold_real_1op_rrr(if_ops opcode, type_node *result_type,
                                 operand src_op);

/*
 *  Return an expression tree for the result of doing a specified
 *  arithmetic operation on the given operands, simplifying the
 *  operation if possible.  Both source operands are used up by this
 *  and cannot be used elsewhere.  The operation is specified in the
 *  function name and corresponds to the name of a SUIF if_ops opcode.
 *
 *  Each operand must have a type that is one of the legal types for
 *  that particular operand of the given operation individually, but
 *  they needn't necessarily be types that can be used together for
 *  that operation.  If both types are non-pointer types and would be
 *  required to have the same type if used directly together in an
 *  instruction with the given opcode, they are both cast to the type
 *  given by cast_up() before anything else.  The only other
 *  requirement for source operands to instructions with these opcodes
 *  that depends on both types at the same time is the case of io_add
 *  or io_sub operations with one pointer type; in this case the input
 *  to this function must meet the same requirements as operands to
 *  the instruction would have to meet, i.e. any pointer type and any
 *  integer type.
 *
 *  For all but the io_add and io_sub operations, the source types
 *  uniquely determine the result type according to the rules for the
 *  various instructions, so for all these operations require only the
 *  source operands to be specified.  For addition and subtraction
 *  with one pointer operand, the result may be of any pointer type,
 *  so there are versions of the corresponding function that allow the
 *  result type to be specified.  The form specifying the result type
 *  explicitly may be used for any addition or subtraction even if it
 *  doesn't use pointer types, but the result type specified must be
 *  correct.  If the result type is not specified for addition or
 *  subtraction with one pointer operand, the result type will be the
 *  pointer type.
 */
extern operand fold_add(type_node *result_type, operand src1_op,
                        operand src2_op);
extern operand fold_add(operand src1_op, operand src2_op);
extern operand fold_sub(type_node *result_type, operand src1_op,
                        operand src2_op);
extern operand fold_sub(operand src1_op, operand src2_op);
extern operand fold_mul(operand src1_op, operand src2_op);
extern operand fold_div(operand src1_op, operand src2_op);
extern operand fold_rem(operand src1_op, operand src2_op);
extern operand fold_mod(operand src1_op, operand src2_op);
extern operand fold_min(operand src1_op, operand src2_op);
extern operand fold_max(operand src1_op, operand src2_op);
extern operand fold_and(operand src1_op, operand src2_op);
extern operand fold_ior(operand src1_op, operand src2_op);
extern operand fold_xor(operand src1_op, operand src2_op);
extern operand fold_asr(operand src1_op, operand src2_op);
extern operand fold_lsl(operand src1_op, operand src2_op);
extern operand fold_lsr(operand src1_op, operand src2_op);
extern operand fold_rot(operand src1_op, operand src2_op);
extern operand fold_seq(operand src1_op, operand src2_op);
extern operand fold_sne(operand src1_op, operand src2_op);
extern operand fold_sl(operand src1_op, operand src2_op);
extern operand fold_sle(operand src1_op, operand src2_op);
extern operand fold_divfloor(operand src1_op, operand src2_op);
extern operand fold_divceil(operand src1_op, operand src2_op);

/*
 *  Return an expression tree for the result of doing the specified
 *  arithmetic operation on the given operand, simplifying the
 *  operation if possible.  The src_op operand is used up by this and
 *  cannot be use_elsewhere.  The operation is specified in the
 *  function name and corresponds to the name of a SUIF if_ops opcode.
 *  Note that the result type is determined uniquely by the operand
 *  type.
 */
extern operand fold_neg(operand src_op);
extern operand fold_not(operand src_op);
extern operand fold_abs(operand src_op);

/*
 *  Return an expression that is the result of loading from the given
 *  address, folding away the load if possible.  The load can be
 *  folded away if the address is a constant variable symbol address
 *  with the correct type and no offset.
 */
extern operand fold_load(operand address);

/*
 *  Return an expression that is the logical negation of the_op
 *  assuming the value of the_op is either the integer zero (FALSE) or
 *  the integer one (TRUE).  If the_op is a direct comparison
 *  instruction, that instruction will be changed to give the negation
 *  of what it would otherwise give, so no new instructions are added
 *  in that case.
 */
extern operand fold_logical_not(operand the_op);

/*
 *  Return a new tree_node_list that when executed has the same effect
 *  as the side effects of evaluating the_op, or NULL if there are no
 *  side effects.  Either way, the_op is destroyed.
 */
extern tree_node_list *reduce_to_side_effects(operand the_op);

/*
 *  Return an expression which is the_op cast to new_type.  If the
 *  type is already correct, the_op is returned.  Otherwise, converts
 *  are added or folded in as necessary to get an expression with the
 *  new_type.  In any case, the result is always going to evaluate to
 *  the same thing as applying a convert instruction to the_op with
 *  new_type as the result_type.
 */
extern operand cast_op(operand the_op, type_node *new_type);

/*----------------------------------------------------------------------*
    End of Constant Folding and Evaluation Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Estimation Routines
 *----------------------------------------------------------------------*/

/*
 *  The following return a very, very rough estimate of the time it
 *  would take to execute the given code.  The unit is the time it
 *  takes to execute one integer SUIF instruction.
 */
extern double rough_time_estimate(tree_node_list *the_list);
extern double rough_time_estimate(tree_node *the_node);
extern double rough_time_estimate(operand the_operand);
extern double rough_time_estimate_instr_tree(instruction *the_instr);
extern double rough_time_estimate_one_instr(instruction *the_instr);

/*----------------------------------------------------------------------*
    End of Estimation Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Array Information Routines
 *----------------------------------------------------------------------*/

extern boolean is_array(operand a);
extern boolean is_array_symbol(operand a);
extern array_type *find_array_type(type_node *tn);
extern array_type *find_array_type(in_array *ia);
extern int array_num_elem(array_type *at, int *too_messy);
extern int array_lower_bound(array_type *at, int *too_messy);
extern int array_upper_bound(array_type *at, int *too_messy);
extern var_sym *get_sym_of_array(operand op);
extern var_sym *get_sym_of_array(in_array *ai);
extern var_sym *get_sym_of_array(instruction *i);
extern var_sym *get_pass_thru_sym(instruction *i);
extern boolean is_lhs(instruction *i, boolean array_ok=TRUE);
extern int numdim(array_type *at);
extern type_node *get_element_type(type_node *curr_type);
extern boolean constant_bounds(type_node *curr_type);
extern boolean known_bounds(type_node *curr_type);
extern int total_num_elems(type_node *curr_type);

/*----------------------------------------------------------------------*
    End of Array Information Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Var_def Initialization Annotation Routines
 *----------------------------------------------------------------------*/

class base_init_struct;
class multi_init_struct;
class repeat_init_struct;
class fill_init_struct;

DECLARE_DLIST_CLASS(base_init_struct_list, base_init_struct *);

class base_init_struct
  {
public:
    virtual ~base_init_struct(void) {}

    virtual multi_init_struct *the_multi_init(void) { return NULL; }
    virtual repeat_init_struct *the_repeat_init(void) { return NULL; }
    virtual fill_init_struct *the_fill_init(void) { return NULL; }
    virtual int total_size(void) = 0;
  };

class multi_init_struct : public base_init_struct
  {
public:
    int size;
    immed_list *data;

    multi_init_struct(void) { size = 0; data = NULL; }
    multi_init_struct(int init_size, immed_list *init_data)
      {
        size = init_size;
        data = init_data;
      }
    ~multi_init_struct(void) {}

    multi_init_struct *the_multi_init(void) { return this; }
    int total_size(void) { return size * data->count(); }
  };

class repeat_init_struct : public base_init_struct
  {
public:
    int repetitions;
    int size;
    immed data;

    repeat_init_struct(void) {}
    repeat_init_struct(int init_repetitions, int init_size, immed init_data)
      {
        repetitions = init_repetitions;
        size = init_size;
        data = init_data;
      }
    ~repeat_init_struct(void) {}

    repeat_init_struct *the_repeat_init(void) { return this; }
    int total_size(void) { return size * repetitions; }
  };

class fill_init_struct : public base_init_struct
  {
public:
    int size;
    int data;

    fill_init_struct(void) {}
    fill_init_struct(int init_size, int init_data)
      {
        size = init_size;
        data = init_data;
      }
    ~fill_init_struct(void) {}

    fill_init_struct *the_fill_init(void) { return this; }
    int total_size(void) { return size; }
  };

/*
 *  Return a list containing the initialization data from annotations
 *  on the var_def in a more easily useable form.  The annotations on
 *  the var_def are unchanged.
 */
extern base_init_struct_list *read_init_data(var_def *the_def);

/*
 *  Deallocate the space used by the initializer list and its
 *  contents.
 */
extern void deallocate_init_data(base_init_struct_list *initializers);

/*
 *  Write the given initialization data onto the var_def in
 *  initialization annotations.  The data list is unchanged.
 */
extern void write_init_data(var_def *the_def, base_init_struct_list *data);

/*
 *  Split the given initialization data into two chunks, the first
 *  containing the first ``offset'' bits and the other containing the
 *  remaining initialization.  If successful, TRUE is returned and the
 *  original list is destroyed.  Otherwise, some data overlaps the cut
 *  point in a way that precludes spliting it there, in which case the
 *  original list is unchanged, the two result list pointers are set
 *  to NULL, and FALSE is returned.
 */
extern boolean split_init_data(base_init_struct_list *original, int offset,
                               base_init_struct_list **chunk1,
                               base_init_struct_list **chunk2);

/*
 *  If the data on the_def is in the form of a simple string on the
 *  target machine that translates into a simple string on the machine
 *  running SUIF, return a newly allocated array containing that
 *  string up to and including the first null character, otherwise
 *  return NULL.
 */
extern char *string_from_init_data(var_def *the_def);

/*
 *  Return TRUE iff it is legal to have the static initializations
 *  specified by the annotations on the_def with a variable of type
 *  the_type.
 */
extern boolean type_is_compatible_with_initializations(type_node *the_type,
                                                       var_def *the_def);

/*
 *  Add fields to the_group to make it compatible with the static
 *  initialization specified by the annotations on the_def, if
 *  necessary.  It is required that the_group be a group type, not a
 *  structure or union type.
 */
extern void fix_group_type_for_initializations(struct_type *the_group,
                                               var_def *the_def);

/*----------------------------------------------------------------------*
    End of Var_def Initialization Annotation Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Miscellaneous Code Transformation Routines
 *----------------------------------------------------------------------*/

/*
 *  Force the destination of the instruction to not be part of an
 *  expression tree.  That is, if the instruction is in an expression
 *  tree, move it out and replace it with a new temporary variable.
 *  The instruction then gets its own tree_instr and writes into the
 *  temporary variable.  If the instruction wasn't part of an
 *  expression tree to begin with, there is no effect.
 */
extern void force_dest_not_expr(instruction *the_instr);

/*
 *  Force all the sources of the instruction to not be expression
 *  trees.  If any are expression trees, they are replaced with
 *  temporary variables, just as if force_dest_not_expr() had been
 *  called on the base instruction of the sub-expression tree.
 *  Sources that are not expression trees are not affected.
 */
extern void force_sources_not_exprs(instruction *the_instr);

/*
 *  Replace the old instruction and its expression tree with the new
 *  instruction and its expression tree.  If the old instruction was
 *  in an expression tree, it is removed from the tree and the new
 *  instruction is put in its place.  If the old instruction had its
 *  own tree_instr, it is removed from that tree_instr and replaced
 *  with the new instruction, then the new instruction's destination
 *  is changed to that of the old instruction.
 *
 *  Before the function is called, old_instr should be attached to the
 *  SUIF code but new_instr should not.  After the call, the old
 *  instruction will not be attached but the new one will.  Then the
 *  old instruction may be deleted or it may be used elsewhere.
 */
extern void replace_instruction(instruction *old_instr,
                                instruction *new_instr);

/*
 *  Return a tree node that has the effect when executed of evaluating
 *  rval and assigning the result to lval.  If rval is already an
 *  expression tree, the base instruction's destination is set to be
 *  the variable, otherwise a copy instruction is created.  In either
 *  case a tree_instr is wrapped around the whole thing and returned.
 */
extern tree_node *create_assignment(var_sym *lval, operand rval);
inline tree_node *assign(var_sym *lval, operand rval)
  { return create_assignment(lval, rval); }

/*
 *  Insert the new_node immediately before ``place''.
 */
extern void insert_tree_node_before(tree_node *new_node, tree_node *place);

/*
 *  Make sure that the_instr, the_op, the_sym, or the_type,
 *  respectively, can be put in new_place without violating scoping
 *  rules, assuming that they came from a nested scope somewhere under
 *  new_place.  If there is a reference to something in a scope that
 *  is not visible, it is moved into a symbol table for a block that
 *  is the parent of new_place.  If this is necessary and new_place is
 *  not already the body of a block, a new block is inserted.  This is
 *  done recursively to make sure there are no out-of-scope
 *  references.
 *
 *  If auto variables are moved out to higher scopes, mark
 *  instructions with "dead" annotations are left to show that the
 *  variables are dead on entry and exit to the old scope.
 */
extern void expand_scope(instruction *the_instr, tree_node_list *new_place);
extern void expand_scope(operand the_op, tree_node_list *new_place);
extern void expand_scope(sym_node *the_sym, tree_node_list *new_place);
extern void expand_scope(type_node *the_type, tree_node_list *new_place);

/*
 *  If the_node is in a tree_node_list, remove it's tree_node_list_e
 *  from that list and delete the tree_node_list_e; otherwise, do
 *  nothing.  the_node is never deleted in any case.
 */
extern void remove_node(tree_node *the_node);

/*
 *  First do a remove_node() on the_node, then delete the_node.
 */
extern void kill_node(tree_node *the_node);

/*
 *  If the_op is an instruction, remove it from it's parent
 *  instruction if necessary, and delete the instruction.  If the_op
 *  is not an instruction, this has no effect.
 */
extern void kill_op(operand the_op);

/*
 *  Insert new_node just before or just after, respectively, place.
 */
extern void insert_before(tree_node *new_node, tree_node *place);
extern void insert_after(tree_node *new_node, tree_node *place);

/*
 *  If the tree_for is not already guarded, add a tree_if around it to
 *  make sure that whenever the tree_for itself is executed, its
 *  landing pad and first iteration will always be executed; if the
 *  tree_for is already guarded (i.e. has a k_guarded annotation),
 *  there is no effect.  In any case, after this function is called,
 *  the tree_for is guaranteed to have a k_guarded annotation.
 */
extern void guard_for(tree_for *the_for);

/*
 *  Replace the lower bound, upper bound, or step operand,
 *  respectively, of the_for with a new temporary variable assigned
 *  the original operand.
 */
extern void make_lb_temp(tree_for *the_for);
extern void make_ub_temp(tree_for *the_for);
extern void make_step_temp(tree_for *the_for);

/*
 *  Return an operand that will evaluate to 1 if the test for tree_for
 *  continuation indicates the for is done executing when the index
 *  equals index_op and evaluates to 0 otherwise.  The operand
 *  index_op is used up by this function.
 */
extern operand for_test_done(tree_for *the_for, operand index_op);

/*
 *  Return a new tree_if node with the given ``then'' part and
 *  ``else'' part (or empty ``else'' part if else_part is omitted or
 *  NULL) and test part such that then_part executes iff test_op
 *  evaluates to true.  The type of test_op must be type_signed.
 */
extern tree_if *if_node(base_symtab *scope, operand test_op,
                        tree_node_list *then_part,
                        tree_node_list *else_part = NULL);

/*
 *  Return a newly allocated tree_instr containing an instruction with
 *  opcode io_lab, io_jmp, io_btrue, or io_bfalse, respectively.
 */
extern tree_instr *label_node(label_sym *the_label_sym);
extern tree_instr *jump_node(label_sym *the_label_sym);
extern tree_instr *btrue_node(label_sym *target, operand test_op);
extern tree_instr *bfalse_node(label_sym *target, operand test_op);

/*
 *  Replace references to types and symbols in the_object as directed
 *  by k_replacement annotations everywhere within the_object and
 *  everything owned by it.  Each reference to a type with a
 *  k_replacement annotation containing an immed list with a type is
 *  replaced by the new type.  Similarly for sym_nodes with
 *  k_replacement annotations containing an immed list with a new
 *  sym_node.  But for sym_nodes another kind of data in the
 *  k_replacement can be used.  If instead of another sym_node, the
 *  data of the k_replacement annotation contains an operand, then
 *  uses of the symbol as source operands (including operands in
 *  immeds of annoations) are replaced by the new operand.  If the
 *  symbol is used in a destination operand or in the symbolic address
 *  of an ldc instruction, the replacement operand is required to be
 *  either a load instruction or a simple symbol operand (so that it
 *  makes sence to think of it as an ``lvalue'' and take its address).
 *  In the case of a destination operand, a symbol replacement simply
 *  replaces the destination operand and a load instruction causes the
 *  whole instruction to be replaced by a store using the address from
 *  the load and the original instruction as a source operand.  In the
 *  case of a load constant instruction, a symbol replacement simply
 *  replaces the symbol and keeps the original offset in the immed,
 *  while a load instruction as replacement causes the whole ldc
 *  instruction to be replaced by the address from the load
 *  instruction, possibly with an addition instruction and integer ldc
 *  to add the offset.  When the replacement data is an operand, other
 *  references to the symbol, such as directly in annotations, are
 *  left unchanged.
 *
 *  Note that in all of the above, replacing anything with an operand
 *  means replacing it with a copy of the operand.  The original in
 *  the k_replacement annotation is left unchanged and as many
 *  replacements may be made as necessary.
 *
 *  Note also that the replacements of symbols with symbol replacement
 *  data come first; if the replacement itself has a replacement with
 *  operand data, it in turn will be replaced.  That is the only case
 *  where cascaded replacement will happen.
 */
extern void do_replacement(suif_object *the_object);

/*
 *  These two functions use k_globalize annotations to mark sets of
 *  symbols and types to be moved into the inter-file global symbol
 *  table from a file symbol table.  They mark the_sym or the_type,
 *  respectively, if necessary, and then recursively mark everything
 *  referenced by the_sym or the_type that also needs to be moved,
 *  including symbols in annotations.  This is necessary to insure
 *  that after the symbols and types are all moved there are no
 *  references in the interfile global symbol table to objects in a
 *  file symbol table.
 */
extern void set_sym_to_interfilize(sym_node *the_sym);
extern void set_type_to_interfilize(type_node *the_type);

/*
 *  This returns an operand that is a ``char *'' pointing to a
 *  statically allocated character string initialized to
 *  ``the_string'', including the terminating zero, and just large
 *  enough for that string.  The variable containing the string is put
 *  in the_fse and is local to that file.
 */
extern operand string_literal_op(const char *the_string, file_set_entry *the_fse);

/*
 *  This function removes any io_mrk instruction that holds a single
 *  "line" annotation and nothing else followed by another io_mrk
 *  instruction with a "line" annotation.
 */
extern void kill_redundant_line_marks(tree_node *the_node);
extern void kill_redundant_line_marks(tree_node_list *the_list);

/*
 *  This function adds k_replacement annotations to base_var and all
 *  its descendents so that if do_replacements is called on any code
 *  it is as if the child variable references were re-written first in
 *  terms of base_var, and then the substitution done for base_var.
 *  ``replacement'' can be anything that would be appropriate as a
 *  value for a k_replacement annotation on base_var that can be used
 *  to meaningfully replace any of its sub-variables also.  That is,
 *  it can be another var_sym, or it can be an operand containing a
 *  direct reference to a var_sym or it can be an operand containing a
 *  load instruction.
 */
extern void mark_var_and_subs_replace(var_sym *base_var, immed replacement);

/*
 *  This function tries to extract upper bound information for an
 *  array reference instruction from the variable for the array being
 *  referenced.  That is, given an array reference that uses the
 *  address of a simple variable (not a sub-variable, because then a
 *  reference beyond the end of the sub-variable might still be ok) as
 *  its base and the same lower bound and element type as the
 *  variable, but calls its upper bound unknown, the array reference
 *  upper bound information is changed to reflect the real upper bound
 *  of the array variable.
 */
extern void try_ub_from_var_extraction(in_array *the_aref);

/*
 *  This function tries to extract constants from variable references
 *  by looking at static initializations of variables marked with
 *  "is constant" annotations.
 */
extern void extract_constants(tree_node *the_node);

/*
 *  This function attempts to improve array bound information for
 *  array types in the_block scope by looking for constant assignments
 *  at the beginning of the_block to variables used in the array
 *  types.
 */
extern void extract_array_bounds(tree_block *the_block);

/*
 *  This function breaks up expression trees into chunks of about
 *  maximum_nodes instructions.  Larger expression trees are detatched
 *  and made into independent expression trees that write into newly
 *  created temporary variables, with the restriction that address
 *  computations are not broken up (i.e. the temporary variables never
 *  have pointer type).
 *
 *  Note that maximum_nodes is not a strict upper bound on the size of
 *  resulting expression trees.  Any expression tree with
 *  maximum_nodes or fewer is left alone, but expression trees with
 *  more nodes might not be broken up if the root node is doing
 *  address calculations.  Since address calculations usually only
 *  involve a few nodes, the maximum size of resulting expression
 *  trees will be no bigger than maximum_nodes plus a few.
 */
extern void breakup_large_expressions(instruction *the_instr,
                                      i_integer maximum_nodes);

/*----------------------------------------------------------------------*
    End of Miscellaneous Code Transformation Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Miscellaneous Symbol Routines
 *----------------------------------------------------------------------*/

enum glob_type { INTRA_FILE, INTER_FILE};

/*
 *  Turn the given variable into a global symbol.  If the_glob_type is
 *  INTRA_FILE, the variable is put in the file symbol table that is a
 *  parent of its previous location.  Otherwise, it is put in the
 *  inter-file global table.  The corresponding var_def is created or
 *  moved as needed.  If the variable is already in the correct symbol
 *  table or a parent of that one, there is no effect.  Note that it
 *  is up to the caller to determine that this is correct -- when
 *  recursion is possible, the program semantics can be changed by
 *  making a local variable global.  It is illegal to call this
 *  function on a parameter.  All sub-variables, if any, are moved
 *  along with their parents.  If this function is called on a
 *  sub-variable, the effect is as if it had been called on its
 *  parent.  Hence calling this on any member of a symbol hierarcy
 *  moves the entire hierarchy.
 */
extern void globalize(var_sym *the_var, glob_type the_glob_type);

/*
 *  Return TRUE iff the_var is a Fortran common block.
 */
extern boolean is_common(var_sym *the_var);

/*
 *  Return a symbol table whose scope is the intersection of the
 *  scopes of the two symbol tables.  If the intersection is empty,
 *  return NULL.
 */
extern base_symtab *joint_symtab(base_symtab *symtab_1, base_symtab *symtab_2);

/*
 *  Return the nearest common ancestor of the two symbol tables.
 */
extern base_symtab *common_symtab(base_symtab *symtab_1,
                                  base_symtab *symtab_2);

/*
 *  Return a newly allocated list containing all of the children of
 *  base_var that overlap the region defined by ``offset'' and
 *  ``length'' (in bits).
 */
extern immed_list *children_touching_region(var_sym *base_var, int offset,
                                            int length);

/*
 *  Return the type that the address of the_sym should have by
 *  default, which will be a pointer type.  For a label symbol, it
 *  will be a void pointer, for a proc_sym or var_sym it will be a
 *  pointer to the type of the symbol.
 */
extern type_node *addr_type(sym_node *the_sym);

/*----------------------------------------------------------------------*
    End of Miscellaneous Symbol Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Routines to Remove Sub-Variables
 *----------------------------------------------------------------------*/

/*
 *  Removed all references to sub-variables in the given table, object
 *  contained in the table, and all annotations on the table or
 *  objects contained in the table; then, remove all sub-variables
 *  from the given symbol table and destroy them.  All references to
 *  these sub-variables outside the symbol table must already have
 *  been removed.
 */
extern void symtab_no_sub_vars(base_symtab *the_symtab);

/*
 *  Replace all references to sub-variables in the given annotation
 *  with references to their root parents with the appropriate offset
 *  change.
 */
extern void no_annote_sub_vars(suif_object *the_object);

/*
 *  Replace all occurances of sub-variables in the given instruction
 *  and all sub-expressions and their annotations with references
 *  through root parents.
 */
extern void instr_no_sub_vars(instruction *the_instr);

/*----------------------------------------------------------------------*
    End of Routines to Remove Sub-Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Label Information Routines
 *----------------------------------------------------------------------*/

/*
 *  NOTE: It is illegal to call any of the functions in this section
 *  unless  build_label_info() has been called on the appropriate
 *  tree_node_list.
 */

struct label_info
  {
    instruction_list forward_jumps;
    instruction *definition;
    instruction_list backward_jumps;
  };

/*
 *  Gather all the information for the functions in this section and
 *  annotate the code with it in temporary annotations.  It puts the
 *  information on for all the nodes in node_list, and in lists
 *  contained in tree_block nodes, but not within any other kind of
 *  tree_node.  It is illegal to call this function on the body of a
 *  tree_block that is not a tree_proc.
 */
extern void build_label_info(tree_node_list *node_list);

/*
 *  Remove the information that was put on annotations by
 *  build_label_info() and free all the space it took.  After this has
 *  been called, none of the other functions in this section may be
 *  called on nodes in node_list until build_label_info() is called
 *  again.
 */
extern void remove_label_info(tree_node_list *node_list);

/*
 *  Return a pointer to the label_info block for the given label.  The
 *  memory it points to can be used until remove_label_info() is
 *  called to de-allocate it.
 */
extern label_info *get_lab_info(label_sym *the_label);

/*
 *  Return TRUE iff the target of the_bj comes above the_bj in the
 *  SUIF code.
 */
extern boolean is_backward_branch(in_bj *the_bj);

/*
 *  Return TRUE iff the given label comes above the_mbr in the SUIF
 *  code.  It is only legal to call this function if the_label is one
 *  of the possible targets of the_mbr.
 */
extern boolean mbr_lab_is_backward(in_mbr *the_mbr, label_sym *the_label);

/*
 *  Return a count of the number of times labels that come above
 *  the_mbr in the SUIF code appear as possible targets of the_mbr.
 *  Note that duplicates of the same label are counted multiple times.
 */
extern int num_mbr_back_labs(in_mbr *the_mbr);

/*
 *  Put the necessary temporary annotation on the label_sym for this
 *  instruction to mark it is defined here and used nowhere else.
 */
extern void create_info_for_new_lab_instr(in_lab *new_lab_instr);

/*----------------------------------------------------------------------*
    End of Label Information Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Inlined Functions
 *----------------------------------------------------------------------*/

inline operand const_op(const i_integer &the_const, type_node *the_type)
  { return const_op(ii_to_immed(the_const), the_type); }

/*----------------------------------------------------------------------*
    End of Inlined Functions
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of tree_node Dismantling Routines
 *----------------------------------------------------------------------*/

/*
 *  Break up the given tree_node into lower-level components.
 *  tree_for nodes are broken into tree_if nodes and tree_loop nodes,
 *  while tree_if, tree_loop, and tree_block nodes are broken into
 *  completely unstructured control-flow and the symbols from the
 *  tree_block are moved up to the parent symbol table.  It is an
 *  error to call this function on a tree_instr or tree_proc node
 *  because there is nothing lower-level that such nodes can be broken
 *  into.
 */
extern void dismantle(tree_node *the_node);

/*----------------------------------------------------------------------*
    End of tree_node Dismantling Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Temporary Marking Routines
 *----------------------------------------------------------------------*/

DECLARE_DLIST_CLASS(suif_object_list, suif_object *);

class temp_marker
  {
private:
    static unsigned long next_id_num;

    unsigned long this_id;
    const char *annote_name;
    suif_object_list marked_list;

public:
    temp_marker(void);
    ~temp_marker(void);

    void mark(suif_object *the_object);
    void unmark(suif_object *the_object);
    boolean is_marked(suif_object *the_object);
    suif_object_list *list(void)  { return &marked_list; }
    void clear_marks(void);
  };

/*----------------------------------------------------------------------*
    End of Temporary Marking Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Outlining Routines
 *----------------------------------------------------------------------*/

/*
 *  ``Outlining'' here is used to mean the opposite of inlining,
 *  i.e. taking a block of code and replacing it with a call to a new
 *  procedure that does the work of the block.
 */

/*
 *  This flag is provided for the user code to decide what happens to
 *  direct references in annotations to local variables that are
 *  converted to being passed by reference.  If set to TRUE, a zero
 *  immed replaces each immed containing such a reference, otherwise
 *  it is replaced by an immed with a reference to the new
 *  call-by-reference parameter which is a pointer to the original
 *  variable, if possible (in the case of sub-variables, there may not
 *  be a corresponding pointer variable, in which case the immed is
 *  replaced with a zero immed).  The default value is TRUE.
 *
 *  The desired behavior really depends on the meaning of the
 *  annotation.  In some cases the annotation still has meaning and is
 *  important, and can be interpreted correctly with a pointer to what
 *  was there originally.  In other cases, the annotation will lose
 *  all meaning if it can't refer to the original symbol.  There's no
 *  general way of handling this that will be correct all the time, so
 *  the user is provided a choice among the less-than-perfect
 *  alternatives.  If the user elects to have the references zero'd
 *  out, that's the safest choice, but the annotations with zeros
 *  still need to be interpretted correctly as having a zero mean the
 *  information is invalid.  If the user chooses to have the pointer
 *  replace the original variable, it is up to the user to see to it
 *  that all annotations are then interpretted correctly.
 */
EXPORTED_BY_USEFUL boolean zero_out_ref_converted_in_annotes;

enum do_write_out { DO_WRITE_OUT, DO_NOT_WRITE_OUT };
/*
 *  This function does basic ``outlining'' of the_block, replacing it
 *  with a call to a new procedure.  If return_value is NULL, the new
 *  procedure returns void; otherwise, it returns the unqualified
 *  version of return_value's type and writes it's result into
 *  return_value -- the new procedure is written so that the correct
 *  value is returned.  The proc_sym for the new procedure is
 *  returned.  If do_write_out is TRUE, the new procedure is written
 *  to the output file (the same file set entry as that of the_block),
 *  otherwise it is up to the caller to write it out eventually.
 *
 *  This function will pass by value any necessary local variables if
 *  its simple analysis can tell this is safe, and it will pass the
 *  rest by reference.  It will make any pointer parameter of the new
 *  function into call-by-ref parameters if it can tell this is safe.
 *
 *  It is illegal to call this function if labels from outside scopes
 *  are referenced in the_block other than labels directly used in
 *  immed lists of annotations.  It is also illegal to call this
 *  function if the_block contains any return statements.  Both of
 *  these conditions can be handled by transforming the code before
 *  calling the outline() function; since these transformations are
 *  more or less independent of the rest of what outline() is doing,
 *  they are better kept separate, if needed.
 */
extern proc_sym *outline(tree_block *the_block, var_sym *return_value = NULL,
                         boolean do_write_out = TRUE);

/*----------------------------------------------------------------------*
    End of Outlining Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Inlining Routines
 *----------------------------------------------------------------------*/

/*
 *  The following inlines the_call.  It assumes that the address of
 *  the procedure called is a direct address of a known proc_sym
 *  (i.e. that proc_for_call() returns non-NULL) and that the body of
 *  that proc_sym is already in memory.
 */
extern void inline_call(in_cal *the_call);

/*----------------------------------------------------------------------*
    End of Inlining Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Linkage Information Routines
 *----------------------------------------------------------------------*/

/*
 *  The following two variables hold the names of annotations that are
 *  used internally by the unreferenced_outside_fileset() function and
 *  getlinkinfo program.  They shouldn't be used directly anywere
 *  else.
 */
EXPORTED_BY_USEFUL const char *k_outlink_set;
EXPORTED_BY_USEFUL const char *k_outlink_not_out;

/*
 *  This function returns TRUE if it is certain that the given symbol
 *  is never referenced outside the current fileset.  This is always
 *  true for symbols in any scope except the interfile symbol table.
 *  For files in the interfile symbol table, the getlinkinfo program
 *  can provide information that this function will use.  If
 *  information from the getlinkinfo program is not available, this
 *  function assumes that any symbol from the interfile symbol table
 *  might be referenced externally.
 */
extern boolean unreferenced_outside_fileset(sym_node *the_sym);

/*----------------------------------------------------------------------*
    End of Linkage Information Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Variable Positioning Routines
 *----------------------------------------------------------------------*/

/*
 *  The offset_kind type specifies one of four possible meanings for a
 *  relative linear offset number: from the start of the first object
 *  to the start of the second; from the start of the first object to
 *  the end of the second; from the end of the first object to the
 *  start of the second; or from the end of the first object to the
 *  end of the second.  These are specified by the
 *  OFFSET_START_TO_START, OFFSET_START_TO_END, OFFSET_END_TO_START,
 *  and OFFSET_END_TO_END enumeration members respectively.
 */
enum offset_kind
  {
    OFFSET_START_TO_START, OFFSET_START_TO_END, OFFSET_END_TO_START,
    OFFSET_END_TO_END
  };

/*
 *  This function tries to set the relative positions of var1 and var2
 *  and returns TRUE if successful and FALSE if unsuccessful.  If var1
 *  and var2 have different scopes, or incompatible storage (auto
 *  v. static, is_reg()), or if either is a parameter, this function
 *  automatically fails and FALSE is returned.  Also, if the variables
 *  are global with external linkage and either lacks a definition
 *  (either its own var_def or an ancestor variable's var_def) in the
 *  the fileset, FALSE is returned.  Otherwise, if both are already
 *  share the same root parent variable, if the relative offset is
 *  equal to that requested, TRUE is returned, otherwise FALSE is
 *  returned.  Otherwise, if the variables or ancestors have var_defs
 *  with initialization annotations with data such that the
 *  initializations would overlap, FALSE is returned.  Otherwise, the
 *  root ancestors of the two variables are made children of a new
 *  variable at appropriate offsets.  The offsets are chosen so that
 *  one of the offsets is non-negative and as close to zero as
 *  possible without violating alignment restrictsions, and the other
 *  offset is greater than or equal to that, and so that the relative
 *  positioning is that specified.  Note that this may result in
 *  overlapping -- it is up to the user of this function to insure
 *  that either there is no overlapping or any overlapping that does
 *  occur preserves the semantics of the program.
 *
 *  Note that as with other static data offsets in SUIF, the offset
 *  here is specified in units of bits.
 *
 *  It is illegal to call this function with a requested offset that
 *  would necessarily result in a violation of the alignment
 *  restrictions of either var1 or var2.  For example, requesting an
 *  offset of 8 between two variables of size 32 and alignment 32
 *  would be illegal.  And since there is always an implicit alignment
 *  requirement of at least the addressability of the target machine
 *  (8 for the typical byte-addressable machine), offsets must be in
 *  multiples of that size.
 */
extern boolean set_relative_positions(var_sym *var1, var_sym *var2,
        i_integer offset = 0,
        offset_kind which_offset = OFFSET_START_TO_START);

/*----------------------------------------------------------------------*
    End of Variable Positioning Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Potential Modification Testing Routines
 *----------------------------------------------------------------------*/

/*
 *  This function returns TRUE if executing node_list might affect the
 *  value of the_variable.
 */
extern boolean might_modify(var_sym *the_variable, tree_node_list *node_list);

/*
 *  This function returns TRUE if executing node_list might affect the
 *  value of the_operand.
 */
extern boolean might_modify(operand the_operand, tree_node_list *node_list);

/*
 *  This function returns TRUE if executing node_list might affect the
 *  value of the_instr.
 */
extern boolean might_modify(instruction *the_instr, tree_node_list *node_list);

/*----------------------------------------------------------------------*
    End of Potential Modification Testing Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Instruction Operand Spilling Routines
 *----------------------------------------------------------------------*/

/*
 *  These three functions spill uses of instructions as operands
 *  (including expression trees) into variables.  The first does so
 *  only for operands in the_list; the other two functions do so
 *  recursively for all instruction operands for which both source and
 *  destination are within the_node or the_list respectively.  These
 *  routines work on both expression trees and flat lists of
 *  instructions connected with instruction operands.
 */
extern void spill_instr_ops(tree_node_list *the_list);
extern void recursive_spill_instr_ops(tree_node *the_node);
extern void recursive_spill_instr_ops(tree_node_list *the_list);

/*
 *  This variable contains the prefix that will be used for variables
 *  that are created by the spill functions above.  It defaults to
 *  "_spill" and this library never changes it, so the user is free to
 *  set this to whatever is desired.  The name of each of the
 *  variables created will be this prefix followed by a positive
 *  decimal integer.
 */
extern const char *spill_prefix;

/*----------------------------------------------------------------------*
    End of Instruction Operand Spilling Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Instruction Operand Building Routines
 *----------------------------------------------------------------------*/

/*
 *  This function builds instruction operands out of single-use,
 *  single-def variables that are not live across basic blocks or
 *  scopes.  If make_instr_for_bounds is set by the user to TRUE, it
 *  will also build for bound expressions from the basic block
 *  preceeding the tree_for node.  The default is to have
 *  make_insr_for_bounds set to FALSE, which means not to move any
 *  code into the bounds.
 *
 *  Note that except in the case of for bounds, which has to be
 *  specially enabled by the user, the instruction ordering is not
 *  changed and expression trees are not built, though the
 *  cvt_to_trees method may be called on the result to build the
 *  expression trees from the instruction operands.
 */
extern void make_instr_ops(tree_node *the_node);

EXPORTED_BY_USEFUL boolean make_instr_for_bounds;

/*----------------------------------------------------------------------*
    End of Instruction Operand Building Routines
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Beginning of Namespace Routines
 *----------------------------------------------------------------------*/

/*
 *  This function maps all the global identifiers in the_symtab into
 *  the space of strings that meet the requirements for a C
 *  identifier: only upper or lower case letters, digits, and
 *  underscores allowed, and the first character cannot be a digit.
 *  It also follows the restriction that identifiers with names that
 *  are already in the target namespace remain unchanged.
 *
 *  There are some points to be made about these requirements:
 *    * It cannot be completely safe to do this under all
 *      circumstances that SUIF allows.  That's because SUIF doesn't
 *      specify what other global identifiers might exist that aren't
 *      listed in the interfile global symbol table but will
 *      eventually be linked in.  So whatever name you choose to map
 *      the string "#" into, there's no guarantee that string hasn't
 *      been used already as an identifier in something else that will
 *      be linked in.  This is true regardless of how we choose to map
 *      into alphanumeric identifiers as long as we keep the
 *      restriction that identifiers that are already alphanumeric map
 *      to themselves -- so the mapping cannot be injective.
 *    * We have to assume that the same mapping is made for every file
 *      that uses each non-alphanumeric identifier.  Since these
 *      identifiers are global, that might mean different filesets.
 *    * As a practical matter, the reason for this mapping is that it
 *      is necessary for most linkers -- linker-visible names can only
 *      be C identifiers.  And many linkers don't even distinguish
 *      between all possible C identifiers -- many have a limited
 *      number of significant characters.  Some aren't even case
 *      sensitive.  So the shorter the identifiers mapped to by the
 *      function are, the less likely we'll run up against linker
 *      limitations, and relying too much on case sensitivity has
 *      similar dangers.
 *
 *  Here's the mapping used here to meet these requirements:
 *    * Identifiers that are already legal are left unchanged.
 *    * The initial mapping for other identifiers is to prepend
 *      ``__tR_'' (to minimize collision chances) then translate the
 *      identifier character-by-character:
 *        * letters and digits are unchanged
 *        * underscores are replaced by two consecutive underscores
 *        * all other characters are replaced by an underscore
 *          followed by two hexidecimal digits (using upper case
 *          letters), where the hexidecimal digits are the ASCII for
 *          the original character.
 *    * If the new identifier doesn't exist already in the interfile
 *      global symbol table, it is used.  Otherwise, decimal integers,
 *      starting with ``1'' are appended until a name is found with no
 *      known conflict, and that name is used.
 */
extern void map_namespace_to_alphanumeric(global_symtab *the_symtab);

/*----------------------------------------------------------------------*
    End of Namespace Routines
 *----------------------------------------------------------------------*/

#endif /* BASIC_H */
