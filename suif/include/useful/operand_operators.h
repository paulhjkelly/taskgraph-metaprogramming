/* file "operand_operators.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the header file for operators for building locally
 *  optimized expression trees from operands.
 */

#ifndef OPERAND_OPERATORS_H
#define OPERAND_OPERATORS_H

#include <suif1.h>

#ifndef BASIC_H
#error "<useful/basic.h> must be included before this point"
#endif

/*
 *  Note: An implicit conversion from an integer constant to an
 *  operand is purposely omitted because the intended result type is
 *  not known and it is undesirable to guess a type that may turn out
 *  to be wrong and cause complications.  Instead, versions of most
 *  arithmetic operators are provided that mix integer constants and
 *  operands and use the type information from the operand.
 */

    /* Operators for operands alone */

inline operand operator<(operand left, operand right)
  { return fold_sl(left, right); }
inline operand operator>(operand left, operand right)
  { return fold_sl(right, left); }
inline operand operator<=(operand left, operand right)
  { return fold_sle(left, right); }
inline operand operator>=(operand left, operand right)
  { return fold_sle(right, left); }

inline operand operator+(operand left, operand right)
  { return fold_add(left, right); }
inline operand operator-(operand left, operand right)
  { return fold_sub(left, right); }
inline operand operator*(operand left, operand right)
  { return fold_mul(left, right); }
inline operand operator/(operand left, operand right)
  { return fold_div(left, right); }
inline operand operator%(operand left, operand right)
  { return fold_rem(left, right); }

inline operand operator^(operand left, operand right)
  { return fold_xor(left, right); }
inline operand operator&(operand left, operand right)
  { return fold_and(left, right); }
inline operand operator|(operand left, operand right)
  { return fold_ior(left, right); }
inline operand operator~(operand op)
  { return fold_not(op); }
inline operand operator<<(operand left, operand right)
  { return fold_lsl(left, right); }
inline operand operator>>(operand left, operand right)
  { return fold_asr(left, right); }

inline operand operator!(operand op)
  { return fold_logical_not(op); }

/* unary */
inline operand operator+(operand op)
  { return op; }
inline operand operator-(operand op)
  { return fold_neg(op); }

inline operand &operator+=(operand &left, operand right)
  { return (left = left + right); }
inline operand &operator-=(operand &left, operand right)
  { return (left = left - right); }
inline operand &operator*=(operand &left, operand right)
  { return (left = left * right); }
inline operand &operator/=(operand &left, operand right)
  { return (left = left / right); }
inline operand &operator%=(operand &left, operand right)
  { return (left = left % right); }
inline operand &operator^=(operand &left, operand right)
  { return (left = left ^ right); }
inline operand &operator&=(operand &left, operand right)
  { return (left = left & right); }
inline operand &operator|=(operand &left, operand right)
  { return (left = left | right); }
inline operand &operator>>=(operand &left, operand right)
  { return (left = left >> right); }
inline operand &operator<<=(operand &left, operand right)
  { return (left = left << right); }


    /* Operators for operands mixed with i_integers */

inline operand operator<(operand left, const i_integer &right)
  { return left < const_op(right, left.type()); }
inline operand operator>(operand left, const i_integer &right)
  { return left > const_op(right, left.type()); }
inline operand operator<=(operand left, const i_integer &right)
  { return left <= const_op(right, left.type()); }
inline operand operator>=(operand left, const i_integer &right)
  { return left >= const_op(right, left.type()); }

inline operand operator<(const i_integer &left, operand right)
  { return const_op(left, right.type()) < right; }
inline operand operator>(const i_integer &left, operand right)
  { return const_op(left, right.type()) > right; }
inline operand operator<=(const i_integer &left, operand right)
  { return const_op(left, right.type()) <= right; }
inline operand operator>=(const i_integer &left, operand right)
  { return const_op(left, right.type()) >= right; }

inline operand operator+(operand left, const i_integer &right)
  {
    if (left.type()->is_ptr())
        return left + const_op(right, type_ptr_diff);
    else
        return left + const_op(right, left.type());
  }
inline operand operator-(operand left, const i_integer &right)
  {
    if (left.type()->is_ptr())
        return left - const_op(right, type_ptr_diff);
    else
        return left - const_op(right, left.type());
  }
inline operand operator*(operand left, const i_integer &right)
  { return left * const_op(right, left.type()); }
inline operand operator/(operand left, const i_integer &right)
  { return left / const_op(right, left.type()); }
inline operand operator%(operand left, const i_integer &right)
  { return left % const_op(right, left.type()); }

inline operand operator+(const i_integer &left, operand right)
  {
    if (right.type()->is_ptr())
        return const_op(left, type_ptr_diff) + right;
    else
        return const_op(left, right.type()) + right;
  }
inline operand operator-(const i_integer &left, operand right)
  { return const_op(left, right.type()) - right; }
inline operand operator*(const i_integer &left, operand right)
  { return const_op(left, right.type()) * right; }
inline operand operator/(const i_integer &left, operand right)
  { return const_op(left, right.type()) / right; }
inline operand operator%(const i_integer &left, operand right)
  { return const_op(left, right.type()) % right; }

inline operand operator^(operand left, const i_integer &right)
  { return left ^ const_op(right, left.type()); }
inline operand operator&(operand left, const i_integer &right)
  { return left & const_op(right, left.type()); }
inline operand operator|(operand left, const i_integer &right)
  { return left | const_op(right, left.type()); }
inline operand operator<<(operand left, const i_integer &right)
  { return left << const_op(right, type_unsigned); }
inline operand operator>>(operand left, const i_integer &right)
  { return left >> const_op(right, type_unsigned); }

inline operand operator^(const i_integer &left, operand right)
  { return const_op(left, right.type()) ^ right; }
inline operand operator&(const i_integer &left, operand right)
  { return const_op(left, right.type()) & right; }
inline operand operator|(const i_integer &left, operand right)
  { return const_op(left, right.type()) | right; }

/*
 *  Note: There are no operator>>() or operator<<() taking an integer
 *  left and ``class operand'' right operand because there is nothing
 *  to indicate the desired result type for that case.
 */

inline operand &operator+=(operand &left, const i_integer &right)
  { return (left = left + right); }
inline operand &operator-=(operand &left, const i_integer &right)
  { return (left = left - right); }
inline operand &operator*=(operand &left, const i_integer &right)
  { return (left = left * right); }
inline operand &operator/=(operand &left, const i_integer &right)
  { return (left = left / right); }
inline operand &operator%=(operand &left, const i_integer &right)
  { return (left = left % right); }
inline operand &operator^=(operand &left, const i_integer &right)
  { return (left = left ^ right); }
inline operand &operator&=(operand &left, const i_integer &right)
  { return (left = left & right); }
inline operand &operator|=(operand &left, const i_integer &right)
  { return (left = left | right); }
inline operand &operator>>=(operand &left, const i_integer &right)
  { return (left = left >> right); }
inline operand &operator<<=(operand &left, const i_integer &right)
  { return (left = left << right); }

/*
 *  Note: There are no assignment operators taking an integer constant
 *  left argument because it makes no sense to assign to a constant.
 */


    /* Operators for operands mixed with ints */

/*
 *  Note: The following comparison operators are provided to
 *  disambiguate the case of a comparison between an operand and the
 *  integer constant zero.  Unfortunately, to C++ the integer constant
 *  zero can as well have any pointer type as integer type, so an
 *  operator having arguments of operand and the integer contant zero
 *  can be treated as an operand and an i_integer or two operands
 *  (since there is an operand constructor taking a var_sym *).  So we
 *  break the tie by giving operators that take an ``int'' directly
 *  without having to use any user-defined conversions.
 *
 *  Only comparison operators are provided because while comparisons
 *  with zero are common, other operations using the constant zero are
 *  usually pointless because they are degenerate cases.
 */
inline operand operator<(operand left, int right)
  { return left < i_integer(right); }
inline operand operator>(operand left, int right)
  { return left > i_integer(right); }
inline operand operator<=(operand left, int right)
  { return left <= i_integer(right); }
inline operand operator>=(operand left, int right)
  { return left >= i_integer(right); }

inline operand operator<(int left, operand right)
  { return i_integer(left) < right; }
inline operand operator>(int left, operand right)
  { return i_integer(left) > right; }
inline operand operator<=(int left, operand right)
  { return i_integer(left) <= right; }
inline operand operator>=(int left, operand right)
  { return i_integer(left) >= right; }


/* prefix */
inline operand &operator++(operand &op)
    { op += 1; return op; }
inline operand &operator--(operand &op)
    { op -= 1; return op; }

/* postfix */
inline operand operator++(operand &op, int)
  { operand result = op; op += 1; return result; }
inline operand operator--(operand &op, int)
  { operand result = op; op -= 1; return result; }

#endif /* OPERAND_OPERATORS_H */
