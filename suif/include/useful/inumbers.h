/* file "inumbers.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the header file for the i_integer class for
 *  extended-precision integer arithmetic.
 */

#ifndef INUMBERS_H
#define INUMBERS_H

#include <suif1/misc.h> /* for definition of boolean only */

#include <cstdio>
#include <climits>


class string_integer;

enum i_integer_tag
  {
    IIT_C_INT, IIT_STRING_INT, IIT_POS_INFINITY, IIT_NEG_INFINITY,
    IIT_SIGNLESS_INFINITY, IIT_UNDETERMINED
  };

class i_integer
  {
    friend i_integer i_positive_infinity(void);
    friend i_integer i_negative_infinity(void);
    friend i_integer i_signless_infinity(void);

private:
    i_integer_tag the_tag;
    union
      {
        long long_val;
        string_integer *si_val;
      } value;

    i_integer(string_integer *initial_value)
      { the_tag = IIT_STRING_INT; value.si_val = initial_value; }

public:
    i_integer(void)  { the_tag = IIT_UNDETERMINED; }
    i_integer(signed char initial_value)
      { the_tag = IIT_C_INT; value.long_val = initial_value; }
    i_integer(unsigned char initial_value)
      { the_tag = IIT_UNDETERMINED; set_c_unsigned_long(initial_value); }
    i_integer(short initial_value)
      { the_tag = IIT_C_INT; value.long_val = initial_value; }
    i_integer(unsigned short initial_value)
      { the_tag = IIT_UNDETERMINED; set_c_unsigned_long(initial_value); }
    i_integer(int initial_value)
      { the_tag = IIT_C_INT; value.long_val = initial_value; }
    i_integer(unsigned int initial_value)
      { the_tag = IIT_UNDETERMINED; set_c_unsigned_long(initial_value); }
    i_integer(long initial_value)
      { the_tag = IIT_C_INT; value.long_val = initial_value; }
    i_integer(unsigned long initial_value)
      { the_tag = IIT_UNDETERMINED; set_c_unsigned_long(initial_value); }
    i_integer(const i_integer &initial_value)
      { the_tag = IIT_UNDETERMINED; set_integer(initial_value); }
    i_integer(const char *initial_string, int base = 10);
    ~i_integer();

    boolean is_undetermined(void) const
      { return (the_tag == IIT_UNDETERMINED); }
    boolean is_signless_infinity(void) const
      { return (the_tag == IIT_SIGNLESS_INFINITY); }
    boolean is_finite(void) const
      { return ((the_tag == IIT_C_INT) || (the_tag == IIT_STRING_INT)); }
    boolean is_negative(void) const;

    boolean is_c_char(void) const;
    boolean is_c_unsigned_char(void) const;
    boolean is_c_signed_char(void) const;
    boolean is_c_short(void) const;
    boolean is_c_unsigned_short(void) const;
    boolean is_c_int(void) const;
    boolean is_c_unsigned_int(void) const;
    boolean is_c_long(void) const;
    boolean is_c_unsigned_long(void) const;

    char c_char(void) const;
    unsigned char c_unsigned_char(void) const;
    signed char c_signed_char(void) const;
    short c_short(void) const;
    unsigned short c_unsigned_short(void) const;
    int c_int(void) const;
    unsigned int c_unsigned_int(void) const;
    long c_long(void) const;
    unsigned long c_unsigned_long(void) const;

    void set_c_char(char new_value);
    void set_c_unsigned_char(unsigned char new_value);
    void set_c_signed_char(signed char new_value);
    void set_c_short(short new_value);
    void set_c_unsigned_short(unsigned short new_value);
    void set_c_int(int new_value);
    void set_c_unsigned_int(unsigned int new_value);
    void set_c_long(long new_value);
    void set_c_unsigned_long(unsigned long new_value);
    void set_integer(const i_integer &new_value);
    void clear(void);

    i_integer written_length(int base = 10) const;
    void write(char *location, int base = 10) const;

    void read(const char *location, int base = 10);

    void print(FILE *fp = stdout, int base = 10) const;

    boolean is_equal_to(const i_integer &other) const;
    boolean is_not_equal_to(const i_integer &other) const
      { return !is_equal_to(other); }
    boolean is_less_than(const i_integer &other) const;
    boolean is_greater_than(const i_integer &other) const
      { return other.is_less_than(*this); }
    boolean is_less_than_or_equal_to(const i_integer &other) const
      { return !is_greater_than(other); }
    boolean is_greater_than_or_equal_to(const i_integer &other) const
      { return !is_less_than(other); }

    boolean is_divisible_by(const i_integer &other) const;

    i_integer add(const i_integer &other) const;
    i_integer subtract(const i_integer &other) const;
    i_integer multiply(const i_integer &other) const;
    i_integer div(const i_integer &other) const;
    i_integer mod(const i_integer &other) const;

    i_integer negate(void) const;

    i_integer &operator=(const i_integer &other)
      { set_integer(other); return *this; }

    boolean operator==(const i_integer &other) const
      { return is_equal_to(other); }
    boolean operator!=(const i_integer &other) const
      { return !(*this == other); }
    boolean operator<(const i_integer &other) const
      { return is_less_than(other); }
    boolean operator>(const i_integer &other) const
      { return (other < *this); }
    boolean operator<=(const i_integer &other) const
      { return !(*this > other); }
    boolean operator>=(const i_integer &other) const
      { return !(*this < other); }

    /*
     *  Note that the following six comparison operators are provided
     *  to make sure that comparing with the integer constant zero
     *  will always use the right operator.  Unfortunately, in C++,
     *  for the purposes of overloading resolution, the type of the
     *  integer constant zero can equally well be taken as ``int'' or
     *  as any pointer type.  So C++ sees a one-step user-defined
     *  conversion from the integer constant zero to any type that has
     *  a constructor taking some pointer type as its only argument.
     *  In particular, there is such a conversion (undesired, but
     *  unavoidable) from zero to ``class operand''.  So if there is a
     *  comparison operator taking i_integer and operand classes as
     *  arguments, C++ can't tell if an i_integer plus zero should use
     *  that or the methods for two i_integers.  Hence we add a direct
     *  method that takes exactly an i_integer and an int without any
     *  conversions at all so C++ will use it.
     *
     *  Note that this is only done for the comparison operators
     *  because while comparisons with the constant zero are common,
     *  other operations using the constant zero are usually pointless
     *  because they are degenerate cases.
     *
     *  Now that we are adding operators taking ``int'', though, other
     *  basic C++ integer types, such as ``unsigned long'', will be
     *  cast to ``int'' and then will use the new method instead of
     *  converting directly to i_integer type.  That's because C++
     *  always prefers its own built-in conversions to any
     *  user-defined conversions, even when its own may lose
     *  information.  So we have to add explicit operators for
     *  ``unsigned long''.  And, in fact, we have to add them for all
     *  eight basic C++ integer types because otherwise C++ wouldn't
     *  know which of the others to convert to.
     *
     *  Since we need this operators, we write them to be optimized
     *  for the particular integer types.  Note that all the
     *  comparisons of upper and lower bounds of one type against the
     *  bounds of a different type are compile-time constants, so
     *  everything under the ``the_tag == IIT_C_INT'' case usually
     *  reduces to a single cast plus a single integer comparison, or
     *  in the worst case to two integer comparisons and a cast.
     */
#define DO_COMPARISON(OP, int_type, max_value, min_value)                  \
    boolean operator OP (int_type other) const                             \
      {                                                                    \
        if (the_tag == IIT_C_INT)                                          \
          {                                                                \
            if (static_cast<long>(min_value) == static_cast<long>(LONG_MIN))                       \
              {                                                            \
                if (static_cast<unsigned long>(max_value) <= static_cast<unsigned long>(LONG_MAX)) \
                    return value.long_val OP static_cast<long>(other);                \
                else                                                       \
                    return static_cast<int_type>(value.long_val) OP other;        \
              }                                                            \
            else if (static_cast<long>(min_value) < static_cast<long>(LONG_MIN))                   \
              {                                                            \
                if (static_cast<unsigned long>(max_value) >= static_cast<unsigned long>(LONG_MAX)) \
                  {                                                        \
                    return static_cast<int_type>(value.long_val) OP other;        \
                  }                                                        \
                else                                                       \
                  {                                                        \
                    if (value.long_val <= static_cast<long>(max_value))                 \
                        return static_cast<int_type>(value.long_val) OP other;    \
                    else                                                   \
                        return 10 OP 5;                                    \
                  }                                                        \
              }                                                            \
            else                                                           \
              {                                                            \
                if (static_cast<unsigned long>(max_value) <= static_cast<unsigned long>(LONG_MAX)) \
                  {                                                        \
                    return value.long_val OP static_cast<long>(other);                \
                  }                                                        \
                else                                                       \
                  {                                                        \
                    if (other <= static_cast<int_type>(LONG_MAX))                       \
                        return value.long_val OP static_cast<long>(other);            \
                    else                                                   \
                        return 5 OP 10;                                    \
                  }                                                        \
              }                                                            \
          }                                                                \
        else                                                               \
          {                                                                \
            return operator OP (i_integer(other));                         \
          }                                                                \
      }
#define DO_COMPARISON_WITHOUT_WARNING(OP, int_type, max_value, min_value)                  \
    boolean operator OP (int_type other) const                             \
      {                                                                    \
        if (the_tag == IIT_C_INT)                                          \
          {                                                                \
            if (static_cast<long>(min_value) == static_cast<long>(LONG_MIN))                       \
              {                                                            \
                if (static_cast<unsigned long>(max_value) <= static_cast<unsigned long>(LONG_MAX)) \
                    return value.long_val OP static_cast<long>(other);                \
                else                                                       \
                    return static_cast<int_type>(value.long_val) OP other;        \
              }                                                            \
            else if (static_cast<long>(min_value) < static_cast<long>(LONG_MIN))                   \
              {                                                            \
                if (static_cast<unsigned long>(max_value) >= static_cast<unsigned long>(LONG_MAX)) \
                  {                                                        \
                    return static_cast<int_type>(value.long_val) OP other;        \
                  }                                                        \
                else                                                       \
                  {                                                        \
                    if (value.long_val <= static_cast<long>(max_value))                 \
                        return static_cast<int_type>(value.long_val) OP other;    \
                    else                                                   \
                        return 10 OP 5;                                    \
                  }                                                        \
              }                                                            \
            else                                                           \
              {                                                            \
                if (static_cast<unsigned long>(max_value) <= static_cast<unsigned long>(LONG_MAX)) \
                  {                                                        \
                    return value.long_val OP static_cast<long>(other);                \
                  }                                                        \
                else                                                       \
                  {                                                        \
                    if (true)                       \
                        return value.long_val OP static_cast<long>(other);            \
                    else                                                   \
                        return 5 OP 10;                                    \
                  }                                                        \
              }                                                            \
          }                                                                \
        else                                                               \
          {                                                                \
            return operator OP (i_integer(other));                         \
          }                                                                \
      }
#define COMP_OPS(int_type, max_value, min_value)      \
    DO_COMPARISON(==, int_type, max_value, min_value) \
    DO_COMPARISON(!=, int_type, max_value, min_value) \
    DO_COMPARISON(<, int_type, max_value, min_value)  \
    DO_COMPARISON(>, int_type, max_value, min_value)  \
    DO_COMPARISON(<=, int_type, max_value, min_value) \
    DO_COMPARISON(>=, int_type, max_value, min_value)
#define COMP_OPS_WITHOUT_WARNING(int_type, max_value, min_value)      \
    DO_COMPARISON_WITHOUT_WARNING(==, int_type, max_value, min_value) \
    DO_COMPARISON_WITHOUT_WARNING(!=, int_type, max_value, min_value) \
    DO_COMPARISON_WITHOUT_WARNING(<, int_type, max_value, min_value)  \
    DO_COMPARISON_WITHOUT_WARNING(>, int_type, max_value, min_value)  \
    DO_COMPARISON_WITHOUT_WARNING(<=, int_type, max_value, min_value) \
    DO_COMPARISON_WITHOUT_WARNING(>=, int_type, max_value, min_value)
COMP_OPS(signed char, SCHAR_MAX, SCHAR_MIN)
COMP_OPS_WITHOUT_WARNING(unsigned char, UCHAR_MAX, 0)
COMP_OPS(short, SHRT_MAX, SHRT_MIN)
COMP_OPS_WITHOUT_WARNING(unsigned short, USHRT_MAX, 0)
COMP_OPS(int, INT_MAX, INT_MIN)
COMP_OPS(unsigned int, UINT_MAX, 0)
COMP_OPS(long, LONG_MAX, LONG_MIN)
COMP_OPS(unsigned long, ULONG_MAX, 0)
#undef COMP_OPS
#undef DO_COMPARISON
#undef COMP_OPS_WITHOUT_WARNING
#undef DO_COMPARISON_WITHOUT_WARNING

    i_integer operator+(const i_integer &other) const  { return add(other); }
    i_integer operator-(const i_integer &other) const
      { return subtract(other); }
    i_integer operator*(const i_integer &other) const
      { return multiply(other); }
    i_integer operator/(const i_integer &other) const  { return div(other); }
    i_integer operator%(const i_integer &other) const  { return mod(other); }

    i_integer operator^(const i_integer &other) const;
    i_integer operator&(const i_integer &other) const;
    i_integer operator|(const i_integer &other) const;
    i_integer operator~(void) const;
    i_integer operator<<(const i_integer &other) const;
    i_integer operator>>(const i_integer &other) const;

    boolean operator!(void) const  { return (*this == 0); }
    boolean operator&&(const i_integer &other) const
      { return ((*this != 0) && (other != 0)); }
    boolean operator||(const i_integer &other) const
      { return ((*this != 0) || (other != 0)); }

    /* unary */
    i_integer operator+(void) const  { return *this; }
    i_integer operator-(void) const  { return negate(); }

    i_integer &operator+=(const i_integer &other)
      { return (*this = *this + other); }
    i_integer &operator-=(const i_integer &other)
      { return (*this = *this - other); }
    i_integer &operator*=(const i_integer &other)
      { return (*this = *this * other); }
    i_integer &operator/=(const i_integer &other)
      { return (*this = *this / other); }
    i_integer &operator%=(const i_integer &other)
      { return (*this = *this % other); }
    i_integer &operator^=(const i_integer &other)
      { return (*this = *this ^ other); }
    i_integer &operator&=(const i_integer &other)
      { return (*this = *this & other); }
    i_integer &operator|=(const i_integer &other)
      { return (*this = *this | other); }
    i_integer &operator>>=(const i_integer &other)
      { return (*this = *this >> other); }
    i_integer &operator<<=(const i_integer &other)
      { return (*this = *this << other); }

    /* prefix */
    i_integer &operator++(void)  { *this += 1; return *this; }
    i_integer &operator--(void)  { *this -= 1; return *this; }

    /* postfix */
    i_integer operator++(int)
      { i_integer result = *this; *this += 1; return result; }
    i_integer operator--(int)
      { i_integer result = *this; *this -= 1; return result; }
  };

extern i_integer i_positive_infinity(void);
extern i_integer i_negative_infinity(void);
extern i_integer i_signless_infinity(void);

/*
 *  The following two functions return the GCD (Greatest Common
 *  Denominator) of op1 and op2.  In addition, the second form sets
 *  *coeff1 and *coeff2 to values such that
 *
 *      (op1 * (*coeff1)) + (op2 * (*coeff2)) = GCD.
 *
 *  These coefficients can be extracted by the same algorithm that
 *  finds the GCD.
 */
extern i_integer ii_gcd(const i_integer &op1, const i_integer &op2);
extern i_integer ii_gcd(const i_integer &op1, const i_integer &op2,
                        i_integer *coeff1, i_integer *coeff2);


class i_rational
  {
private:
    i_integer the_numerator;
    i_integer the_denominator;

    void reduce(void);

public:
    i_rational(void) : the_denominator(1) { }
    i_rational(signed char initial_value)  { set_integer(initial_value); }
    i_rational(unsigned char initial_value)  { set_integer(initial_value); }
    i_rational(short initial_value)  { set_integer(initial_value); }
    i_rational(unsigned short initial_value)  { set_integer(initial_value); }
    i_rational(int initial_value)  { set_integer(initial_value); }
    i_rational(unsigned int initial_value)  { set_integer(initial_value); }
    i_rational(long initial_value)  { set_integer(initial_value); }
    i_rational(unsigned long initial_value)  { set_integer(initial_value); }
    i_rational(const i_integer &initial_value)  { set_integer(initial_value); }
    i_rational(const i_integer &initial_numerator,
               const i_integer &initial_denominator) :
            the_numerator(initial_numerator),
            the_denominator(initial_denominator)
      { reduce(); }
    i_rational(const i_rational &initial_value)
      { set_rational(initial_value); }
    i_rational(const char *initial_string, int base = 10)
      { read(initial_string, base); }

    i_rational numerator(void) const  { return the_numerator; }
    i_rational denominator(void) const  { return the_denominator; }

    boolean is_undetermined(void) const
      { return the_numerator.is_undetermined(); }
    boolean is_signless_infinity(void) const
      { return the_numerator.is_signless_infinity(); }
    boolean is_finite(void) const  { return the_numerator.is_finite(); }
    boolean is_negative(void) const  { return the_numerator.is_negative(); }

    boolean is_c_char(void) const
      { return (is_integer() && the_numerator.is_c_char()); }
    boolean is_c_unsigned_char(void) const
      { return (is_integer() && the_numerator.is_c_unsigned_char()); }
    boolean is_c_signed_char(void) const
      { return (is_integer() && the_numerator.is_c_signed_char()); }
    boolean is_c_short(void) const
      { return (is_integer() && the_numerator.is_c_short()); }
    boolean is_c_unsigned_short(void) const
      { return (is_integer() && the_numerator.is_c_unsigned_short()); }
    boolean is_c_int(void) const
      { return (is_integer() && the_numerator.is_c_int()); }
    boolean is_c_unsigned_int(void) const
      { return (is_integer() && the_numerator.is_c_unsigned_int()); }
    boolean is_c_long(void) const
      { return (is_integer() && the_numerator.is_c_long()); }
    boolean is_c_unsigned_long(void) const
      { return (is_integer() && the_numerator.is_c_unsigned_long()); }

    char c_char(void) const
      { assert(is_c_char()); return the_numerator.c_char(); }
    unsigned char c_unsigned_char(void) const
      { assert(is_c_unsigned_char()); return the_numerator.c_unsigned_char(); }
    signed char c_signed_char(void) const
      { assert(is_c_signed_char()); return the_numerator.c_signed_char(); }
    short c_short(void) const
      { assert(is_c_short()); return the_numerator.c_short(); }
    unsigned short c_unsigned_short(void) const
      {
        assert(is_c_unsigned_short());
        return the_numerator.c_unsigned_short();
      }
    int c_int(void) const
      { assert(is_c_int()); return the_numerator.c_int(); }
    unsigned int c_unsigned_int(void) const
      { assert(is_c_unsigned_int()); return the_numerator.c_unsigned_int(); }
    long c_long(void) const
      { assert(is_c_long()); return the_numerator.c_long(); }
    unsigned long c_unsigned_long(void) const
      { assert(is_c_unsigned_long()); return the_numerator.c_unsigned_long(); }

    void set_c_char(char new_value)  { set_integer(new_value); }
    void set_c_unsigned_char(unsigned char new_value)
      { set_integer(new_value); }
    void set_c_signed_char(signed char new_value)  { set_integer(new_value); }
    void set_c_short(short new_value)  { set_integer(new_value); }
    void set_c_unsigned_short(unsigned short new_value)
      { set_integer(new_value); }
    void set_c_int(int new_value)  { set_integer(new_value); }
    void set_c_unsigned_int(unsigned int new_value)
      { set_integer(new_value); }
    void set_c_long(long new_value)  { set_integer(new_value); }
    void set_c_unsigned_long(unsigned long new_value)
      { set_integer(new_value); }
    void set_integer(const i_integer &new_value)
      { the_numerator = new_value; the_denominator = 1; }
    void set_rational(const i_rational &new_value)
      {
        the_numerator = new_value.the_numerator;
        the_denominator = new_value.the_denominator;
      }

    i_integer written_length(int base = 10) const;
    void write(char *location, int base = 10) const;

    void read(const char *location, int base = 10);

    void print(FILE *fp = stdout, int base = 10) const;

    boolean is_integer(void) const  { return (the_denominator == 1); }
    i_integer floor(void) const;
    i_integer ceiling(void) const;
    i_integer round(void) const;

    i_rational &operator=(const i_rational &other)
      { set_rational(other); return *this; }

    boolean operator==(const i_rational &other) const
      {
        return ((the_numerator == other.the_numerator) &&
                (the_denominator == other.the_denominator));
      }
    boolean operator!=(const i_rational &other) const
      { return !(*this == other); }
    boolean operator<(const i_rational &other) const
      {
        return ((the_numerator * other.the_denominator) <
                (other.the_numerator * the_denominator));
      }
    boolean operator>(const i_rational &other) const
      { return (other < *this); }
    boolean operator<=(const i_rational &other) const
      { return !(*this > other); }
    boolean operator>=(const i_rational &other) const
      { return !(*this < other); }

    i_rational operator+(const i_rational &other) const
      {
        return i_rational((the_numerator * other.the_denominator) +
                          (other.the_numerator * the_denominator),
                          the_denominator * other.the_denominator);
      }
    i_rational operator-(const i_rational &other) const
      { return *this + (-other); }
    i_rational operator*(const i_rational &other) const
      {
        return i_rational(the_numerator * other.the_numerator,
                          the_denominator * other.the_denominator);
      }
    i_rational operator/(const i_rational &other) const
      {
        return i_rational(the_numerator * other.the_denominator,
                          the_denominator * other.the_numerator);
      }
    i_rational operator%(const i_rational &) const  { return 0; }

    boolean operator!(void) const  { return (*this == 0); }
    boolean operator&&(const i_rational &other) const
      { return ((*this != 0) && (other != 0)); }
    boolean operator||(const i_rational &other) const
      { return ((*this != 0) || (other != 0)); }

    /* unary */
    i_rational operator+(void) const  { return *this; }
    i_rational operator-(void) const
      { return i_rational(-the_numerator, the_denominator); }

    i_rational &operator+=(const i_rational &other)
      { return (*this = *this + other); }
    i_rational &operator-=(const i_rational &other)
      { return (*this = *this - other); }
    i_rational &operator*=(const i_rational &other)
      { return (*this = *this * other); }
    i_rational &operator/=(const i_rational &other)
      { return (*this = *this / other); }
    i_rational &operator%=(const i_rational &other)
      { return (*this = *this % other); }

    /* prefix */
    i_rational &operator++(void)  { *this += 1; return *this; }
    i_rational &operator--(void)  { *this -= 1; return *this; }

    /* postfix */
    i_rational operator++(int)
      { i_rational result = *this; *this += 1; return result; }
    i_rational operator--(int)
      { i_rational result = *this; *this -= 1; return result; }
  };

#endif /* INUMBERS_H */
