/* file "inumbers.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements extended-precision integer arithmetic
 *  routines for the i_integer class.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_inumbers_cc

#include <suif1/misc.h> /* for definitions of boolean and assert() only */
#include "inumbers.h"
#include <climits>
#include <cstring>
#include <cctype>

RCS_BASE(
    "$Id$")


/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

    The i_integer Class
    -------------------

The i_integer class is intended to model the mathematic concept of
integers, with some simple extensions for dealing with infinite and
undetermined quantities.

The values representable by the i_integer class include all the finite
integer numbers (subject to memory limitations of the machine that are
system dependent but large), plus four additional distinguishable
non-finite elements.  There are of course various mathematical
concepts of heirarchies of infinite ordinals and cardinals, but those
concepts are not useful for the kinds of things we currently plan to
use this class for.  The kinds of trans-finite concepts we do
represent are based on what the system will be used for.

These four new elements are ``positive infinity'', ``negative
infinity'', ``unsigned infinity'' , and ``undetermined''.

The purpose of ``unsigned infinity'' and ``undetermined'' is to make
multiplication and division defined over the whole class: ``unsigned
infinity'' essentially represents ``1/0'' and ``undetermined''
represents ``0/0''.  Neither of these quantities may be compared with
``greater than'' or ``less than'' comparisons with anything, and
``undetermined'' may not be compared for equality with anything
either.

The purpose of ``positive infinity'' and ``negative infinity'' is to
have upper and lower bounds to represent unlimited ranges.  The
distinction between positive and negative must be made for this
purpose, but this wouldn't work with ``1/0'' because we want ``-1/0 =
1/-0 = 1/0'' by the axioms for ordinary division, so we cannot
distinguish sign in the result of dividing by zero.  Hence ``unsigned
infinity'' is a different concept from either ``positive infinity'' or
``negative infinity''.

The rules for addition, subtraction, negation, multiplication,
reciprication, and division involving these trans-finite elements are
defined as follows:
  (1) Any finite number added to or subtracted from a trans-finite
      gives the same transfinite.
  (2) Any finite number minus a transfinite gives the negation of the
      trans-finite.
  (3) Negation of ``positive infinity'' gives ``negative infinity''
      and vice-versa; negation of either of ``unsigned infinity'' or
      ``undetermined'' gives back the same trans-finite.
  (4) Multiplication of any trans-finite by a positive finite gives
      back the same trans-finite; multiplication of any transfinite by
      a negative finite gives the negation of the trans-finite;
      multiplication of zero by any trans-finite gives back
      ``undetermined''.
  (5) The reciprical of zero is ``unsigned infinity''; the reciprical
      of ``unsigned infinity'', ``positive infinity'', and ``negative
      infinity'' are all zero; and the reciprical of ``undetermined''
      is ``undetermined''.
  (6) Division is defined as multiplication of the numerator by the
      reciprical of the denominator.
  (7) Either of ``positive infinity'' or ``negative infinity'' added
      to itself is the same trans-finite back; any other case of
      addition of trans-finites gives ``undetermined''.
  (8) ``positive infinity'' times itself or ``negative infinity''
      times itself is ``positive infinity''; ``positive infinity''
      times ``negative infinity'' is ``negative infinity''; ``unsigned
      infinity'' times itself is ``unsigned infinity''; any other case
      of multiplication by two trans-finites gives ``undetermined''.


    Implementation
    --------------

The implementations of these functions have time complexity O(n) for
addition and subtraction, O(n^log_2(3)) for multiplication and base
change (including all the bit-wise operations, which are implemented
with base change), and O(n^2) for division, where ``n'' is the length
of the larger of the string operands (the log of the integer value).


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Macro Definitions
 *----------------------------------------------------------------------*/

/* The following is strangely defined because we only use it for
 * machines on which LONG_MAX + LONG_MIN >= 0 and we want to avoid
 * compiler warnings about overflow in constant expressions on other
 * machines.  NEG_INT_MIN is similar. */

#define NEG_LONG_MIN (((long)(LONG_MAX + LONG_MIN >= 0 ? 0 : LONG_MIN)) - \
                      LONG_MIN)
#define NEG_INT_MIN (((int)(INT_MAX + INT_MIN >= 0 ? 0 : INT_MIN)) - INT_MIN)

/*----------------------------------------------------------------------*
    End Macro Definitions
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Constant Declarations
 *----------------------------------------------------------------------*/

/* CUTOFF must be at least 4 */
#define CUTOFF 10 

/*----------------------------------------------------------------------*
    End Constant Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

class string_integer
  {
public:
    unsigned long link_count;
    char *the_digits;
    boolean is_negative;

    string_integer(unsigned long num_digits)
      {
        link_count = 1;
        the_digits = new char[num_digits + 1];
        is_negative = FALSE;
      }
    ~string_integer()
      {
        assert(link_count == 0);
        delete[] the_digits;
      }
  };

enum binary_bit_ops { BBO_XOR, BBO_AND, BBO_IOR };

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Global Variables
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void add_link(string_integer *the_si);
static void cut_link(string_integer *the_si);
static string_integer *add_string_integers(string_integer *op1,
                                           string_integer *op2);
static string_integer *multiply_string_integers(string_integer *op1,
                                                string_integer *op2);
static string_integer *div_string_integers(string_integer *op1,
                                           string_integer *op2);
static string_integer *mod_string_integers(string_integer *op1,
                                           string_integer *op2);
static string_integer *bitwise_op_string_integers(string_integer *op1,
                                                  string_integer *op2,
                                                  binary_bit_ops op);
static string_integer *right_shift_string_integers(string_integer *op1,
                                                   string_integer *op2);
static boolean string_integer_fits_long(string_integer *the_si);
static long long_from_string_integer(string_integer *the_si);
static string_integer *string_integer_from_long(long the_long);
static string_integer *string_integer_from_unsigned_long(
        unsigned long the_ulong);
static void string_integer_to_binary(unsigned char **result,
                                     unsigned long *result_size,
                                     int *carry_bit,
                                     string_integer *the_si);
static string_integer *binary_to_string_integer(unsigned char *bits,
                                                unsigned long bit_size,
                                                int carry_bit);
static boolean string_magnitude_is_less(char *op1, char *op2,
                                        unsigned long op1_len,
                                        unsigned long op2_len, int offset);
static void add_string_magnitudes(char *result, char *op1, char *op2,
                                  unsigned long op1_len,
                                  unsigned long op2_len, int base, int offset);
static void add_to_string_magnitude(char *result, char *op2,
                                    unsigned long result_len,
                                    unsigned long op2_len, int base,
                                    int offset);
static void subtract_string_magnitudes(char *result, char *op1, char *op2,
                                       unsigned long op1_len,
                                       unsigned long op2_len, int base,
                                       int offset);
static void multiply_string_magnitudes(char *result, char *scratch, char *op1,
                                       char *op2, unsigned long op1_len,
                                       unsigned long op2_len, int base,
                                       int offset);
static void divide_string_magnitudes(char *div_result, char *mod_result,
                                     char *op1, char *op2,
                                     unsigned long op1_len,
                                     unsigned long op2_len, int offset);
static void justify_int_string(char *string, unsigned long size, int offset);
static void base_convert(char *new_string, char *scratch, char *old_string,
                         unsigned long old_size, int old_base, int new_base,
                         int old_offset, int new_offset);
static int blow_up_factor(int old_base, int new_base);
static i_rational read_digits_with_decimal_point(const char **location,
                                                 int base);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Method Implementations
 *----------------------------------------------------------------------*/

i_integer::i_integer(const char *initial_string, int base)
  {
    the_tag = IIT_UNDETERMINED;
    read(initial_string, base);
  }


i_integer::~i_integer()
  {
    if (the_tag == IIT_STRING_INT)
        cut_link(value.si_val);
  }


boolean i_integer::is_negative(void) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            return (value.long_val < 0);
        case IIT_STRING_INT:
            return (value.si_val->is_negative);
        case IIT_NEG_INFINITY:
            return TRUE;
        default:
            return FALSE;
      }
  }

boolean i_integer::is_c_char(void) const
  {
    return (is_finite() && (*this >= CHAR_MIN) && (*this <= CHAR_MAX));
  }

boolean i_integer::is_c_unsigned_char(void) const
  {
    return (is_finite() && (*this >= 0) && (*this <= UCHAR_MAX));
  }

boolean i_integer::is_c_signed_char(void) const
  {
    return (is_finite() && (*this >= SCHAR_MIN) && (*this <= SCHAR_MAX));
  }

boolean i_integer::is_c_short(void) const
  {
    return (is_finite() && (*this >= SHRT_MIN) && (*this <= SHRT_MAX));
  }

boolean i_integer::is_c_unsigned_short(void) const
  {
    return (is_finite() && (*this >= 0) && (*this <= USHRT_MAX));
  }

boolean i_integer::is_c_int(void) const
  {
    return (is_finite() && (*this >= INT_MIN) && (*this <= INT_MAX));
  }

boolean i_integer::is_c_unsigned_int(void) const
  {
    return (is_finite() && (*this >= 0) && (*this <= UINT_MAX));
  }

boolean i_integer::is_c_long(void) const
  {
    return (the_tag == IIT_C_INT);
  }

boolean i_integer::is_c_unsigned_long(void) const
  {
    return (is_finite() && (*this >= 0) && (*this <= ULONG_MAX));
  }

char i_integer::c_char(void) const
  {
    assert(is_c_char());
    if (CHAR_MIN == 0)
        return (char)(c_unsigned_long());
    else
        return (char)(c_long());
  }

unsigned char i_integer::c_unsigned_char(void) const
  {
    assert(is_c_unsigned_char());
    return (unsigned char)(c_unsigned_long());
  }

signed char i_integer::c_signed_char(void) const
  {
    assert(is_c_signed_char());
    return (signed char)(c_long());
  }

short i_integer::c_short(void) const
  {
    assert(is_c_short());
    return (short)(c_long());
  }

unsigned short i_integer::c_unsigned_short(void) const
  {
    assert(is_c_unsigned_short());
    return (unsigned short)(c_unsigned_long());
  }

int i_integer::c_int(void) const
  {
    assert(is_c_int());
    return (int)(c_long());
  }

unsigned int i_integer::c_unsigned_int(void) const
  {
    assert(is_c_unsigned_int());
    return (unsigned int)(c_unsigned_long());
  }

long i_integer::c_long(void) const
  {
    assert(the_tag == IIT_C_INT);
    return value.long_val;
  }

unsigned long i_integer::c_unsigned_long(void) const
  {
    assert(is_c_unsigned_long());
    if (the_tag == IIT_C_INT)
        return (long)(value.long_val);

    assert(the_tag == IIT_STRING_INT);
    unsigned long result = 0;
    char *digit_follow = value.si_val->the_digits;
    while (*digit_follow != 0)
      {
        char this_digit = *digit_follow - '0';
        assert(result <= ((ULONG_MAX - this_digit) / 10));
        result *= 10;
        result += this_digit;
        ++digit_follow;
      }
    return result;
  }

void i_integer::set_c_char(char new_value)
  {
    set_c_long(new_value);
  }

void i_integer::set_c_unsigned_char(unsigned char new_value)
  {
    set_c_long(new_value);
  }

void i_integer::set_c_signed_char(signed char new_value)
  {
    clear();
    the_tag = IIT_C_INT;
    value.long_val = new_value;
  }

void i_integer::set_c_short(short new_value)
  {
    clear();
    the_tag = IIT_C_INT;
    value.long_val = new_value;
  }

void i_integer::set_c_unsigned_short(unsigned short new_value)
  {
    set_c_long(new_value);
  }

void i_integer::set_c_int(int new_value)
  {
    clear();
    the_tag = IIT_C_INT;
    value.long_val = new_value;
  }

void i_integer::set_c_unsigned_int(unsigned int new_value)
  {
    if (new_value > (unsigned long)LONG_MAX)
        set_c_unsigned_long(new_value);
    else
        set_c_long(new_value);
  }

void i_integer::set_c_long(long new_value)
  {
    clear();
    the_tag = IIT_C_INT;
    value.long_val = new_value;
  }

void i_integer::set_c_unsigned_long(unsigned long new_value)
  {
    clear();
    if (new_value <= (unsigned long)LONG_MAX)
      {
        set_c_long(new_value);
        return;
      }

    the_tag = IIT_STRING_INT;
    value.si_val = string_integer_from_unsigned_long(new_value);
  }

void i_integer::set_integer(const i_integer &new_value)
  {
    if (&new_value == this) return;
    clear();
    the_tag = new_value.the_tag;
    switch (the_tag)
      {
        case IIT_C_INT:
            value.long_val = new_value.value.long_val;
            break;
        case IIT_STRING_INT:
            value.si_val = new_value.value.si_val;
            add_link(value.si_val);
            break;
        default:
            break;
      }
  }

void i_integer::clear(void)
  {
    if (the_tag == IIT_STRING_INT)
        cut_link(value.si_val);
    the_tag = IIT_UNDETERMINED;
  }

i_integer i_integer::written_length(int base) const
  {
    assert((base > 1) && (base <= 36));

    switch (the_tag)
      {
        case IIT_C_INT:
          {
            long remaining_value = value.long_val;
            if (remaining_value == 0)
                return 1;

            long result;
            if (remaining_value < 0)
              {
                result = 1;
                while (remaining_value != 0)
                  {
                    long this_digit = remaining_value % base;
                    remaining_value /= base;
                    if (this_digit > 0)
                        ++remaining_value;
                    ++result;
                  }
              }
            else
              {
                result = 0;
                while (remaining_value != 0)
                  {
                    remaining_value /= base;
                    ++result;
                  }
              }
            return result;
          }
        case IIT_STRING_INT:
          {
            if (base == 10)
              {
                return (strlen(value.si_val->the_digits) +
                        (value.si_val->is_negative ? 1 : 0));
              }

            static char *scratch_buffer = NULL;
            static unsigned long scratch_size = 0;

            char *this_digits = value.si_val->the_digits;
            unsigned long decimal_size = strlen(this_digits);

            unsigned long result_size =
                    blow_up_factor(10, base) * decimal_size;

            if ((scratch_buffer == NULL) || (scratch_size < 10 * result_size))
              {
                if (scratch_buffer != NULL)
                    delete[] scratch_buffer;
                scratch_size = 10 * result_size;
                scratch_buffer = new char[scratch_size];
              }

            base_convert(scratch_buffer, &(scratch_buffer[result_size]),
                         this_digits, decimal_size, 10, base, '0', 0);

            char *follow_result = scratch_buffer;
            char *result_end = &(scratch_buffer[result_size - 1]);
            while (*follow_result == 0)
              {
                assert(follow_result != result_end);
                ++follow_result;
              }

            return ((result_end - follow_result) +
                    (value.si_val->is_negative ? 1 : 0));
          }
        case IIT_POS_INFINITY:
            return 4;
        case IIT_NEG_INFINITY:
            return 4;
        case IIT_SIGNLESS_INFINITY:
            return 3;
        case IIT_UNDETERMINED:
            return 2;
        default:
            assert(FALSE);
            return 0;
      }
  }

void i_integer::write(char *location, int base) const
  {
    assert((base > 1) && (base <= 36));

    switch (the_tag)
      {
        case IIT_C_INT:
            if (base == 10)
              {
                sprintf(location, "%ld", value.long_val);
              }
            else
              {
                char *follow_location = location;
                long remaining_value = value.long_val;
                if (remaining_value == 0)
                  {
                    *follow_location = '0';
                    ++follow_location;
                  }
                else if (remaining_value < 0)
                  {
                    *follow_location = '-';
                    ++follow_location;
                    while (remaining_value != 0)
                      {
                        long this_digit = remaining_value % base;
                        remaining_value /= base;
                        if (this_digit > 0)
                          {
                            this_digit -= base;
                            ++remaining_value;
                          }
                        this_digit = -this_digit;
                        if (this_digit <= 9)
                            *follow_location = this_digit + '0';
                        else
                            *follow_location = (this_digit - 10) + 'a';
                        ++follow_location;
                      }
                  }
                else
                  {
                    while (remaining_value != 0)
                      {
                        long this_digit = remaining_value % base;
                        remaining_value /= base;
                        if (this_digit <= 9)
                            *follow_location = this_digit + '0';
                        else
                            *follow_location = (this_digit - 10) + 'a';
                        ++follow_location;
                      }
                  }
                *follow_location = 0;
              }
            break;
        case IIT_STRING_INT:
          {
            char *follow_location = location;
            if (value.si_val->is_negative)
              {
                sprintf(follow_location, "-");
                ++follow_location;
              }
            if (base == 10)
              {
                strcpy(follow_location, value.si_val->the_digits);
              }
            else
              {
                static char *scratch_buffer = NULL;
                static unsigned long scratch_size = 0;

                char *this_digits = value.si_val->the_digits;
                unsigned long decimal_size = strlen(this_digits);

                unsigned long result_size =
                        blow_up_factor(10, base) * decimal_size;

                if ((scratch_buffer == NULL) ||
                    (scratch_size < 10 * result_size))
                  {
                    if (scratch_buffer != NULL)
                        delete[] scratch_buffer;
                    scratch_size = 10 * result_size;
                    scratch_buffer = new char[scratch_size];
                  }

                base_convert(scratch_buffer, &(scratch_buffer[result_size]),
                             this_digits, decimal_size, 10, base, '0', 0);

                char *follow_result = scratch_buffer;
                char *result_end = &(scratch_buffer[result_size - 1]);
                while (*follow_result == 0)
                  {
                    assert(follow_result != result_end);
                    ++follow_result;
                  }

                while (follow_result != result_end)
                  {
                    int this_digit = *follow_result;
                    if (this_digit <= 9)
                        *follow_location = this_digit + '0';
                    else
                        *follow_location = (this_digit - 10) + 'a';
                    ++follow_location;
                    ++follow_result;
                  }

                *follow_location = 0;
              }
            break;
          }
        case IIT_POS_INFINITY:
            strcpy(location, "+Inf");
            break;
        case IIT_NEG_INFINITY:
            strcpy(location, "-Inf");
            break;
        case IIT_SIGNLESS_INFINITY:
            strcpy(location, "Inf");
            break;
        case IIT_UNDETERMINED:
            strcpy(location, "??");
            break;
        default:
            assert(FALSE);
      }
  }

void i_integer::read(const char *location, int base)
  {
    assert((base > 1) && (base <= 36));

    static char *scratch_buffer = NULL;
    static unsigned long scratch_size = 0;

    clear();

    const char *remaining_string = location;
    boolean is_negative = FALSE;

    while (isspace(*remaining_string))
        ++remaining_string;

    if (strncmp(remaining_string, "+Inf", 4) == 0)
      {
        the_tag = IIT_POS_INFINITY;
        return;
      }
    else if (strncmp(remaining_string, "-Inf", 4) == 0)
      {
        the_tag = IIT_NEG_INFINITY;
        return;
      }
    else if (strncmp(remaining_string, "Inf", 3) == 0)
      {
        the_tag = IIT_SIGNLESS_INFINITY;
        return;
      }
    else if (strncmp(remaining_string, "??", 4) == 0)
      {
        the_tag = IIT_UNDETERMINED;
        return;
      }

    if (*remaining_string == '-')
      {
        is_negative = TRUE;
        ++remaining_string;
      }

    while (isspace(*remaining_string))
        ++remaining_string;

    const char *follow_string = remaining_string;
    long this_long = 0;
    boolean overflow = FALSE;
    while (*follow_string != 0)
      {
        int this_digit = *follow_string;
        if ((this_digit >= '0') && (this_digit <= '9'))
            this_digit -= '0';
        else if ((this_digit >= 'a') && (this_digit <= 'z'))
            this_digit -= ('a' - 10);
        else if ((this_digit >= 'A') && (this_digit <= 'Z'))
            this_digit -= ('A' - 10);
        else
            break;

        if (this_digit >= base)
            break;

        if (is_negative)
          {
            if (this_long <
                (LONG_MIN / base) + (((LONG_MIN % base) < 0) ? 1 : 0))
              {
                overflow = TRUE;
                break;
              }
            this_long *= base;
            if (this_long < LONG_MIN + this_digit)
              {
                overflow = TRUE;
                break;
              }
            this_long -= this_digit;
          }
        else
          {
            if (this_long > LONG_MAX / base)
              {
                overflow = TRUE;
                break;
              }
            this_long *= base;
            if (this_long > LONG_MAX - this_digit)
              {
                overflow = TRUE;
                break;
              }
            this_long += this_digit;
          }

        ++follow_string;
      }

    if (!overflow)
      {
        the_tag = IIT_C_INT;
        value.long_val = this_long;
        return;
      }

    while (*follow_string != 0)
      {
        int this_digit = *follow_string;
        if ((this_digit >= '0') && (this_digit <= '9'))
            this_digit -= '0';
        else if ((this_digit >= 'a') && (this_digit <= 'z'))
            this_digit -= ('a' - 10);
        else if ((this_digit >= 'A') && (this_digit <= 'Z'))
            this_digit -= ('A' - 10);
        else
            break;

        if (this_digit >= base)
            break;

        ++follow_string;
      }

    unsigned long string_size = follow_string - remaining_string;
    unsigned long new_string_size = string_size;
    if (base > 10)
        new_string_size = 2 * string_size;

    if ((base != 10) &&
        ((scratch_buffer == NULL) || (scratch_size < 10 * new_string_size)))
      {
        if (scratch_buffer != NULL)
            delete[] scratch_buffer;
        scratch_size = 10 * new_string_size;
        scratch_buffer = new char[scratch_size];
      }

    string_integer *new_si = new string_integer(new_string_size);
    new_si->is_negative = is_negative;
    if (base == 10)
      {
        const char *follow_old = remaining_string;
        char *follow_new = new_si->the_digits;
        const char *old_end = &(remaining_string[string_size - 1]);
        while (TRUE)
          {
            assert((*follow_old >= '0') && (*follow_old <= '9'));
            *follow_new = *follow_old;
            if (follow_old == old_end)
                break;
            ++follow_new;
            ++follow_old;
          }
        assert(follow_new == &(new_si->the_digits[new_string_size - 1]));
      }
    else
      {
        const char *follow_old = remaining_string;
        char *follow_new = scratch_buffer;
        const char *old_end = &(remaining_string[string_size - 1]);
        while (TRUE)
          {
            int this_digit = *follow_old;
            if ((this_digit >= '0') && (this_digit <= '9'))
                this_digit -= '0';
            else if ((this_digit >= 'a') && (this_digit <= 'z'))
                this_digit -= ('a' - 10);
            else if ((this_digit >= 'A') && (this_digit <= 'Z'))
                this_digit -= ('A' - 10);
            else
                assert(FALSE);

            *follow_new = this_digit;
            if (follow_old == old_end)
                break;
            ++follow_new;
            ++follow_old;
          }
        assert(follow_new == &(scratch_buffer[string_size - 1]));

        base_convert(new_si->the_digits, &(scratch_buffer[string_size]),
                     scratch_buffer, string_size, base, 10, 0, '0');
      }
    justify_int_string(new_si->the_digits, new_string_size, '0');

    the_tag = IIT_STRING_INT;
    value.si_val = new_si;
  }

void i_integer::print(FILE *fp, int base) const
  {
    assert((base > 1) && (base <= 36));

    switch (the_tag)
      {
        case IIT_C_INT:
            if (base == 10)
              {
                fprintf(fp, "%ld", value.long_val);
              }
            else
              {
                long remaining_value = value.long_val;
                if (remaining_value == 0)
                  {
                    fputc('0', fp);
                  }
                else if (remaining_value < 0)
                  {
                    fputc('-', fp);
                    while (remaining_value != 0)
                      {
                        long this_digit = remaining_value % base;
                        remaining_value /= base;
                        if (this_digit > 0)
                          {
                            this_digit -= base;
                            ++remaining_value;
                          }
                        this_digit = -this_digit;
                        if (this_digit <= 9)
                            fputc(this_digit + '0', fp);
                        else
                            fputc((this_digit - 10) + 'a', fp);
                      }
                  }
                else
                  {
                    while (remaining_value != 0)
                      {
                        long this_digit = remaining_value % base;
                        remaining_value /= base;
                        if (this_digit <= 9)
                            fputc(this_digit + '0', fp);
                        else
                            fputc((this_digit - 10) + 'a', fp);
                      }
                  }
              }
            break;
        case IIT_STRING_INT:
            if (value.si_val->is_negative)
                fputc('-', fp);
            if (base == 10)
              {
                fputs(value.si_val->the_digits, fp);
              }
            else
              {
                static char *scratch_buffer = NULL;
                static unsigned long scratch_size = 0;

                char *this_digits = value.si_val->the_digits;
                unsigned long decimal_size = strlen(this_digits);

                unsigned long result_size =
                        blow_up_factor(10, base) * decimal_size;

                if ((scratch_buffer == NULL) ||
                    (scratch_size < 10 * result_size))
                  {
                    if (scratch_buffer != NULL)
                        delete[] scratch_buffer;
                    scratch_size = 10 * result_size;
                    scratch_buffer = new char[scratch_size];
                  }

                base_convert(scratch_buffer, &(scratch_buffer[result_size]),
                             this_digits, decimal_size, 10, base, '0', 0);

                char *follow_result = scratch_buffer;
                char *result_end = &(scratch_buffer[result_size - 1]);
                while (*follow_result == 0)
                  {
                    assert(follow_result != result_end);
                    ++follow_result;
                  }

                while (follow_result != result_end)
                  {
                    int this_digit = *follow_result;
                    if (this_digit <= 9)
                        fputc(this_digit + '0', fp);
                    else
                        fputc((this_digit - 10) + 'a', fp);
                    ++follow_result;
                  }
              }
            break;
        case IIT_POS_INFINITY:
            fputs("+Inf", fp);
            break;
        case IIT_NEG_INFINITY:
            fputs("-Inf", fp);
            break;
        case IIT_SIGNLESS_INFINITY:
            fputs("Inf", fp);
            break;
        case IIT_UNDETERMINED:
            fputs("??", fp);
            break;
        default:
            assert(FALSE);
      }
  }

boolean i_integer::is_equal_to(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            return ((other.the_tag == IIT_C_INT) &&
                    (value.long_val == other.value.long_val));
        case IIT_STRING_INT:
            return ((other.the_tag == IIT_STRING_INT) &&
                    (value.si_val->is_negative ==
                     other.value.si_val->is_negative) &&
                    (strcmp(value.si_val->the_digits,
                            other.value.si_val->the_digits) == 0));
        case IIT_POS_INFINITY:
            return (other.the_tag == IIT_POS_INFINITY);
        case IIT_NEG_INFINITY:
            return (other.the_tag == IIT_NEG_INFINITY);
        case IIT_SIGNLESS_INFINITY:
            return (other.the_tag == IIT_SIGNLESS_INFINITY);
        case IIT_UNDETERMINED:
            return FALSE;
        default:
            assert(FALSE);
            return FALSE;
      }
  }

boolean i_integer::is_less_than(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    return (value.long_val < other.value.long_val);
                case IIT_STRING_INT:
                    return !(other.value.si_val->is_negative);
                case IIT_POS_INFINITY:
                    return TRUE;
                case IIT_NEG_INFINITY:
                    return FALSE;
                case IIT_SIGNLESS_INFINITY:
                    return FALSE;
                case IIT_UNDETERMINED:
                    return FALSE;
                default:
                    assert(FALSE);
                    return FALSE;
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    return value.si_val->is_negative;
                case IIT_STRING_INT:
                  {
                    if (value.si_val->is_negative &&
                        !other.value.si_val->is_negative)
                      {
                        return TRUE;
                      }
                    else if ((!value.si_val->is_negative) &&
                             other.value.si_val->is_negative)
                      {
                        return FALSE;
                      }

                    char *this_digits = value.si_val->the_digits;
                    char *other_digits = other.value.si_val->the_digits;
                    unsigned long this_count = strlen(this_digits);
                    unsigned long other_count = strlen(other_digits);
                    return string_magnitude_is_less(this_digits, other_digits,
                                                    this_count, other_count,
                                                    '0');
                  }
                case IIT_POS_INFINITY:
                    return TRUE;
                case IIT_NEG_INFINITY:
                    return FALSE;
                case IIT_SIGNLESS_INFINITY:
                    return FALSE;
                case IIT_UNDETERMINED:
                    return FALSE;
                default:
                    assert(FALSE);
                    return FALSE;
              }
        case IIT_POS_INFINITY:
            return FALSE;
        case IIT_NEG_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    return TRUE;
                case IIT_STRING_INT:
                    return TRUE;
                case IIT_POS_INFINITY:
                    return TRUE;
                case IIT_NEG_INFINITY:
                    return FALSE;
                case IIT_SIGNLESS_INFINITY:
                    return FALSE;
                case IIT_UNDETERMINED:
                    return FALSE;
                default:
                    assert(FALSE);
                    return FALSE;
              }
        case IIT_SIGNLESS_INFINITY:
            return FALSE;
        case IIT_UNDETERMINED:
            return FALSE;
        default:
            assert(FALSE);
            return FALSE;
      }
  }

boolean i_integer::is_divisible_by(const i_integer &other) const
  {
    return (div(other) == 0);
  }

i_integer i_integer::add(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;
                    if (this_long < 0)
                      {
                        if (other_long < 0)
                          {
                            if (this_long >= LONG_MIN - other_long)
                                return i_integer(this_long + other_long);
			  }
                        else
                          {
                            return i_integer(this_long + other_long);
			  }
                      }
                    else
                      {
                        if (other_long < 0)
                          {
                            return i_integer(this_long + other_long);
			  }
                        else
                          {
                            if (this_long <= LONG_MAX - other_long)
                                return i_integer(this_long + other_long);
			  }
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si = add_string_integers(si1, si2);
                    cut_link(si1);
                    cut_link(si2);
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            add_string_integers(this_si, other_si);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            add_string_integers(this_si, other_si);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            add_string_integers(this_si, other_si);
                    if (this_si->is_negative != other_si->is_negative)
                      {
                        if (string_integer_fits_long(new_si))
                          {
                            long new_long = long_from_string_integer(new_si);
                            cut_link(new_si);
                            return i_integer(new_long);
                          }
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                case IIT_POS_INFINITY:
                    return *this;
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_NEG_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                case IIT_NEG_INFINITY:
                    return *this;
                case IIT_POS_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                    return *this;
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::subtract(const i_integer &other) const
  {
    i_integer other_negation = other.negate();
    return add(other_negation);
  }

i_integer i_integer::multiply(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;
                    if ((this_long == 0) || (other_long == 0))
                        return i_integer(0);
                    if (this_long < 0)
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                if (-this_long <= LONG_MAX / -other_long)
                                    return i_integer(this_long * other_long);
                              }
                            else
                              {
                                if (this_long > LONG_MAX / other_long)
                                    return i_integer(this_long * other_long);
                              }
			  }
                        else
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                if (-this_long <= NEG_LONG_MIN / other_long)
                                    return i_integer(this_long * other_long);
                              }
                            else
                              {
                                if (this_long > LONG_MIN / other_long)
                                    return i_integer(this_long * other_long);
                              }
			  }
                      }
                    else
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                if (this_long <= NEG_LONG_MIN / -other_long)
                                    return i_integer(this_long * other_long);
                              }
                            else
                              {
                                if (other_long > LONG_MIN / this_long)
                                    return i_integer(this_long * other_long);
                              }
			  }
                        else
                          {
                            if (this_long <= LONG_MAX / other_long)
                                return i_integer(this_long * other_long);
			  }
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si =
                            multiply_string_integers(si1, si2);
                    cut_link(si1);
                    cut_link(si2);
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            multiply_string_integers(this_si, other_si);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                    if (value.long_val == 0)
                        return i_integer();
                    else if (value.long_val < 0)
                        return other.negate();
                    else
                        return other;
                case IIT_SIGNLESS_INFINITY:
                    return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            multiply_string_integers(this_si, other_si);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            multiply_string_integers(this_si, other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                    if (is_negative())
                        return other.negate();
                    else
                        return other;
                case IIT_SIGNLESS_INFINITY:
                    return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    if (other.value.long_val == 0)
                        return i_integer();
                    /* fall through */
                case IIT_STRING_INT:
                    if (other.is_negative())
                        return negate();
                    else
                        return *this;
                case IIT_POS_INFINITY:
                    return *this;
                case IIT_NEG_INFINITY:
                    return negate();
                case IIT_SIGNLESS_INFINITY:
                    return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    if (other.value.long_val == 0)
                        return i_integer();
                    /* fall through */
                case IIT_STRING_INT:
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return *this;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::div(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;
                    if (other_long == 0)
                      {
                        if (this_long == 0)
                          {
                            return i_integer();
                          }
                        else
                          {
                            i_integer result;
                            result.the_tag = IIT_SIGNLESS_INFINITY;
                            return result;
                          }
                      }

                    if (this_long == 0)
                        return i_integer(0);
                    if (other_long == 1)
                        return i_integer(this_long);

                    if (this_long < 0)
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                return i_integer((-this_long) / (-other_long));
                              }
                            else
                              {
                                if ((this_long >= -LONG_MAX) &&
                                    (other_long >= -LONG_MAX))
                                  {
                                    return i_integer((-this_long) /
                                                     (-other_long));
                                  }
                              }
			  }
                        else
                          {
                            if ((LONG_MAX + LONG_MIN) == 0)
                              {
                                return i_integer(-((-this_long) / other_long));
                              }
                            else if ((LONG_MAX + LONG_MIN) > 0)
                              {
                                if ((-this_long) / other_long <= NEG_LONG_MIN)
                                  {
                                    return i_integer(-((-this_long) /
                                                       other_long));
                                  }
                              }
                            else
                              {
                                if (this_long >= -LONG_MAX)
                                  {
                                    return i_integer(-((-this_long) /
                                                       other_long));
                                  }
                              }
			  }
                      }
                    else
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) == 0)
                              {
                                return i_integer(-(this_long / (-other_long)));
                              }
                            else if ((LONG_MAX + LONG_MIN) > 0)
                              {
                                if (this_long / (-other_long) <= NEG_LONG_MIN)
                                  {
                                    return i_integer(-(this_long /
                                                       (-other_long)));
                                  }
                              }
                            else
                              {
                                if (other_long >= -LONG_MAX)
                                  {
                                    return i_integer(-(this_long /
                                                       (-other_long)));
                                  }
                              }
			  }
                        else
                          {
                            return i_integer(this_long / other_long);
			  }
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si = div_string_integers(si1, si2);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            div_string_integers(this_si, other_si);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return i_integer(0);
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    if (other.value.long_val == 0)
                      {
                        i_integer result;
                        result.the_tag = IIT_SIGNLESS_INFINITY;
                        return result;
                      }
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            div_string_integers(this_si, other_si);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            div_string_integers(this_si, other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return i_integer(0);
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    if (other.value.long_val == 0)
                        return i_integer();
                    /* fall through */
                case IIT_STRING_INT:
                    if (other.is_negative())
                        return negate();
                    else
                        return *this;
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                    if (other.value.long_val == 0)
                        return i_integer();
                    /* fall through */
                case IIT_STRING_INT:
                    return *this;
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::mod(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;
                    if (other_long == 0)
                        return i_integer();

                    if (this_long == 0)
                        return i_integer(0);
                    if (other_long == 1)
                        return i_integer(0);

                    if (this_long < 0)
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                return i_integer(-((-this_long) %
                                                   (-other_long)));
                              }
                            else
                              {
                                if ((this_long >= -LONG_MAX) &&
                                    (other_long >= -LONG_MAX))
                                  {
                                    return i_integer(-((-this_long) %
                                                       (-other_long)));
                                  }
                              }
			  }
                        else
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                return i_integer(-((-this_long) % other_long));
                              }
                            else
                              {
                                if (this_long >= -LONG_MAX)
                                  {
                                    return i_integer(-((-this_long) %
                                                       other_long));
                                  }
                              }
			  }
                      }
                    else
                      {
                        if (other_long < 0)
                          {
                            if ((LONG_MAX + LONG_MIN) >= 0)
                              {
                                return i_integer(this_long % (-other_long));
                              }
                            else
                              {
                                if (other_long >= -LONG_MAX)
                                  {
                                    return i_integer(this_long %
                                                     (-other_long));
                                  }
                              }
			  }
                        else
                          {
                            return i_integer(this_long % other_long);
			  }
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si = mod_string_integers(si1, si2);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            mod_string_integers(this_si, other_si);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return *this;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    if (other.value.long_val == 0)
                        return i_integer();
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            mod_string_integers(this_si, other_si);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            mod_string_integers(this_si, other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return *this;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
        case IIT_SIGNLESS_INFINITY:
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::negate(void) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            if ((LONG_MAX + LONG_MIN) == 0)
              {
                return i_integer(-(value.long_val));
              }
            else if ((LONG_MAX + LONG_MIN) > 0)
              {
                long this_long = value.long_val;
                if ((this_long < 0) || (this_long + LONG_MIN <= 0))
                    return i_integer(-this_long);
                string_integer *new_si = string_integer_from_long(this_long);
                new_si->is_negative = TRUE;
                return i_integer(new_si);
              }
            else
              {
                long this_long = value.long_val;
                if ((this_long > 0) || (this_long + LONG_MAX >= 0))
                    return i_integer(-this_long);
                string_integer *new_si = string_integer_from_long(this_long);
                new_si->is_negative = FALSE;
                return i_integer(new_si);
              }
        case IIT_STRING_INT:
          {
            char *old_digits = value.si_val->the_digits;
            string_integer *new_si = new string_integer(strlen(old_digits));
            strcpy(new_si->the_digits, old_digits);
            new_si->is_negative = !(value.si_val->is_negative);
            if (string_integer_fits_long(new_si))
              {
                long new_long = long_from_string_integer(new_si);
                cut_link(new_si);
                return i_integer(new_long);
              }
            return i_integer(new_si);
          }
        case IIT_POS_INFINITY:
          {
            i_integer result;
            result.the_tag = IIT_NEG_INFINITY;
            return result;
          }
        case IIT_NEG_INFINITY:
          {
            i_integer result;
            result.the_tag = IIT_POS_INFINITY;
            return result;
          }
        case IIT_SIGNLESS_INFINITY:
        case IIT_UNDETERMINED:
            return *this;
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::operator^(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;

                    if ((this_long >= 0) && (other_long >= 0))
                      {
                        return i_integer(((unsigned long)this_long) ^
                                         ((unsigned long)other_long));
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si =
                            bitwise_op_string_integers(si1, si2, BBO_XOR);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_XOR);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return ~other;
                    else
                        return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_XOR);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_XOR);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return ~other;
                    else
                        return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                    if (other.is_negative())
                        return ~(*this);
                    else
                        return *this;
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::operator&(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;

                    if ((this_long >= 0) && (other_long >= 0))
                      {
                        return i_integer(((unsigned long)this_long) &
                                         ((unsigned long)other_long));
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si =
                            bitwise_op_string_integers(si1, si2, BBO_AND);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_AND);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return other;
                    else
                        return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_AND);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_AND);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return other;
                    else
                        return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                    if (other.is_negative())
                        return *this;
                    else
                        return i_integer();
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::operator|(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;

                    if ((this_long >= 0) && (other_long >= 0))
                      {
                        return i_integer(((unsigned long)this_long) |
                                         ((unsigned long)other_long));
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si =
                            bitwise_op_string_integers(si1, si2, BBO_IOR);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_IOR);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return i_integer();
                    else
                        return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_IOR);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            bitwise_op_string_integers(this_si, other_si,
                                                       BBO_IOR);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    if (is_negative())
                        return i_integer();
                    else
                        return other;
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                    if (other.is_negative())
                        return i_integer();
                    else
                        return *this;
                case IIT_POS_INFINITY:
                case IIT_NEG_INFINITY:
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }

i_integer i_integer::operator~(void) const
  {
    return -(add(1));
  }

i_integer i_integer::operator<<(const i_integer &other) const
  {
    return (*this >> -other);
  }

i_integer i_integer::operator>>(const i_integer &other) const
  {
    switch (the_tag)
      {
        case IIT_C_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
                    long this_long = value.long_val;
                    long other_long = other.value.long_val;

                    if (other_long == 0)
                      {
                        return *this;
                      }
                    else if (other_long > 0)
                      {
                        if (this_long < 0)
                            return (~((~(*this)) >> other));

                        if (other_long <
                            (long)(sizeof(unsigned long) * CHAR_BIT))
                          {
                            return i_integer(((unsigned long)this_long) >>
                                             other_long);
                          }
                        else
                          {
                            return 0;
                          }
                      }
                    else
                      {
                        if ((this_long >= 0) &&
                            (other_long >
                             -(long)(sizeof(unsigned long) * CHAR_BIT)))
                          {
                            unsigned long this_ulong =
                                    (unsigned long)this_long;
                            unsigned long result = (this_ulong << -other_long);
                            if ((result >> -other_long) == this_ulong)
                                return result;
                          }
                      }

                    string_integer *si1 = string_integer_from_long(this_long);
                    string_integer *si2 = string_integer_from_long(other_long);
                    string_integer *new_si =
                            right_shift_string_integers(si1, si2);
                    cut_link(si1);
                    cut_link(si2);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si =
                            string_integer_from_long(value.long_val);
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            right_shift_string_integers(this_si, other_si);
                    cut_link(this_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                    if (is_negative())
                        return -1;
                    else
                        return 0;
                case IIT_NEG_INFINITY:
                    if (value.long_val == 0)
                        return 0;
                    else if (is_negative())
                        return other;
                    else
                        return -other;
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_STRING_INT:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                  {
		    if (other.value.long_val == 0)
		      return *this;

                    string_integer *this_si = value.si_val;
                    string_integer *other_si =
                            string_integer_from_long(other.value.long_val);
                    string_integer *new_si =
                            right_shift_string_integers(this_si, other_si);
                    cut_link(other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_STRING_INT:
                  {
                    string_integer *this_si = value.si_val;
                    string_integer *other_si = other.value.si_val;
                    string_integer *new_si =
                            right_shift_string_integers(this_si, other_si);
                    if (string_integer_fits_long(new_si))
                      {
                        long new_long = long_from_string_integer(new_si);
                        cut_link(new_si);
                        return i_integer(new_long);
                      }
                    return i_integer(new_si);
                  }
                case IIT_POS_INFINITY:
                    if (is_negative())
                        return -1;
                    else
                        return 0;
                case IIT_NEG_INFINITY:
                    if (is_negative())
                        return other;
                    else
                        return -other;
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_POS_INFINITY:
        case IIT_NEG_INFINITY:
        case IIT_SIGNLESS_INFINITY:
            switch (other.the_tag)
              {
                case IIT_C_INT:
                case IIT_STRING_INT:
                    return *this;
                case IIT_POS_INFINITY:
                    return i_integer();
                case IIT_NEG_INFINITY:
                    return *this;
                case IIT_SIGNLESS_INFINITY:
                    return i_integer();
                case IIT_UNDETERMINED:
                    return i_integer();
                default:
                    assert(FALSE);
                    return i_integer();
              }
        case IIT_UNDETERMINED:
            return i_integer();
        default:
            assert(FALSE);
            return i_integer();
      }
  }


void i_rational::reduce(void)
  {
    if (the_numerator.is_finite() && the_denominator.is_finite())
      {
        if (the_denominator == 0)
          {
            the_numerator = i_signless_infinity();
            the_denominator = 1;
          }
        else
          {
            boolean result_negative =
                    (the_numerator.is_negative() !=
                     the_numerator.is_negative());
            if (the_numerator.is_negative())
                the_numerator = -the_numerator;
            if (the_denominator.is_negative())
                the_denominator = -the_denominator;
            i_integer the_gcd = ii_gcd(the_numerator, the_denominator);
            the_numerator /= the_gcd;
            the_denominator /= the_gcd;
            if (result_negative)
                the_numerator = -the_numerator;
          }
      }
    else
      {
        if (the_numerator.is_undetermined() ||
            the_denominator.is_undetermined())
          {
            the_denominator = 1;
          }
        else if (the_numerator.is_finite())
          {
            the_numerator = 0;
            the_denominator = 1;
          }
        else if (the_denominator.is_finite())
          {
            if (the_denominator == 0)
              {
                the_numerator = i_integer();
                the_denominator = 1;
              }
            else if (the_denominator.is_negative())
              {
                the_numerator = -the_numerator;
                the_denominator = 1;
              }
            else
              {
                the_denominator = 1;
              }
          }
        else
          {
            the_numerator = i_integer();
            the_denominator = 1;
          }
      }
  }

i_integer i_rational::written_length(int base) const
  {
    if (the_denominator == 1)
      {
        return the_numerator.written_length(base);
      }
    else
      {
        return the_numerator.written_length(base) +
               the_denominator.written_length(base) + 1;
      }
  }

void i_rational::write(char *location, int base) const
  {
    the_numerator.write(location, base);
    if (the_denominator != 1)
      {
        unsigned long denominator_length = strlen(location);
        location[denominator_length] = '/';
        the_denominator.write(&(location[denominator_length + 1]), base);
      }
  }

void i_rational::read(const char *location, int base)
  {
    const char *follow_string = location;

    i_rational new_numerator =
            read_digits_with_decimal_point(&follow_string, base);

    while (isspace(*follow_string))
        ++follow_string;

    if (*follow_string == '/')
      {
        ++follow_string;
        i_rational new_denominator =
                read_digits_with_decimal_point(&follow_string, base);
        operator=(new_numerator / new_denominator);
      }
    else
      {
        operator=(new_numerator);
      }
  }

void i_rational::print(FILE *fp, int base) const
  {
    the_numerator.print(fp, base);
    if (the_denominator != 1)
      {
        fputc('/', fp);
        the_denominator.print(fp, base);
      }
  }

i_integer i_rational::floor(void) const
  {
    i_integer div = the_numerator / the_denominator;
    i_integer mod = the_numerator % the_denominator;
    if (mod < 0)
        return div - 1;
    else
        return div;
  }

i_integer i_rational::ceiling(void) const
  {
    return -((-(*this)).floor());
  }

i_integer i_rational::round(void) const
  {
    return (*this + i_rational(1, 2)).floor();
  }

/*----------------------------------------------------------------------*
    End Public Method Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern i_integer i_positive_infinity(void)
  {
    i_integer result;
    result.the_tag = IIT_POS_INFINITY;
    return result;
  }

extern i_integer i_negative_infinity(void)
  {
    i_integer result;
    result.the_tag = IIT_NEG_INFINITY;
    return result;
  }

extern i_integer i_signless_infinity(void)
  {
    i_integer result;
    result.the_tag = IIT_SIGNLESS_INFINITY;
    return result;
  }

extern i_integer ii_gcd(const i_integer &op1, const i_integer &op2)
  {
    i_integer a = op1;
    i_integer b = op2;

    if (a < 0)
        a = -a;
    if (b < 0)
        b = -b;

    if (a < b)
      {
        i_integer temp_ii = a;
        a = b;
        b = temp_ii;
      }

    while (TRUE)
      {
        if (b == 0)
            return a;

        if (b == 1)
            return 1;

        i_integer new_a = b;
        b = a % b;
        a = new_a;
      }
  }

extern i_integer ii_gcd(const i_integer &op1, const i_integer &op2,
                        i_integer *coeff1, i_integer *coeff2)
  {
    i_integer a = op1;
    i_integer b = op2;

    i_integer n = 1;
    i_integer p = 0;

    if (a < 0)
      {
        a = -a;
        n = -1;
      }
    if (b < 0)
        b = -b;

    if (a < b)
      {
        i_integer temp_ii = a;
        a = b;
        b = temp_ii;

        p = n;
        n = 0;
      }

    while (TRUE)
      {
        /*
         *  Invariant conditions:
         *      op1 * n + op2 * m = a
         *      op1 * p + op2 * q = b
         *  where all variables are integers, op1 and op2 never change.
         *
         *  The values of a and b on the next iteration:
         *      new_a = b
         *      new_b = a % b
         *
         *  Which means that if we let:
         *      new_n = p
         *      new_m = q
         *      new_p = n - p * (a div b)
         *      new_q = m - q * (a div b)
         *
         *  Note that there is a recurrence between n and p, and one
         *  between m and q, and both depend on the values of a and b
         *  at each iteration.  But it is not necessary to keep track
         *  of q and m to calculate n and p (or equivalently, it would
         *  not be necessary to keep track of n and p to calculate q
         *  and m).  Since we can derive m and q from the other
         *  values, it is not necessary to calculate them at every
         *  stage.  So here we keep track of just n and p, and at the
         *  end, when we want n and m, we have n and just derive m to
         *  write into *coeff1 and *coeff2.
         */
        if (b == 0)
          {
            *coeff1 = n;
            *coeff2 = (a - (op1 * n)) / op2;
            return a;
          }

        if (b == 1)
          {
            *coeff1 = p;
            *coeff2 = (i_integer(1) - (op1 * p)) / op2;
            return 1;
          }

        i_integer new_n = p;
        p = n - (p * (a / b));
        n = new_n;

        i_integer new_a = b;
        b = a % b;
        a = new_a;
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void add_link(string_integer *the_si)
  {
    if (the_si->link_count < ULONG_MAX)
        ++the_si->link_count;
  }

static void cut_link(string_integer *the_si)
  {
    if (the_si->link_count < ULONG_MAX)
        --the_si->link_count;
    if (the_si->link_count == 0)
        delete the_si;
  }

static string_integer *add_string_integers(string_integer *op1,
                                           string_integer *op2)
  {
    char *this_digits = op1->the_digits;
    char *other_digits = op2->the_digits;
    unsigned long this_count = strlen(this_digits);
    unsigned long other_count = strlen(other_digits);

    boolean reversed = FALSE;

    if (string_magnitude_is_less(this_digits, other_digits, this_count,
                                 other_count, '0'))
      {
        reversed = TRUE;
        char *temp_ptr = this_digits;
        this_digits = other_digits;
        other_digits = temp_ptr;
        unsigned long temp_count = this_count;
        this_count = other_count;
        other_count = temp_count;
      }

    if (op1->is_negative != op2->is_negative)
      {
        string_integer *new_si = new string_integer(this_count);
        if (reversed)
            new_si->is_negative = op2->is_negative;
        else
            new_si->is_negative = op1->is_negative;
        subtract_string_magnitudes(new_si->the_digits, this_digits,
                                   other_digits, this_count, other_count, 10,
                                   '0');
        justify_int_string(new_si->the_digits, this_count, '0');
        if (new_si->the_digits[0] == 0)
            new_si->is_negative = FALSE;
        return new_si;
      }
    else
      {
        string_integer *new_si = new string_integer(this_count + 1);
        new_si->is_negative = op1->is_negative;
        add_string_magnitudes(new_si->the_digits, this_digits, other_digits,
                              this_count, other_count, 10, '0');

        if (*(new_si->the_digits) == '0')
          {
            memmove(new_si->the_digits, &(new_si->the_digits[1]), this_count);
            new_si->the_digits[this_count] = 0;
          }
        else
          {
            new_si->the_digits[this_count + 1] = 0;
          }
        return new_si;
      }
  }

struct thescratcher {
	thescratcher() : size(0), buffer(0) { }
	~thescratcher ( ) {
		delete [] buffer;
	}
	unsigned long size;
	char *buffer;
};

static string_integer *multiply_string_integers(string_integer *op1,
                                                string_integer *op2)
  {
	static thescratcher scratch;

    char *this_digits = op1->the_digits;
    char *other_digits = op2->the_digits;
    unsigned long this_count = strlen(this_digits);
    unsigned long other_count = strlen(other_digits);

    if (this_count < other_count)
      {
        char *temp_ptr = this_digits;
        this_digits = other_digits;
        other_digits = temp_ptr;
        unsigned long temp_count = this_count;
        this_count = other_count;
        other_count = temp_count;
      }

    if (this_count > ULONG_MAX - other_count)
        error_line(1, NULL, "out of memory address space");
    unsigned long new_count = this_count + other_count;

    if (9 * new_count > scratch.size)
      {
        if (scratch.buffer != NULL)
            delete[] scratch.buffer;
        scratch.buffer = new char[9 * new_count];
        scratch.size = 9 * new_count;
      }

    string_integer *new_si = new string_integer(new_count);
    new_si->is_negative = (op1->is_negative != op2->is_negative);
    multiply_string_magnitudes(new_si->the_digits, scratch.buffer, this_digits,
                               other_digits, this_count, other_count, 10, '0');
    justify_int_string(new_si->the_digits, new_count, '0');
    return new_si;
  }


static string_integer *div_string_integers(string_integer *op1,
                                           string_integer *op2)
  {
	static thescratcher scratch;
    char *this_digits = op1->the_digits;
    char *other_digits = op2->the_digits;
    unsigned long this_count = strlen(this_digits);
    unsigned long other_count = strlen(other_digits);

    if (this_count < other_count)
        return new string_integer(0);

    if (other_count > scratch.size)
      {
        if (scratch.buffer != NULL)
            delete[] scratch.buffer;
        scratch.buffer = new char[other_count];
        scratch.size = other_count;
      }

    string_integer *new_si =
            new string_integer((this_count - other_count) + 1);
    new_si->is_negative = (op1->is_negative != op2->is_negative);
    divide_string_magnitudes(new_si->the_digits, scratch.buffer, this_digits,
                             other_digits, this_count, other_count, '0');
    justify_int_string(new_si->the_digits, (this_count - other_count) + 1,
                       '0');
    return new_si;
  }

static string_integer *mod_string_integers(string_integer *op1,
                                           string_integer *op2)
  {
	static thescratcher scratch;

    char *this_digits = op1->the_digits;
    char *other_digits = op2->the_digits;
    unsigned long this_count = strlen(this_digits);
    unsigned long other_count = strlen(other_digits);

    if (this_count < other_count)
      {
        add_link(op1);
        return op1;
      }

    if ((this_count - other_count) + 1 > scratch.size)
      {
        if (scratch.buffer != NULL)
            delete[] scratch.buffer;
        scratch.size = (this_count - other_count) + 1;
        scratch.buffer = new char[scratch.size];
      }

    string_integer *new_si = new string_integer(other_count);
    new_si->is_negative = op1->is_negative;
    divide_string_magnitudes(scratch.buffer, new_si->the_digits, this_digits,
                             other_digits, this_count, other_count, '0');
    justify_int_string(new_si->the_digits, other_count, '0');
    return new_si;
  }

static string_integer *bitwise_op_string_integers(string_integer *op1,
                                                  string_integer *op2,
                                                  binary_bit_ops op)
  {
    unsigned char *bits1;
    unsigned char *bits2;
    unsigned long size1;
    unsigned long size2;
    int carry1;
    int carry2;

    string_integer_to_binary(&bits1, &size1, &carry1, op1);
    string_integer_to_binary(&bits2, &size2, &carry2, op2);

    if (size1 < size2)
      {
        unsigned char *temp_bits = bits1;
        bits1 = bits2;
        bits2 = temp_bits;
        unsigned long temp_size = size1;
        size1 = size2;
        size2 = temp_size;
        int temp_carry = carry1;
        carry1 = carry2;
        carry2 = temp_carry;
      }

    unsigned char *follow1 = &(bits1[size1 - 1]);
    unsigned char *follow2 = &(bits2[size2 - 1]);

    switch (op)
      {
        case BBO_XOR:
            while (TRUE)
              {
                *follow1 = *follow1 ^ *follow2;
                if (follow2 == bits2)
                    break;
                --follow1;
                --follow2;
              }
            break;
        case BBO_AND:
            while (TRUE)
              {
                *follow1 = *follow1 & *follow2;
                if (follow2 == bits2)
                    break;
                --follow1;
                --follow2;
              }
            break;
        case BBO_IOR:
            while (TRUE)
              {
                *follow1 = *follow1 | *follow2;
                if (follow2 == bits2)
                    break;
                --follow1;
                --follow2;
              }
            break;
        default:
            assert(FALSE);
      }
    assert(follow1 >= bits1);

    if (carry2 == 1)
      {
        if (op == BBO_AND)
            follow1 = bits1;
        else
            carry1 = 1;
      }
    else
      {
        if (op == BBO_AND)
            carry1 = 0;
        else
            follow1 = bits1;
      }

    string_integer *result =
            binary_to_string_integer(follow1,
                                     (&(bits1[size1 - 1]) - follow1) + 1,
                                     carry1);
    delete[] bits1;
    delete[] bits2;
    return result;
  }

static string_integer *right_shift_string_integers(string_integer *op1,
                                                   string_integer *op2)
  {
    unsigned char *bits;
    unsigned long bit_size;
    int carry;

    string_integer_to_binary(&bits, &bit_size, &carry, op1);

    long right_amount;
    if (op2->is_negative)
      {
        char *old_digits = op2->the_digits;
        string_integer *left_si = new string_integer(strlen(old_digits));
        strcpy(left_si->the_digits, old_digits);
        left_si->is_negative = FALSE;

        string_integer *eight_si = string_integer_from_long(8);
        string_integer *div_si = div_string_integers(left_si, eight_si);
        string_integer *mod_si = mod_string_integers(left_si, eight_si);
        cut_link(left_si);
        cut_link(eight_si);

        assert(string_integer_fits_long(mod_si));
        long mod_long = long_from_string_integer(mod_si);
        cut_link(mod_si);
        assert((mod_long < 8) && (mod_long >= 0));
        right_amount = ((mod_long == 0) ? 0 : 8 - mod_long);

        if (!string_integer_fits_long(div_si))
            error_line(1, NULL, "out of memory address space");
        long add_digits = long_from_string_integer(div_si);
        cut_link(div_si);

        if (mod_long != 0)
          {
            if (add_digits == LONG_MAX)
                error_line(1, NULL, "out of memory address space");
            ++add_digits;
          }

        assert(add_digits > 0);
        unsigned long ulong_add = add_digits;
        if (bit_size > LONG_MAX - ulong_add)
            error_line(1, NULL, "out of memory address space");
        unsigned char *new_bits = new unsigned char[bit_size + ulong_add];
        strncpy((char *)new_bits, (char *)bits, bit_size);
        memset(&(new_bits[bit_size]), 0, ulong_add);
        delete[] bits;
        bits = new_bits;
        bit_size += ulong_add;
      }
    else
      {
        string_integer *eight_si = string_integer_from_long(8);
        string_integer *div_si = div_string_integers(op2, eight_si);
        string_integer *mod_si = mod_string_integers(op2, eight_si);
        cut_link(eight_si);

        assert(string_integer_fits_long(mod_si));
        right_amount = long_from_string_integer(mod_si);
        cut_link(mod_si);

        if (!string_integer_fits_long(div_si))
          {
            cut_link(div_si);
            return string_integer_from_long((carry == 1) ? -1 : 0);
          }
        long cut_digits = long_from_string_integer(div_si);
        cut_link(div_si);

        assert(cut_digits >= 0);
        if (((unsigned long)cut_digits) >= bit_size)
            return string_integer_from_long((carry == 1) ? -1 : 0);
        bit_size -= cut_digits;
      }
    assert((right_amount >= 0) && (right_amount < 8));

    if (right_amount != 0)
      {
        unsigned char *follow_bits = bits;
        unsigned char *bit_end = &(bits[bit_size - 1]);
        unsigned char mask = ~((~((unsigned)0)) << right_amount);
        unsigned char temp_byte = ((carry == 1) ? mask : 0);
        while (TRUE)
          {
            unsigned char old_byte = *follow_bits;
            *follow_bits =
                    (old_byte >> right_amount) |
                    (temp_byte << (8 - right_amount));
            temp_byte = old_byte & mask;
            if (follow_bits == bit_end)
                break;
            ++follow_bits;
          }
      }

    string_integer *result = binary_to_string_integer(bits, bit_size, carry);
    delete[] bits;
    return result;
  }

static boolean string_integer_fits_long(string_integer *the_si)
  {
    long long_total = 0;

    char *digit_follow = the_si->the_digits;
    if (the_si->is_negative)
      {
        while (*digit_follow != 0)
          {
            long this_digit = *digit_follow - '0';
            if (long_total <
                (((LONG_MIN + this_digit) / 10) +
                 ((((LONG_MIN + this_digit) % 10) > 0) ? 1 : 0)))
              {
                return FALSE;
              }
            long_total *= 10;
            long_total -= this_digit;
            ++digit_follow;
          }
      }
    else
      {
        while (*digit_follow != 0)
          {
            long this_digit = *digit_follow - '0';
            if (long_total > ((LONG_MAX - this_digit) / 10))
                return FALSE;
            long_total *= 10;
            long_total += this_digit;
            ++digit_follow;
          }
      }
    return TRUE;
  }

static long long_from_string_integer(string_integer *the_si)
  {
    long long_total = 0;

    char *digit_follow = the_si->the_digits;
    if (the_si->is_negative)
      {
        while (*digit_follow != 0)
          {
            long this_digit = *digit_follow - '0';
            assert(long_total >=
                   (((LONG_MIN + this_digit) / 10) +
                    ((((LONG_MIN + this_digit) % 10) > 0) ? 1 : 0)));
            long_total *= 10;
            long_total -= this_digit;
            ++digit_follow;
          }
      }
    else
      {
        while (*digit_follow != 0)
          {
            long this_digit = *digit_follow - '0';
            assert(long_total <= ((LONG_MAX - this_digit) / 10));
            long_total *= 10;
            long_total += this_digit;
            ++digit_follow;
          }
      }
    return long_total;
  }

static string_integer *string_integer_from_long(long the_long)
  {
    static char buffer[sizeof(unsigned long) * CHAR_BIT / 3 + 2];

    char *digit_follow = buffer;
    long remainder = the_long;
    if (the_long < 0)
      {
        while (remainder < 0)
          {
            long this_digit = remainder % 10;
            remainder /= 10;
            if (this_digit > 0)
              {
                this_digit -= 10;
                ++remainder;
              }
            *digit_follow = ((char)(-this_digit)) + '0';
            ++digit_follow;
          }
      }
    else
      {
        while (remainder > 0)
          {
            *digit_follow = ((char)(remainder % 10)) + '0';
            remainder /= 10;
            ++digit_follow;
          }
      }

    unsigned long digits = digit_follow - buffer;

    string_integer *new_si = new string_integer(digits);
    new_si->is_negative = (the_long < 0);

    char *follow_back = new_si->the_digits;
    while (digit_follow != buffer)
      {
        --digit_follow;
        *follow_back = *digit_follow;
        ++follow_back;
      }
    assert(follow_back == &(new_si->the_digits[digits]));
    *follow_back = 0;
    return new_si;
  }

static string_integer *string_integer_from_unsigned_long(
        unsigned long the_ulong)
  {
    static char buffer[sizeof(unsigned long) * CHAR_BIT / 3 + 2];

    char *digit_follow = buffer;
    unsigned long remainder = the_ulong;
    while (remainder > 0)
      {
        *digit_follow = ((char)(remainder % 10)) + '0';
        remainder /= 10;
        ++digit_follow;
      }

    unsigned long digits = digit_follow - buffer;

    string_integer *new_si = new string_integer(digits);
    new_si->is_negative = FALSE;

    char *follow_back = new_si->the_digits;
    while (digit_follow != buffer)
      {
        --digit_follow;
        *follow_back = *digit_follow;
        ++follow_back;
      }
    assert(follow_back == &(new_si->the_digits[digits]));
    *follow_back = 0;
    return new_si;
  }

static void string_integer_to_binary(unsigned char **result,
                                     unsigned long *result_size,
                                     int *carry_bit,
                                     string_integer *the_si)
  {
    static thescratcher scratch;

    string_integer *this_si = the_si;
    add_link(this_si);

    if (the_si->is_negative)
      {
        string_integer *one_si = new string_integer(1);
        one_si->is_negative = FALSE;
        one_si->the_digits[0] = '1';
        one_si->the_digits[1] = 0;
        string_integer *sum_si = add_string_integers(this_si, one_si);
        cut_link(this_si);
        cut_link(one_si);
        this_si = sum_si;
      }

    char *this_digits = this_si->the_digits;
    unsigned long this_size = strlen(this_digits);

    *carry_bit = (the_si->is_negative ? 1 : 0);

    if (this_size == 0)
      {
        unsigned char *bits = new unsigned char[1];
        if (the_si->is_negative)
            bits[0] = ~0;
        else
            bits[0] = 0;
        *result = bits;
        *result_size = 1;
        cut_link(this_si);
        return;
      }

    if ((scratch.buffer == NULL) || (scratch.size < this_size * 20))
      {
        if (scratch.buffer != NULL)
            delete[] scratch.buffer;
        scratch.size = this_size * 20;
        scratch.buffer = new char[scratch.size];
      }

    base_convert(scratch.buffer, &(scratch.buffer[this_size]), this_digits, this_size, 10,
                 16, '0', 0);

    char *follow_buffer = scratch.buffer;
    char *end_buffer = &(scratch.buffer[this_size - 1]);
    while ((*follow_buffer == 0) && (follow_buffer < end_buffer))
        ++follow_buffer;

    unsigned long final_size = (end_buffer - follow_buffer) / 2 + 1;
    *result_size = final_size;
    unsigned char *bits = new unsigned char[final_size];
    *result = bits;
    unsigned char *follow_bits = bits;
    if (the_si->is_negative)
      {
        if ((end_buffer - follow_buffer) % 2 == 0)
          {
            unsigned char digit = *follow_buffer;
            *follow_bits = ~digit;
            if (follow_buffer < end_buffer)
              {
                ++follow_bits;
                ++follow_buffer;
              }
          }
        if (follow_buffer < end_buffer)
          {
            while (TRUE)
              {
                unsigned char digit1 = *follow_buffer;
                ++follow_buffer;
                unsigned char digit2 = *follow_buffer;
                *follow_bits = ~((digit1 << 4) | digit2);
                if (follow_buffer == end_buffer)
                    break;
                ++follow_bits;
                ++follow_buffer;
              }
          }
      }
    else
      {
        if ((end_buffer - follow_buffer) % 2 == 0)
          {
            unsigned char digit = *follow_buffer;
            *follow_bits = digit;
            if (follow_buffer < end_buffer)
              {
                ++follow_bits;
                ++follow_buffer;
              }
          }
        if (follow_buffer < end_buffer)
          {
            while (TRUE)
              {
                unsigned char digit1 = *follow_buffer;
                ++follow_buffer;
                unsigned char digit2 = *follow_buffer;
                *follow_bits = ((digit1 << 4) | digit2);
                if (follow_buffer == end_buffer)
                    break;
                ++follow_bits;
                ++follow_buffer;
              }
          }
      }
    assert(follow_bits == &(bits[final_size - 1]));
    cut_link(this_si);
  }

static string_integer *binary_to_string_integer(unsigned char *bits,
                                                unsigned long bit_size,
                                                int carry_bit)
  {
    assert(bit_size > 0);

    thescratcher scratch;

    if ((scratch.buffer == NULL) || (scratch.size < bit_size * 20))
      {
        if (scratch.buffer != NULL)
            delete[] scratch.buffer;
        scratch.size = bit_size * 20;
        scratch.buffer = new char[scratch.size];
      }

    unsigned char *bit_follow = bits;
    unsigned char *bit_end = &(bits[bit_size - 1]);
    char *buf_follow = scratch.buffer;
    if (carry_bit == 1)
      {
        while (TRUE)
          {
            unsigned char this_char = *bit_follow;
            *buf_follow = (~(this_char >> 4)) & 0xf;
            ++buf_follow;
            *buf_follow = ((~this_char) & 0xf);
            if (bit_follow == bit_end)
                break;
            ++bit_follow;
            ++buf_follow;
          }
      }
    else
      {
        while (TRUE)
          {
            unsigned char this_char = *bit_follow;
            *buf_follow = (this_char >> 4);
            ++buf_follow;
            *buf_follow = (this_char & 0xf);
            if (bit_follow == bit_end)
                break;
            ++bit_follow;
            ++buf_follow;
          }
      }
    assert(buf_follow == &(scratch.buffer[bit_size * 2 - 1]));

    string_integer *new_si = new string_integer(bit_size * 4);
    new_si->is_negative = (carry_bit == 1);
    base_convert(new_si->the_digits, &(scratch.buffer[bit_size * 2]), scratch.buffer,
                 bit_size * 2, 16, 10, 0, '0');
    justify_int_string(new_si->the_digits, bit_size * 4, '0');
    if (carry_bit == 1)
      {
        string_integer *neg_one_si = new string_integer(1);
        neg_one_si->is_negative = TRUE;
        neg_one_si->the_digits[0] = '1';
        neg_one_si->the_digits[1] = 0;
        string_integer *sum_si = add_string_integers(new_si, neg_one_si);
        cut_link(new_si);
        cut_link(neg_one_si);
        return sum_si;
      }
    return new_si;
  }

static boolean string_magnitude_is_less(char *op1, char *op2,
                                        unsigned long op1_len,
                                        unsigned long op2_len, int offset)
  {
    char *new_op1 = op1;
    char *new_op2 = op2;
    unsigned long new_op1_len = op1_len;
    unsigned long new_op2_len = op2_len;

    while ((new_op1_len > 0) && (*new_op1 == offset))
      {
        --new_op1_len;
        ++new_op1;
      }

    while ((new_op2_len > 0) && (*new_op2 == offset))
      {
        --new_op2_len;
        ++new_op2;
      }

    if (new_op1_len < new_op2_len)
        return TRUE;
    if (new_op1_len > new_op2_len)
        return FALSE;
    return (strncmp(new_op1, new_op2, new_op1_len) < 0);
  }

/* This assumes that op1_len >= op2_len.  The result will have length
 * op1_len + 1. */
static void add_string_magnitudes(char *result, char *op1, char *op2,
                                  unsigned long op1_len,
                                  unsigned long op2_len, int base, int offset)
  {
    assert((base >= 2) && (base - 1 <= CHAR_MAX - offset) &&
           (base <= INT_MAX / 2));

    char *sum_follow = &(result[op1_len + 1]);
    char *op1_follow = &(op1[op1_len]);
    char *op2_follow = &(op2[op2_len]);

    int remainder = 0;
    while (op2_follow != op2)
      {
        --op1_follow;
        --op2_follow;
        --sum_follow;
        remainder += (*op1_follow - offset) + (*op2_follow - offset);
        *sum_follow = (remainder % base) + offset;
        remainder /= base;
      }

    while ((op1_follow != op1) && (remainder != 0))
      {
        --op1_follow;
        --sum_follow;
        remainder += (*op1_follow - offset);
        *sum_follow = (remainder % base) + offset;
        remainder /= base;
      }

    while (op1_follow != op1)
      {
        --op1_follow;
        --sum_follow;
        *sum_follow = *op1_follow;
      }

    assert(sum_follow == &(result[1]));
    assert((remainder == 0) || (remainder == 1));
    *result = remainder + offset;
  }

/* assumes that adding op2 will not result in overflow */
static void add_to_string_magnitude(char *result, char *op2,
                                    unsigned long result_len,
                                    unsigned long op2_len, int base,
                                    int offset)
  {
    assert((base >= 2) && (base - 1 <= CHAR_MAX - offset) &&
           (base <= INT_MAX / 2));

    char *sum_follow = &(result[result_len]);
    char *op2_follow = &(op2[op2_len]);

    char *op2_target = op2;
    while (((unsigned long)(op2_follow - op2_target)) > result_len)
      {
        assert(*op2_target == offset);
        ++op2_target;
      }

    int remainder = 0;
    while (op2_follow != op2_target)
      {
        --op2_follow;
        --sum_follow;
        remainder += (*sum_follow - offset) + (*op2_follow - offset);
        *sum_follow = (remainder % base) + offset;
        remainder /= base;
      }

    while (remainder != 0)
      {
        assert(sum_follow != result);
        --sum_follow;
        remainder += (*sum_follow - offset);
        *sum_follow = (remainder % base) + offset;
        remainder /= base;
      }
  }

/* This assumes that op1 >= op2.  The result will have length op1_len.
 */
static void subtract_string_magnitudes(char *result, char *op1, char *op2,
                                       unsigned long op1_len,
                                       unsigned long op2_len, int base,
                                       int offset)
  {
    assert((base >= 2) && (base - 1 <= CHAR_MAX - offset));

    if (INT_MAX + INT_MIN > 0)
      {
        assert(base <= NEG_INT_MIN);
      }
    else if (INT_MAX + INT_MIN < 0)
      {
        assert(-base >= INT_MIN);
      }

    char *sum_follow = &(result[op1_len]);
    char *op1_follow = &(op1[op1_len]);
    char *op2_follow = &(op2[op2_len]);
    assert(op1_len >= op2_len);

    int remainder = 0;
    while (op2_follow != op2)
      {
        --op1_follow;
        --op2_follow;
        --sum_follow;
        remainder += (*op1_follow - offset) - (*op2_follow - offset);
        if (remainder < 0)
          {
            *sum_follow = (remainder + base) + offset;
            remainder = -1;
          }
        else
          {
            *sum_follow = remainder + offset;
            remainder = 0;
          }
      }

    while ((op1_follow != op1) && (remainder != 0))
      {
        --op1_follow;
        --sum_follow;
        remainder += (*op1_follow - offset);
        if (remainder < 0)
          {
            *sum_follow = (remainder + base) + offset;
            remainder = -1;
          }
        else
          {
            *sum_follow = remainder + offset;
            remainder = 0;
          }
      }

    while (op1_follow != op1)
      {
        --op1_follow;
        --sum_follow;
        *sum_follow = *op1_follow;
      }

    assert(sum_follow == &(result[0]));
    assert(remainder == 0);
  }

/* This assumes that op1_len >= op2_len and that the space in scratch
 * is >= (9 * op1_len).  The result will have length op1_len +
 * op2_len. */
static void multiply_string_magnitudes(char *result, char *scratch, char *op1,
                                       char *op2, unsigned long op1_len,
                                       unsigned long op2_len, int base,
                                       int offset)
  {
    assert((base >= 2) && (base - 1 <= CHAR_MAX - offset));
    assert(base <= INT_MAX / 2);
    assert(base - 1 <= (INT_MAX - 2 * (base - 1))/(base - 1));
    if (INT_MAX + INT_MIN > 0)
      {
        assert(base <= NEG_INT_MIN);
      }
    else if (INT_MAX + INT_MIN < 0)
      {
        assert(-base >= INT_MIN);
      }

    if (op2_len == 0)
      {
        strncpy(result, op1, op1_len);
        return;
      }

    if (op1_len <= CUTOFF)
      {
        char *result_start = &(result[op1_len + (op2_len - 1)]);
        char *result_follow = result_start;
        char *op1_follow = &(op1[op1_len - 1]);
        char *op2_follow = &(op2[op2_len - 1]);
        int carry = 0;
        int this_digit = (*op2_follow - offset);
        if (this_digit == 0)
          {
            *result_follow = offset;
          }
        else
          {
            while (TRUE)
              {
                carry += (*op1_follow - offset) * this_digit;
                *result_follow = ((char)(carry % base)) + offset;
                carry /= base;
                assert(carry <= base - 2);
                if (op1_follow == op1)
                    break;
                assert(result_follow > result);
                --op1_follow;
                --result_follow;
              }
            if (carry != 0)
              {
                assert(result_follow > result);
                --result_follow;
                *result_follow = carry + offset;
              }
          }
        while (result_follow > result)
          {
            --result_follow;
            *result_follow = offset;
          }

        while (op2_follow > op2)
          {
            --op2_follow;
            assert(result_start > result);
            --result_start;
            result_follow = result_start;
            op1_follow = &(op1[op1_len - 1]);
            carry = 0;
            this_digit = (*op2_follow - offset);
            if (this_digit != 0)
              {
                while (TRUE)
                  {
                    carry += (*result_follow - offset);
                    carry += (*op1_follow - offset) * this_digit;
                    *result_follow = ((char)(carry % base)) + offset;
                    carry /= base;
                    assert(carry <= base - 1);
                    if (op1_follow == op1)
                        break;
                    assert(result_follow > result);
                    --op1_follow;
                    --result_follow;
                  }
                if (carry != 0)
                  {
                    assert(result_follow > result);
                    --result_follow;
                    *result_follow = carry + offset;
                  }
              }
          }
      }
    else
      {
        unsigned long piece_length =
                (op1_len / 2) + (((op1_len % 2) == 0) ? 0 : 1);
        if (op2_len <= piece_length)
          {
	    // DLH comments
	    // piece_length = roundup(op1_len/2)
	    // Let's figure this out.
	    // we want to 
	    // 1) partition op1 into (v1 x BASE^n  + v2)
	    // 2) multiply each part by op2.
	    // 3) add them in the end.
	    // example is base 10.
	    //   22222 222222
	    //  x           5
	    //   ------------
	    //  111110 000000
	    // +     1 111110
	    //  -------------
	    //  111111 111110
	    // 
	    // build a result with the right number of zeros.
	    // assume there are op1_len + op2_len digits available
	    // with a \0 already at the end?
	    // I've tested this against bc on Factorial and FactorialSquared
	    // from 1 to 300.
	    memset(result, offset, op1_len + op2_len);
	    multiply_string_magnitudes(&(result[0]),
					 scratch,
				       op1, op2,
				       op1_len - piece_length,
				       op2_len, base, offset);
            multiply_string_magnitudes(scratch,
                                       &(scratch[op2_len + piece_length]),
				       &(op1[op1_len - piece_length]), 
				       op2,
				       piece_length, op2_len,
				       base, offset);
            add_to_string_magnitude(result, scratch,
				    op1_len + op2_len,
                                    op2_len + piece_length, base,
                                    offset);
            return;
          }
        multiply_string_magnitudes(result, scratch, op1, op2,
                                   op1_len - piece_length,
                                   op2_len - piece_length, base, offset);
        multiply_string_magnitudes(&(result[op1_len + op2_len -
                                            (2 * piece_length)]), scratch,
                                   &(op1[op1_len - piece_length]),
                                   &(op2[op2_len - piece_length]),
                                   piece_length, piece_length, base, offset);
        add_string_magnitudes(&(scratch[2 * piece_length + 2]),
                              &(op1[op1_len - piece_length]), op1,
                              piece_length, op1_len - piece_length, base,
                              offset);
        add_string_magnitudes(&(scratch[3 * piece_length + 3]),
                              &(op2[op2_len - piece_length]), op2,
                              piece_length, op2_len - piece_length, base,
                              offset);
        multiply_string_magnitudes(scratch, &(scratch[4 * piece_length + 4]),
                                   &(scratch[2 * piece_length + 2]),
                                   &(scratch[3 * piece_length + 3]),
                                   piece_length + 1, piece_length + 1, base,
                                   offset);
        assert(scratch[0] == offset);
        subtract_string_magnitudes(&(scratch[2 * piece_length + 2]),
                                   &(scratch[1]), result, 2 * piece_length + 1,
                                   op1_len + op2_len - (2 * piece_length),
                                   base, offset);
        subtract_string_magnitudes(scratch, &(scratch[2 * piece_length + 2]),
                                   &(result[op1_len + op2_len -
                                            (2 * piece_length)]),
                                   2 * piece_length + 1, 2 * piece_length,
                                   base, offset);
        add_to_string_magnitude(result, scratch,
                                op1_len + op2_len - piece_length,
                                2 * piece_length + 1, base, offset);
      }
  }

struct remainder_t {
	remainder_t ( ) : remainder(0), alt_remainder(0) {
		for ( int i = 0; i < 9; ++i ) {
			factors[i] = 0;
		}
	}
	~remainder_t() {
		for (int index = 0; index < 9; ++index) {
			delete [] factors[index]; factors[index] = 0;
		}
	
		delete[] remainder; remainder = 0;
		delete[] alt_remainder; alt_remainder = 0;
	}
    char *factors[9];
    unsigned long factor_length;
    char *remainder;
    char *alt_remainder;
    unsigned long remainder_length;
};

/* This assumes that op1_len >= op2_len.  div_result will have length
 * op1_len - op2_len + 1, and mod_result will have length op2_len. */
static void divide_string_magnitudes(char *div_result, char *mod_result,
                                     char *op1, char *op2,
                                     unsigned long op1_len,
                                     unsigned long op2_len, int offset)
  {
     static remainder_t rem;

    if ((rem.factors[0] == NULL) || (rem.factor_length < op1_len + 1))
      {
        if (rem.factors[0] != NULL)
          {
            for (int index = 0; index < 9; ++index)
                delete[] rem.factors[index];
          }
        rem.factor_length = op1_len + 1;
        for (int index = 0; index < 9; ++index)
            rem.factors[index] = new char[rem.factor_length];
      }

    if ((rem.remainder == NULL) || (rem.remainder_length < op1_len))
      {
        if (rem.remainder != NULL)
          {
            delete[] rem.remainder;
            delete[] rem.alt_remainder;
          }
        rem.remainder_length = op1_len;
        rem.remainder = new char[rem.remainder_length];
        rem.alt_remainder = new char[rem.remainder_length];
      }

    for (int index = 0; index < 9; ++index)
      {
        if (op1_len > op2_len)
            memset(&(rem.factors[index][op2_len + 1]), offset, op1_len - op2_len);
        int carry = 0;
        char *follow_factor = &(rem.factors[index][op2_len]);
        char *follow_op2 = &(op2[op2_len - 1]);
        while (TRUE)
          {
            carry += (*follow_op2 - offset) * (index + 1);
            *follow_factor = ((char)(carry % 10)) + offset;
            carry /= 10;
            assert(carry <= 8);
            --follow_factor;
            if (follow_op2 == op2)
                break;
            assert(follow_factor > rem.factors[index]);
            --follow_op2;
          }
        assert(follow_factor == rem.factors[index]);
        *follow_factor = carry + offset;
      }

    memcpy(rem.remainder, op1, op1_len);

    char *follow_div = div_result;
    char *div_end = &(div_result[op1_len - op2_len]);
    unsigned long this_factor_length = op1_len + 1;
    while (TRUE)
      {
        int this_digit;
        if (string_magnitude_is_less(rem.remainder, rem.factors[4], op1_len,
                                     this_factor_length, offset))
          {
            if (string_magnitude_is_less(rem.remainder, rem.factors[2], op1_len,
                                         this_factor_length, offset))
              {
                if (string_magnitude_is_less(rem.remainder, rem.factors[1], op1_len,
                                             this_factor_length, offset))
                  {
                    if (string_magnitude_is_less(rem.remainder, rem.factors[0],
                                                 op1_len, this_factor_length,
                                                 offset))
                      {
                        this_digit = 0;
                      }
                    else
                      {
                        this_digit = 1;
                      }
                  }
                else
                  {
                    this_digit = 2;
                  }
              }
            else
              {
                if (string_magnitude_is_less(rem.remainder, rem.factors[3], op1_len,
                                             this_factor_length, offset))
                  {
                    this_digit = 3;
                  }
                else
                  {
                    this_digit = 4;
                  }
              }
          }
        else
          {
            if (string_magnitude_is_less(rem.remainder, rem.factors[7], op1_len,
                                         this_factor_length, offset))
              {
                if (string_magnitude_is_less(rem.remainder, rem.factors[6], op1_len,
                                             this_factor_length, offset))
                  {
                    if (string_magnitude_is_less(rem.remainder, rem.factors[5],
                                                 op1_len, this_factor_length,
                                                 offset))
                      {
                        this_digit = 5;
                      }
                    else
                      {
                        this_digit = 6;
                      }
                  }
                else
                  {
                    this_digit = 7;
                  }
              }
            else
              {
                if (string_magnitude_is_less(rem.remainder, rem.factors[8], op1_len,
                                             this_factor_length, offset))
                  {
                    this_digit = 8;
                  }
                else
                  {
                    this_digit = 9;
                  }
              }
          }

        *follow_div = this_digit + offset;
        if (this_digit > 0)
          {
            unsigned fact_len = this_factor_length;
            char *this_factor = rem.factors[this_digit - 1];
            while ((fact_len > 1) && (*this_factor == offset))
              {
                --fact_len;
                ++this_factor;
              }
            subtract_string_magnitudes(rem.alt_remainder, rem.remainder, this_factor,
                                       op1_len, fact_len, 10, offset);
            char *temp = rem.alt_remainder;
            rem.alt_remainder = rem.remainder;
            rem.remainder = temp;
          }

        if (follow_div == div_end)
            break;
        ++follow_div;
        assert(this_factor_length > op2_len + 1);
        --this_factor_length;
      }
    assert(this_factor_length == op2_len + 1);
    for (unsigned long ul_index = 0; ul_index < op1_len - op2_len; ++ul_index)
        assert(rem.remainder[ul_index] == offset);

    memcpy(mod_result, &(rem.remainder[op1_len - op2_len]), op2_len);
  }

static void justify_int_string(char *string, unsigned long size, int offset)
{
    char *zero_follow = string;
    while ((zero_follow + 1 < &(string[size])) &&
           (*zero_follow == offset))
      {
        ++zero_follow;
      }
    unsigned long zero_count = zero_follow - string;
    if (zero_count > 0)
        memmove(string, zero_follow, size - zero_count);
    string[size - zero_count] = 0;
}

/* This assumes that the space in scratch is >= (9 * <result size>).  The
 * result will have length old_size * ceil(log_new_base(old_base)). */
static void base_convert(char *new_string, char *scratch, char *old_string,
                         unsigned long old_size, int old_base, int new_base,
                         int old_offset, int new_offset)
  {
    assert((new_base >= 2) && (new_base - 1 <= CHAR_MAX - new_offset));
    assert((old_base >= 2) && (old_base - 1 <= CHAR_MAX - old_offset));
    assert(old_base <= LONG_MAX / old_base);

    if (new_base == old_base)
      {
        strcpy(new_string, old_string);
        return;
      }

    if (old_size == 0)
        return;

    /* set blow_up_factor = ceil(log_new_base(old_base)) */
    int blow_up = blow_up_factor(old_base, new_base);

    if (old_size <= 2)
      {
        long old_digit = *old_string - old_offset;
        assert(old_digit < old_base);
        if (old_size == 2)
          {
            assert(old_string[1] - old_offset < old_base);
            old_digit *= old_base;
            old_digit += (old_string[1] - old_offset);
          }
        char *follow_new = &(new_string[old_size * blow_up - 1]);
        while (TRUE)
          {
            *follow_new = ((char)(old_digit % new_base)) + new_offset;
            old_digit /= new_base;
            if (follow_new == new_string)
                break;
            --follow_new;
          }
        assert(old_digit == 0);
        return;
      }

    unsigned long right_size = old_size / 2;
    unsigned long left_size = old_size - right_size;
    char *s1 = &(scratch[(right_size + 1) * blow_up]);
    char *s2 = &(s1[right_size + 1]);
    char *s3 = &(s2[left_size * blow_up]);

    base_convert(s2, s3, old_string, left_size, old_base, new_base,
                 old_offset, new_offset);

    /* set s1 = "1000...00" */
    s1[0] = 1 + old_offset;
    char *follow_s1 = s1;
    char *end_s1 = &(s1[right_size]);
    while (follow_s1 != end_s1)
      {
        ++follow_s1;
        *follow_s1 = old_offset;
      }

    base_convert(scratch, s3, s1, right_size + 1, old_base, new_base,
                 old_offset, new_offset);
    for (int index = 0; index < blow_up; ++index)
        assert(scratch[index] == new_offset);
    multiply_string_magnitudes(new_string, s3, s2, &(scratch[blow_up]),
                               left_size * blow_up, right_size * blow_up,
                               new_base, new_offset);
    base_convert(scratch, &(scratch[right_size * blow_up]),
                 &(old_string[left_size]), right_size, old_base, new_base,
                 old_offset, new_offset);
    add_to_string_magnitude(new_string, scratch, old_size * blow_up,
                            right_size * blow_up, new_base, new_offset);
  }

/* return ceil(log_new_base(old_base)) */
static int blow_up_factor(int old_base, int new_base)
  {
    int result = 0;
    int remainder = old_base - 1;
    while (remainder > 0)
      {
        remainder /= new_base;
        ++result;
      }
    return result;
  }

static i_rational read_digits_with_decimal_point(const char **location,
                                                 int base)
  {
    i_integer pre_decimal_point(*location, base);

    const char *follow_string = *location;

    while (isspace(*follow_string))
        ++follow_string;

    if (*follow_string == '-')
        ++follow_string;

    while (isspace(*follow_string))
        ++follow_string;

    while (*follow_string != 0)
      {
        int this_digit = *follow_string;
        if ((this_digit >= '0') && (this_digit <= '9'))
            this_digit -= '0';
        else if ((this_digit >= 'a') && (this_digit <= 'z'))
            this_digit -= ('a' - 10);
        else if ((this_digit >= 'A') && (this_digit <= 'Z'))
            this_digit -= ('A' - 10);
        else
            break;

        if (this_digit >= base)
            break;

        ++follow_string;
      }

    if (*follow_string != '.')
      {
        *location = follow_string;
        return pre_decimal_point;
      }

    const char *decimal_place = follow_string;

    ++follow_string;

    while (*follow_string != 0)
      {
        int this_digit = *follow_string;
        if ((this_digit >= '0') && (this_digit <= '9'))
            this_digit -= '0';
        else if ((this_digit >= 'a') && (this_digit <= 'z'))
            this_digit -= ('a' - 10);
        else if ((this_digit >= 'A') && (this_digit <= 'Z'))
            this_digit -= ('A' - 10);
        else
            break;

        if (this_digit >= base)
            break;

        ++follow_string;
      }

    *location = follow_string;

    if (follow_string == decimal_place + 1)
        return pre_decimal_point;

    i_integer post_decimal_point(decimal_place + 1, base);

    i_integer denominator = 1;
    while (follow_string > decimal_place + 1)
      {
        denominator *= base;
        --follow_string;
      }

    return i_rational(pre_decimal_point) +
           i_rational(post_decimal_point, denominator);
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
