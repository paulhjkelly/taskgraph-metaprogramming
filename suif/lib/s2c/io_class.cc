/* file "io_class.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file contains the implementation of the io_class class and
 *  some sub-classes, which are used by s2c, the SUIF to C converter.
 */

#pragma implementation "io_class.h"

#include <suif1.h>
#include "io_class.h"
#include <cstdarg>
#include <cstring>

static FILE *null_file = NULL;

int file_io::printf(const char *format, ...)
  {
    va_list ap;
    va_start(ap, format);
    int result = vfprintf(fp, format, ap);
    va_end(ap);
    return result;
  }

void file_io::print_ii(const i_integer &ii)
  {
    ii.print(fp);
  }


void string_io::initialize_io(void)
  {
    if (null_file == NULL)
      {
#ifndef WIN32
        null_file = fopen("/dev/null", "w");
#else
        null_file = fopen("NUL", "w");
#endif
        assert(null_file != NULL);
      }
  }

void string_io::extend(int amount)
  {
    int new_size = current_size + amount;
    char *new_array = new char[new_size];
    memcpy(new_array, *sp, current_size);
    delete[] *sp;
    *sp = new_array;
    current_size = new_size;
  }

int string_io::printf(const char *format, ...)
  {
    va_list ap;
    va_start(ap, format);

    int num_to_print = vfprintf(null_file, format, ap);
    if (num_to_print + current_position >= current_size)
        extend(num_to_print + 50);
    int result = vsprintf(&((*sp)[current_position]), format, ap);
    current_position += num_to_print;

    va_end(ap);
    return result;
  }

int string_io::io_putc(int c)
  {
    if (current_position + 1 >= current_size)
        extend(51);
    (*sp)[current_position] = c;
    ++current_position;
    (*sp)[current_position] = 0;
    return c;
  }

void string_io::print_ii(const i_integer &ii)
  {
    i_integer print_length = ii.written_length();
    if (!print_length.is_c_unsigned_long())
        error_line(1, NULL, "out of memory address space");
    unsigned long ul_length = ii.c_unsigned_long();
    if (ul_length + current_position >= current_size)
        extend(ul_length + 50);
    ii.write(&((*sp)[current_position]));
    current_position += ul_length;
  }
