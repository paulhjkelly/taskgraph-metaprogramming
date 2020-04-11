/* file "io_class.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file contains the interface to the io_class class and some
 *  sub-classes, which are used by s2c, the SUIF to C converter.
 */

#include <useful/inumbers.h>
#include <cstdio>

#pragma interface

class io_class
  {
public:
    virtual int printf(const char *format, ...) = 0;
    virtual int io_putc(int c) = 0;
    virtual void print_ii(const i_integer &ii) = 0;
    virtual ~io_class() {}
  };

/*
 *  The file_io class simply prints everything to the file for which
 *  it was instantiated.
 */
class file_io : public io_class
  {
private:
    FILE *fp;

public:
    file_io(FILE *new_fp) { fp = new_fp; }
    virtual ~file_io(void) { }

    int printf(const char *format, ...);
    int io_putc(int c) { return fputc(c, fp); }
    void print_ii(const i_integer &ii);
  };

/*
 *  The string_io class is invoked with a location into which to write
 *  a pointer to a string.  After the destructor is called, that
 *  pointer is changed to point to a newly allocated string into which
 *  has been written everything specified by calls to the printing
 *  method.  Note that the pointer should never be used between the
 *  time the constructor is called and the time the destructor is
 *  called because its value will change to point to newly allocated
 *  arrays as necessary to accomodate newly printed data.
 */
class string_io : public io_class
  {
private:
    char **sp;
    unsigned current_size;
    unsigned current_position;

    static void initialize_io(void);
    void extend(int amount);

public:
    string_io(char **new_sp)
      {
        initialize_io();
        sp = new_sp;
        current_size = 100;
        current_position = 0;
        *sp = new char[current_size];
      }
    virtual ~string_io(void) { }

    int printf(const char *format, ...);
    int io_putc(int c);
    void print_ii(const i_integer &ii);
  };
