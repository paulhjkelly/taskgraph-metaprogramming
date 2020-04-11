/*  SUIF Input/Output Stream Classes */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef SUIFSTREAMS_H
#define SUIFSTREAMS_H

#pragma interface

RCS_HEADER(suifstreams_h,
    "$Id$")

#define CACHE_SZ 509


/*
 *  SUIF streams are only used within the SUIF library; users can only read
 *  and write entire procedures and never need to access the streams directly.
 *  The streams are only capable of reading and writing integers and character
 *  strings.  When a stream is closed, the current file position is recorded
 *  so that if it is reopened, the position can be restored to its former
 *  value.  This allows us to have an arbitrary number of streams, as long
 *  as all of them are not open at the same time.
 */

class base_stream {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    base_stream(const base_stream &);
    base_stream &operator=(const base_stream &);

protected:
    FILE *fp;
    long pos;
    const char *nm;
    const char *cache[CACHE_SZ];

    int hash(const char *x);
    void check_open();

public:
    base_stream(const char *n);
    virtual ~base_stream()		{ close(); }

    const char *name()			{ return nm; }
    long getpos();
    void setpos(long p);

    boolean is_open()			{ return (fp != NULL); }
    virtual void open() = 0;
    virtual void close();
    void flush_cache();
};


/*
 *  Each SUIF file contains a header with a magic number and a version number.
 *  When an output stream is opened for the first time, the header is
 *  automatically written out.  Integers are always written with the native
 *  byte ordering, assuming that it is most likely for the file to be used
 *  next on the same machine.
 */

class out_stream : public base_stream {
    void write_chunk(const void *cp, size_t sz);

public:
    out_stream(const char *n) : base_stream(n) { }

    void open();
    void write_byte(char c);
    void write_int(int i);
    void write_string(const char *s);
    void write_cached(const char *s);
};


/*
 *  Input streams.  When an input stream is opened for the first time, the
 *  header is automatically read.  The magic number is used to detect files
 *  written with the reverse byte order, so that the swap_bytes flag can be
 *  set to indicate that the byte order must be reversed when reading integers.
 */

class in_stream : public base_stream {

    boolean swap_bytes;
    unsigned long write_int_size;
    char *int_read_buffer;
    char *decimal_buffer;

    void read_chunk(void *cp, size_t sz);

public:
    in_stream(const char *n) : base_stream(n), swap_bytes(FALSE),
	write_int_size(0), int_read_buffer(0), decimal_buffer(0) 
	{ }
    ~in_stream(void);

    void open();
    char read_byte();
    int read_int(boolean *overflow = NULL, char **decimal = NULL);
    const char *read_string();
    const char *read_cached();
};


enum suif_check_result { SF_CANT_OPEN, SF_NOT_SUIF, SF_SUIF_FILE };

extern suif_check_result check_for_suif_file(char *file_name);

#endif /* SUIFSTREAMS_H */
