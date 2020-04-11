/*  SUIF Input/Output Streams */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "suifstreams.h"

#define RCS_BASE_FILE suifstreams_cc

#include "misc.h"
#include "stringtable.h"
#include "suifstreams.h"
#include "glist.h"
#include "dlist.h"
#include "initsuif.h"
#include <cstring>
#include <cerrno>
#include <cctype>

// unistd.h is not available on Win32
#ifndef _WIN32
#include <unistd.h>  /* used for SEEK_SET in SunOS */
#endif

RCS_BASE(
    "$Id$")

static char SUIF_magic_word[4] = {0x50, 0x69, 0x67, 0x73};
static char SUIF_reverse_magic_word[4] = {0x73, 0x67, 0x69, 0x50};
static char SUIF_32_magic_word[4] = {0x70, 0x69, 0x67, 0x73};
static char SUIF_32_reverse_magic_word[4] = {0x73, 0x67, 0x69, 0x70};
static char oldsuif_magic_word[4] = {0x73, 0x75, 0x69, 0x66};
static char oldsuif_reverse_magic_word[4] = {0x66, 0x69, 0x75, 0x73};

static boolean magic_words_initialized = FALSE;

/*
 *  This string contains the SUIF system release version string for
 *  the first release to use the binary format used in the current
 *  release.  Hence when the binary format changes, this should be
 *  reset to the new system version string.
 */
static char SUIF_series_start_ver[] = "1.1.1";

/*
 *  This string contains the SUIF system release version string for
 *  the first release to use a binary format that is a subset of that
 *  used in the current release.  This may be earlier than or equal to
 *  that in the variable above.
 */
static char SUIF_series_understood_ver[] = "1.1.1";

static char padding[] = "        ";

static int compare_ver_strings(const char *ver_1, const char *ver_2);
static void initialize_magic_words(void);
static void convert_to_decimal(char *target, char *source, int size,
			       boolean is_big_endian);
static void add_leuds(char *target, const char *to_add);
static void mul_leuds(char *leuds, int amount);


/*****************************************************************************/

// override default copy constructor and assignment op
base_stream::base_stream(const base_stream &) { assert(FALSE); }
base_stream &base_stream::operator=(const base_stream &)  { assert(FALSE); return *this; }

base_stream::base_stream (const char *n)
    : fp(0), pos(0), nm(n)
{
    flush_cache();
}


/*
 *  Closing a stream.  Since the OS may prevent all of the streams from being
 *  open at once, the file_set may need to temporarily close streams.  Thus,
 *  the current file position is recorded before closing the file.  This
 *  function is also used to close the stream when it is destroyed.
 */

void
base_stream::close ()
{
    if (fp) {
	pos = getpos();
	fclose(fp);
    }
    fp = NULL;
}


/*
 *  Find the current position of the file.  If the stream is not open, the
 *  position is zero.
 */

long
base_stream::getpos ()
{
    if (fp) return ftell(fp);
    return 0;
}


/*
 *  Change the current file position.
 */

void
base_stream::setpos (long p)
{
    if (!fp || fseek(fp, p, SEEK_SET)) {
	error_line(1, NULL, "Unable to seek to position %ld in file %s - %s",
		   p, name(), strerror(errno));
    }
}


/*
 *  Flush the stream cache.  Since procedures may be read in any order,
 *  the contents of the cache are undefined at the beginning of each
 *  procedure.  This means that the cache must be flushed between
 *  procedures when writing, and just to be safe it can also be flushed
 *  when reading.
 */

void
base_stream::flush_cache ()
{
    int i;

    for (i = 0; i < CACHE_SZ; i++) {
	cache[i] = NULL;
    }
}


/*
 *  Hash function for mapping character strings to cache slots.
 */

int
base_stream::hash (const char *x)
{
    return (((long)x) >> 4) % CACHE_SZ;
}


/*
 *  Make sure that the file is open.  This is called before every read or
 *  write operation.
 */

void
base_stream::check_open ()
{
    assert_msg(name(), ("Attempt to access a file with no name"));
    assert_msg(fp, ("Attempt to access unopened file \"%s\"", name()));
}


/*****************************************************************************/


/*
 *  Open an output file for writing.  If the file was previously open, the
 *  old file position is restored.  Otherwise, this must be the first time
 *  that the file has been opened, so the SUIF header is automatically
 *  written out.
 */

void
out_stream::open ()
{
    /* do nothing if the file is already open */
    if (fp) return;

    /* check if the file was previously open */
    if (pos != 0) {

	/* open the file in update mode (not append mode; we need seeks) */
	fp = fopen(name(), "rb+");
	if (!fp) {
	    error_line(1, NULL, "Unable to reopen file \"%s\" for output - %s",
		       name(), strerror(errno));
	}

	/* restore the old file position */
	setpos(pos);

    } else {

	/* open the file in write mode (to remove the previous contents) */
	fp = fopen(name(), "wb");
	if (!fp) {
	    error_line(1, NULL, "Unable to open file \"%s\" for output - %s",
		       name(), strerror(errno));
	}

	/* write out the header */
	initialize_magic_words();
	write_chunk(SUIF_magic_word, 4);

	unsigned long remainder = sizeof(int);
	char char_chunk;
	while (remainder != 0) {
	    if ((remainder >> suif_byte_size) != 0)
		char_chunk = 1;
	    else
		char_chunk = 0;
	    write_chunk(&char_chunk, 1);
	    char_chunk = 0;
	    write_chunk(&char_chunk, 1);
	    write_chunk(&char_chunk, 1);
	    char_chunk = remainder ^
			 ((remainder >> suif_byte_size) << suif_byte_size);
	    write_chunk(&char_chunk, 1);
	    remainder >>= suif_byte_size;
	}

	write_string(SUIF_series_start_ver);
	write_string(libsuif1_suif_string);
    }
}


/*
 *  Write a chunk of data and do error detection.
 */

void
out_stream::write_chunk (const void *cp, size_t sz)
{
    check_open();
    if (fwrite(cp, 1, sz, fp) != sz) {
	error_line(1, NULL, "Unable to write to file \"%s\" - %s", name(),
		   strerror(errno));
    }
}


/*
 *  Write a byte.
 */

void
out_stream::write_byte (char c)
{
    write_chunk(&c, sizeof(char));
}


/*
 *  Write an integer.
 */

void
out_stream::write_int (int i)
{
    /* always write with the native byte-order */
    write_chunk(&i, sizeof(int));
}


/*
 *  Writing out strings: A string in a SUIF file is either cached or
 *  not cached.  If it is cached then it is just one word indicating
 *  the index within the cache array where the pointer is stored.  If
 *  it is not cached, then we write out a load cache operation that is
 *  much longer.  It starts with a negative value equal to -index-1
 *  followed by the length of the string and then the contents of the
 *  string padded to make it a integral multiple of words.
 */

void
out_stream::write_string (const char *s)
{
    int len = strlen(s) + 1;
    write_int(len);
    write_chunk(s, len);

    /* pad to the next word if necessary */
    int wordlen = suif_round_to_int(len) * sizeof(int);
    if (wordlen > len) {
	write_chunk(padding, wordlen - len);
    }
}


void
out_stream::write_cached (const char *s)
{
    int ndx = hash(s);
    if (cache[ndx] == s) {
	write_int(ndx);
    } else {
	write_int(-ndx - 1);
	write_string(s);
	cache[ndx] = s;
    }
}


/*****************************************************************************/


in_stream::~in_stream(void)
{
    if (int_read_buffer != NULL)
	delete[] int_read_buffer;

    if (decimal_buffer != NULL)
	delete[] decimal_buffer;
}


/*
 *  Open an input stream for reading.  If the file was previously open, the
 *  old file position is restored.  Otherwise, this must be the first time
 *  that the file has been opened, so the magic number and version are read
 *  and checked.  If the byte order of the magic number is reversed, the
 *  "swap_bytes" flag is set so that all of the other integers will have
 *  their bytes swapped when they are read.
 */

void
in_stream::open ()
{
    /* do nothing if the file is already open */
    if (fp) return;

    fp = fopen(name(), "rb");
    if (!fp) {
	error_line(1, NULL, "Unable to open file \"%s\" for input - %s",
		   name(), strerror(errno));
    }

    /* check if the file was previously open */
    if (pos != 0) {
	setpos(pos);
    } else {

	/* read and check the magic number */
	initialize_magic_words();
	char magic[4];
	read_chunk(&magic, 4);
	boolean need_write_int_size = false;
	if (memcmp(magic, SUIF_magic_word, 4) == 0) {
	    swap_bytes = FALSE;
	    need_write_int_size = TRUE;
	} else if (memcmp(magic, SUIF_reverse_magic_word, 4) == 0) {
	    swap_bytes = TRUE;
	    need_write_int_size = TRUE;
	} else if (memcmp(magic, SUIF_32_magic_word, 4) == 0) {
	    swap_bytes = FALSE;
	    write_int_size = 4;
	    need_write_int_size = FALSE;
	} else if (memcmp(magic, SUIF_32_reverse_magic_word, 4) == 0) {
	    swap_bytes = TRUE;
	    write_int_size = 4;
	    need_write_int_size = FALSE;
	} else if ((memcmp(magic, oldsuif_magic_word, 4) == 0) ||
		   (memcmp(magic, oldsuif_reverse_magic_word, 4) == 0)) {
	    error_line(0, NULL, "File \"%s\" is in the oldsuif file format;",
		       name());
	    error_line(0, NULL, "this pass uses the newsuif format.");
	    error_line(0, NULL, "The file must go through the ``newsuif''");
	    error_line(1, NULL, "pass before this pass may be used.");
	} else {
	    /* probably not a SUIF file */
	    error_line(1, NULL, "File \"%s\" is not a SUIF file", name());
	}

	if (need_write_int_size) {
	    write_int_size = 0;
	    while (TRUE) {
		char char_chunk;
		boolean done = FALSE;

		read_chunk(&char_chunk, 1);
		if (char_chunk == 1) {
		    done = FALSE;
		} else if (char_chunk == 0) {
		    done = TRUE;
		} else {
		    error_line(1, NULL, "corrupted header");
		}

		read_chunk(&char_chunk, 1);
		if (char_chunk != 0) {
		    error_line(1, NULL, "corrupted header");
		}

		read_chunk(&char_chunk, 1);
		if (char_chunk != 0) {
		    error_line(1, NULL, "corrupted header");
		}

		read_chunk(&char_chunk, 1);
		if (((write_int_size << suif_byte_size) >> suif_byte_size) !=
		    write_int_size) {
		    error_line(1, NULL,
			       "overflow reading size of int for writer");
		}
		write_int_size <<= suif_byte_size;
		write_int_size |= char_chunk;

		if (done)
		    break;
	    }
	}

	if ((write_int_size != sizeof(int)) || swap_bytes) {
	    int_read_buffer = new char[write_int_size > sizeof(int) ?
				       write_int_size : sizeof(int)];
	    decimal_buffer = new char[(write_int_size > sizeof(int) ?
				       write_int_size : sizeof(int)) * 3 + 2];
	  }

	/* read and check the version numbers */
	const char *file_series_start_ver = read_string();
	const char *file_system_ver = read_string();
	if (compare_ver_strings(file_series_start_ver, libsuif1_suif_string) >
	    0) {
	    error_line(0, NULL, "File \"%s\" was produced under SUIF %s;",
		       name(), file_system_ver);
	    error_line(0, NULL, "this pass is based on SUIF %s.",
		       libsuif1_suif_string);
	    error_line(0, NULL, "You will have to upgrade this pass");
	    error_line(1, NULL, "to at least SUIF %s.", file_series_start_ver);
	}
	if (compare_ver_strings(SUIF_series_understood_ver,
				file_system_ver) > 0) {
	    error_line(0, NULL, "File \"%s\" was produced under SUIF %s;",
		       name(), file_system_ver);
	    error_line(0, NULL, "this pass is based on SUIF %s",
		       libsuif1_suif_string);
	    error_line(0, NULL, "You will have to upgrade the pass");
	    error_line(0, NULL, "that created this file to at least");
	    error_line(1, NULL, "SUIF %s.", SUIF_series_understood_ver);
	}
	delete[] file_series_start_ver;
	delete[] file_system_ver;
    }
}


/*
 *  Read a chunk and do error detection.
 */

void
in_stream::read_chunk (void *cp, size_t sz)
{
    check_open();
    if (fread(cp, 1, sz, fp) != sz) {
	error_line(1, NULL, "Unable to read from file \"%s\" - %s", name(),
		   strerror(errno));
    }
}


/*
 *  Read a byte.
 */

char
in_stream::read_byte ()
{
    char c;
    read_chunk(&c, sizeof(char));
    return c;
}


/*
 *  Read an integer.
 */

int
in_stream::read_int (boolean *overflow, char **decimal)
{
    if ((write_int_size == sizeof(int)) && !swap_bytes) {
	int i;
	read_chunk(&i, write_int_size);
	return i;
    } else {
	read_chunk(int_read_buffer, write_int_size);

	/* check if this file has the reverse byte-order */
	if (swap_bytes) {
	    unsigned long position;
	    for (position = 0; position < (write_int_size / 2); ++position) {
		char temp = int_read_buffer[position];
		int_read_buffer[position] =
			int_read_buffer[(write_int_size - position) - 1];
		int_read_buffer[(write_int_size - position) - 1] = temp;
	    }
	}

	if (write_int_size == sizeof(int))
	    return *(int *)int_read_buffer;

	int x = 1;
	boolean is_big_endian = (*(char *)&x != 1);

	if (write_int_size < sizeof(int)) {
	    unsigned long position;
	    if (is_big_endian) {
		position = sizeof(int);
		while (TRUE) {
		    --position;
		    int_read_buffer[position] =
			int_read_buffer[position -
					(sizeof(int) - write_int_size)];
		    if (position == sizeof(int) - write_int_size)
			break;
		}
	    } else {
		position = write_int_size - 1;
	    }

	    char extend_char;
	    if ((signed char)(int_read_buffer[position]) < 0) {
		extend_char = (char)-1;
	    } else {
		extend_char = 0;
	    }

	    if (is_big_endian) {
		--position;
		for (; position > 0; --position) {
		    int_read_buffer[position] = extend_char;
		}
		int_read_buffer[0] = extend_char;
	    } else {
		++position;
		for (; position < sizeof(int); ++position) {
		    int_read_buffer[position] = extend_char;
		}
	    }
	    return *(int *)int_read_buffer;
	} else {
	    unsigned long position;
	    if (is_big_endian) {
		position = 0;
	    } else {
		position = write_int_size - 1;
	    }

	    char extend_char;
	    if ((signed char)(int_read_buffer[position]) < 0) {
		extend_char = (char)-1;
	    } else {
		extend_char = 0;
	    }

	    boolean found_overflow = FALSE;
	    if (is_big_endian) {
		for (; position < write_int_size - sizeof(int); ++position) {
		    if (int_read_buffer[position] != extend_char)
			found_overflow = TRUE;
		}
	    } else {
		for (; position >= sizeof(int); --position) {
		    if (int_read_buffer[position] != extend_char)
			found_overflow = TRUE;
		}
	    }

	    if (found_overflow) {
		if (overflow != NULL) {
		    *overflow = TRUE;
		    if (decimal != NULL) {
			convert_to_decimal(decimal_buffer, int_read_buffer,
					   write_int_size, is_big_endian);
			*decimal = decimal_buffer;
		    }
		} else {
		    error_line(1, NULL, "overflow reading int");
		}
	    }

	    if (is_big_endian) {
		return *(int *)(&int_read_buffer[write_int_size -
						 sizeof(int)]);
	    } else {
		return *(int *)int_read_buffer;
	    }
	}
    }
}


/*
 *  Read an uncached string.
 */

const char *
in_stream::read_string ()
{
    /* read the length and round it up */
    int len = read_int();
    len = ((len + write_int_size - 1) / write_int_size) * write_int_size;

    /* read the string */
    char *cp = new char[len];
    read_chunk(cp, len);

    return cp;
}


/*
 *  Read a cached string.  See the write_cached function for details.
 */

const char *
in_stream::read_cached ()
{
    int ndx = read_int();

    /* check if the string is already in the cache */
    if (ndx >= 0) return cache[ndx];

    /* read the string and enter it in the lexicon */
    const char *s1 = read_string();
    const char *s2 = lexicon->enter(s1, TRUE)->sp;
    if (s2 != s1) delete[] s1;

    /* save the string in the cache */
    cache[-ndx - 1] = s2;
    return s2;
}


extern suif_check_result check_for_suif_file(char *file_name)
  {
    FILE *fp = fopen(file_name, "rb");
    if (fp == NULL)
        return SF_CANT_OPEN;

    initialize_magic_words();
    char magic[4];
    if (fread(&magic, 1, 4, fp) != 4)
      {
        fclose(fp);
        return SF_NOT_SUIF;
      }

    suif_check_result result;
    if (memcmp(magic, SUIF_magic_word, 4) == 0)
        result = SF_SUIF_FILE;
    else if (memcmp(magic, SUIF_reverse_magic_word, 4) == 0)
        result = SF_SUIF_FILE;
    else if (memcmp(magic, SUIF_32_magic_word, 4) == 0)
        result = SF_SUIF_FILE;
    else if (memcmp(magic, SUIF_32_reverse_magic_word, 4) == 0)
        result = SF_SUIF_FILE;
    else
        result = SF_NOT_SUIF;

    fclose(fp);
    return result;
  }


/*
 *  Compare the two version strings.  If ver_1 is earlier, return -1.
 *  If ver_2 is earlier, return 1.  If they are the same or
 *  incomperable (e.g. 1.2.beta and 1.2.alpha), return 0.
 */
static int compare_ver_strings(const char *ver_1, const char *ver_2)
{
    const char *follow1 = ver_1;
    const char *follow2 = ver_2;

    while ((!isdigit(*follow1)) || (!isdigit(*follow2))) {
	if ((*follow1 == '\0') || (*follow1 != *follow2))
	    return 0;
	++follow1;
	++follow2;
    }

    while (isdigit(*follow1) && isdigit(*follow2) && (*follow1 == *follow2)) {
	++follow1;
	++follow2;
    }

    if ((!isdigit(*follow1)) && (!isdigit(*follow2))) {
	return compare_ver_strings(follow1, follow2);
    }

    if (!isdigit(*follow1)) {
	return -1;
    }

    if (!isdigit(*follow2)) {
	return 1;
    }

    int first_compare;
    if (*follow1 < *follow2) {
	first_compare = -1;
    } else {
	first_compare = 1;
    }

    while (isdigit(*follow1) && isdigit(*follow2)) {
	++follow1;
	++follow2;
    }

    if ((!isdigit(*follow1)) && (!isdigit(*follow2))) {
	return first_compare;
    }

    if (!isdigit(*follow1)) {
	return -1;
    }

    return 1;
}

static void initialize_magic_words(void)
{
    if (magic_words_initialized)
	return;

    /*
     *  If this is a little endian machine, swap the magic word and
     *  reverse magic word.
     */
    int x = 1;
    if (*(char *)&x == 1) {
	char temp[4];
	memcpy(temp, SUIF_magic_word, 4);
	memcpy(SUIF_magic_word, SUIF_reverse_magic_word, 4);
	memcpy(SUIF_reverse_magic_word, temp, 4);

	memcpy(temp, SUIF_32_magic_word, 4);
	memcpy(SUIF_32_magic_word, SUIF_32_reverse_magic_word, 4);
	memcpy(SUIF_32_reverse_magic_word, temp, 4);
    }
    magic_words_initialized = TRUE;
}

/*
 *  Convert ``size'' bytes of binary to decimal and put the result in
 *  the buffer pointed to by ``target''.  The buffer ``target'' points
 *  to must already be large enough for the ASCII decimal string,
 *  including possible minus sign and trailing null character.
 */
static void convert_to_decimal(char *target, char *source, int size,
			       boolean is_big_endian)
{
    /* Note that we are assuming two's complement here. */

    char *first = (is_big_endian ? source : &(source[size - 1]));
    char *last = (is_big_endian ? &(source[size - 1]) : source);
    int increment = (is_big_endian ? 1 : -1);

    char *accumulator = new char[size * 3 + 1];
    char *temp1 = new char[size * 3 + 1];
    char *temp2 = new char[size * 3 + 1];
    *accumulator = 0;

    boolean is_neg = (*(signed char *)first < 0);
    *accumulator = 0;

    for (char *follow = first; TRUE; follow += increment) {
	*temp1 = 0;
	unsigned char this_char =
		(is_neg ? ~(*(unsigned char *)follow) :
			  (*(unsigned char *)follow));

	int shift_amount = sizeof(char) * suif_byte_size;
	while (shift_amount >= 3) {
	    mul_leuds(accumulator, 8);
	    shift_amount -= 3;
	}
	if (shift_amount == 2) {
	    mul_leuds(accumulator, 4);
	} else if (shift_amount == 1) {
	    mul_leuds(accumulator, 2);
	}

	while (this_char != 0) {
	    mul_leuds(temp1, 10);
	    *temp2 = (this_char % 10) + '0';
	    temp2[1] = 0;
	    add_leuds(temp1, temp2);
	    this_char /= 10;
	}

	add_leuds(accumulator, temp1);
	if (follow == last)
	    break;
    }

    if (is_neg)
	add_leuds(accumulator, "1");

    char *follow_target = target;
    if (is_neg) {
	*follow_target = '-';
	++follow_target;
    }
    int acc_length = strlen(accumulator);
    for (int acc_position = acc_length; acc_position > 0; --acc_position) {
	*follow_target = accumulator[acc_position - 1];
	++follow_target;
    }
    *follow_target = 0;

    delete[] accumulator;
    delete[] temp1;
    delete[] temp2;
}

/*
 *  Add two little endian unsigned decimal strings.
 */
static void add_leuds(char *target, const char *to_add)
{
    int carry = 0;
    char *follow_target = target;
    const char *follow_add = to_add;
    while ((*follow_target != 0) && (*follow_add != 0)) {
	assert((*follow_target >= '0') && (*follow_target <= '9'));
	assert((*follow_add >= '0') && (*follow_add <= '9'));
	int this_val = (*follow_add - '0') + (*follow_target - '0') + carry;
	*follow_target = ((char)(this_val % 10)) + '0';
	carry = this_val / 10;
	++follow_target;
	++follow_add;
	assert((carry >= 0) && (carry < 10));
    }

    while (*follow_target != 0) {
	assert((*follow_target >= '0') && (*follow_target <= '9'));
	int this_val = (*follow_target - '0') + carry;
	*follow_target = ((char)(this_val % 10)) + '0';
	carry = this_val / 10;
	++follow_target;
	assert((carry >= 0) && (carry < 10));
    }

    while (*follow_add != 0) {
	assert((*follow_add >= '0') && (*follow_add <= '9'));
	int this_val = (*follow_add - '0') + carry;
	*follow_target = ((char)(this_val % 10)) + '0';
	carry = this_val / 10;
	++follow_target;
	++follow_add;
	assert((carry >= 0) && (carry < 10));
    }

    if (carry > 0) {
	*follow_target = ((char)carry) + '0';
	++follow_target;
    }

    *follow_target = 0;
}

/*
 *  Multiply a little endian unsigned decimal string by a constant ``amount''
 *  where 0 <= amount <= 10.
 */
static void mul_leuds(char *leuds, int amount)
{
    int carry = 0;
    char *follow = leuds;
    while (*follow != 0) {
	assert((*follow >= '0') && (*follow <= '9'));
	int this_val = (*follow - '0') * amount + carry;
	*follow = ((char)(this_val % 10)) + '0';
	carry = this_val / 10;
	++follow;
	assert((carry >= 0) && (carry < 10));
    }

    if (carry > 0) {
	*follow = ((char)carry) + '0';
	*(follow + 1) = 0;
    }
}
