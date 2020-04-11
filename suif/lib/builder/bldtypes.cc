/* file "bldtypes.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/* Implementation of Hi-level Types */
#define _MODULE_ "libbuilder.a"
#pragma implementation "bldtypes.h"
#define BUILDERLIB


#include <suif1.h>
#include <useful.h>
#include <cstring>
#include <cstdlib>
#include "builder.h"
#include "bldtypes.h"


int         type_template::unum = 0;


/***************************************************************************
 * set-up and do parse                                                     *
 ***************************************************************************/
type_node * type_template::parse_type(char * parse,
                                      type_node * tn0, type_node * tn1,
                                      type_node * tn2, type_node * tn3,
                                      type_node * tn4, type_node * tn5,
                                      type_node * tn6, type_node * tn7,
                                      type_node * tn8, type_node * tn9)
{
    arg_list[0] = tn0;
    arg_list[1] = tn1;
    arg_list[2] = tn2;
    arg_list[3] = tn3;
    arg_list[4] = tn4;
    arg_list[5] = tn5;
    arg_list[6] = tn6;
    arg_list[7] = tn7;
    arg_list[8] = tn8;
    arg_list[9] = tn9;
    curr_arg = 0;

    if(parse==NULL) ERROR(et_type, ("input string is NULL"));
    ptr = parse;
    type_node * tp = pt_extend();

    char * ptr = next_token();
    if(*ptr != 0)
        ERROR(et_type, ("extra at the end `%s'", ptr));

    if(arg_list[curr_arg] != NULL)
        ERROR(et_type, ("too many arguments"));

    return tp;
}

/***************************************************************************
 * consume next i chars from the input                                     *
 ***************************************************************************/
void type_template::consume(int i)
{
    if(((int)strlen(ptr)) < i) ERROR(et_type, ("end of string is prematurelly reached"));
    ptr += i;
}


/***************************************************************************
 * eliminate the whitespace and go to the beginning of the next token      *
 ***************************************************************************/
char * type_template::next_token()
{
    while(*ptr == ' ') ptr++;
    if(ptr == 0) ERROR(et_type, ("end of string is prematurelly reached"));
    return ptr;
}


/***************************************************************************
 * check if given string is the next token available.                      *
 * if so and gulp is true, then consume that token.                        *
 ***************************************************************************/
boolean type_template::check(const char * nm, boolean gulp)
{
    char * ptr = next_token();
    int len = strlen(nm);
    if (strncmp(ptr, nm, len)) return FALSE;
    if(gulp) consume(len);
    return TRUE;
}

/***************************************************************************
 * parse the extend                                                        *
 ***************************************************************************/
type_node * type_template::pt_extend()
{
#ifdef DEBUG
    printf("Extend  `%s'\n", next_token());
#endif
    type_node *curr_type;
    if (check("const")) {
        curr_type = pt_extend();
        curr_type = new modifier_type(TYPE_CONST, curr_type);
    } else if (check("volatile")) {
        curr_type = pt_extend();
        curr_type = new modifier_type(TYPE_VOLATILE, curr_type);
    } else if (check("signed")) {
        curr_type = pt_extend();
        ERROR(et_type, ("Currently the default"));
    } else if (check("restrict")) {
        curr_type = pt_extend();
	curr_type = new modifier_type(TYPE_RESTRICT, curr_type);
    } else {
        curr_type = pt_spec();
    }


    curr_type = symtab->install_type(curr_type);

#ifdef DEBUG
    printf("--> ");
    curr_type->print_full();
#endif

    return curr_type;
}


/***************************************************************************
 * parse the spec                                                          *
 ***************************************************************************/
type_node * type_template::pt_spec()
{
#ifdef DEBUG
    printf("Spec    `%s'\n", next_token());
#endif
    type_node *curr_type = NULL;

    // ************ Base types ************
    if (check("void")) {
        curr_type = type_void;
    } else if (check("char")) {
        curr_type = type_char;
    } else if (check("short")) {
        curr_type = new base_type(TYPE_INT, 16);
        ((base_type *)curr_type)->set_signed(TRUE);
    } else if (check("int16")) {
        curr_type = new base_type(TYPE_INT, 16);
        ((base_type *)curr_type)->set_signed(TRUE);
    } else if (check("int32")) {
        curr_type = new base_type(TYPE_INT, 32);
        ((base_type *)curr_type)->set_signed(TRUE);
    } else if (check("int64")) {
        curr_type = new base_type(TYPE_INT, 64);
        ((base_type *)curr_type)->set_signed(TRUE);
    } else if (check("int")) {
        curr_type = type_signed;
    } else if (check("long")) {
        curr_type = type_signed_long;
    } else if (check("unsigned16")) {
        curr_type = new base_type(TYPE_INT, 16);
        ((base_type *)curr_type)->set_signed(FALSE);
    } else if (check("unsigned32")) {
        curr_type = new base_type(TYPE_INT, 32);
        ((base_type *)curr_type)->set_signed(FALSE);
    } else if (check("unsigned64")) {
        curr_type = new base_type(TYPE_INT, 64);
        ((base_type *)curr_type)->set_signed(FALSE);
    } else if (check("unsigned")) {
        curr_type = type_unsigned;
        ((base_type *)curr_type)->set_signed(FALSE);
    } else if (check("float32")) {
        curr_type = new base_type(TYPE_FLOAT, 32);
    } else if (check("float64")) {
        curr_type = new base_type(TYPE_FLOAT, 64);
    } else if (check("float")) {
        curr_type = type_float;
    } else if (check("double")) {
        curr_type = type_double;
    }
    // ************ Structures ************
    else if ((check("struct", FALSE))||
               (check("union", FALSE))) {
        boolean s;
        if(check("struct", FALSE)) {
            s = TRUE;
            consume(6);
        } else {
            s = FALSE;
            consume(5);
        }

        // Left parenthsis
        if(!check("{"))
           ERROR(et_type, ("Parenthesis expected at `%s'", next_token()));

        su_list * lst = pt_su();        // get the list in struct/union

        // Find size (for both union and struct)
        int tot_s = 0;
        int max_s = 0;
        int cnt = 0;
        type_node_list_iter iter1(lst->ty);
        while(!iter1.is_empty()) {
            type_node  * tn = iter1.step();
            int sz = tn->size();
            tot_s = align_up(tot_s, tn);
            tot_s += sz;
            max_s  = MAX(max_s, sz);
            cnt++;
        }

        char name[64];
        sprintf(name, "_BLDR_struct_%03d", unum++); // Find a unique name
        curr_type = new struct_type((s)?TYPE_STRUCT:TYPE_UNION,
                                    (s)?tot_s:max_s,
                                    name,
                                    cnt);

        // assign to the structure from the list
        type_node_list_iter iter2(lst->ty);
        cnt = 0;
        int offset = 0;
        while(!iter2.is_empty()) {
            type_node  * tn = iter2.step();
            ((struct_type *)curr_type)->set_field_type(cnt, tn);
            ((struct_type *)curr_type)->set_field_name(cnt, lst->nm[cnt]);
            if(s) offset = align_up(offset, tn);
            ((struct_type *)curr_type)->set_offset(cnt, offset);
            if(s) offset += tn->size();
            cnt++;
        }

        // Right parenthsis
        if(!check("}"))
            ERROR(et_type, ("Parenthesis expected at `%s' (unbalanced parenthesis)", next_token()));
    }
    // ************ Type from Argument List ************
    else if (check("%%")) { // %% will denote a use of an argument
        if(curr_arg == 9)
            ERROR(et_type, ("not many arguments available"));
        curr_type = arg_list[curr_arg++];
        if(curr_type == NULL)
            ERROR(et_type, ("%dth argument is not given or is a NULL pointer", curr_arg));

    }
    // ************ Find type with Type ID ************
    else if (check("%")) { // %<num> will denote a tag number
            int tag = pt_integer();
        curr_type = symtab->lookup_type_id(tag);
        if(curr_type == NULL)
            ERROR(et_type, ("no type with id %d", tag));

    } else {
        ERROR(et_type, ("Type spec expected at `%s'", next_token()));
    }


    // A Type Specification was found
    // Now lets see if a compound type is build using the specification
    // We check, in order, pointers, function calls and arrays
    // This may not give the full flexibility of C types, but it sure is
    // easy to implement.


    // ************ Pointers ************
    while (check("*")) {
        curr_type = new ptr_type(curr_type);
    }

    // ************ Arrays ************
    int lb[32];
    int ub[32];
    int  ndim = 0;
    while(check("[")) {
        boolean fort = (get_proc())?((get_proc()->proc()->src_lang() == src_fortran)):FALSE;
        int sz = pt_integer(); // Get the size
        lb[ndim] = (fort)?1:0;
        ub[ndim] = (fort)?sz:(sz-1);
        ndim++;
        if(ndim>32)
            ERROR(et_type, ("Too many dimensions, current max is 32"));
        if(!check("]"))
            ERROR(et_type, ("Parenthesis expected at `%s' (unbalanced parenthesis)", next_token()));
    }

    for(int i=ndim-1; i>=0; i--) {
        curr_type = new array_type(curr_type);
        ((array_type *)curr_type)->set_lower_bound(array_bound(lb[i]));
        ((array_type *)curr_type)->set_upper_bound(array_bound(ub[i]));
    }

    // ************ Function calls ************
    while (check("(")) {
        su_list * lst = pt_su(FALSE);  // get the list of arguments

        curr_type = new func_type(curr_type,
                                  lst->ty->count());

        // assign the list of args
        int cnt = 0;
        type_node_list_iter iter(lst->ty);
        while(!iter.is_empty()) {
            type_node  * tn = iter.step();
            ((func_type *)curr_type)->set_arg_type(cnt, tn);
            cnt++;
        }

        // right parenthisis
        if(!check(")"))
            ERROR(et_type, ("Parenthesis expected at `%s' (unbalanced parenthesis)", next_token()));
    }


    return curr_type;
}



/***************************************************************************
 * Argument list for structures, unions and also function calls            *
 ***************************************************************************/
su_list * type_template::pt_su(boolean has_name)
{
#ifdef DEBUG
    printf("SU list `%s'\n", next_token());
#endif
    char * names[MAXSULIST];
    su_list * lst = new su_list;
    lst->ty = new type_node_list;

    int cnt = 0;


    // check end of struct/union list of list of args of a function call
    while(!(check("}", FALSE) || check(")", FALSE))) {
        type_node * tn = pt_extend();

        char *nm = NULL;
        if(has_name)                    // struct/union has a name for each
            nm = pt_name();

        // at end?
        if(check(",", FALSE) || check(";", FALSE))
            consume(1);
        else
            if(!(check("}", FALSE)||
                 check(")", FALSE)))
                ERROR(et_type, ("comma expected, got `%s'", next_token()));

        names[cnt++] = nm;
        lst->ty->append(tn);

//        if(tn->is_struct())
//            ((struct_type *)tn)->set_name(nm);

#ifdef DEBUG
        printf("  %d: ", cnt);
        if(has_name)
            printf(" \"%s\" ", nm);
        tn->print_full();
#endif
    }

    // move the names to a short list to return
    if(has_name) {
        lst->nm = new charp[cnt];
        for(int i=0; i<cnt; i++)
            lst->nm[i] = names[i];
    }

    return lst;
}


/***************************************************************************
 * Parse a proper symbol name                                              *
 ***************************************************************************/
char * type_template::pt_name()
{
#ifdef DEBUG
    printf("Name    `%s'\n", next_token());
#endif

    char * p = next_token();
    char name[255];
    int cnt = 0;

    if(!(((*ptr >= 'a')&&(*ptr <= 'z'))||
         ((*ptr >= 'A')&&(*ptr <= 'Z'))||
         (*ptr == '_')))
        ERROR(et_type, ("First character of a name must be a letter, got `%s'", p));
    name[cnt++] = *ptr++;

    while(((*ptr >= 'a')&&(*ptr <= 'z'))||
          ((*ptr >= 'A')&&(*ptr <= 'Z'))||
          ((*ptr >= '0')&&(*ptr <= '9'))||
          (*ptr == '_')) {
        name[cnt++] = *ptr++;
        if(cnt >= 255)
            ERROR(et_type, ("string too long, `%s'", p));
    }
    name[cnt] = 0;

    char * ret = new char[cnt+1];
    strcpy(ret, name);
    return ret;
}

/***************************************************************************
 * Parse an integer                                                        *
 ***************************************************************************/
int type_template::pt_integer()
{
#ifdef DEBUG
    printf("Integer `%s'\n", next_token());
#endif
    int val = 0;

    next_token();

    while((*ptr >= '0')&&(*ptr <= '9'))
        val = 10*val + (*ptr++ - '0');

    return val;
}
