/* file "bldtypes.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

/***************************************************************************
 *                                                                         *
 *   <type name>:                                       tl_name            *
 *                                                                         *
 *   <type extend>:                                     tl_extend          *
 *                      const                                              *
 *                      volatile                                           *
 *                                                      tl_extend          *
 *   <type extend list>:                                                   *
 *                      <type extend>                                      *
 *                      <type extend list> <type extend>                   *
 *                                                                         *
 *   <type spec>:                                       tl_spec            *
 *                      void                                               *
 *                      char                                               *
 *                      short                                              *
 *                      int                                                *
 *                      int16                                              *
 *                      int32                                              *
 *                      int64                                              *
 *                      long                                               *
 *                      unsigned                                           *
 *                      unsigned16                                         *
 *                      unsigned32                                         *
 *                      unsigned64                                         *
 *                      float                                              *
 *                      float64                                            *
 *                      double                                             *
 *                      <type spec> *                                      *
 *                      <struct/union spec>                                *
 *                                                                         *
 *   <full type spec>:                                  tl_spec            *
 *                      %%                      next in the arg list       *
 *                      % <number>              type with id <number>      *
 *                      <type spec>                                        *
 *                      <type extend list> <type spec>                     *
 *                      <type spec> <array disc.>                          *
 *                      <type extend list> <type spec> <array disc.>       *
 *                                                                         *
 *   <named full type spec>:                            tl_spec            *
 *                      <type name>                                        *
 *                      <type spec>  <type name>                           *
 *                      <type extend list> <type spec> <type name>         *
 *                      <type spec> <type name> <array disc.>              *
 *                      <type extend list> <type spec> <type name>         *
 *                                                           <array disc.> *
 *                      <func call>                                        *
 *                                                                         *
 *   <func call>:                                                          *
 *                      <named full type spec> ( )                         *
 *                      <named full type spec> ( <expr list> )             *
 *                                                                         *
 *   <struct/union spec>:                                tl_su             *
 *                      <type name>                                        *
 *                      struct { <decl. list> }                            *
 *                      union { <decl. list> }                             *
 *                                                                         *
 *   <decl. list>:                                       tl_su             *
 *                      <named full type spec>                             *
 *                      <decl. list>; <named full type spec>               *
 *                                                                         *
 *   <array disc.>:                                      tl_su             *
 *                      [ <number> ]                                       *
 *                      <array disc.> [ <number> ]                         *
 *                                                                         *
 *   <expr list>:                                        tl_su             *
 *                      <full type spec>                                   *
 *                      <named full type spec>                             *
 *                      <expr list> , <full type spec>                     *
 *                      <expr list> , <named full type spec>               *
 *                                                                         *
 ***************************************************************************/


/*************************************************************************
 * a place holder to return a lis of <type, name> pairs                  *
 *************************************************************************/
struct su_list {
    type_node_list * ty;
    char ** nm;
};



/*************************************************************************
 ***                                                                   ***
 *** Parse a C-like type definition string into SUIF type structure    ***
 ***                                                                   ***
 *************************************************************************/
class type_template : private builder_base {
private:
    static int unum;
    base_symtab * symtab;
    char * ptr;
    type_node * arg_list[10];
    int curr_arg;

    char * next_token();
    void consume(int i);
    boolean check(const char *, boolean gulp=TRUE);
public:
    type_template(base_symtab * bt) { symtab = bt; }
    type_node * parse_type(char* parse,
                           type_node * tn0=NULL, type_node * tn1=NULL,
                           type_node * tn2=NULL, type_node * tn3=NULL,
                           type_node * tn4=NULL, type_node * tn5=NULL,
                           type_node * tn6=NULL, type_node * tn7=NULL,
                           type_node * tn8=NULL, type_node * tn9=NULL);
private:
    char *      pt_name();
    int         pt_integer();
    type_node * pt_extend();
    type_node * pt_spec();
    su_list   * pt_su(boolean is_name=TRUE);
};

#define MAXSULIST       512

typedef char * charp;




