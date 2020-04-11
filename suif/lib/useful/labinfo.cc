/* file "labinfo.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements routines to get information about how labels
 *  are used in the SUIF code for the SUIF library of miscellaneous
 *  useful routines.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_labinfo_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Constant Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Constant Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Global Variables
 *----------------------------------------------------------------------*/

static const char *k_useful_labinfo_info;
static const char *k_useful_labinfo_backward;
static const char *k_useful_labinfo_num_back;

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void add_data_for_node(tree_node *the_node);
static void add_data_for_list(tree_node_list *the_list);
static void add_data_for_label_instr(in_lab *the_label_instr);
static void add_data_for_branch(in_bj *the_branch);
static void add_data_for_mbr(in_mbr *the_mbr);
static void remove_data_for_node(tree_node *the_node);
static void remove_data_for_list(tree_node_list *the_list);
static label_info *create_or_get_info(label_sym *the_label);
static void delete_label_info(label_sym *the_label);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void build_label_info(tree_node_list *node_list)
  {
    tree_node *parent_node = node_list->parent();
    switch (parent_node->kind())
      {
        case TREE_LOOP:
        case TREE_FOR:
        case TREE_IF:
            break;
        case TREE_BLOCK:
            if (parent_node->is_proc())
                break;
            /* fall through */
        case TREE_INSTR:
        default:
            assert(FALSE);
      }

    add_data_for_list(node_list);
  }

extern void remove_label_info(tree_node_list *node_list)
  {
    tree_node *parent_node = node_list->parent();
    switch (parent_node->kind())
      {
        case TREE_LOOP:
        case TREE_FOR:
        case TREE_IF:
            break;
        case TREE_BLOCK:
            if (parent_node->is_proc())
                break;
            /* fall through */
        case TREE_INSTR:
        default:
            assert(FALSE);
      }

    remove_data_for_list(node_list);
  }

extern label_info *get_lab_info(label_sym *the_label)
  {
    void *data = the_label->peek_annote(k_useful_labinfo_info);
    assert(data != NULL);
    return (label_info *)data;
  }

extern boolean is_backward_branch(in_bj *the_bj)
  {
    return (the_bj->annotes()->peek_annote(k_useful_labinfo_backward) != NULL);
  }

extern boolean mbr_lab_is_backward(in_mbr *the_mbr, label_sym *the_label)
  {
    void *backward_data = the_mbr->peek_annote(k_useful_labinfo_backward);
    assert(backward_data != NULL);
    boolean *backward_table = (boolean *)backward_data;

    unsigned num_labs = the_mbr->num_labs();

    if (the_label == the_mbr->default_lab())
        return backward_table[num_labs];

    for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
      {
        if (the_label == the_mbr->label(lab_num))
            return backward_table[lab_num];
      }
    assert(FALSE);
    return FALSE;
  }

extern int num_mbr_back_labs(in_mbr *the_mbr)
  {
    void *num_back_data = the_mbr->peek_annote(k_useful_labinfo_num_back);
    return (int)(long)num_back_data;
  }

extern void create_info_for_new_lab_instr(in_lab *new_lab_instr)
  {
    add_data_for_label_instr(new_lab_instr);
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Internal Function Implementations
 *----------------------------------------------------------------------*/

extern void init_labinfo(void)
  {
    k_useful_labinfo_info = lexicon->enter("useful labinfo info")->sp;
    k_useful_labinfo_backward = lexicon->enter("useful labinfo backward")->sp;
    k_useful_labinfo_num_back = lexicon->enter("useful labinfo num back")->sp;
  }

/*----------------------------------------------------------------------*
    End Internal Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void add_data_for_node(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            switch (the_instr->opcode())
              {
                case io_lab:
                  {
                    in_lab *the_label_instr = (in_lab *)the_instr;
                    add_data_for_label_instr(the_label_instr);
                    break;
                  }
                case io_btrue:
                case io_bfalse:
                case io_jmp:
                  {
                    in_bj *the_branch = (in_bj *)the_instr;
                    add_data_for_branch(the_branch);
                    break;
                  }
                case io_mbr:
                  {
                    in_mbr *the_mbr = (in_mbr *)the_instr;
                    add_data_for_mbr(the_mbr);
                    break;
                  }
                default:
                    break;
              }
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            add_data_for_list(the_block->body());
            break;
          }
        default:
            break;
      }
  }

static void add_data_for_list(tree_node_list *the_list)
  {
    tree_node_list_iter node_iter(the_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        add_data_for_node(this_node);
      }
  }

static void add_data_for_label_instr(in_lab *the_label_instr)
  {
    label_sym *the_label_sym = the_label_instr->label();
    label_info *info_block = create_or_get_info(the_label_sym);
    if (info_block->definition != NULL)
      {
        error_line(1, the_label_instr, "label `%s' is multiply defined",
                   the_label_sym->name());
      }
    info_block->definition = the_label_instr;
  }

static void add_data_for_branch(in_bj *the_branch)
  {
    label_info *info_block = create_or_get_info(the_branch->target());
    if (info_block->definition == NULL)
      {
        info_block->forward_jumps.append(the_branch);
      }
    else
      {
        info_block->backward_jumps.append(the_branch);
        the_branch->append_annote(k_useful_labinfo_backward, NULL);
      }
  }

static void add_data_for_mbr(in_mbr *the_mbr)
  {
    unsigned num_labs = the_mbr->num_labs();
    boolean *backward_table = new boolean[num_labs + 1];
    int back_counter = 0;
    for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
      {
        label_sym *this_label_sym = the_mbr->label(lab_num);
        label_info *info_block = create_or_get_info(this_label_sym);
        if (info_block->definition == NULL)
          {
            info_block->forward_jumps.append(the_mbr);
            backward_table[lab_num] = FALSE;
          }
        else
          {
            info_block->backward_jumps.append(the_mbr);
            backward_table[lab_num] = TRUE;
            ++back_counter;
          }
      }

    label_info *info_block = create_or_get_info(the_mbr->default_lab());
    if (info_block->definition == NULL)
      {
        info_block->forward_jumps.append(the_mbr);
        backward_table[num_labs] = FALSE;
      }
    else
      {
        info_block->backward_jumps.append(the_mbr);
        backward_table[num_labs] = TRUE;
        ++back_counter;
      }

    the_mbr->append_annote(k_useful_labinfo_backward, backward_table);
    the_mbr->append_annote(k_useful_labinfo_num_back, (void *)back_counter);
  }

static void remove_data_for_node(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            switch (the_instr->opcode())
              {
                case io_lab:
                  {
                    in_lab *the_label_instr = (in_lab *)the_instr;
                    delete_label_info(the_label_instr->label());
                    break;
                  }
                case io_btrue:
                case io_bfalse:
                case io_jmp:
                  {
                    in_bj *the_branch = (in_bj *)the_instr;
                    delete_label_info(the_branch->target());
                    if (the_branch->annotes()->peek_annote(
                                k_useful_labinfo_backward) != NULL)
                      {
                        (void)(the_branch->get_annote(
                                k_useful_labinfo_backward));
                      }
                    break;
                  }
                case io_mbr:
                  {
                    in_mbr *the_mbr = (in_mbr *)the_instr;
                    unsigned num_labs = the_mbr->num_labs();
                    for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
                        delete_label_info(the_mbr->label(lab_num));
                    delete_label_info(the_mbr->default_lab());
                    void *backward_data =
                            the_mbr->get_annote(k_useful_labinfo_backward);
                    assert(backward_data != NULL);
                    boolean *backward_table = (boolean *)backward_data;
                    delete[] backward_table;
                    (void)(the_mbr->get_annote(k_useful_labinfo_num_back));
                    break;
                  }
                default:
                    break;
              }
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            remove_data_for_list(the_block->body());
            break;
          }
        default:
            break;
      }
  }

static void remove_data_for_list(tree_node_list *the_list)
  {
    tree_node_list_iter node_iter(the_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        remove_data_for_node(this_node);
      }
  }

static label_info *create_or_get_info(label_sym *the_label)
  {
    void *data = the_label->peek_annote(k_useful_labinfo_info);
    if (data != NULL)
        return (label_info *)data;

    label_info *new_info_block = new label_info;
    new_info_block->definition = NULL;
    the_label->append_annote(k_useful_labinfo_info, new_info_block);
    return new_info_block;
  }

static void delete_label_info(label_sym *the_label)
  {
    if (the_label->peek_annote(k_useful_labinfo_info) != NULL)
      {
        void *old_data = the_label->get_annote(k_useful_labinfo_info);
        label_info *old_info_block = (label_info *)old_data;
        delete old_info_block;
      }
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
