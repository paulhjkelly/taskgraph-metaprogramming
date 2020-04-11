/* file "expr_map.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  Implementation of expr_map class: expression mapping for
 *  libuseful, the SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "expr_map.h"

#define RCS_BASE_FILE expr_map_cc

#include "useful_internal.h"
#include "expr_map.h"

RCS_BASE(
    "$Id$")


expr_map::expr_map(void)
  {
    left_cache = NULL;
    right_cache = NULL;
    cache_num = 0;
    pair_count = 0;
  }

expr_map::~expr_map(void)
  {
    operand_dlist_iter left_iter(&left_list);
    while (!left_iter.is_empty())
      {
        operand this_op = left_iter.step();
        kill_op(this_op);
      }

    operand_dlist_iter right_iter(&right_list);
    while (!right_iter.is_empty())
      {
        operand this_op = right_iter.step();
        kill_op(this_op);
      }
  }

void expr_map::kill_cached(void)
  {
    assert((left_cache != NULL) && (right_cache != NULL));

    --pair_count;

    operand_dlist_e *new_left_cache;
    operand_dlist_e *new_right_cache;

    if (cache_num == 0)
      {
        new_left_cache = left_cache->next();
        new_right_cache = right_cache->next();
      }
    else
      {
        --cache_num;
        new_left_cache = left_cache->prev();
        new_right_cache = right_cache->prev();
      }

    left_list.remove(left_cache);
    right_list.remove(right_cache);

    kill_op(left_cache->contents);
    kill_op(right_cache->contents);

    delete left_cache;
    delete right_cache;

    left_cache = new_left_cache;
    right_cache = new_right_cache;
  }

void expr_map::cache_match(operand to_match, boolean use_left)
  {
    left_cache = left_list.head();
    right_cache = right_list.head();
    cache_num = 0;

    while (cache_num < pair_count)
      {
        assert((left_cache != NULL) && (right_cache != NULL));

        operand test_op = (use_left ? left_cache : right_cache)->contents;
        if (operands_are_same_expr(test_op, to_match))
            return;

        left_cache = left_cache->next();
        right_cache = right_cache->next();
        ++cache_num;
      }
  }

operand expr_map::map_internal(operand source_op, boolean use_left)
  {
    cache_match(source_op, use_left);

    if (cache_num != pair_count)
      {
        operand_dlist_e *element = (use_left ? left_cache : right_cache);
        assert(element != NULL);
        kill_op(source_op);
        return element->contents.clone();
      }

    operand result = source_op;

    if (result.is_expr())
      {
        instruction *the_instr = result.instr();
        unsigned num_srcs = the_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            this_op.remove();
            the_instr->set_src_op(src_num, map_internal(this_op, use_left));
          }
      }

    return result;
  }

void expr_map::init(expr_map &other_map)
  {
    clear();

    pair_count = other_map.num_pairs();
    for (unsigned pair_num = 0; pair_num < pair_count; ++pair_num)
      {
        operand left_op, right_op;
        other_map.get_pair(pair_num, &left_op, &right_op);
        left_list.append(left_op);
        right_list.append(right_op);
      }
  }

void expr_map::clear(void)
  {
    while (!left_list.is_empty())
      {
        operand this_op = left_list.pop();
        kill_op(this_op);
      }

    while (!right_list.is_empty())
      {
        operand this_op = right_list.pop();
        kill_op(this_op);
      }

    left_cache = NULL;
    right_cache = NULL;
    cache_num = 0;
    pair_count = 0;
  }

void expr_map::add(operand source_op, operand target_op)
  {
    left_list.append(source_op.clone());
    right_list.append(target_op.clone());

    cache_num = pair_count;
    ++pair_count;
    left_cache = left_list.tail();
    right_cache = right_list.tail();
  }

void expr_map::kill_source(operand source_op)
  {
    cache_match(source_op, TRUE);
    if (cache_num != pair_count)
        kill_cached();
  }

void expr_map::kill_target(operand target_op)
  {
    cache_match(target_op, FALSE);
    if (cache_num != pair_count)
        kill_cached();
  }

void expr_map::invert(void)
  {
    left_cache = left_list.head();
    right_cache = right_list.head();
    cache_num = 0;

    while (cache_num < pair_count)
      {
        assert((left_cache != NULL) && (right_cache != NULL));

        operand temp_op = left_cache->contents;
        left_cache->contents = right_cache->contents;
        right_cache->contents = temp_op;

        left_cache = left_cache->next();
        right_cache = right_cache->next();
        ++cache_num;
      }
  }

operand expr_map::map(operand source_op)
  {
    operand new_op = source_op.clone();
    return map_internal(new_op, TRUE);
  }

operand expr_map::inverse_map(operand source_op)
  {
    operand new_op = source_op.clone();
    return map_internal(new_op, FALSE);
  }

expr_map *expr_map::compose(expr_map *first_map)
  {
    expr_map *new_map = new expr_map();

    unsigned pair_num;
    for (pair_num = 0; pair_num < num_pairs(); ++pair_num)
      {
        operand left_op, right_op;
        get_pair(pair_num, &left_op, &right_op);
        operand_list *image = first_map->inverse_image(left_op);
        while (!image->is_empty())
          {
            operand new_left = image->pop();
            new_map->add(new_left, right_op);
            kill_op(new_left);
          }
        delete image;
        kill_op(left_op);
        kill_op(right_op);
      }

    for (pair_num = 0; pair_num < first_map->num_pairs(); ++pair_num)
      {
        operand left_op, right_op;
        first_map->get_pair(pair_num, &left_op, &right_op);
        operand new_right = map(right_op);
        new_map->add(left_op, new_right);
        kill_op(new_right);
        kill_op(left_op);
        kill_op(right_op);
      }

    return new_map;
  }

operand_list *expr_map::inverse_image(operand target_op)
  {
    operand_list *result = new operand_list();

    if (target_op.is_expr())
      {
        instruction *target_instr = target_op.instr();
        unsigned num_srcs = target_instr->num_srcs();

        operand_list **src_images;
        src_images = new operand_list *[num_srcs];
        operand_list_iter **src_iters;
        src_iters = new operand_list_iter *[num_srcs];

        unsigned src_num;
        boolean empty_list = FALSE;
        for (src_num = 0; src_num < num_srcs; ++src_num)
          {
            src_images[src_num] = inverse_image(target_instr->src_op(src_num));
            src_iters[src_num] = new operand_list_iter(src_images[src_num]);
            if (src_iters[src_num]->is_empty())
                empty_list = TRUE;
          }

        if (!empty_list)
          {
            while (TRUE)
              {
                operand this_op = target_op.clone();
                assert(this_op.is_expr());
                instruction *new_instr = this_op.instr();
                assert(new_instr->num_srcs() == num_srcs);
                for (src_num = 0; src_num < num_srcs; ++src_num)
                  {
                    kill_op(new_instr->src_op(src_num));
                    new_instr->set_src_op(src_num,
                                          src_iters[src_num]->peek().clone());
                  }
                result->append(this_op);

                for (src_num = 0; src_num < num_srcs; ++src_num)
                  {
                    (void)(src_iters[src_num]->step());
                    if (src_iters[src_num]->is_empty())
                        src_iters[src_num]->reset(src_images[src_num]);
                    else
                        break;
                  }

                if (src_num == num_srcs)
                    break;
              }
          }

        for (src_num = 0; src_num < num_srcs; ++src_num)
          {
            while (!src_images[src_num]->is_empty())
              {
                operand this_op = src_images[src_num]->pop();
                kill_op(this_op);
              }
            delete src_images[src_num];
            delete src_iters[src_num];
          }

        delete[] src_images;
        delete[] src_iters;
      }

    result->append(target_op.clone());

    operand_list_e *follow = result->head();
    while (follow != NULL)
      {
        operand_list_e *next_place = follow->next();
        cache_match(follow->contents, TRUE);
        if (cache_num == pair_count)
          {
            result->remove(follow);
            kill_op(follow->contents);
            delete follow;
          }
        follow = next_place;
      }

    left_cache = left_list.head();
    right_cache = right_list.head();
    cache_num = 0;

    while (cache_num < pair_count)
      {
        assert((left_cache != NULL) && (right_cache != NULL));

        operand test_op = right_cache->contents;
        if (operands_are_same_expr(test_op, target_op))
            result->append(left_cache->contents.clone());

        left_cache = left_cache->next();
        right_cache = right_cache->next();
        ++cache_num;
      }

    return result;
  }

void expr_map::get_pair(unsigned pair_num, operand *left, operand *right)
  {
    assert(pair_num < pair_count);

    if ((left_cache == NULL) || (right_cache == NULL))
      {
        left_cache = left_list.head();
        right_cache = right_list.head();
        cache_num = 0;
      }

    while (cache_num < pair_num)
      {
        assert((left_cache != NULL) && (right_cache != NULL));
        left_cache = left_cache->next();
        right_cache = right_cache->next();
        ++cache_num;
      }

    while (cache_num > pair_num)
      {
        assert((left_cache != NULL) && (right_cache != NULL));
        left_cache = left_cache->prev();
        right_cache = right_cache->prev();
        --cache_num;
      }

    *left = left_cache->contents.clone();
    *right = right_cache->contents.clone();
  }

void expr_map::print(FILE *fp)
  {
    left_cache = left_list.head();
    right_cache = right_list.head();
    cache_num = 0;

    while (cache_num < pair_count)
      {
        assert((left_cache != NULL) && (right_cache != NULL));

        fprintf(fp, "(");
        left_cache->contents.print_source(fp);
        fprintf(fp, " => ");
        right_cache->contents.print_source(fp);
        fprintf(fp, ")\n");

        left_cache = left_cache->next();
        right_cache = right_cache->next();
        ++cache_num;
      }

    if (pair_count == 0)
        fprintf(fp, "[identity expr_map]\n");
  }
