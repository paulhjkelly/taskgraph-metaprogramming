/* file "arrayinfo.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libuseful.a"

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

static operand op_base_addr_op(operand address);
static operand base_from_instr(instruction *the_instr);

extern boolean is_array(operand a)
{
   if(a.is_null()) 
      return FALSE;
   if(a.is_instr()) {
      if(a.instr()->format() == inf_array)
           return TRUE; 
      if(a.instr()->opcode() == io_ldc) {
          immed im(((in_ldc *)a.instr())->value());
          if(im.is_symbol())
              if(im.symbol()->is_var()) {
                  var_sym * v = (var_sym *)im.symbol();
                  assert(im.offset() == 0);
                  if(v->type()->unqual()->op() == TYPE_ARRAY)
                      return TRUE;                  
                  if(v->type()->unqual()->op() == TYPE_PTR) {
                      ptr_type * tt = (ptr_type *)v->type()->unqual();
                      if(tt->ref_type()->unqual()->op() == TYPE_ARRAY)
                          return TRUE;                  
                  }
              }
      }
      if(a.instr()->opcode() == io_cvt) {
          var_sym *v = get_sym_of_array(a.instr());
          if (v && v->type()->is_array()) return TRUE;
          return FALSE;
      }
   } else {
      var_sym * v = a.symbol();
      if(v->type()->unqual()->op() == TYPE_ARRAY)
         return TRUE;
   }
   return FALSE;
}

extern boolean is_array_symbol(operand a)
{
   if(a.is_symbol()) {
      if(a.symbol()->type()->op() == TYPE_ARRAY)
         return TRUE;
   } 
   if (a.is_instr()) {
      instruction *i = a.instr();
      
      if (i->opcode() == io_cvt) {
        in_rrr * rr = (in_rrr *)i;
        operand  cvtop = rr->src1_op();
        if (cvtop.is_symbol()) {
            var_sym * v = cvtop.symbol();
            if(v->type()->unqual()->op() == TYPE_ARRAY)
                 return TRUE;                  
            if(v->type()->unqual()->op() == TYPE_PTR) {
                 ptr_type * tt = (ptr_type *)v->type()->unqual();
                 if(tt->ref_type()->unqual()->op() == TYPE_ARRAY)                                  return TRUE;                  
            }
         }
         else if (cvtop.is_instr()) {
            i = cvtop.instr();
         }
     }

   if (i->opcode() == io_ldc) {
       immed im(((in_ldc *)i)->value());
       if(im.is_symbol()) {
            if(im.symbol()->is_var()) {
                var_sym * v = (var_sym *)im.symbol();
                assert(im.offset() == 0);
                if(v->type()->unqual()->op() == TYPE_ARRAY)
                      return TRUE;                  
                if(v->type()->unqual()->op() == TYPE_PTR) {
                      ptr_type * tt = (ptr_type *)v->type()->unqual();
                      if(tt->ref_type()->unqual()->op() == TYPE_ARRAY)
                          return TRUE;                  
                }
             }
         }
    }
   }

   return FALSE;
}

extern array_type * find_array_type(type_node * tn)
{
    switch(tn->op()) {
    case TYPE_INT:
    case TYPE_FLOAT:
    case TYPE_VOID:
    case TYPE_FUNC:
    case TYPE_GROUP:
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_ENUM:
        return NULL;
    case TYPE_ARRAY:
        return (array_type *)tn;
    case TYPE_PTR:
        return find_array_type(((ptr_type *)tn)->ref_type());
    case TYPE_CALL_BY_REF:
    case TYPE_VOLATILE:
    case TYPE_CONST:
    case TYPE_RESTRICT:
    case TYPE_NULL:
        return find_array_type(tn->unqual());
    }
    assert(FALSE);
    return NULL;
}

extern array_type *find_array_type(in_array * ia)
  {
    return find_array_type(ia->base_op().type());
  }


extern int array_num_elem(array_type * at, int * too_messy)
{
    assert(at);
    assert(too_messy);

    int lb = array_lower_bound(at, too_messy);
    if(*too_messy) return 0;
    int ub = array_upper_bound(at, too_messy);

    return ub - lb + 1;
}   
    


extern int array_lower_bound(array_type * at, int * too_messy)
{
    assert(at);

    *too_messy = FALSE;
    array_bound lb = at->lower_bound();
    int l;
    if(!lb.is_constant()) {
        if(too_messy) {
            *too_messy = TRUE;
            return 0;
        }
        printf("Error lower bound not a constant\n");
        l = 1;
    } else 
        l = lb.constant();
    
    return l;
}

extern int array_upper_bound(array_type * at, int * too_messy)
{
    assert(at);

    *too_messy = FALSE;
    array_bound ub = at->upper_bound();
    int u;
    
    if(!ub.is_constant()) {
        if(too_messy) {
            *too_messy = TRUE;
            return 0;
        }
        printf("Error upper bound not a constant\n");
        u = 100;
    }
    else
        u = ub.constant();
    
    return u;
}


/***************************************************************************
 * Given an operand (parameter to be passed) find the array                *
 ***************************************************************************/
extern var_sym * get_sym_of_array(operand op)
{
    if(op.is_instr()) {
        return get_sym_of_array(op.instr()); 
    } else {
        return op.symbol();
    }
}


extern var_sym *get_pass_thru_sym(instruction *i)
  {
    instruction *base_instr = i;
    operand addr_op = base_from_instr(base_instr);
    if (!addr_op.is_null())
        addr_op = op_base_addr_op(addr_op);
    if (addr_op.is_symbol())
        return addr_op.symbol();
    if (addr_op.is_expr())
        base_instr = addr_op.instr();
    if (base_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)base_instr;
        immed value = the_ldc->value();
        if (value.is_symbol())
          {
            sym_node *the_sym = value.symbol();
            if (the_sym->is_var())
                return (var_sym *)the_sym;
          }
      }

    return NULL;
  }

/***************************************************************************
 * For a given array instruction, find the variable symbol.                * 
 ***************************************************************************/
extern var_sym * get_sym_of_array(in_array *ai)
  {
    return get_pass_thru_sym(ai);
  }

extern var_sym * get_sym_of_array(instruction *i)
  {
    return get_pass_thru_sym(i);
  }

/***************************************************************************
 * 
 ***************************************************************************/
extern boolean is_lhs(instruction * i, boolean array_ok)
{
    if(i->opcode() == io_str) return TRUE;              // A[] = ..
    if(i->opcode() == io_lod) return FALSE;             
    if(i->opcode() == io_ldc) return FALSE;
    if(i->opcode() == io_cal) return FALSE;             // foo(A[])
    if(i->opcode() == io_gen) return FALSE;
    if(!array_ok)
        if(i->opcode() == io_array) return FALSE;       // B[A[]]

    if (!i->dst_op().is_instr())
        return FALSE;

    instruction * ins = i->dst_op().instr();
    if(ins->opcode() == io_memcpy) {
        in_rrr * ins_3r = (in_rrr *)ins;
        
        // *dst_addr <= *src_addr 
        if(ins_3r->src_addr_op().is_instr()) 
            if(ins_3r->src_addr_op().instr() == i)
                return FALSE;
        if(ins_3r->dst_addr_op().is_instr()) 
            if(ins_3r->dst_addr_op().instr() == i)
                return TRUE;
        assert(0);
    }
    return is_lhs(ins, FALSE);
}


/***************************************************************************
 * 
 ***************************************************************************/
extern int numdim(array_type * at)
{
    assert(at);

    if(at->elem_type()->unqual()->op() == TYPE_ARRAY)
        return 1+numdim((array_type *)at->elem_type()->unqual());
    else 
        return 1;
}

extern type_node *get_element_type(type_node *curr_type)
{
    assert(curr_type);

    if (!curr_type->unqual()->is_array()) return curr_type;

    array_type *at = (array_type *) curr_type->unqual();
    return get_element_type(at->elem_type());
}


extern boolean constant_bounds(type_node *curr_type)
{
    if (!curr_type->unqual()->is_array()) return TRUE;

    array_type *at = (array_type *) curr_type->unqual();
    if (! (at->lower_bound().is_constant() && 
	   at->upper_bound().is_constant())) return FALSE;

    return constant_bounds(at->elem_type());
}


extern boolean known_bounds(type_node *curr_type)
{
    if (!curr_type->unqual()->is_array()) return TRUE;

    array_type *at = (array_type *) curr_type->unqual();
    if (at->lower_bound().is_unknown() || 
		at->upper_bound().is_unknown()) return FALSE;

    return known_bounds(at->elem_type());
}


extern int total_num_elems(type_node *curr_type)
{
    if (constant_bounds(curr_type)) {
	type_node *elem_type = get_element_type(curr_type);
	assert(elem_type->size() > 0);

	return(curr_type->size() / elem_type->size());
    }    

    return 0;
}

static operand op_base_addr_op(operand address)
  {
    assert(address.type()->is_ptr());
    if (address.is_expr())
      {
        operand test_op = base_from_instr(address.instr());
        if (!test_op.is_null())
            return op_base_addr_op(test_op);
      }

    return address;
  }

static operand base_from_instr(instruction *the_instr)
  {
    switch (the_instr->opcode())
      {
        case io_cpy:
        case io_cvt:
          {
            in_rrr *the_cvt = (in_rrr *)the_instr;
            if (the_cvt->src_op().type()->is_ptr())
                return the_cvt->src_op();
            break;
          }
        case io_add:
        case io_sub:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            if (the_rrr->src1_op().type()->is_ptr())
                return the_rrr->src1_op();
            if ((the_instr->opcode() == io_add) &&
                the_rrr->src2_op().type()->is_ptr())
              {
                return the_rrr->src2_op();
              }
            break;
          }
        case io_array:
          {
            in_array *the_array = (in_array *)the_instr;
            return the_array->base_op();
          }
        default:
            break;
      }

    return operand();
  }
