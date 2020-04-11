#include <cassert>
#include <cstdio>
#include <cstddef>
#include <climits>
#include <limits>
#include <set>
#include <suif1.h>
#include <useful.h>
#include <dependence.h>

#ifdef VERBOSE_MALLOC_LARGE_ARRAYS
#define INFO printf
#else
#define INFO if ( false ) printf
#endif

namespace
{

// This transformation attempts to turn stack allocated arrays into heap
// allocated arrays. It is potentially very broken as SUIF's handling of sizes
// doesn't seem to adapt to the current platform.

class MallocLargeArrays
{
private:
  std::size_t maxStackAllocation;
  type_node* type_size_t;
  proc_sym* malloc_function;
  proc_sym* free_function;

  proc_sym* createMallocFunction() const 
  {
    func_type *funcType = new func_type(type_void->ptr_to());
    funcType->set_num_args(1);
    funcType->set_arg_type(0, type_size_t);
    funcType->set_varargs(false);

    proc_sym *procSym = new proc_sym(funcType, src_c, const_cast<char*>("malloc"));
    return procSym;
  }

  proc_sym* createFreeFunction() const
  {
    func_type *funcType = new func_type(type_void);
    funcType->set_num_args(1);
    funcType->set_arg_type(0, type_void->ptr_to());
    funcType->set_varargs(false);

    proc_sym *procSym = new proc_sym(funcType, src_c, const_cast<char*>("free"));
    return procSym;
  }

  instruction* createMallocCall(const std::size_t bytes) const
  {
    in_cal *instr = new in_cal();
    instruction* base = new in_ldc (malloc_function->type()->ptr_to(), operand(), immed(malloc_function));
    instruction* loadBytes = new in_ldc(type_signed_long, operand(), immed(bytes));
    instr->set_addr_op(base);
    instr->set_num_args(1);
    instr->set_argument(0u, loadBytes);
    return instr;
  }

  instruction* createFreeCall(var_sym* const ptr) const
  {
    in_cal *instr = new in_cal();
    instruction* base = new in_ldc (free_function->type()->ptr_to(), operand(), immed(free_function));
    instr->set_addr_op(base);
    instr->set_num_args(1);
    instr->set_argument(0u, ptr);
    return instr;
  }

  static std::size_t getSize(array_type* const arrayType)
  {
    type_node* type = arrayType; 
    std::size_t size = 1;

    while(type->is_array())
    {
      array_type* const subArrayType = static_cast<array_type*>(type);
      size *= subArrayType->upper_bound().constant() - subArrayType->lower_bound().constant() + 1;
      type = subArrayType->elem_type();
    }

    // FIXME: correct handling of types of different types should be handled by
    // SUIF. This is especially important for SUIF to determine the correct type
    // of size_t

    std::size_t elemSize;

    if (type == type_ptr)
      elemSize = sizeof(void*);
    else if (type == type_char)
      elemSize = sizeof(char);
    else if (type == type_signed_char)
      elemSize = sizeof(signed char);
    else if (type == type_unsigned_char)
      elemSize = sizeof(unsigned char);
    else if (type == type_signed_short)
      elemSize = sizeof(signed short);
    else if (type == type_unsigned_short)
      elemSize = sizeof(unsigned short);
    else if (type == type_signed)
      elemSize = sizeof (signed int);
    else if (type == type_unsigned)
      elemSize = sizeof (unsigned int);
    else if (type == type_signed_long)
      elemSize = sizeof (signed long);
    else if (type == type_unsigned_long)
      elemSize = sizeof (unsigned long);
  //else if (type == type_signed_longlong)
  //  elemSize = sizeof(signed long long);
  //else if (type == type_unsigned_longlong)
  //  elemSize = sizeof(unsigned long long);
    else if (type == type_float)
      elemSize = sizeof(float);
    else if (type == type_double)
      elemSize = sizeof(double);
    else if (type == type_longdouble)
      elemSize = sizeof(long double);
    else
      elemSize = type->size() / CHAR_BIT;

    size *= elemSize;

    return size;
  }


  std::set<var_sym*> modifyTypesAndCallAllocationRoutines(tree_block * const tb)
  {
    std::set<var_sym*> heapAllocated;

    block_symtab* const symTab = tb->symtab();
    sym_node_list* const symbols = symTab->symbols();
    sym_node_list_iter iter(symbols);
  
    // First collect all arrays that need to be heap allocated, change their type
    // and add appropriate malloc statements.
    while (!iter.is_empty())
    {
      sym_node* const symNode = iter.step();
      if (symNode->is_var())
      {
        var_sym* const var = static_cast<var_sym*>(symNode);

        if (!var->is_param() && var->type()->is_array() && 
            getSize(static_cast<array_type*>(var->type())) > maxStackAllocation)
        {
          // Change variable type to a restricted array pointer
          array_type* const arrayType = static_cast<array_type*>(var->type());
          type_node* const newType = symTab->install_type(new modifier_type(TYPE_RESTRICT, arrayType->ptr_to())); 
          var->set_type(newType);
          heapAllocated.insert(var);
  
          // Create call to malloc
          instruction* const mallocCall = createMallocCall(getSize(arrayType));
          instruction* const mallocAssignment = new in_rrr(io_cvt, newType, var, mallocCall, operand()); 
          tb->body()->push(new tree_instr(mallocAssignment));
  
          // Create call to free
          instruction* const freeCall = createFreeCall(var);
          tb->body()->append(new tree_instr(freeCall));
        }
      }
    }

    return heapAllocated;
  }
  
  void rewriteInstruction(instruction* const ins, const std::set<var_sym*>& heapAllocated)
  {
    if (ins->opcode() == io_array)
    {
      in_array* const arrayInstr = static_cast<in_array*>(ins);
      var_sym* const array = get_sym_of_array(arrayInstr);

      if (heapAllocated.find(array) != heapAllocated.end())
      {
        arrayInstr->base_op().remove();
        arrayInstr->set_base_op(new in_rrr(io_lod, array->type(), operand(), array));
      }
    }

    for (unsigned i = 0; i < ins->num_srcs(); ++i)
    {
      operand op(ins->src_op(i));

      if (op.is_instr())
        rewriteInstruction(op.instr(), heapAllocated);
    }
  }

  void mallocLargeArrays(tree_block* const tb, const std::set<var_sym*>& heapAllocated)
  {
    std::set<var_sym*> localHeapAllocated(modifyTypesAndCallAllocationRoutines(tb));
    localHeapAllocated.insert(heapAllocated.begin(), heapAllocated.end());
    mallocLargeArrays(tb->body(), localHeapAllocated);
  }

  void mallocLargeArrays(tree_node_list* const tnl, const std::set<var_sym*>& heapAllocated)
  {
    tree_node_list_iter iter(tnl);

    while (!iter.is_empty())
    {
      tree_node* const tn = iter.step();

      switch (tn->kind())
      {
        case TREE_FOR:
        {
          tree_for *const tnf = static_cast <tree_for*>(tn);
          mallocLargeArrays(tnf->lb_list(), heapAllocated);
          mallocLargeArrays(tnf->ub_list(), heapAllocated);
          mallocLargeArrays(tnf->step_list(), heapAllocated);
          mallocLargeArrays(tnf->landing_pad(), heapAllocated);
          mallocLargeArrays(tnf->body(), heapAllocated);
          break;
        }
        case TREE_IF:
        {
          tree_if *const tni = static_cast <tree_if*>(tn);
          mallocLargeArrays(tni->header(), heapAllocated);
          mallocLargeArrays(tni->then_part(), heapAllocated);
          mallocLargeArrays(tni->else_part(), heapAllocated);
          break;
        }
        case TREE_LOOP:
        {
          tree_loop *const tnl = static_cast <tree_loop*>(tn);
          mallocLargeArrays(tnl->body(), heapAllocated);
          mallocLargeArrays(tnl->test(), heapAllocated);
          break;
        }
        case TREE_BLOCK:
        {
          tree_block *const tb = static_cast <tree_block*>(tn);
          mallocLargeArrays(tb, heapAllocated);
          break;
        }
        case TREE_INSTR:
        {
          tree_instr* const tnin = static_cast <tree_instr*>(tn);
          instruction* const instr = tnin->instr();
          rewriteInstruction(instr, heapAllocated);
          
          // Insert free statements before this instruction if it is a return
          if (instr->format() == inf_rrr && instr->opcode() == io_ret)
          {
            for(std::set<var_sym*>::const_iterator symIter(heapAllocated.begin()); symIter!=heapAllocated.end(); ++symIter)
            {
              instruction* const freeCall = createFreeCall(*symIter);
              tnl->insert_before(new tree_instr(freeCall), iter.cur_elem());
            }
          }
          break;
        }
        default:
        {
          assert(false);
          break;
        }
      }
    }
  }

public:
  MallocLargeArrays() : maxStackAllocation(4 * 1024)
  {
  }

  void setupGlobals(tree_proc* const tp)
  {
    // FIXME: This attempts to create a C type corresponding to std::size_t so the generated
    // malloc prototype will be correct. It will break when the returned SUIF type does not correspond
    // to the appropriate C type on the target platform. It should be removed once headers can be 
    // specified for TaskGraph generated code.
    block_symtab* const symTab = tp->symtab();
    type_size_t = symTab->install_type(new base_type(TYPE_INT, sizeof(std::size_t)*CHAR_BIT, std::numeric_limits<std::size_t>::is_signed));

    malloc_function = createMallocFunction();
    free_function = createFreeFunction();

    symTab->add_sym(malloc_function);
    symTab->add_sym(free_function);
  }

  void mallocLargeArrays(tree_block* const tb)
  {
    const std::set<var_sym*> heapAllocated;
    mallocLargeArrays(tb, heapAllocated);
  }
};

}

namespace tg
{
  void mallocLargeArraysDoProc(tree_proc* const tp, void *)
  {
    proc_sym* const psym = tp->proc();
    INFO("=======%s======= \n", psym->name());
    MallocLargeArrays transform;
    transform.setupGlobals(tp);
    transform.mallocLargeArrays(tp);
  }
}
