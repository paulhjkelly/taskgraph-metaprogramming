#include <set>
#include <cassert>
#include <cstdio>
#include <suif1.h>
#include <useful.h>

#ifdef VERBOSE_RAISE_ASSIGNMENTS
#define INFO printf
#else
#define INFO if ( false ) printf
#endif

// This is a pass that will raise the assignment of a constant
// to a local variable to the top of a block if it is the first
// assignment or read of that variable.

// Note that this implementation is probably unsafe. Please verify any code
// produced using it.

namespace
{

class AssignmentRaiser
{
private:
  bool tooComplicated;
  std::set<var_sym*> seen;
  std::set<var_sym*> movable;

public:
  AssignmentRaiser() : tooComplicated(false)
  {
  }

  bool isTooComplicated() const
  {
    return tooComplicated;
  }

  void findVariableAccess(instruction* const ins)
  {
    if (ins->opcode() == io_jmp)
    {
      INFO("Complicated: Jump Statement\n");
      tooComplicated = true;
      return;
    }
    
    for(unsigned i=0; i < ins->num_dsts(); ++i)
    {
      operand dst_op(ins->dst_op(i));

      if (dst_op.is_symbol())
      {
        if (ins->opcode() == io_ldc && seen.find(dst_op.symbol()) == seen.end())
          movable.insert(dst_op.symbol());

        seen.insert(dst_op.symbol());
      }
    }

    for (unsigned i = 0; i < ins->num_srcs(); ++i)
    {
      operand op(ins->src_op(i));

      if (op.is_symbol())
        seen.insert(op.symbol());

      if (op.is_instr())
        findVariableAccess(op.instr());
    }
  }

  void findVariableAccess(tree_node * const tn)
  {
    switch (tn->kind())
    {
      case TREE_FOR:
      {
        tree_for *const tnf = static_cast < tree_for * >(tn);
        findVariableAccess(tnf->lb_list());
        findVariableAccess(tnf->ub_list());
        findVariableAccess(tnf->step_list());
        findVariableAccess(tnf->landing_pad());
        findVariableAccess(tnf->body());
        break;
      }
      case TREE_IF:
      {
        tree_if *const tni = static_cast < tree_if * >(tn);
        findVariableAccess(tni->header());
        findVariableAccess(tni->then_part());
        findVariableAccess(tni->else_part());
        break;
      }
      case TREE_LOOP:
      {
        tree_loop *const tnl = static_cast < tree_loop * >(tn);
        findVariableAccess(tnl->body());
        findVariableAccess(tnl->test());
        break;
      }
      case TREE_BLOCK:
      {
        tree_block *const tnb = static_cast < tree_block * >(tn);
        findVariableAccess(tnb->body());
        break;
      }
      case TREE_INSTR:
      {
        tree_instr *const tnin = static_cast < tree_instr * >(tn);
        findVariableAccess(tnin->instr());
        break;
      }
      default:
      {
        assert(false);
        break;
      }
    }
  }

  void findVariableAccess(tree_node_list* const loop)
  {
    tree_node_list_iter iter(loop);
    while (!iter.is_empty())
      findVariableAccess(iter.step());
  }
  
  void moveAssignments(tree_node_list* const tnl)
  {
    std::set<tree_node*> assignments;
    tree_node_list_iter iter(tnl);

    while (!iter.is_empty())
    {
      tree_node* const tn = iter.step();
      switch (tn->kind())
      {
        case TREE_FOR:
        {
          tree_for *const tnf = static_cast < tree_for * >(tn);
          moveAssignments(tnf->lb_list());
          moveAssignments(tnf->ub_list());
          moveAssignments(tnf->step_list());
          moveAssignments(tnf->landing_pad());
          moveAssignments(tnf->body());
          break;
        }
        case TREE_IF:
        {
          tree_if *const tni = static_cast < tree_if * >(tn);
          moveAssignments(tni->header());
          moveAssignments(tni->then_part());
          moveAssignments(tni->else_part());
          break;
        }
        case TREE_LOOP:
        {
          tree_loop *const tnl = static_cast < tree_loop * >(tn);
          moveAssignments(tnl->body());
          moveAssignments(tnl->test());
          break;
        }
        case TREE_BLOCK:
        {
          tree_block *const tnb = static_cast < tree_block * >(tn);
          moveAssignments(tnb->body());
          break;
        }
        case TREE_INSTR:
        {
          tree_instr* const tnin = static_cast < tree_instr * >(tn);
          instruction* const ins = tnin->instr();

          if (ins->opcode() == io_ldc)
          {
            operand op(ins->dst_op());

            if (op.is_symbol() && movable.find(op.symbol()) != movable.end())
              assignments.insert(tn);
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

    for(std::set<tree_node*>::iterator instrIter(assignments.begin()); instrIter!=assignments.end(); ++instrIter)
    {
      remove_node(*instrIter);
      tnl->push(*instrIter);
    }
  }
};

}

namespace tg
{
  void raiseInitialAssignmentsDoProc(tree_proc* const tp, void *)
  {
    proc_sym* const psym = tp->proc();
    INFO("=======%s======= \n", psym->name());
    AssignmentRaiser raiser;
    raiser.findVariableAccess(tp->body());

    if (!raiser.isTooComplicated())
      raiser.moveAssignments(tp->body());
  }
}
