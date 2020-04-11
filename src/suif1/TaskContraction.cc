/* Copyright (c) 2006 Francis Russell */

#include <iostream>
#include <suif1.h>
#include <useful.h>
#include <map>
#include <vector>
#include <set>

namespace tg
{
  void init_scalarization();
  void scalarize_proc(tree_proc*);
	
  enum IndexState
  {
    UNUSED,
    LOOP_INDEX,
    TOO_COMPLEX
  };

  static var_sym *getSymbol(operand o)
  {
    if (o.is_symbol())
      return o.symbol();

    // FIXME: Or at least, understand me. I believe this instruction may represent
    // some trivial copy but I'm not 100% sure. All array indexes in the taskgraph
    // generated code seem to use it.
    if (o.is_instr() && o.instr()->format() == inf_rrr)
      return getSymbol(static_cast < in_rrr * >(o.instr())->src1_op());

    return NULL;
  }

  class IndexInfo
  {
  private:
    IndexState state;
    tree_for* loop;

  public:
    IndexInfo() : state(UNUSED)
    {
    }

    tree_for *getLoop() const
    {
      return loop;
    }

    void addUsage(const std::map<var_sym*, tree_for*>& loopIndices, operand o)
    {
      var_sym *const symbol = getSymbol(o);

      if (symbol != NULL && loopIndices.find(symbol) != loopIndices.end())
      {
        tree_for *const symbolLoop = loopIndices.find(symbol)->second;

        if (state == UNUSED)
        {
          state = LOOP_INDEX;
          loop = symbolLoop;
        }
        else if (state == LOOP_INDEX && loop != symbolLoop)
        {
          // Index is the loop variable of a different loop
          state = TOO_COMPLEX;
        }
      }
      else
      {
	// Index is not a loop variable
        state = TOO_COMPLEX;
      }
    }

    IndexState getState() const
    {
      return state;
    }

    void print() const
    {
      if (state == TOO_COMPLEX)
      {
        std::cout << "Complex";
      }
      else if (state == UNUSED)
      {
        std::cout << "Unused";
      }
      else
      {
        std::cout << loop->index()->name();
      }
    }
  };

  class ArrayContractor
  {
  private:
    tree_block * const block;
    std::map < var_sym *, tree_for * >loopIndices;
    std::map < var_sym *, std::vector < IndexInfo > >arrayUsage;

    void find_array_access(tree_block * const tb)
    {
      block_symtab *const symTab = tb->symtab();
      sym_node_list *const symbols = symTab->symbols();

      sym_node_list_iter symNodeIter(symbols);
      while (!symNodeIter.is_empty())
      {
        sym_node *const symNode = symNodeIter.step();
        if (symNode->is_var() && !static_cast < var_sym * >(symNode)->is_param() &&
            static_cast < var_sym * >(symNode)->type()->is_array())
        {
          var_sym *const arraySymbol = static_cast < var_sym * >(symNode);
          array_type *const array = static_cast < array_type * >(arraySymbol->type());
          arrayUsage[arraySymbol] = std::vector < IndexInfo > (numdim(array));
        }
      }

      find_array_access(tb->body());
    }

    void find_array_access(instruction * const instr)
    {
      if (instr->opcode() == io_array)
      {
        in_array *const arrayInstr = static_cast < in_array * >(instr);
        var_sym *const arraySym = get_sym_of_array(arrayInstr);

        if (arrayUsage.find(arraySym) != arrayUsage.end())
        {
          std::vector < IndexInfo > &indexInfo = arrayUsage.find(arraySym)->second;
          for (unsigned int index = 0; index < arrayInstr->dims(); ++index)
          {
            indexInfo[index].addUsage(loopIndices, arrayInstr->index(index));
          }
        }
      }

      for (unsigned int i = 0; i < instr->num_srcs(); ++i)
      {
        operand op(instr->src_op(i));
        if (op.is_instr())
          find_array_access(op.instr());
      }
    }

    void find_array_access(tree_node_list * const tnl)
    {
      tree_node_list_iter iter(tnl);
      while (!iter.is_empty())
      {
        tree_node *const tn = iter.step();
        switch (tn->kind())
        {
          case TREE_FOR:
          {
            tree_for *const tnf = static_cast < tree_for * >(tn);
            loopIndices[tnf->index()] = tnf;
            find_array_access(tnf->lb_list());
            find_array_access(tnf->ub_list());
            find_array_access(tnf->step_list());

            find_array_access(tnf->landing_pad());
            find_array_access(tnf->body());
            break;
          }

          case TREE_IF:
          {
            tree_if *const tni = static_cast < tree_if * >(tn);
            find_array_access(tni->header());
            find_array_access(tni->then_part());
            find_array_access(tni->else_part());
            break;
          }
          case TREE_LOOP:
          {
            tree_loop *const tnl = static_cast < tree_loop * >(tn);
            find_array_access(tnl->body());
            find_array_access(tnl->test());
            break;
          }
          case TREE_BLOCK:
          {
            tree_block *const tb = static_cast < tree_block * >(tn);
            find_array_access(tb);
            break;
          }
          case TREE_INSTR:
          {
            tree_instr *const tnin = static_cast < tree_instr * >(tn);
            find_array_access(tnin->instr());
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

    void rewrite_instructions(tree_node_list * const tnl)
    {
      tree_node_list_iter iter(tnl);
      while (!iter.is_empty())
      {
        tree_node *const tn = iter.step();
        switch (tn->kind())
        {
          case TREE_FOR:
          {
            tree_for *const tnf = static_cast < tree_for * >(tn);
            loopIndices[tnf->index()] = tnf;
            rewrite_instructions(tnf->lb_list());
            rewrite_instructions(tnf->ub_list());
            rewrite_instructions(tnf->step_list());

            rewrite_instructions(tnf->landing_pad());
            rewrite_instructions(tnf->body());
            loopIndices.erase(tnf->index());
            break;
          }

          case TREE_IF:
          {
            tree_if *const tni = static_cast < tree_if * >(tn);
            rewrite_instructions(tni->header());
            rewrite_instructions(tni->then_part());
            rewrite_instructions(tni->else_part());
            break;
          }
          case TREE_LOOP:
          {
            tree_loop *const tnl = static_cast < tree_loop * >(tn);
            rewrite_instructions(tnl->body());
            rewrite_instructions(tnl->test());
            break;
          }
          case TREE_BLOCK:
          {
            tree_block *const tb = static_cast < tree_block * >(tn);
            rewrite_instructions(tb->body());
            break;
          }
          case TREE_INSTR:
          {
            tree_instr *const tnin = static_cast < tree_instr * >(tn);
            rewrite_instructions(tnin->instr());
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

    void rewrite_instructions(instruction * const instr)
    {
      if (instr->opcode() == io_array)
      {
        in_array *const arrayInstr = static_cast < in_array * >(instr);
        var_sym *const arraySym = get_sym_of_array(arrayInstr);

        if (arrayUsage.find(arraySym) != arrayUsage.end())
        {
          std::vector < IndexInfo > &indexInfo = arrayUsage.find(arraySym)->second;
          for (unsigned int index = 0; index < arrayInstr->dims(); ++index)
          {
            if (indexInfo[index].getState() == LOOP_INDEX)
            {
              operand oldIndex = arrayInstr->index(index);

              in_ldc *const loadConstant = new in_ldc();
              loadConstant->set_value(immed(0));
              loadConstant->set_result_type(oldIndex.type());

              oldIndex.remove();
              arrayInstr->set_index(index, loadConstant);
            }
          }
        }
      }

      for (unsigned int i = 0; i < instr->num_srcs(); ++i)
      {
        operand op(instr->src_op(i));
        if (op.is_instr())
          rewrite_instructions(op.instr());
      }
    }

  public:
    ArrayContractor(tree_block * const b):block(b)
    {
    }

    void find_array_access()
    {
      find_array_access(block);
    }

    void printResults() const
    {
      for (std::map < var_sym *, std::vector < IndexInfo > >::const_iterator i = arrayUsage.begin(); i != arrayUsage.end(); ++i)
      {
        std::cout << "Array: " << i->first->name();
        const std::vector < IndexInfo > &indexInfo = i->second;
        for (unsigned int index = 0; index < indexInfo.size(); ++index)
        {
          std::cout << "Index: " << index << "   Value: ";
          indexInfo[index].print();
          std::cout << std::endl;
        }
        std::cout << std::endl;
      }
    }

    void rewriteAccesses()
    {
      rewrite_instructions(block->body());
    }

    void resizeArrays()
    {
      for (std::map < var_sym *, std::vector < IndexInfo > >::iterator arrayIterator = arrayUsage.begin();
           arrayIterator != arrayUsage.end(); ++arrayIterator)
      {
        var_sym* const array = arrayIterator->first;
        assert(array->type()->is_array());

	// Create a new type representing the reduced array
	array_type* const newType = static_cast<array_type*>(array->type()->clone());
	array_type* elementType = newType;
		
	const std::vector<IndexInfo>& indexInfo = arrayIterator->second;
 
        for(unsigned int index = 0; index<indexInfo.size(); ++index)
        {
          if (indexInfo[index].getState() == LOOP_INDEX)
	  {
            elementType->set_lower_bound(array_bound(0));
	    elementType->set_upper_bound(array_bound(0));
          }

	  if (elementType->elem_type()->is_array())
	  {
	    elementType = static_cast<array_type*>(elementType->elem_type());
	  }
	  else
	  {
	    // Final element type will be actual element type of array eg. double
	    // All types before this should be array types
            assert(index == indexInfo.size()-1);
	  }
        }

	// This method installs the type in the symbol table
	// and returns the existing one if present otherwise, the new one
	array_type* const arrayType = static_cast<array_type*>(array->parent()->install_type(newType));
	array->set_type(arrayType);
      }
    }
  };

  void do_proc(tree_proc * const tp)
  {
    // proc_sym *const psym = tp->proc();
    // std::cout << psym->name() << std::endl;
    ArrayContractor contractor(tp);
    contractor.find_array_access();
    contractor.rewriteAccesses();
    contractor.resizeArrays();

    init_scalarization();
    scalarize_proc(tp);
  }

  void contractionDoProc(tree_proc * tp, void *)
  {
    do_proc(tp);
  }
}
