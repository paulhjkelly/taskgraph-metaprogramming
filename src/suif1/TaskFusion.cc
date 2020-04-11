#include <memory>
#include <vector>
#include <set>
#include <list>
#include <map>
#include <utility>
#include <iterator>
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <suif1.h>
#include <useful.h>
#include <dependence.h>
#include <iostream>

//#define VERBOSE_FUSION

#ifdef VERBOSE_FUSION
#define INFO printf
#else
#define INFO if ( false ) printf
#endif

//char *prog_who_string = "fusion";
//char *prog_ver_string = "1";

namespace
{
  enum LoopFusionResult
  {
    FUSED_INTO_FIRST,
    FUSED_INTO_SECOND,
    FUSION_FAILED
  };

  void fuseLoopsByUsage(std::list<tree_for*>& loops);	    
  void rename(instruction * ins, var_sym * from, var_sym * to);
  void rename(tree_node * tn, var_sym * from, var_sym * to);
  void rename(tree_node_list * tnl, var_sym * from, var_sym * to);

  void rename(tree_node_list * tnl, var_sym * from, var_sym * to)
  {
    tree_node_list_iter iter(tnl);

    while (!iter.is_empty())
    {
      tree_node *tn = iter.step();
      rename(tn, from, to);
    }
  }

  void rename(tree_node * tn, var_sym * from, var_sym * to)
  {
    switch (tn->kind())
    {
      case TREE_FOR:
      {
        tree_for *const tnf = static_cast < tree_for * >(tn);
        rename(tnf->lb_list(), from, to);
        rename(tnf->ub_list(), from, to);
        rename(tnf->step_list(), from, to);
        rename(tnf->landing_pad(), from, to);
        rename(tnf->body(), from, to);
        break;
      }

      case TREE_IF:
      {
        tree_if *const tni = static_cast < tree_if * >(tn);
        rename(tni->header(), from, to);
        rename(tni->then_part(), from, to);
        rename(tni->else_part(), from, to);
        break;
      }

      case TREE_LOOP:
      {
        tree_loop *const tnl = static_cast < tree_loop * >(tn);
        rename(tnl->body(), from, to);
        rename(tnl->test(), from, to);
        break;
      }

      case TREE_BLOCK:
      {
        tree_block *const tnb = static_cast < tree_block * >(tn);
        rename(tnb->body(), from, to);
        break;
      }

      case TREE_INSTR:
      {
        tree_instr *const tnin = static_cast < tree_instr * >(tn);
        rename(tnin->instr(), from, to);
        break;
      }

      default:
      {
        assert(false);
        break;
      }
    }
  }


  void rename(instruction* const ins, var_sym* const from, var_sym* const to)
  {
    for (unsigned i = 0; i < ins->num_srcs(); ++i)
    {
      operand op(ins->src_op(i));

      if (op.is_instr())
      {
        rename(op.instr(), from, to);
      }
      else if (op.is_symbol() && op.symbol()->is_var() && op.symbol() == from)
      {
        ins->set_src_op(i, operand(to));
      }
    }
  }

  struct compareArray
  {
    compareArray(in_array* const _array) : array(_array)
    {
    }

    bool operator() (in_array* const rhs) const
    {
      return get_sym_of_array(array) == get_sym_of_array(rhs);
    }

    in_array* const array;
  };

  class VariableAccess
  {
  public:
    VariableAccess():tooComplicated(false)
    {
    }

    void clear()
    {
      readScalarId.clear();
      writeScalarId.clear();
      readArray.clear();
      writeArray.clear();
      tooComplicated = false;
    }

    void merge(const VariableAccess& access)
    {
      tooComplicated = tooComplicated || access.tooComplicated;
      readScalarId.insert(access.readScalarId.begin(), access.readScalarId.end());
      writeScalarId.insert(access.writeScalarId.begin(), access.writeScalarId.end());
      readArray.insert(access.readArray.begin(), access.readArray.end());
      writeArray.insert(access.writeArray.begin(), access.writeArray.end());
    }

    void findVariableAccess(instruction* const ins, const bool base, const bool write)
    {
      unsigned start = 0;

      if (ins->opcode() == io_array)
      {
        in_array* const ia = static_cast<in_array*>(ins);
        var_sym* const sym = get_sym_of_array(ia);

        if (write)
        {
          INFO("Write Array:%s\n", sym->name());
          writeArray.insert(ia);
        }
        else
        {
          INFO("Read Array:%s\n", sym->name());
          readArray.insert(ia);
        }
      }

      if (base)
      {
        operand op(ins->dst_op());

        if (op.is_symbol())
        {
          INFO("Base Write:%s\n", op.symbol()->name());
          writeScalarId.insert(op.symbol());
        }
        if (op.is_instr())
        {
          assert(false);
        }
      }

      if (ins->opcode() == io_jmp)
      {
        INFO("Complicated: Jump Statement\n");
        tooComplicated = true;
        return;
      }

      if (ins->opcode() == io_str || ins->opcode() == io_memcpy)
      {
        operand op(ins->src_op(0));

        if (op.is_symbol())
        {
          INFO("Write:%s\n", op.symbol()->name());
          writeScalarId.insert(op.symbol());
        }
        else if (op.is_instr())
        {
          if (op.instr()->opcode() == io_array)
          {
            findVariableAccess(op.instr(), false, true);
          }
          else
          {
            INFO("Complicated: Expected array\n");
            tooComplicated = true;
            return;
          }
        }
        else
        {
          INFO("Complicated: Expected array or symbol\n");
          tooComplicated = true;
          return;
        }
        start = 1;
      }
      for (unsigned i = start; i < ins->num_srcs(); ++i)
      {
        operand op(ins->src_op(i));

        if (op.is_symbol())
        {
          INFO("Read:%s\n", op.symbol()->name());
          readScalarId.insert(op.symbol());
        }
        if (op.is_instr())
          findVariableAccess(op.instr(), false, false);
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
          findVariableAccess(tnin->instr(), true, false);
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

    void findVariableAccessBetween(tree_for* const first, tree_for* const second)
    {
      assert(first != second);
      assert(first->parent() == second->parent());
      
      tree_node_list* const list = first->parent();

      tree_node_list_iter iter(list);

      bool passedFirst = false;
      bool reachedSecond = false;

      while(!iter.is_empty() && !reachedSecond)
      {
        tree_node* const node = iter.step();
	        
	if (node == second)
	{
	  reachedSecond = true;
	  assert(passedFirst);
	}

	if (passedFirst && !reachedSecond)
	{
          findVariableAccess(node);
	}
		
	if (node == first)
	{
          passedFirst = true;
	  assert(!reachedSecond);
	}
      }
      
    }

    bool checkScalars(const VariableAccess & varAccB) const
    {
      INFO("\nChecking Scalars\n");
      for(std::set<sym_node*>::const_iterator writeIter=writeScalarId.begin(); writeIter!=writeScalarId.end(); ++writeIter)
      {
        if (varAccB.readScalarId.find(*writeIter) != varAccB.readScalarId.end())
        {
          INFO("Write/Read Conflict %s\n", (*writeIter)->name());
          return false;
        }
        if (varAccB.writeScalarId.find(*writeIter) != varAccB.writeScalarId.end())
        {
          INFO("Write/Write Conflict %s\n", (*writeIter)->name());
          return false;
        }
      }

      for(std::set<sym_node*>::const_iterator readIter=readScalarId.begin(); readIter!=readScalarId.end(); ++readIter)
      {
        if(varAccB.writeScalarId.find(*readIter) != varAccB.writeScalarId.end())
	{
          INFO ( "Read/Write Conflict %s\n", (*readIter)->name ( ) );
          return false;										
	}
      }
      return true;
    }

    // Only for inbetween code
    bool checkArrays(const VariableAccess & varAccB) const
    {
      INFO("\nChecking Arrays\n");
      for(std::set<in_array*>::const_iterator writeIter=writeArray.begin(); writeIter!=writeArray.end(); ++writeIter)
      {
        if (std::find_if(varAccB.readArray.begin(), varAccB.readArray.end(), compareArray(*writeIter)) != varAccB.readArray.end())
        {
          INFO("Read/Write Conflict %s\n", get_sym_of_array(*writeIter)->name());
          return false;
        }
        if (std::find_if(varAccB.writeArray.begin(), varAccB.writeArray.end(), compareArray(*writeIter)) !=
            varAccB.writeArray.end())
        {
          INFO("Write/Write Conflict %s\n", get_sym_of_array(*writeIter)->name());
          return false;
        }
      }

      for(std::set<in_array*>::iterator readIter=readArray.begin(); readIter!=readArray.end(); ++readIter)
      {
        if ( std::find_if(varAccB.writeArray.begin(), varAccB.writeArray.end(), compareArray(*readIter)) != varAccB.writeArray.end()) 
	{
          INFO ( "Write/Read Conflict %s\n", get_sym_of_array(*readIter )->name());
          return false;
        }
      }
      return true;
    }

    static var_sym* getArraySymbolFromInstruction(in_array* const instr)
    {
      return get_sym_of_array(instr);
    }

    std::set<var_sym*> getReadArrays() 
    {
      std::set<var_sym*> arrays;
      std::transform(readArray.begin(), readArray.end(), std::inserter(arrays, arrays.begin()), getArraySymbolFromInstruction);
      return arrays;
    }

    std::set<var_sym*> getWrittenArrays() 
    {
      std::set<var_sym*> arrays;
      std::transform(writeArray.begin(), writeArray.end(), std::inserter(arrays, arrays.begin()), getArraySymbolFromInstruction);
      return arrays;		  
    }

  public:
    std::set<sym_node*> readScalarId, writeScalarId;
    std::set<in_array*> readArray, writeArray;
    bool tooComplicated;
  };

  bool dependenceTest(in_array* const arrayA, in_array* const arrayB)
  {
    deptest_result res;
    std::auto_ptr<dvlist> dep(DependenceTest(arrayA, arrayB, 0, &res, TRUE));

    if (res == dt_none)
    {
      INFO("None\n");
      dep->clear();
      return false;
    }
    else if (res == dt_indep)
    {
      INFO("Independent\n");
      dep->clear();
    }
    else if (res == dt_no_common_nest)
    {
      INFO("No Common Nest\n");
      dep->clear();
      return false;
    }
    else if (res == dt_too_messy)
    {
      INFO("Messy\n");
      dep->clear();
      return false;
    }
    assert(dep.get() != NULL);
#ifdef VERBOSE_FUSION
    dep->print(stdout);
#endif

    dvlist_iter depiter(dep.get());
    while (!depiter.is_empty())
    {
      distance_vector* const dv = depiter.step()->dv;
      const int level = dv->first_not_eq();
      if ( level != 0 ) 
      {
	const std::auto_ptr<distance> d(dv->thresh(level));
        if (!(d->dir() == d_gt)) 
	{
          INFO ( "Illegal fusion\n" );
          dep->clear();
          return false;
        }
      }
    }
                                                                                                      
    dep->clear();
    return true;
  }

// Checks the reads and write of the array accesses.
// If array names are same, then do dependence checks.
  bool checkArrayAccess(const VariableAccess & varAccA, const VariableAccess & varAccB)
  {
    INFO("\nChecking Arrays\n");
    std::vector<in_array*>::const_iterator writeEnd, writeIter;

    for(std::set<in_array*>::const_iterator writeIter=varAccA.writeArray.begin(); writeIter!=varAccA.writeArray.end(); ++writeIter)
    {
      var_sym* const writeSym = get_sym_of_array(*writeIter);

      for(std::set<in_array*>::const_iterator iter=varAccB.readArray.begin(); iter!=varAccB.readArray.end(); ++iter)
      {
        if (writeSym == get_sym_of_array(*iter))
        {
          // Check Write Read
          INFO("Write/Read Check %s %s\n", writeSym->name(), get_sym_of_array(*iter)->name());
          if (!dependenceTest(*writeIter, *iter))
            return false;
        }
      }
      
      for(std::set<in_array*>::const_iterator iter=varAccB.writeArray.begin(); iter!=varAccB.writeArray.end(); ++iter)
      {
        if (writeSym == get_sym_of_array(*iter))
        {
          // Check Write Read
          INFO("Write/Write Check %s %s\n", writeSym->name(), get_sym_of_array(*iter)->name());
          if (!dependenceTest(*writeIter, *iter))
            return false;
        }
      }
    }

    for(std::set<in_array*>::const_iterator writeIter=varAccB.writeArray.begin(); writeIter!=varAccB.writeArray.end(); ++writeIter)
    {
      var_sym* const writeSym = get_sym_of_array(*writeIter);
      for(std::set<in_array*>::const_iterator iter=varAccA.readArray.begin(); iter!=varAccA.readArray.end(); ++iter)
      {
        if (writeSym == get_sym_of_array(*iter))
        {
          // Check Write Read
          INFO("Read/Write Check %s %s\n", writeSym->name(), get_sym_of_array(*iter)->name());
          if (!dependenceTest(*iter, *writeIter))
            return false;
        }
      }
    }
    return true;
  }

  bool isBefore(tree_for* const first, tree_for* const second)
  {
    assert(first != second);
    assert(first->parent() == second->parent());
    
    tree_node_list* const list = first->parent();
    tree_node_list_iter iter(list);
    while(!iter.is_empty())
    {
      tree_node* const node = iter.step();
      if (node == first)
        return true;
      if (node == second)
        return false;
    }

    assert(false);
    return false;
  }
							 

  LoopFusionResult attemptLoopFusion(tree_for* const loopA, tree_for* const loopB)
  {
    assert(loopA != loopB);
    assert(isBefore(loopA, loopB));
 
    VariableAccess access;
    access.findVariableAccessBetween(loopA, loopB);
	
    bool mergeWithFirstLoop = true;

    // Check Bounds
    int lb, ub, step, blb, bub, bstep;

    if (!(loopA->lb_is_constant(&lb) && loopA->ub_is_constant(&ub) && loopA->step_is_constant(&step)))
    {
      INFO("Non-constant bounds\n");
      return FUSION_FAILED;
    }
    else if (!(loopB->lb_is_constant(&blb) && loopB->ub_is_constant(&bub) && loopB->step_is_constant(&bstep)))
    {
      INFO("Non-constant bounds\n");
      return FUSION_FAILED;
    }
    else if (lb != blb || ub != bub || step != bstep)
    {
      INFO("Bounds do not match\n");
      return FUSION_FAILED;
    }
    else if (loopA->test() != loopB->test())
    {
      INFO("Loops have different test\n");
      return FUSION_FAILED;
    }
    else if (access.tooComplicated)
    {
      INFO("No fusion - In betwen code too Complicated\n");
      return FUSION_FAILED;
    }

    // TODO: Check for landing pad
    INFO("\nChecking Var Access\n");
    VariableAccess varAccA, varAccB;

    INFO("\nFirst Loop\n");
    varAccA.findVariableAccess(loopA->body());
    INFO("\nSecond Loop\n");
    varAccB.findVariableAccess(loopB->body());
    if (varAccA.tooComplicated)
    {
      INFO("No fusion - Loop A Complicated\n");
      return FUSION_FAILED;
    }
    else if (varAccB.tooComplicated)
    {
      INFO("No fusion - Loop B Complicated\n");
      return FUSION_FAILED;
    }

    // Check Scalars
    INFO("\nChecking Scalars\n");
    if (!varAccA.checkScalars(varAccB) )
      return FUSION_FAILED;

    // Check inbetween code
    if (varAccA.checkScalars(access) && varAccA.checkArrays(access))
    {
      // If first loop does not interfere with intervening, prepend onto second
      mergeWithFirstLoop = false;
      INFO("Attempting merge with second loop\n");
    }
    else if (varAccB.checkScalars(access) && varAccB.checkArrays(access))
    {
      // If second loop does not interfere with the intervening, append to first
      mergeWithFirstLoop = true;
      INFO("Attempting merge with first loop\n");
    }
    else
    {
      // Both loops have dependencies on intervening, so fusion can't be done
      return FUSION_FAILED;
    }

    bool renamed = false;
    if (loopA->index() != loopB->index())
    {
      INFO("Loops have different index var\n");
      if (find(varAccB.writeScalarId.begin(), varAccB.writeScalarId.end(), loopA->index()) != varAccB.writeScalarId.end())
      {
        INFO("First index variable used in second loop\n");
        return FUSION_FAILED;
      }
      else if (find(varAccB.readScalarId.begin(), varAccB.readScalarId.end(), loopA->index()) != varAccB.readScalarId.end())
      {
        INFO("First index variable used in second loop\n");
        return FUSION_FAILED;
      }

      // TODO: Set index to max if needed
      if (mergeWithFirstLoop)
      {
        rename(loopB, loopB->index(), loopA->index());
        renamed = true;
      }
      else
      {
        rename(loopA, loopA->index(), loopB->index());
        renamed = true;
      }
    }

    if (mergeWithFirstLoop)
    {
      tree_node_list_e *const headB = loopB->body()->head();

      loopA->body()->append(loopB->body());
      if (!checkArrayAccess(varAccA, varAccB))
      {
        INFO("Merging with first loop failed - putting body back\n");
        tree_node_list_e *currentNode = headB;

        while (currentNode != NULL)
        {
          tree_node_list_e *const nextNode = currentNode->next();
          loopA->body()->remove(currentNode);
          loopB->body()->append(currentNode);
          currentNode = nextNode;
        }
        if (renamed)
          rename(loopB, loopA->index(), loopB->index());

        return FUSION_FAILED;
      }
      else
      {
	kill_node(loopB);
        return FUSED_INTO_FIRST;
      }
    }
    else
    {
      tree_node_list_e *const headA = loopA->body()->head();
      tree_node_list_e *const headB = loopB->body()->head();

      loopB->body()->push(loopA->body());
      if (!checkArrayAccess(varAccA, varAccB))
      {
        INFO("Merging with second loop failed - putting body back\n");
        tree_node_list_e *currentNode = headA;

        while (currentNode != headB)
        {
          tree_node_list_e *const nextNode = currentNode->next();
          loopB->body()->remove(currentNode);
          loopA->body()->append(currentNode);
          currentNode = nextNode;
        }
        if (renamed)
          rename(loopA, loopB->index(), loopA->index());

        return FUSION_FAILED;
      }
      else
      {
	kill_node(loopA);
	return FUSED_INTO_SECOND;
      }

    }
  }

  void fuseLoopsByUsage(tree_node_list* const tnl)
  {
    std::list<tree_for*> loops;
    tree_node_list_iter iter(tnl);

    // Get list of high level loops
    while (!iter.is_empty())
    {
      tree_node* const tn = iter.step();
      if (tn->kind() == TREE_FOR)
      {
        tree_for* const tnf = static_cast < tree_for * >(tn);
	loops.push_back(tnf);
      }
    }

    // try to fuse high level loops
    fuseLoopsByUsage(loops);

    iter.reset(tnl);
    while(!iter.is_empty())
    {
      tree_node* const tn = iter.step();
      switch(tn->kind())
      {
        case TREE_FOR:
        {
          tree_for* const tnf = static_cast < tree_for * >(tn);
          fuseLoopsByUsage(tnf->body());
	  break;
        }
        case TREE_IF:
        {
          tree_if *const tni = static_cast < tree_if * >(tn);
          fuseLoopsByUsage(tni->then_part());
          fuseLoopsByUsage(tni->else_part());
          break;
        }
        case TREE_LOOP:
        {
          tree_loop *const tnl = static_cast < tree_loop * >(tn);
          fuseLoopsByUsage(tnl->body());
          fuseLoopsByUsage(tnl->test());
          break;
        }
        case TREE_BLOCK:
        {
          tree_block *const tnb = static_cast < tree_block * >(tn);
          fuseLoopsByUsage(tnb->body());
          break;
        }
        case TREE_INSTR:
        {
          // Nothing to do as no loops to fuse inside instruction
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

  class LoopInfo
  {
  private:
    tree_for* loop;
    std::set<var_sym*> arrayAccess;
    bool dead;

    static const std::set<var_sym*> findArrayAccess(tree_for* const loop)
    {
      std::set<var_sym*> arrayAccess;
      VariableAccess varAccess;

      varAccess.findVariableAccess(loop);
      const std::set<var_sym*> readAccess(varAccess.getReadArrays()), writeAccess(varAccess.getWrittenArrays());
      arrayAccess.insert(readAccess.begin(), readAccess.end());
      arrayAccess.insert(writeAccess.begin(), writeAccess.end());
      return arrayAccess;
    }
    
  public:
    LoopInfo(tree_for* const l) : loop(l), arrayAccess(findArrayAccess(l)), dead(false)
    {
    }

    bool isDead() const
    {
      return dead;
    }

    bool attemptFusion(LoopInfo& otherLoop, const bool agressive)
    {
      assert(!dead);
      assert(!otherLoop.dead);

      // Give up if loops are same
      if (loop == otherLoop.loop)
        return false;
               
      // If loops are wrong way round, reverse the method call
      if (!isBefore(loop, otherLoop.loop))
        return otherLoop.attemptFusion(*this, agressive);

      // Do not fuse if loops do not access same array
      std::set<var_sym*> intersection;
      std::insert_iterator< std::set<var_sym*> > intersectionInserter(intersection, intersection.begin());
      std::set_intersection(arrayAccess.begin(), arrayAccess.end(),
                            otherLoop.arrayAccess.begin(), otherLoop.arrayAccess.end(),
                            intersectionInserter);

      // Fused only when they share array accesses or agressive is requested
       if (intersection.empty() && !agressive)
         return false;

      const LoopFusionResult result = attemptLoopFusion(loop, otherLoop.loop);

      if (result == FUSED_INTO_FIRST)
      {
        otherLoop.dead=true;
	arrayAccess.insert(otherLoop.arrayAccess.begin(), otherLoop.arrayAccess.end());
	return true;
      }
      else if (result == FUSED_INTO_SECOND)
      {
        dead = true;
	otherLoop.arrayAccess.insert(arrayAccess.begin(), arrayAccess.end());
        return true;
      }
      else if (result == FUSION_FAILED)
      {
        return false;
      }
      else
      {
        assert(false);
	return false;
      }
    }
  };

  void fuseAll(std::vector<LoopInfo>& usage, const bool agressive)
  {
    bool changed = true;
    while (changed)
    {
      changed = false;
      for(std::vector<LoopInfo>::size_type firstIndex=0; firstIndex<usage.size(); ++firstIndex)
      {
        for(std::vector<LoopInfo>::size_type secondIndex=firstIndex; secondIndex<usage.size(); ++secondIndex)
        {
          if (!(usage[firstIndex].isDead() || usage[secondIndex].isDead()))
          {
            const bool successfulFusion = usage[firstIndex].attemptFusion(usage[secondIndex], agressive);
            changed = changed || successfulFusion;
          }
        }
      }
    } 
  }

  void fuseLoopsByUsage(std::list<tree_for*>& loops)
  {
    std::vector<LoopInfo> usage;
    
    for(std::list<tree_for*>::iterator loopIter = loops.begin(); loopIter!=loops.end(); ++loopIter)
    {
      usage.push_back(LoopInfo(*loopIter));
    }

    fuseAll(usage, true);
  }

}

/***************************************************************************
 * create the annotations needed by dependence by calling fill_in_access   *
 * and start processing..                                                  *
 ***************************************************************************/
namespace tg
{
  void fusionDoProc(tree_proc* const tp, void *)
  {
    proc_sym* const psym = tp->proc();
    INFO("=======%s======= \n", psym->name());
    fill_in_access(tp);
    fuseLoopsByUsage(tp->body());
  }
}

/***************************************************************************
 * Initialize and iterate over all the procedures.                         *
 ***************************************************************************/
/*main(int argc, char * argv[])
{
    start_suif ( argc, argv );
    suif_proc_iter ( argc, argv, do_proc, TRUE );
    return 0;
}
*/
