#include <TaskGraph>
#include <boost/numeric/conversion/cast.hpp>

// SUIF headers
#undef assert
#include "suif1.h"
#include "transform.h"

namespace {
  using tg::LoopIdentifier;

  bool findLoop ( tree_node_list *loop, LoopIdentifier &loopId, unsigned found, unsigned depth, tree_for **tnf );

  bool findLoop ( tree_node *tn, LoopIdentifier &loopId, unsigned found, unsigned depth, tree_for **loop ) {
    switch ( tn->kind ( ) ) {
    case TREE_FOR: {
      tree_for * tnf = static_cast<tree_for *>(tn);
      if ( ++found == loopId[depth] ) {
	if ( ++depth == loopId.depth ) {
	  *loop = tnf;
	  return true;
	} else {
	  return findLoop ( tnf->body(), loopId, 0, depth, loop );
	}
      }
      break;
    }

    case TREE_IF: {
      tree_if * tni = static_cast<tree_if *>(tn);
      return findLoop ( tni->then_part(), loopId, found, depth, loop )
	|| findLoop ( tni->else_part(), loopId, found, depth, loop );
      break;
    }

    case TREE_LOOP: {
      tree_loop * tnl = static_cast<tree_loop *>(tn);
      return findLoop ( tnl->body(), loopId, found, depth, loop );
      break;
    }

    case TREE_BLOCK: {
      tree_block * tnb = static_cast<tree_block *>(tn);
      return findLoop ( tnb->body(), loopId, found, depth, loop );
      break;
    }

    default:
      break;
    }
    return false;
  }

  bool findLoop ( tree_node_list *loop, LoopIdentifier &loopId, unsigned found, unsigned depth, tree_for **tnf ) {
    tree_node_list_iter iter ( loop );
    while ( !iter.is_empty ( ) ) {
      if ( findLoop ( iter.step ( ), loopId, found, depth, tnf ) ) {
	return true;
      }
    }
    return false;
  }

  tree_for *findLoop ( tree_proc * tp, LoopIdentifier &loopId ) {
    tree_for *tnf;
    if ( findLoop ( tp->body(), loopId, 0, 0, &tnf ) ) {
      return tnf;
    }
    std::cout << "Failed to find loop\n";
    return 0;
  }

  tree_for *findLoop ( tree_for *start, LoopIdentifier &loopId ) {
    tree_for *tnf;
    if ( findLoop ( start->body(), loopId, 0, 0, &tnf ) ) {
      return tnf;
    }
    std::cout << "Failed to find loop\n";
    return 0;
  }

}

namespace tg {
  void tileDoProc ( tree_proc * tp, void *param ) {
    if ( !param )
      throw tg::TaskException ( "No parameters provided for tiling." );
    TileSettings *settings = static_cast<TileSettings *>( param );

    LoopIdentifier nextLoop ( 1 );

    boolean coalesceArr[] = { settings->coalesce ? TRUE : FALSE };
    int first[] = { 0, boost::numeric_cast<int>(settings->depth) };
    tree_for **fors = new tree_for*[settings->depth];
    boolean *doall = new boolean[settings->depth];
    int *trip = new int[settings->depth];

    fors[0] = findLoop ( tp, settings->firstLoop );
    doall[0] = FALSE;
    trip[0] = settings->tileSize;
    for ( unsigned a = 1; a < settings->depth; ++a ) {
      fors[a] = findLoop ( fors[a - 1], nextLoop );
      doall[a] = FALSE;
      trip[a] = settings->tileSize;
    }

    {
      loop_transform lt ( settings->depth, fors, doall );
      lt.tile_transform ( trip, 1, coalesceArr, first );
    }
    delete [] fors;
    delete [] doall;
    delete [] trip;
  }

  template<typename T, typename R>
  inline void swap ( T *a, T *b, R (T::*getFunc)(), void (T::*setFunc)(R) ) {
    R temp = (a->*getFunc) ( );
    (a->*setFunc) ( (b->*getFunc) ( ) );
    (b->*setFunc) ( temp );
  }

  template<typename T>
  inline void swap ( T *a, T *b, operand (T::*getFunc)(), void (T::*setFunc)(operand) ) {
    operand tempa = (a->*getFunc) ( );
    operand tempb = (b->*getFunc) ( );
    tempa.remove();
    tempb.remove();
    (a->*setFunc) ( tempb );
    (b->*setFunc) ( tempa );
  }

  void interchangeLoopsDoProc ( tree_proc * tp, void *param ) {
    if ( !param )
      throw tg::TaskException ( "No parameters provided for interchange." );

    InterchangeSettings *settings = static_cast<InterchangeSettings *>( param );

    tree_for *fors[2];
    fors[0] = findLoop ( tp, settings->firstLoop );
    fors[1] = findLoop ( tp, settings->secondLoop );
    //	boolean doall[2] = { FALSE, FALSE };

    /*	integer_matrix mat ( 2, 2 );
	mat[0][0] = 0;
	mat[1][1] = 0;
	mat[0][1] = 1;
	mat[1][0] = 1;

	loop_transform lt ( 2, fors, doall );
	lt.unimodular_transform ( mat );
    */

    swap ( fors[0], fors[1], &tree_for::index, &tree_for::set_index );
    swap ( fors[0], fors[1], &tree_for::test, &tree_for::set_test );
    swap ( fors[0], fors[1], &tree_for::landing_pad, &tree_for::set_landing_pad );
    swap ( fors[0], fors[1], &tree_for::lb_op, &tree_for::set_lb_op );
    swap ( fors[0], fors[1], &tree_for::ub_op, &tree_for::set_ub_op );
    swap ( fors[0], fors[1], &tree_for::step_op, &tree_for::set_step_op );
  }

  void skewLoopsDoProc ( tree_proc * tp, void *param ) {
    if ( !param )
      throw tg::TaskException ( "No parameters provided for skewing." );

    SkewSettings *settings = static_cast<SkewSettings *>( param );

    tree_for *fors[2];
    fors[0] = findLoop ( tp, settings->firstLoop );
    fors[1] = findLoop ( tp, settings->secondLoop );
    boolean doall[2] = { FALSE, FALSE };

    integer_matrix mat ( 2, 2 );
    mat[0][0] = 1;
    mat[1][1] = 1;
    mat[0][1] = 0;
    mat[1][0] = settings->distance;
    {
    loop_transform lt ( 2, fors, doall );
    lt.unimodular_transform ( mat );
    }
  }

}
