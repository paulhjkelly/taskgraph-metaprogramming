#include <TaskGraph.h>
#include <TaskIRSuif.h>
#include <TaskException.h>
#include <TaskType.h>
#include <boost/shared_ptr.hpp>

// SUIF headers
#undef assert
#include "suif1.h"
#include "useful.h"
#include <algorithm>
#include <cstring>

const char *prog_who_string = "TaskGraph";
const char *prog_ver_string = "v1";

#define BITS_PER_BYTE 8

namespace {

  // These have to be in this order
  if_ops binaryOperator[] = {
    io_add,
    io_sub,
    io_mul,
    io_div,
    io_seq,
    io_sne,
    io_sl,
    io_sl, // greater than (operands are swapped)
    io_sle,
    io_sle, // greater than or equal to  (operands are swapped)
    io_ior,
    io_and,
    io_ior,  // Bitwise
    io_and,  // Bitwise
    io_lsl,
    io_lsr
  };

  // These have to be in this order
  if_ops unaryOperator[] = {
    io_neg,
    io_not,
    io_neg
  };
}

int s2c_main( int argc, char **argv, file_set_entry *entry, bool toScreen );

// This needs to be in the global namespace because of dodgy macros.
// Hence the long name to avoid conflicts
void TaskGraph_SUIFIR_DoLibraryInit ( ) {
  LIBRARY(dependence, init_dependence, exit_dependence);
  LIBRARY(suifmath, init_suifmath, exit_suifmath);
  LIBRARY(builder, init_builder, exit_builder);
  LIBRARY(useful, init_useful, exit_useful);
  LIBRARY(transform, init_transform, exit_transform);
}

namespace {
  struct suifcleaner {
    ~suifcleaner() {
       tg::TaskIR::exit();
    }
  } theonlyone;
}

namespace tg {

  void fusionDoProc ( tree_proc * tp, void * );
  void tileDoProc ( tree_proc * tp, void * );
  void interchangeLoopsDoProc ( tree_proc * tp, void * );
  void skewLoopsDoProc ( tree_proc * tp, void * );
  void contractionDoProc( tree_proc* tp, void * );
  void raiseInitialAssignmentsDoProc( tree_proc* tp, void * );
  void mallocLargeArraysDoProc( tree_proc* tp, void * );
  
  std::map<std::string, TaskIR::optFuncSig> TaskIR::optimisationMap;

  static int scopeNum = 0;

  static char *makeScopeName( char *buffer ) {
    sprintf ( buffer, "scope%d_", scopeNum++ );
    return buffer;
  }

  static int stringNum = 0;

  static char *createStringName ( char *buffer ) {
    sprintf ( buffer, "string%d_", stringNum++ );
    return buffer;
  }

  bool TaskIR::initialised = false;

  void TaskIR::initialise ( ) {
    if (initialised)
       return;
    int argc = 1;
    const char* argv[] = { "taskgraph", NULL };
    TaskGraph_SUIFIR_DoLibraryInit ( );
    init_suif ( argc, const_cast<char**>(argv));
    addOptimisation ( "fusion", &fusionDoProc );
    addOptimisation ( "tile", &tileDoProc );
    addOptimisation ( "interchange", &interchangeLoopsDoProc );
    addOptimisation ( "skew", &skewLoopsDoProc );
    addOptimisation ( "array_contraction", &contractionDoProc );
    addOptimisation ( "raise_initial_assignments", &raiseInitialAssignmentsDoProc );
    addOptimisation ( "malloc_large_arrays", &mallocLargeArraysDoProc );
    initialised = true;
  }

  void TaskIR::exit ( ) {
    exit_suif ( );
    initialised = false;
  }

  tree_node_list *StatementListHolder::makeTreeList ( StatementListHolder *sc ) {
    /*	if ( ownScope && !baseScope ) {
	tree_node_list *returnList = new tree_node_list ( );
	returnList->append(new tree_block ( list, scope ));
	sc->scope->add_child(scope);
	return returnList;
	}*/
    return list;
    /*	if ( ownScope && !baseScope ) {
	printf ( "Making\n" );
	return new tree_node_list ( new tree_block ( list ,scope ) );
	}
	return list;*/
  }

  TaskIR::TaskIR() : entry ( 0 ) {
  }

  TaskIR::~TaskIR ( ) {
    delete procSym->block(); procSym->set_block(0);

    // Avoids a SUIF bug with remove_file
    delete fileset->file_list()->remove( fileset->file_list()->lookup( entry ) );
    delete entry; entry = 0;
    delete procSym; procSym = 0;
  }

  void TaskIR::deepClone ( const TaskIR &object, const char *_procName ) {
    procName = _procName;

    voidptrType = type_void->ptr_to();
    voidvoidptrType = voidptrType->ptr_to();
    func_type *funcType = new func_type ( type_void, voidvoidptrType );

    entry = fileset->add_file ( NULL, NULL );

    proc_symtab *st = new proc_symtab ( const_cast<char *>(procName) );
    entry->symtab()->add_child(st);

    tree_proc *mainBlock = new tree_proc ( object.procSym->block()->body()->clone( st ), st );

    procSym = new proc_sym ( funcType, src_c, const_cast<char *>(procName) ); // SUIF const fix
    procSym->set_fse ( entry );
    //	tree_proc *mainBlock = object.procSym->block()->clone( );
    procSym->set_block ( mainBlock );

    paramSym = mainBlock->proc_syms()->new_var ( voidvoidptrType, const_cast<char *>("params") );
    paramSym->set_param ( );

    mainBlock->proc_syms()->params()->append( paramSym );

    //procSym = (proc_sym*)object.procSym->copy ( );
    //procSym->set_fse ( entry );

    // TODO: Get paramSym
  }

  /** Appends a statement onto a list of statements
   */
  void TaskIR::appendStatement ( tgStatementList list, tgStatement statement ) {
    list->list->append ( statement );
  }


  /** Creates a statement list object
   */
  TaskIR::tgStatementList TaskIR::createStatementList ( tgStatementList scope ) {
    StatementListHolder *listHolder = new StatementListHolder ( );
    listHolder->list = new tree_node_list();
    //	listHolder->scope = scope->scope;
    listHolder->ownScope = false;
    listHolder->baseScope = false;
    return boost::shared_ptr<StatementListHolder>(listHolder);
  }

  int num = 0;

  TaskIR::tgCodeLabel TaskIR::createCodeLabel ( tgStatementList scope ) {
    // TODO:Improve
    char name[128];
    sprintf ( name, "lab%d", num++ );
    return symTab->new_label(name);
  }

  /** create a while loop with expression 'test' and body of 'list'
   */
  TaskIR::tgLoopStatement TaskIR::createWhileStatement ( tgStatementList scope, tgExpression test, tgStatementList list ) {
    label_sym *label = createCodeLabel ( scope );
    label_sym *loop_label = createCodeLabel ( scope );
    label_sym *breakLabel = createCodeLabel( scope );
    label_sym *continueLabel = createCodeLabel( scope );

    tree_node_list *hiflist = new tree_node_list ( );
    hiflist->append ( new tree_instr ( new in_bj ( io_bfalse, label, test ) ) );
    tree_node_list *hllist = new tree_node_list ( );
    hllist->append ( new tree_instr ( new in_bj ( io_btrue, loop_label, test->clone() ) ) );

    tree_node_list *loop_node = new tree_node_list ( );
    loop_node->append ( new tree_loop ( list->makeTreeList(scope.get()),
					hllist,
					continueLabel,
					breakLabel,
					loop_label ) );
    // SUIF 1 only supports do while loops use if to simulate them
    return new tree_if ( label,
			 hiflist,
			 loop_node,
			 new tree_node_list ( ) );

  }

  /** Create an if statement
   */
  TaskIR::tgStatement TaskIR::createIfStatement ( tgStatementList scope, tgExpression ifpart, tgStatementList thenpart, tgStatementList elsepart ) {
    label_sym *label = createCodeLabel ( scope );
    tree_node_list *hlist = new tree_node_list ( );
    hlist->append ( new tree_instr ( new in_bj ( io_bfalse, label, ifpart ) ) );
    return new tree_if ( label,
			 hlist,
			 thenpart->makeTreeList(scope.get()),
			 (elsepart != 0) ? elsepart->makeTreeList(scope.get()) : new tree_node_list ( ) );
  }

  tree_node_list *createNodeList ( instruction *expr ) {
    tree_node_list *list = new tree_node_list ( );
    list->append ( new tree_instr ( expr ) );
    return list;
  }

  /** Create a for statement
   */
  TaskIR::tgLoopStatement TaskIR::createForStatement ( tgStatementList scope, tgVariable symbol, bool varSigned, tgExpression from, tgExpression to, tgExpression step, tgStatementList list, BinaryOperatorType comparison ) {
    tree_for_test compare;
    if ( varSigned )
      compare = (comparison == opLESSTHANOREQUALTO) ? FOR_SLTE : FOR_SGTE;
    else
      compare = (comparison == opLESSTHANOREQUALTO) ? FOR_ULTE : FOR_UGTE;

    label_sym *breakLabel = createCodeLabel(scope);
    label_sym *continueLabel = createCodeLabel(scope);

    return new tree_for ( symbol,
			  compare,
			  continueLabel,
			  breakLabel,
			  list->makeTreeList(scope.get()),
			  from,
			  to,
			  step,
			  new tree_node_list ( ) );
  }

  TaskIR::tgStatement TaskIR::createContinueStatement ( tgLoopStatement loop ) {
    switch (loop->kind()) {
    case TREE_FOR:
      return createJumpStatement(static_cast<tree_for *>(loop)->contlab());
    case TREE_LOOP:
      return createJumpStatement(static_cast<tree_loop *>(loop)->contlab());
    case TREE_IF:
      {
	tree_if *tif = static_cast<tree_if *>(loop);
	tree_node *child = tif->then_part()->head()->contents;
	return createContinueStatement(child);
      }
    default:
      return 0;
    }
  }

  TaskIR::tgStatement TaskIR::createBreakStatement ( tgLoopStatement loop ) {
    switch (loop->kind()) {
    case TREE_FOR:
      return createJumpStatement(static_cast<tree_for *>(loop)->brklab());
    case TREE_LOOP:
      return createJumpStatement(static_cast<tree_loop *>(loop)->brklab());
    case TREE_IF:
      {
	tree_if *tif = static_cast<tree_if *>(loop);
	tree_node *child = tif->then_part()->head()->contents;
	return createBreakStatement(child);
      }
    default:
      return 0;
    }
  }

  TaskIR::tgStatement TaskIR::finishWhileStatement ( tgLoopStatement loop ) {
    return loop;
  }

  TaskIR::tgStatement TaskIR::finishForStatement ( tgLoopStatement loop ) {
    return loop;
  }

  TaskIR::tgStatement TaskIR::createScopeStatement ( tgStatementList scope, tgStatementList list ) {
    //	if ( !list->ownScope && !list->baseScope ) {
    //		char buffer[128];
    //		list->scope = new block_symtab ( makeScopeName(buffer) );
    //	}
    //	scope->scope->add_child(list->scope);
    block_symtab *symtab = new block_symtab ( "" );
    symTab->add_child ( symtab );
    return new tree_block ( list->list, symtab );
  }


  /** Init the library returning the statement list of the body
   */
  TaskIR::tgStatementList TaskIR::init ( const char *functionName, tgDataType returnType ) {
    if ( !initialised )
      initialise();

    voidptrType = type_void->ptr_to();
    voidvoidptrType = voidptrType->ptr_to();

    entry = fileset->add_file ( NULL, NULL/*"taskgraph.c"*/ );

//    func_type *funcType = new func_type ( type_void, voidvoidptrType );
    func_type *funcType = new func_type ( returnType, voidvoidptrType );
    entry->symtab()->install_type ( funcType );

    symTab = new proc_symtab ( const_cast<char *>(functionName) );
    node_list = new tree_node_list();
    tree_proc *mainBlock = new tree_proc ( node_list, symTab );

    procSym = new proc_sym ( funcType, src_c, const_cast<char *>(functionName) ); // SUIF const fix
    procSym->set_block ( mainBlock );
    procSym->set_fse ( entry );
    entry->symtab()->add_child(symTab);

    paramSym = mainBlock->proc_syms()->new_var ( voidvoidptrType, const_cast<char *>("params") );
    paramSym->set_param ( );
    mainBlock->proc_syms()->params()->append( paramSym );

    StatementListHolder *listHolder = new StatementListHolder ( );
    listHolder->list = node_list;
    //	listHolder->scope = mainBlock->proc_syms();
    listHolder->ownScope = true;
    listHolder->baseScope = true;
    return boost::shared_ptr<StatementListHolder>(listHolder);
  }

  /** Create an integer type
   */
  TaskIR::tgDataType TaskIR::createIntegerType ( unsigned size, bool isSigned ) {
    if ( size == 0 ) {
      return type_void;
    }
    if ( isSigned ) {
      if ( size == 1 ) {
	return type_s8;
      } else if ( size == 2 ) {
	return type_s16;
      } else if ( size == 4 ) {
	return type_s32;
      } else if ( size == 8 ) {
	return type_s64;
      }
    } else {
      if ( size == 1 ) {
	return type_u8;
      } else if ( size == 2 ) {
	return type_u16;
      } else if ( size == 4 ) {
	return type_u32;
      } else if ( size == 8 ) {
	return type_u64;
      }
    }
    throw TaskException ( "Type not implemented\n" );
  }

  /** Create an float type
   */
  TaskIR::tgDataType TaskIR::createFloatType ( unsigned size ) {
    if ( size == 4 ) {
      return type_f32;
    } else if ( size == 8 ) {
      return type_f64;
    } else if ( size == 16 ) {
      return type_f128;
    }
    throw TaskException ( "Type not implemented\n" );
  }

  void TaskIR::installStructType( tgDataType type ) {
	  fileset->globals()->install_type(type);
  }

    TaskIR::tgDataType TaskIR::createStructType ( const char *name, int size, int fields ) {
        struct_type *type = new struct_type(TYPE_STRUCT,size,name,fields);
        return type;
    }
    
    void TaskIR::setStructField ( TaskIR::tgDataType type, unsigned pos,  const char *name, TaskIR::tgDataType fieldType, int offset ) {
         ((struct_type*)type)->set_field_name(pos, name);
         ((struct_type*)type)->set_field_type(pos, fieldType);
         ((struct_type*)type)->set_offset(pos, offset * BITS_PER_BYTE);
    }

  TaskIR::tgDataType TaskIR::createPointerType ( tgDataType baseType ) {
	    return baseType->ptr_to ( );
  }

	TaskIR::tgDataType TaskIR::createArrayType (tgDataType baseType, unsigned size ) {
		return new array_type ( baseType, array_bound(0), array_bound(size) );
	}

  /** Add a variable definition
   */
  TaskIR::tgVariable TaskIR::addVariableDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo ) {
    char buffer[128];

    if ( !list->ownScope ) {
      list->ownScope = true;
      list->name = makeScopeName ( buffer );
    }
    type_node *type = dataInfo;
	/*if ( type->is_struct() ) {
		printf("Install Struct\n");
		type = fileset->globals()->install_type(type);
		printf("Done\n");
	}*/
    if ( parameter ) {
      type = symTab->install_type(new modifier_type(TYPE_RESTRICT, type->ptr_to()));
    }
    type = symTab->install_type ( type );
    if ( !list->baseScope ) {
      std::string sname ( list->name );
      sname += name;
      name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
    }

    // TODO: Arrays need this
#ifdef TASKGRAPH_IMPLICIT_SCOPE
    if ( symTab->lookup_var ( const_cast<char *>(name) ) ) {
      if ( !parameter ) {
	makeScopeName ( buffer );
	std::string sname ( buffer );
	sname += name;
	name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
      } else {
	return symTab->lookup_var ( const_cast<char *>(name) );
      }
    }
#endif
    var_sym *sym = symTab->new_var ( type, const_cast<char *>(name) );

    if ( parameter ) {
      base_type *index_type = static_cast<base_type *>(type_s32);
      type_node *basetype = new array_type ( type_void->ptr_to(), 0 );
      basetype = symTab->install_type ( basetype );

      int number = taskgraph->addParameter ( sym->name() );

      in_array *iarr =  new in_array ( );

      instruction *index = new in_ldc ( index_type, operand ( ), immed ( number ) );
      instruction *base = new in_rrr ( io_cvt, basetype->ptr_to(), operand ( ), paramSym );

      iarr->set_base_op ( base );
      iarr->set_dims ( 1 );
      iarr->set_index ( 0, index );
      iarr->set_result_type ( voidvoidptrType );
      iarr->set_elem_size ( voidptrType->size() );

      instruction *access = new in_rrr ( io_lod, voidptrType, operand(), operand( iarr ), operand() );

      instruction *i = new in_rrr ( io_cvt, type, operand( sym ), operand ( access ), operand() );
      list->list->append ( new tree_instr ( i ) );
    }
    return sym;
  }

  TaskIR::tgVariable TaskIR::addPointerDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo ) {
    char buffer[128];

    if ( !list->ownScope ) {
      list->ownScope = true;
      list->name = makeScopeName ( buffer );
    }

    type_node *type = dataInfo;

    if ( !list->baseScope ) {
      std::string sname ( list->name );
      sname += name;
      name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
    }

    // TODO: Arrays need this
#ifdef TASKGRAPH_IMPLICIT_SCOPE
    if ( symTab->lookup_var ( const_cast<char *>(name) ) ) {
      if ( !parameter ) {
	makeScopeName ( buffer );
	std::string sname ( buffer );
	sname += name;
	name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
      } else {
	return symTab->lookup_var ( const_cast<char *>(name) );
      }
    }
#endif
    var_sym *sym = symTab->new_var ( type, const_cast<char *>(name) );

    if ( parameter ) {
      base_type *index_type = static_cast<base_type *>(type_s32);
      type_node *basetype = new array_type ( type_void->ptr_to(), 0 );
      basetype = symTab->install_type ( basetype );

      int number = taskgraph->addParameter ( sym->name() );

      in_array *iarr =  new in_array ( );

      instruction *index = new in_ldc ( index_type, operand ( ), immed ( number ) );
      instruction *base = new in_rrr ( io_cvt, basetype->ptr_to(), operand ( ), paramSym );

      iarr->set_base_op ( base );
      iarr->set_dims ( 1 );
      iarr->set_index ( 0, index );
      iarr->set_result_type ( voidvoidptrType );
      iarr->set_elem_size ( voidptrType->size() );

      instruction *access = new in_rrr ( io_lod, voidptrType, operand(), operand( iarr ), operand() );

      instruction *i = new in_rrr ( io_cvt, type, operand( sym ), operand ( access ), operand() );
      list->list->append ( new tree_instr ( i ) );
    }
    return sym;
  }

  /** Create array definition
   */
  TaskIR::tgVariable TaskIR::addArrayDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, unsigned dimensions, const unsigned *sizes, bool parameter, tgDataType dataInfo ) {
    char buffer[128];
    if ( !(list->ownScope) ) {
      //		list->name = makeScopeName ( buffer );
      //		list->scope = new block_symtab ( makeScopeName(buffer) );
      list->ownScope = true;
      list->name = makeScopeName ( buffer );
    }
    if ( !list->baseScope ) {
      std::string sname ( list->name );
      sname += name;
      name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
    }
#ifdef TASKGRAPH_IMPLICIT_SCOPE
    if ( symTab->lookup_var ( const_cast<char *>(name) ) ) {
      if ( !parameter ) {
	makeScopeName ( buffer );
	std::string sname ( buffer );
	sname += name;
	name = lexicon->enter ( const_cast<char *>(sname.c_str ()) )->sp;
      } else {
	return symTab->lookup_var ( const_cast<char *>(name) );
      }
    }
#endif

    type_node *type = dataInfo;

    int last = 0;
    if ( parameter ) {
      last = 1;
    }

    for ( int a = dimensions - 1; a >= last; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
    }

    if ( parameter ) {
      type =  symTab->install_type(new modifier_type(TYPE_RESTRICT, type->ptr_to ()));
    }

    var_sym *sym = symTab->new_var ( type, const_cast<char *>(name) );
    if ( parameter ) {
      base_type *index_type = static_cast<base_type *>(type_s32);
      type_node *basetype = new array_type ( type_void->ptr_to(), 0 );
      basetype = symTab->install_type ( basetype );

      int number = taskgraph->addParameter ( sym->name() );

      in_array *iarr =  new in_array ( );

      instruction *index = new in_ldc ( index_type, operand ( ), immed ( number ) );
      instruction *base = new in_rrr ( io_cvt, basetype->ptr_to(), operand ( ), paramSym );

      iarr->set_base_op ( base );
      iarr->set_dims ( 1 );
      iarr->set_index ( 0, index );
      iarr->set_result_type ( voidvoidptrType );
      iarr->set_elem_size ( voidptrType->size() );

      instruction *access = new in_rrr ( io_lod, voidptrType, operand(), operand( iarr ), operand() );

      instruction *i = new in_rrr ( io_cvt, type, operand( sym ), operand ( access ), operand() );
      list->list->append ( new tree_instr ( i ) );
    }
    return sym;
  }

  TaskIR::tgExpression TaskIR::createSignedIntegerConstantExpression ( long number, unsigned size, tgDataType dataType ) {
    return new in_ldc ( dataType, operand(), number );
  }

  TaskIR::tgExpression TaskIR::createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size, tgDataType dataType ) {
    return new in_ldc ( dataType, operand(), number );
  }

  TaskIR::tgExpression TaskIR::createFloatConstantExpression ( double number, unsigned size, tgDataType dataType ) {
    return new in_ldc ( dataType, operand(), number );
  }

  TaskIR::tgExpression TaskIR::createStringConstantExpression ( const char *text, tgDataType dataType ) {
    char buffer[512];

    const char *name = createStringName ( buffer );
    name = lexicon->enter ( const_cast<char*>(name) )->sp;

    type_node *the_type = static_cast<ptr_type *>(dataType)->ref_type ();

    tgDataType ardataType = new array_type ( the_type, 0, strlen( text ) + 1 );
    ardataType = symTab->install_type ( ardataType );
    var_sym *sym = symTab->new_var ( ardataType, const_cast<char *>(name ));
    sym->set_addr_taken ();
    var_def *def = symTab->define_var (sym, get_alignment ( ardataType ) );

    annote *a = new annote ( k_multi_init );
    a->immeds()->append ( immed ( the_type->size ( ) ) );

    base_type *the_base = static_cast<base_type *>( the_type->unqual ( ) );
    for ( unsigned i = 0; i < strlen( text ) + 1; ++i ) {
		char c = text[i];
      a->immeds()->append(immed(the_base->is_signed() ? static_cast<signed char>(c) :
				static_cast<unsigned char>(c)));
    }
    def->annotes()->append(a);
    return new in_ldc ( dataType, operand(), sym );
  }

  TaskIR::tgExpression TaskIR::createBinaryMathExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType ) {
    return new in_rrr ( binaryOperator[operation], dataType, operand(), operand ( lhs ), operand ( rhs ) );
  }

  TaskIR::tgExpression TaskIR::createBinaryComparisonExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType ) {
    // SUIF has no support for greater than so swap the arguments aroun
    // The binaryOperator table is set-up for this
    if ( operation == opGREATERTHANOREQUALTO || operation == opGREATERTHAN) {
      std::swap ( lhs, rhs );
    }

    return new in_rrr ( binaryOperator[operation], dataType, operand(), lhs, rhs );
  }

  TaskIR::tgExpression TaskIR::createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType ) {
    // TODO: Implement C short-circuting
    return new in_rrr ( binaryOperator[operation], dataType, operand(),  lhs, rhs );
  }

  TaskIR::tgExpression TaskIR::createUnaryExpression ( UnaryOperatorType operation, tgExpression expr, tgDataType dataType ) {
    return new in_rrr ( unaryOperator[operation], dataType, operand(), expr, operand ( ) );
  }

  TaskIR::tgExpression TaskIR::createCastExpression ( tgDataType dataType, tgExpression expr ) {
    instruction *i = new in_rrr ( io_cvt, dataType, operand(), expr, operand ( ) );
    return i;
  }

  TaskIR::tgExpression TaskIR::createLoadVariableExpression ( tgVariable variable, tgDataType dataType ) {
    instruction *i = new in_rrr ( io_cpy, dataType, operand(), variable, operand() );
    return i;
  }

    TaskIR::tgExpression TaskIR::createLoadWithBaseExpression ( tgExpression base, tgDataType dataType ) {
	    return new in_rrr ( io_lod, dataType, operand(), base, operand() );
	}

  TaskIR::tgExpression TaskIR::createLoadAndDereferenceVariableExpression ( tgVariable variable, tgDataType dataType ) {
    instruction *i = new in_rrr ( io_lod, dataType, operand(), variable, operand() );
    return i;
  }

  TaskIR::tgExpression TaskIR::createLoadStructureExpression ( tgVariable variable, tgDataType structType, unsigned offset, tgDataType fieldType, bool isParameter ) {
	  if ( !isParameter ) {
		  instruction *i = new in_ldc ( fieldType->ptr_to(), operand(), immed(variable, offset*BITS_PER_BYTE) );
		  i->append_annote(k_fields, new immed_list );
		  int left;
		  int pos = ((struct_type*)structType)->find_field_by_offset(offset * BITS_PER_BYTE, left);
		  const char *name = ((struct_type*)structType)->field_name(pos);
		  i->annotes()->peek_annote(k_fields)->immeds()->push(immed(name));
		  return i;//new in_rrr ( io_lod, fieldType, operand(),  i, operand() );
	  } else {
		  instruction *val = new in_ldc ( type_s32, operand(), immed(offset) );
		  instruction *i = new in_rrr ( io_add, fieldType->ptr_to(), operand(), variable , val );
		  i->append_annote(k_fields, new immed_list );
		  int left;
		  int pos = ((struct_type*)structType)->find_field_by_offset(offset * BITS_PER_BYTE, left);
		  const char *name = ((struct_type*)structType)->field_name(pos);
		  i->annotes()->peek_annote(k_fields)->immeds()->push(immed(name));
		  return i;//new in_rrr ( io_lod, fieldType, operand(),  i, operand() );
	  }
  }

  //FIXME: duplicates?
  TaskIR::tgExpression TaskIR::createLoadStructureWithBaseExpression ( tgExpression expr, tgDataType structType, unsigned offset, tgDataType fieldType, bool isParameter ) {
	  if (expr->format() == inf_ldc ) {
		  in_ldc* const il = static_cast<in_ldc*>(expr);
		  immed op = il->value();
		  il->set_value(immed(op.symbol(), op.offset() + offset * BITS_PER_BYTE ));
		  il->set_result_type ( fieldType->ptr_to() );
		  if ( il->annotes()->peek_annote(k_fields) == 0 )
			  il->append_annote(k_fields, new immed_list );
		  int left;
		  int pos = ((struct_type*)structType)->find_field_by_offset(offset * BITS_PER_BYTE, left);
		  const char *name = ((struct_type*)structType)->field_name(pos);
		  il->annotes()->peek_annote(k_fields)->immeds()->append(immed(name));
		  return il;
	  }
	  if (expr->format() == inf_array ) {
		  in_array* const ia = static_cast<in_array*>(expr);
		  ia->set_offset(ia->offset() + offset*BITS_PER_BYTE);
		  ia->set_result_type ( fieldType->ptr_to() );
		  if ( ia->annotes()->peek_annote(k_fields) == 0 )
			  ia->append_annote(k_fields, new immed_list );
		  int left;
		  int pos = ((struct_type*)structType)->find_field_by_offset(offset * BITS_PER_BYTE, left);
		  const char *name = ((struct_type*)structType)->field_name(pos);
		  ia->annotes()->peek_annote(k_fields)->immeds()->append(immed(name));
		  return ia;//new in_rrr ( io_lod, fieldType, operand(),  ia, operand() );
	  } else {
		  instruction *val = new in_ldc ( type_s32, operand(), immed(offset) );
		  instruction *i = new in_rrr ( io_add, fieldType->ptr_to(), operand(), expr , val );
		  i->append_annote(k_fields, new immed_list );
		  int left;
		  int pos = ((struct_type*)structType)->find_field_by_offset(offset * BITS_PER_BYTE, left);
		  const char *name = ((struct_type*)structType)->field_name(pos);
		  i->annotes()->peek_annote(k_fields)->immeds()->append(immed(name));
		  return i;
	  }
  }

  TaskIR::tgStatement TaskIR::createStoreStructureStatement ( tgVariable variable, tgDataType structType, unsigned offset, tgDataType fieldType, tgExpression expr, bool isParameter ) {
	  if ( !isParameter ) {
		  instruction *i = new in_ldc ( fieldType->ptr_to(), operand(), immed(variable, offset*BITS_PER_BYTE) );
		  return new tree_instr (  new in_rrr ( io_str, fieldType, operand(), i ,  expr ) );
	  } else {
		  instruction *val = new in_ldc ( type_s32, operand(), immed(offset) );
		  instruction *i = new in_rrr ( io_add, fieldType->ptr_to(), operand(), variable , val );
		  return new tree_instr (  new in_rrr ( io_str, fieldType, operand(), i,  expr ) );
	  }
  }

  TaskIR::tgStatement TaskIR::createStoreStructureWithBaseStatement ( tgExpression base, tgDataType structType, unsigned offset, tgDataType fieldType, tgExpression expr, bool isParameter ) {
	  if (base->format() == inf_array ) {
		  in_array *ia = (in_array*)base;
		  ia->set_offset(offset*BITS_PER_BYTE);
		  ia->set_result_type ( fieldType->ptr_to() );
		  return new tree_instr (  new in_rrr ( io_str, fieldType, operand(), ia,  expr ) );
	  } else {
//		  instruction *val = new in_ldc ( type_s32, operand(), immed(offset) );
//		  instruction *i = new in_rrr ( io_add, fieldType->ptr_to(), operand(), expr, val );
		  printf("Not supported\n");
		  return 0;
	  }
  }

  TaskIR::tgStatement TaskIR::addStoreVariableStatement ( tgVariable variable, tgExpression expression ) {
    expression->set_dst ( variable );

    return new tree_instr ( expression );
  }

  TaskIR::tgStatement TaskIR::addStoreWithBaseStatement ( tgExpression base, tgExpression expression, tgDataType dataType ) {
    return new tree_instr ( new in_rrr ( io_str, dataType, operand(), base, expression ) );
  }

  TaskIR::tgStatement TaskIR::addDereferenceAndStoreVariableExpression( tgVariable variable, tgExpression expression, tgDataType dataType ) {
    return new tree_instr ( new in_rrr ( io_str, dataType, operand(), variable, expression ) );
  }

  TaskIR::tgStatement TaskIR::createJumpStatement ( tgCodeLabel label ) {
    return new tree_instr ( new in_bj ( io_jmp, label ) );
  }

	TaskIR::tgStatement TaskIR::createReturnStatement ( tgExpression expr, tgDataType dataType ) {
		return new tree_instr ( new in_rrr (io_ret, dataType, operand(), expr, operand() ) );
	}

  TaskIR::tgExpression TaskIR::createSubArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, unsigned indiceSize, tgExpression *indices, tgDataType dataType, bool parameter ) {
    in_array *i =  new in_array ( );
    base_type *bound_type = static_cast<base_type *>(type_s32);

    type_node *type = dataType;
		type_node *sub_array_type = dataType;
		type_node *sub_sub_array_type = dataType;
    for ( int a = dimensions - 1; a >= 0; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
			if ( a > static_cast<int>(dimensions - indiceSize-1) ) {
				sub_sub_array_type = sub_array_type;
				sub_array_type = new array_type ( sub_array_type, 0, sizes[ a ] - 1 );
				sub_array_type = symTab->install_type ( sub_array_type );
			}

    }
    instruction *base;
    if ( !parameter ) {
      base = new in_ldc ( type->ptr_to(), operand ( ), immed ( variable ) );
    } else {
      base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), variable );
    }

    i->set_base_op ( base );
    i->set_dims ( indiceSize );
    for ( int a = indiceSize - 1; a >= 0; --a ) {
      instruction *bound = new in_ldc ( bound_type, operand( ), immed( sizes[a] ) );
      i->set_bound ( a, bound );
      i->set_index ( indiceSize - a - 1, indices[a] );
    }
    i->set_result_type ( sub_array_type->ptr_to() );

    i->set_elem_size ( static_cast<base_type *>(dataType)->size() );
    return new in_rrr ( io_lod, sub_sub_array_type->ptr_to(), operand(),  i, operand() );
  }

  TaskIR::tgExpression TaskIR::createArrayAccessWithBaseExpression ( tgStatementList scope, tgExpression base, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType ) {
    in_array *i =  new in_array ( );
    base_type *bound_type = static_cast<base_type *>(type_s32);

    type_node *type = dataType;
    for ( int a = dimensions - 1; a >= 0; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
    }
/*    instruction *base;
    if ( !parameter ) {
      base = new in_ldc ( type->ptr_to(), operand ( ), immed ( variable ) );
    } else {
      base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), variable );
    }
*/
    i->set_base_op ( base );
    i->set_dims ( dimensions );
    for ( int a = dimensions - 1; a >= 0; --a ) {
      instruction *bound = new in_ldc ( bound_type, operand( ), immed( sizes[a] ) );
      i->set_bound ( a, bound );
      i->set_index ( dimensions - a - 1, indices[a] );
    }
    i->set_result_type ( dataType->ptr_to() );
    i->set_elem_size ( static_cast<base_type *>(dataType)->size() );
    return i;//new in_rrr ( io_lod, dataType, operand(),  i, operand() );
  }

  TaskIR::tgExpression TaskIR::createArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter ) {
    in_array *i =  new in_array ( );
    base_type *bound_type = static_cast<base_type *>(type_s32);

    type_node *type = dataType;
    for ( int a = dimensions - 1; a >= 0; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
    }
    instruction *base;
    if ( !parameter ) {
      base = new in_ldc ( type->ptr_to(), operand ( ), immed ( variable ) );
    } else {
      base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), variable );
    }

    i->set_base_op ( base );
    i->set_dims ( dimensions );
    for ( int a = dimensions - 1; a >= 0; --a ) {
      instruction *bound = new in_ldc ( bound_type, operand( ), immed( sizes[a] ) );
      i->set_bound ( a, bound );
      i->set_index ( dimensions - a - 1, indices[a] );
    }
    i->set_result_type ( dataType->ptr_to() );
    i->set_elem_size ( static_cast<base_type *>(dataType)->size() );
    return i;//new in_rrr ( io_lod, dataType, operand(),  i, operand() );
  }

  TaskIR::tgExpression TaskIR::createPointerAccessExpression ( tgStatementList scope, tgVariable variable, tgExpression index, tgDataType dataType, bool parameter ) {
    in_array *i =  new in_array ( );

    array_type *type = new array_type ( static_cast<ptr_type*>(dataType)->ref_type(), 0 );

    instruction *base;
//    if ( !parameter ) {
//      base = new in_ldc ( dataType, operand ( ), immed ( variable ) );
//    } else {
      base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), variable );
//    }

    i->set_base_op ( base );
    i->set_dims ( 1 );
    // TODO:Check
    //instruction *bound = new in_ldc ( bound_type, operand( ), operand( ) );
    //i->set_bound ( a, bound );
    i->set_index ( 0, index );
    i->set_result_type ( dataType );
    i->set_elem_size ( dataType->size() );

    return i;//new in_rrr ( io_lod, dataType, operand(),  i, operand() );
  }

	TaskIR::tgExpression TaskIR::createPointerAccessWithBaseExpression ( tgStatementList scope, tgExpression base, tgExpression index, tgDataType dataType ) {
		in_array *i =  new in_array ( );

		array_type *type = new array_type ( static_cast<ptr_type*>(dataType)->ref_type(), 0 );
		base = new in_rrr ( io_lod, dataType, operand(),  base, operand() );
		base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), base );

		i->set_base_op ( base );
		i->set_dims ( 1 );
		i->set_index ( 0, index );
		i->set_result_type ( dataType );
		i->set_elem_size ( dataType->size() );

		return i;//new in_rrr ( io_lod, dataType, operand(),  i, operand() );
	}

  TaskIR::tgStatement TaskIR::addArrayStoreStatement ( tgStatementList scope, tgVariable variable, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter ) {
    in_array *i =  new in_array ( );
    base_type *bound_type = static_cast<base_type *>(type_s32);

    type_node *type = dataType;
    for ( int a = dimensions - 1; a >= 0; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
    }
    instruction *base;
    if ( !parameter ) {
      base = new in_ldc ( type->ptr_to(), operand ( ), immed ( variable ) );
    } else {
      base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), variable );
    }

    i->set_base_op ( base );
    i->set_dims ( dimensions );
    for ( int a = dimensions - 1; a >= 0; --a ) {
      instruction *bound = new in_ldc ( bound_type, operand( ), immed( sizes[a] ) );
      i->set_bound ( a, bound );
      i->set_index ( dimensions - a - 1, indices[a] );
    }
    i->set_result_type ( dataType->ptr_to() );
    i->set_elem_size ( static_cast<base_type *>(dataType)->size() );
    return new tree_instr ( new in_rrr ( io_str, dataType, operand(), operand( i ), operand ( rhs ) ) );
  }

  TaskIR::tgStatement TaskIR::addArrayStoreStatement ( tgStatementList scope, tgExpression base, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType ) {
    in_array *i =  new in_array ( );
    base_type *bound_type = static_cast<base_type *>(type_s32);

    type_node *type = dataType;
    for ( int a = dimensions - 1; a >= 0; --a ) {
      type = new array_type ( type, 0, sizes[ a ] - 1 );
      type = symTab->install_type ( type );
    }
//    instruction *base;
//    if ( !parameter ) {
//      base = new in_ldc ( type->ptr_to(), operand ( ), immed ( variable ) );
//    } else {
      //base = new in_rrr ( io_cvt, type->ptr_to(), operand ( ), base );
//    }

    i->set_base_op ( base );
    i->set_dims ( dimensions );
    for ( int a = dimensions - 1; a >= 0; --a ) {
      instruction *bound = new in_ldc ( bound_type, operand( ), immed( sizes[a] ) );
      i->set_bound ( a, bound );
      i->set_index ( dimensions - a - 1, indices[a] );
    }
    i->set_result_type ( dataType->ptr_to() );
    i->set_elem_size ( static_cast<base_type *>(dataType)->size() );
    return new tree_instr ( new in_rrr ( io_str, dataType, operand(), operand( i ), operand ( rhs ) ) );
  }

  TaskIR::tgExpression TaskIR::deepClone ( tgExpression expression ) {
    return expression->clone();
  }

  void TaskIR::deleteExpression ( tgExpression expr ) {
	delete expr;
  }

  void TaskIR::outputToC( const char *filename ) {
    fileset->globals()->add_sym ( procSym );
    int argc = 3;
    const char* argv[] = { "s2c", "", filename };
    s2c_main( argc, const_cast<char**>(argv), entry, false );
    fileset->globals()->remove_sym ( procSym );
  }

  void TaskIR::print ( ) {
    fileset->globals()->add_sym( procSym );
    int argc = 3;
    const char *argv[] = { "s2c", "", "" };
    s2c_main( argc, const_cast<char**>(argv), entry, true );
    // fileset->add_file( NULL, "test_file" );
    // file_set_entry *fse;
    //     fse = fileset->find_by_num( 0 );
    //     fse->add_outfile( "test_file" );
    // fileset->reset_iter();
    // emit();
    // file_set_entry *fse = new file_set_entry( fileset, NULL, "test_file" );
    //     if( ! procSym->is_written() )
    //       procSym->write_proc( fse );
    //     procSym->flush_proc();
    //     delete fse;
    fileset->globals()->remove_sym( procSym );
  }

  void TaskIR::emit( ) {
    symTab->number_locals();
    fileset->globals()->number_globals();
    printf("    ****************** Globals ******************\n"  );
    fileset->globals()->print ( stdout, 0 );

    fileset->globals()->add_sym ( procSym );
	fileset->globals()->number_globals();
    fileset->reset_iter();
    file_set_entry *fse;
    while ((fse = fileset->next_file())) {
      printf("\n    ****************** FILE ******************"  );
      if (fse->are_annotations()) fse->print_annotes(stdout, 2);
      if (!_suif_no_symtabs) {
	fputs("\n", stdout);
	fse->symtab()->print(stdout, 2);
      } else {
	fputs("\n", stdout);
      }

      proc_sym *psym;
      fse->reset_proc_iter();
      while ((psym = fse->next_proc())) {
	fputs("\n\n", stdout);
	psym->block()->print(stdout, 2);
      }

      fputs("\n\n", stdout);
    }
    fileset->globals()->remove_sym ( procSym );
  }

  void TaskIR::addOptimisation ( const char *name, TaskIR::optFuncSig func ) {
    optimisationMap[name] = func;
  }

  void TaskIR::applyOptimisation ( const char *name ) {
    std::map<std::string, optFuncSig>::const_iterator iter = optimisationMap.find ( name );
    if ( iter == optimisationMap.end ( ) ) {
      throw TaskException ( "Optimisation not found\n" );
    }
    iter->second( procSym->block ( ), 0 );
  }

  void TaskIR::applyOptimisation ( const char *name, void *param ) {
    std::map<std::string, optFuncSig>::const_iterator iter = optimisationMap.find ( name );
    if ( iter == optimisationMap.end ( ) ) {
      throw TaskException ( "Optimisation not found\n" );
    }
    iter->second( procSym->block ( ), param );
  }

  TaskIR::tgFunction TaskIR::createFunction ( const char *name, Type *returnType, unsigned numberArgs, Type **arguments, bool varArgs ) {
    func_type *funcType = new func_type ( returnType->getIRType() );
    funcType->set_num_args ( numberArgs );
    for ( unsigned a = 0; a < numberArgs; ++a ) {
      funcType->set_arg_type ( a, arguments[a]->getIRType() );
    }
    funcType->set_varargs ( varArgs ? TRUE : FALSE );

    proc_sym *procSym = new proc_sym ( funcType, src_c, const_cast<char *>(name) );
    fileset->globals()->add_sym ( procSym );
    return procSym;
  }

  TaskIR::tgStatement TaskIR::callFunctionStatement ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args ) {
    return new tree_instr ( callFunction ( func, numberArgs, args ) );
  }

  TaskIR::tgExpression TaskIR::callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args ) {
    in_cal *instr = new in_cal ( );
//    type_node *type = symTab->install_type ( symTab->install_type ( func->type() )->ptr_to() );
    instruction *base = new in_ldc ( func->type()->ptr_to() , operand ( ), immed ( func ) );
    instr->set_addr_op ( base );
    instr->set_num_args ( numberArgs );
    for ( unsigned a = 0; a < numberArgs; ++a ) {
      instr->set_argument ( a, args[a] );
    }
    return instr;
  }

  TaskIRGroup::TaskIRGroup ( ) {
    if ( !TaskIR::initialised )
      TaskIR::initialise();

    entry = fileset->add_file ( NULL, NULL );
  }

  TaskIRGroup::~TaskIRGroup ( ) {
    // Avoids a SUIF bug with remove_file
    delete fileset->file_list()->remove( fileset->file_list()->lookup( entry ) );
    delete entry; entry = 0;
  }

  void TaskIRGroup::addToGroup ( TaskIR *taskIR ) {
    fileset->globals()->add_sym ( taskIR->procSym );
    taskIR->procSym->set_fse ( entry );
  }

  void TaskIRGroup::removeFromGroup ( TaskIR *taskIR ) {
    fileset->globals()->remove_sym ( taskIR->procSym );
    taskIR->procSym->set_fse ( taskIR->entry );
  }

  void TaskIRGroup::outputToC ( const char *filename ) {
    int argc = 3;
    const char* argv[] = { "s2c", "", const_cast<char *>(filename) };
    s2c_main( argc, const_cast<char**>(argv), entry, false );
  }

  void TaskIRGroup::print ( ) {
    int argc = 3;
    const char* argv[] = { "s2c", "", "" };
    s2c_main( argc, const_cast<char**>(argv), entry, true );
  }

}
