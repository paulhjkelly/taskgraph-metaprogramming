#ifndef TASKIR_H__
#define TASKIR_H__

#include "TaskDefines.h"
#include "TaskTypes.h"
#include <map>
#include <string>
#include <boost/shared_ptr.hpp>

class tree_node_list;
class type_node;
class var_sym;
class tree_node;
class tree_proc;
class block_symtab;
class ptr_type;
class proc_sym;
class file_set_entry;
class proc_symtab;

class instruction;
class label_sym;

namespace tg {
  class Type;

  struct LoopIdentifier {
    unsigned depth;
    unsigned loop[10];
    LoopIdentifier ( ) : depth ( 0 ) {
    }
    LoopIdentifier ( unsigned _loopA ) : depth ( 1 ) {
      loop[0] = _loopA;
    }
    LoopIdentifier ( unsigned _loopA, unsigned _loopB ) : depth ( 2 ) {
      loop[0] = _loopA;
      loop[1] = _loopB;
    }
    LoopIdentifier ( unsigned _loopA, unsigned _loopB, unsigned _loopC ) : depth ( 3 ) {
      loop[0] = _loopA;
      loop[1] = _loopB;
      loop[2] = _loopC;
    }
    unsigned operator[] ( unsigned index ) const {
      return loop[index];
    }
  };

  struct TileSettings {
    TileSettings ( const LoopIdentifier &_firstLoop, unsigned _depth, unsigned _tileSize )
      : tileSize ( _tileSize ), depth ( _depth ), firstLoop ( _firstLoop ), coalesce ( false ) {
    }
    unsigned tileSize;
    unsigned depth;
    LoopIdentifier firstLoop;
    bool coalesce;
  };

  struct InterchangeSettings {
    LoopIdentifier firstLoop, secondLoop;
  };

  struct SkewSettings {
    SkewSettings ( const LoopIdentifier &_firstLoop, const LoopIdentifier &_secondLoop, unsigned _distance )
      : firstLoop ( _firstLoop ), secondLoop ( _secondLoop ), distance ( _distance ) {
    }
    LoopIdentifier firstLoop, secondLoop;
    int distance;
  };


  class coreTaskGraph;
  class TaskLoopIdentifier;

  struct StatementListHolder {
    tree_node_list *list;
    bool ownScope, baseScope;
    std::string name;
    tree_node_list *makeTreeList ( StatementListHolder *sc );
  };


  void tileLoop ( unsigned depth, TaskLoopIdentifier *ids, int *trip, bool coalesce = false );
  void interchangeLoops( const int loopnest_depth,
			 tg::TaskLoopIdentifier loops[],
			 const int loop_order[] );
  void skewLoops ( TaskLoopIdentifier &a, TaskLoopIdentifier &b, int distance );

  class TaskIR;

  class TaskIROutputter {
  public:
    virtual ~TaskIROutputter ( ) { }
    virtual void outputToC ( const char *fileName ) = 0;
  };

  class TaskIRGroup
    : public TaskIROutputter {
    public:
    TaskIRGroup ( );
    ~TaskIRGroup ( );

    void addToGroup ( TaskIR *taskIR );
    void removeFromGroup ( TaskIR *taskIR );

    void outputToC ( const char *fileName );
    void print ( );
    private:
    file_set_entry *entry;
  };

  class TaskIR
    : public TaskIROutputter {
    public:
    typedef boost::shared_ptr<StatementListHolder> tgStatementList;
    typedef tree_node *tgStatement;
    typedef tree_node *tgLoopStatement;
    typedef var_sym *tgVariable;
    typedef instruction *tgExpression;
    typedef type_node *tgDataType;
    typedef label_sym *tgCodeLabel;
    typedef proc_sym *tgFunction;
    typedef void (*optFuncSig) ( tree_proc *, void * );
    private:
    // No copying use deepClone instead
    TaskIR( const TaskIR &object );

    public:
    TaskIR();
    virtual ~TaskIR ( );

    void deepClone ( const TaskIR &object, const char *_procName );
    void outputToC ( const char *fileName );
    void print ( );


    tgStatementList init ( const char *functionName, tgDataType returnType );

    void appendStatement ( tgStatementList list, tgStatement statement );
    tgStatementList createStatementList ( tgStatementList scope );

    tgDataType createIntegerType ( unsigned size, bool isSigned );
    tgDataType createFloatType ( unsigned size );
    tgDataType createPointerType ( tgDataType baseType );
	tgDataType createArrayType ( tgDataType baseType, unsigned size );
    tgDataType createStructType ( const char *name, int size, int fields );
    void setStructField ( tgDataType type, unsigned pos,  const char *name, tgDataType fieldType, int offset );

	void installStructType( tgDataType type );

    tgCodeLabel createCodeLabel ( tgStatementList scope );

    tgVariable addVariableDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo );
    tgVariable addPointerDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo );
    tgVariable addArrayDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, unsigned dimensions, const unsigned *sizes, bool parameter, tgDataType dataInfo );

    tgExpression createSignedIntegerConstantExpression ( long number, unsigned size, tgDataType dataType );
    tgExpression createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size, tgDataType dataType );
    tgExpression createFloatConstantExpression ( double number, unsigned size, tgDataType dataType );
    tgExpression createStringConstantExpression ( const char *text, tgDataType dataType );

    tgExpression createBinaryMathExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
    tgExpression createBinaryComparisonExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
    tgExpression createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );

    tgExpression createUnaryExpression ( UnaryOperatorType operation, tgExpression expr, tgDataType dataType );

    tgExpression createLoadVariableExpression ( tgVariable variable, tgDataType dataType );
    tgExpression createLoadWithBaseExpression ( tgExpression base, tgDataType dataType );
    tgExpression createLoadAndDereferenceVariableExpression ( tgVariable variable, tgDataType dataType );
    tgExpression createSubArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, unsigned indiceSize, tgExpression *indices, tgDataType dataInfo, bool parameter );
    tgExpression createArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataInfo, bool parameter );
    tgExpression createArrayAccessWithBaseExpression ( tgStatementList scope, tgExpression base, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataInfo );

    tgExpression createCastExpression ( tgDataType dataType, tgExpression expr );

    tgStatement addStoreVariableStatement ( tgVariable variable, tgExpression expression );
    tgStatement addDereferenceAndStoreVariableExpression( tgVariable variable, tgExpression expression, tgDataType dataType );
    tgStatement addArrayStoreStatement ( tgStatementList scope, tgVariable variable, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter  );
    tgStatement addArrayStoreStatement ( tgStatementList scope, tgExpression base, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType  );

	tgStatement addStoreWithBaseStatement ( tgExpression base, tgExpression expression, tgDataType dataType );

	tgExpression createPointerAccessExpression ( tgStatementList scope, tgVariable variable, tgExpression index, tgDataType dataType, bool parameter );
	tgExpression createPointerAccessWithBaseExpression ( tgStatementList scope, tgExpression base, tgExpression index, tgDataType dataType );

	tgExpression createLoadStructureExpression ( tgVariable variable, tgDataType structType, unsigned offset, tgDataType fieldType, bool isParameter );
	tgExpression createLoadStructureWithBaseExpression ( tgExpression base, tgDataType structType, unsigned offset, tgDataType fieldType, bool isParameter );
    tgLoopStatement createWhileStatement ( tgStatementList scope, tgExpression test, tgStatementList list );
    tgStatement createIfStatement ( tgStatementList scope, tgExpression ifpart, tgStatementList thenpart, tgStatementList elsepart );
    tgLoopStatement createForStatement ( tgStatementList scope, tgVariable symbol, bool varSigned, tgExpression from, tgExpression to, tgExpression step, tgStatementList list, BinaryOperatorType comparison );
    tgStatement createScopeStatement ( tgStatementList scope, tgStatementList list );
	tgStatement createStoreStructureWithBaseStatement ( tgExpression base, tgDataType structType, unsigned offset, tgDataType fieldType, tgExpression expr, bool isParameter );
	tgStatement createStoreStructureStatement ( tgVariable variable, tgDataType structType, unsigned offset, tgDataType fieldType, tgExpression expr, bool isParameter );

    tgStatement createContinueStatement ( tgLoopStatement loop );
    tgStatement createBreakStatement ( tgLoopStatement loop );

    tgStatement finishWhileStatement ( tgLoopStatement loop );
    tgStatement finishForStatement ( tgLoopStatement loop );

    tgStatement createJumpStatement ( tgCodeLabel label );

	tgStatement createReturnStatement ( tgExpression expr, tgDataType dataType );

    tgExpression deepClone ( tgExpression expression  );

    tgFunction createFunction ( const char *name, Type *returnType, unsigned numberArgs, Type **arguments, bool varArgs );
    tgExpression callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args );
    tgStatement callFunctionStatement ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args );

    static void deleteExpression ( tgExpression expr );
    void emit( );

    static void exit ( );
    static void initialise ( );

    static void addOptimisation ( const char *name, TaskIR::optFuncSig func );
    void applyOptimisation ( const char *name );
    void applyOptimisation ( const char *name, void *param );

    private:
    static bool initialised;
    tree_node_list *node_list;
    var_sym *paramSym;
    ptr_type *voidvoidptrType, *voidptrType;
    const char *procName;
    proc_sym *procSym;
    file_set_entry *entry;
    proc_symtab *symTab;

    static std::map<std::string, optFuncSig> optimisationMap;

    //TODO: Remove friendship
    friend class TaskIRGroup;
  };

}

#endif
