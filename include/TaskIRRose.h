#ifndef TASKIR_H__
#define TASKIR_H__

#include "TaskDefines.h"
#include "TaskTypes.h"
#include <iostream>
#include <map>
#include <list>
#include <TaskTypes.h>

class SgStatement;
class SgBasicBlock;
class SgVariableSymbol;
class SgExpression;
class SgType;
class SgLabelStatement;
class SgFunctionSymbol;
class SgFile;
class Sg_File_Info;
class SgFunctionDefinition;
class SgFunctionDeclaration;
class SgVariableSymbol;
class SgFunctionType;
class SgScopeStatement;
class SgName;
class SgExprStatement;
class LoopTransformOptions;

// This is silly really ... it is just because TaskGraph.cc wants to delete a tgStatementList (a SgBasicBlock) so we have to wrap it

class SgBasicBlockHolder {

  public:
    SgBasicBlockHolder ( SgBasicBlock* _block ) : block ( _block ) { }
    // TODO: put code here \|/ so that the SgBasicBlock is REALLY deleted (by making a call to a method in TaskIRRose.cc?)
    ~SgBasicBlockHolder ( ) { }

    SgBasicBlock* block;

};

namespace tg {

  class coreTaskGraph;
  class Type;

  class RoseFunctionHolder;

  class TaskIROutputter {

    public:
      virtual ~TaskIROutputter ( ) { }
      virtual void outputToC ( const char* fileName ) = 0;
    
  };

  class TaskIR : public TaskIROutputter {

    public:
    typedef SgBasicBlockHolder *tgStatementList;
    typedef SgStatement *tgStatement;
    typedef SgScopeStatement *tgLoopStatement;
    typedef SgVariableSymbol *tgVariable;
    typedef SgExpression *tgExpression;
    typedef SgType *tgDataType;
    typedef SgLabelStatement *tgCodeLabel; /* Statement or Symbol ??? */
    typedef RoseFunctionHolder *tgFunction;

    TaskIR ( );
    virtual ~TaskIR ( );
    tgStatementList init ( const char* functionName );
    tgExpression deepClone ( tgExpression expr );
    virtual void outputToC ( const char* filename );
    virtual void print ( );
    void appendStatement ( tgStatementList list, tgStatement statement );
    tgStatementList createStatementList ( tgStatementList scope );

    tgCodeLabel createCodeLabel ( tgStatementList scope );
    tgLoopStatement createWhileStatement ( tgStatementList scope, tgExpression test, tgStatementList list );

    tgVariable addVariableDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo );
    tgVariable addArrayDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, unsigned dimensions, const unsigned *sizes, bool parameter, tgDataType dataInfo );

    tgDataType createIntegerType ( unsigned size, bool isSigned );
    tgDataType createFloatType ( unsigned size );
    tgDataType createPointerType ( tgDataType baseType );
    tgDataType createArrayType ( tgDataType baseType, unsigned size );

    tgExpression createSignedIntegerConstantExpression ( long number, unsigned size, tgDataType dataType );
    tgExpression createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size, tgDataType dataType );
    tgExpression createFloatConstantExpression ( double number, unsigned size, tgDataType dataType );

    tgExpression createBinaryMathExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
          tgExpression createBinaryComparisonExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
    tgExpression createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
    tgExpression createUnaryExpression ( UnaryOperatorType operation, tgExpression expr, tgDataType dataType );

    /* TODO: put these in some kind of sensible order */
    tgExpression createCastExpression ( tgDataType dataType, tgExpression expr );
    tgExpression createLoadVariableExpression ( tgVariable variable, tgDataType dataInfo );
    tgExpression createLoadAndDereferenceVariableExpression ( tgVariable variable, tgDataType dataInfo );
    tgStatement addStoreVariableStatement ( tgVariable variable, tgExpression expression );
    tgStatement addDereferenceAndStoreVariableExpression ( tgVariable variable, tgExpression expression, tgDataType dataType );
    tgExpression createArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter );
    tgStatement addArrayStoreStatement ( tgStatementList scope, tgVariable variable, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter );
    tgLoopStatement createForStatement ( tgStatementList scope, tgVariable symbol, bool varSigned, tgExpression from, tgExpression to, tgExpression step, tgStatementList list, BinaryOperatorType comparison );

    tgStatement createBreakStatement ( tgLoopStatement loop );
    tgStatement createContinueStatement ( tgLoopStatement loop );

    tgStatement finishForStatement ( tgLoopStatement loop );
    tgStatement finishWhileStatement ( tgLoopStatement loop );

    tgStatement createIfStatement ( tgStatementList scope, tgExpression ifpart, tgStatementList thenpart, tgStatementList elsepart );
    tgStatement createJumpStatement ( tgCodeLabel label );
    tgStatement createReturnStatement ( tgExpression expr, tgDataType dataType );
    tgStatement createScopeStatement ( tgStatementList scope, tgStatementList list );
    tgExpression createStringConstantExpression ( const char *text, tgDataType dataType );

    void deepClone ( const TaskIR &object, const char *_procName );

    tgFunction createFunction ( const char *name, const Type *returnType, unsigned numberArgs, const Type **arguments, bool varArgs );
    tgExpression callFunction ( tgFunction func, unsigned numberArgs, tgExpression *args );
    tgStatement callFunctionStatement ( tgFunction func, unsigned numberArgs, tgExpression *args );

    /* Optimisation passes */

    void blockInner ( int blockSize );
    void blockOuter ( int blockSize );
    void blockAll ( int blockSize );

    void loopInterchange ( );

    void allFission ( );
    void innerFission ( );

    void singleFusion ( );
    void multiFusion ( );

    void unrollLoops ( int size, bool leftover, bool newvar );
    
    void outputPDF ( );

    private:
    SgFile *file;
    static Sg_File_Info *fileinfo;
    static SgFunctionType *funcType;
    // SgFunctionDefinition *func_def;
    SgBasicBlock *func_body;
    // SgFunctionDeclaration *func_decl;
    SgVariableSymbol *params_symbol;

    struct ltstr
    {
      bool operator() ( const char* s1, const char* s2 ) const {
        return strcmp ( s1, s2 ) < 0;
      }
    };

    std::map<const char *, SgFunctionSymbol *, ltstr> userFunctions;

    void unparseTo ( std::ostream& out );
    void escapeString ( char *to, const char *from );
    SgFunctionSymbol *declareFunction ( RoseFunctionHolder *f );
    void makeScopeName ( char *buf, const char *varname );

    void fixupExprStatement ( SgExprStatement *es );
    void doOptimisation ( LoopTransformOptions &opt );

    void defineMin ( SgFunctionDeclaration *min_decl, SgVariableSymbol *a, SgVariableSymbol *b );
    void addPhantomDefinitions ( );
    
    friend class TaskIRGroup;
  };

  class TaskIRGroup : public TaskIROutputter {
  
    public:
    TaskIRGroup ( ) { }
    ~TaskIRGroup ( ) { }

    void addToGroup ( TaskIR *taskIR );
    void removeFromGroup ( TaskIR* taskIR );
    
    void outputToC ( const char *fileName );
    void print ( );

    private:
    std::list<TaskIR *> graphs;

  };

}

#endif /* TASKIR_H__ */
