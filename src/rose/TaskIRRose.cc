// TaskGraph includes

#include <TaskGraph>

// ROSE Includes

/* We need to change HAVE_CONFIG_H  ( and "config.h" itself ) to something less
 * generic
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "rose.h"
#include <LoopTransformInterface.h>
#include <LoopTransformOptions.h>
#include <BlockingAnal.h>
#include <FusionAnal.h>
#include <InterchangeAnal.h>
#include <TransformAstTree.h>

/* This output filename is actually a dummy to satisfy various internal
 * SAGE routines - we define the REAL output filename later on
 */
const char* output_filename = "TaskGraph-Source.c";

namespace tg {

  Sg_File_Info *TaskIR::fileinfo = 0;
  SgFunctionType *TaskIR::funcType = 0;

  TaskIR::TaskIR ( ) {
  }

  TaskIR::~TaskIR ( ) {
    delete file;
    // delete func_decl->get_parameterList ( );
    // XXX: delete more here
  }


  TaskIR::tgStatementList TaskIR::init ( const char* functionName ) {
    /* Create our file info, if it doesn't already exist */
    if ( !fileinfo ) fileinfo = new Sg_File_Info ( output_filename, 0, 0 );

    /* Create the file object */
    file = new SgFile ( );
    delete file->root ( ).get_startOfConstruct ( );
    file->root ( ).set_startOfConstruct ( fileinfo );
    file->root ( ).set_parent ( file );
    file->set_unparse_output_filename ( const_cast<char*> ( output_filename ) );

    /* Construct type of function */
    SgTypeVoid *voidType = SgTypeVoid::createType ( );
    SgPointerType *voidPtr = SgPointerType::createType ( voidType );
    SgPointerType *voidPtrPtr = SgPointerType::createType ( voidPtr );
    if ( funcType == 0 ) {
      funcType = new SgFunctionType ( voidPtr, FALSE );
      funcType->append_argument ( voidPtrPtr );
    }

    /* Create function */
    SgFunctionDeclaration *func_decl = new SgFunctionDeclaration ( fileinfo, SgName ( functionName ), funcType, NULL );
    SgInitializedName *paramsParam = new SgInitializedName ( SgName ( "params" ), voidPtrPtr );
    func_decl->append_arg ( paramsParam );
    paramsParam->set_parent ( func_decl->get_parameterList ( ) );
    params_symbol = new SgVariableSymbol ( paramsParam );
    func_decl->get_declarationModifier ( ).get_storageModifier ( ).setExtern ( );
    SgBasicBlock *func_body = new SgBasicBlock ( fileinfo );
    func_body->insert_symbol ( SgName ( "params" ), params_symbol );
    SgFunctionDefinition *func_def = new SgFunctionDefinition ( fileinfo, func_decl, func_body );
    func_def->insert_symbol ( SgName ( "params" ), params_symbol );
    func_decl->set_definition ( func_def );

    /* Add function to file */
    file->root ( ).append_declaration ( func_decl );

    this->func_body = func_body;

    return new SgBasicBlockHolder ( func_body );
  }

  void TaskIR::unparseTo ( ostream& out ) {
    Unparser_Opt roseOptions ( 
        FALSE,   /* _auto */
        FALSE,   /* linefile */
        FALSE,   /* useOverloadedOperators */
        FALSE,   /* num */
        TRUE,    /* _this */
        FALSE,   /* caststring */
        FALSE,   /* _debug */
        FALSE,   /* _class */
        FALSE,   /* _forced_transformation_format */
        FALSE ); /* _unparse_includes */
    SgUnparse_Info inheritedAttributeInfo;
    Unparser roseUnparser ( &out, const_cast<char*> ( output_filename ), roseOptions, 0 );
    roseUnparser.unparseFile ( file, inheritedAttributeInfo );
  }

  void TaskIR::outputToC ( const char* filename ) {
    ofstream fileout ( filename );
    unparseTo ( fileout );
  }

  void TaskIR::print ( ) {
    unparseTo ( cout );
  }

  TaskIR::tgExpression TaskIR::deepClone ( TaskIR::tgExpression expr ) {
    return static_cast<SgExpression*> ( expr->copy ( SgTreeCopy ( ) ) );
  }

  void TaskIR::deepClone ( const TaskIR &object, const char *_procName ) {
    file = static_cast<SgFile*> ( object.file->copy ( SgTreeCopy ( ) ) );
    SgFunctionDeclaration *func_decl = static_cast<SgFunctionDeclaration *> ( file->root ( ).get_declarations ( ).front ( ) );
    SgInitializedName *paramsParam = func_decl->get_args ( ).front ( );
    params_symbol = new SgVariableSymbol ( paramsParam );
  }
    
  void TaskIR::appendStatement ( TaskIR::tgStatementList list, TaskIR::tgStatement statement ) {
    list->block->append_statement ( statement );
  }

  TaskIR::tgCodeLabel TaskIR::createCodeLabel ( TaskIR::tgStatementList scope ) {
    char name[128];
    static int num = 0;
    sprintf ( name, "lab%d", ++num );
    return new SgLabelStatement ( fileinfo, name );
  }

  TaskIR::tgLoopStatement TaskIR::createWhileStatement ( tgStatementList scope, tgExpression test, tgStatementList list ) {
    SgExprStatement *testStatement = new SgExprStatement ( fileinfo, test );
    fixupExprStatement ( testStatement );
    SgWhileStmt *wLoop = new SgWhileStmt ( fileinfo, testStatement, list->block );

    return wLoop;
  }

  TaskIR::tgExpression TaskIR::createSignedIntegerConstantExpression ( long number, unsigned size, TaskIR::tgDataType dataType ) {
    switch (size) {
    case 1:
      return new SgCharVal ( fileinfo, (char)number );
    case 2:
      return new SgShortVal ( fileinfo, (short)number );
    case 4:
      return new SgIntVal ( fileinfo, (int)number );
    case 8:
      return new SgLongLongIntVal ( fileinfo, (long long int)number );
    default: ;
    }
    throw TaskException ( "Type not implemented\n" );
  }

  TaskIR::tgExpression TaskIR::createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size, TaskIR::tgDataType dataType ) {
    switch (size) {
    case 1:
      return new SgUnsignedCharVal ( fileinfo, (unsigned char)number );
    case 2:
      return new SgUnsignedShortVal ( fileinfo, (unsigned short)number );
    case 4:
      return new SgUnsignedIntVal ( fileinfo, (unsigned int)number );
    case 8:
      return new SgUnsignedLongLongIntVal ( fileinfo, (unsigned long long int)number );
    default: ;
    }
    throw TaskException ( "Type not implemented\n" );
  }

  TaskIR::tgExpression TaskIR::createFloatConstantExpression ( double number, unsigned size, TaskIR::tgDataType dataType ) {
    switch (size) {
    case 4:
      return new SgFloatVal ( fileinfo, (float)number );
    case 8:
      return new SgDoubleVal ( fileinfo, (double)number );
    case 16:
      return new SgLongDoubleVal ( fileinfo, (long double)number );
    default: ;
    }
    throw TaskException ( "Type not implemented\n" );
  }

  TaskIR::tgStatementList TaskIR::createStatementList ( TaskIR::tgStatementList scope ) {
    return new SgBasicBlockHolder ( new SgBasicBlock ( fileinfo ) );
  }

  TaskIR::tgDataType TaskIR::createIntegerType ( unsigned size, bool isSigned ) {
    if ( size == 0 ) {
      return SgTypeVoid::createType ( );
    }
    if ( isSigned ) {
      switch (size) {
      case 1:
        return SgTypeChar::createType ( );
      case 2:
        return SgTypeShort::createType ( );
      case 4:
        return SgTypeInt::createType ( );
      case 8:
        return SgTypeLongLong::createType ( );
      default: ;
      }
    } else {
      switch (size) {
      case 1:
        return SgTypeUnsignedChar::createType ( );
      case 2:
        return SgTypeUnsignedShort::createType ( );
      case 4:
        return SgTypeUnsignedInt::createType ( );
      case 8:
        return SgTypeUnsignedLongLong::createType ( );
      default: ;
      }
    }
    throw TaskException ( "Type not implemented\n" );
  }

  TaskIR::tgDataType TaskIR::createFloatType ( unsigned size ) {
    switch (size) {
    case 4:
      return SgTypeFloat::createType ( );
    case 8:
      return SgTypeDouble::createType ( );
    case 16:
      return SgTypeLongDouble::createType ( );
    default: ;
    }
    throw TaskException ( "Type not implemented\n" );
  }

  TaskIR::tgDataType TaskIR::createPointerType ( tgDataType baseType ) {
    return SgPointerType::createType ( baseType );
  }

  TaskIR::tgDataType TaskIR::createArrayType ( tgDataType baseType, unsigned size ) {
    return SgArrayType::createType ( baseType, new SgUnsignedIntVal ( fileinfo, size ) );
  }

  void TaskIR::makeScopeName ( char *buf, const char *varname ) {
    static unsigned int scopenum = 0;
    sprintf ( buf, "scope%d_%s", scopenum++, varname );
  }

  TaskIR::tgVariable TaskIR::addVariableDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, bool parameter, tgDataType dataInfo ) {
    SgType *type = dataInfo;
    SgInitializer *init = 0;

#ifdef TASKGRAPH_IMPLICIT_SCOPE
    SgVariableSymbol *lookedUpSymbol = func_body->lookup_var_symbol ( SgName ( name ) );
    if ( lookedUpSymbol ) {
      if ( parameter ) {
        return lookedUpSymbol;
      } else {
        char *sname =  (char *)malloc ( strlen ( name ) + 20 ); // XXX this needs to be cleaned up
        makeScopeName ( sname, name );
        name = sname;
      }
    }
#endif

    if ( parameter ) {
      type = createPointerType ( type );
      unsigned index = taskgraph->addParameter ( name );
      init = new SgAssignInitializer ( fileinfo,
               new SgCastExp ( fileinfo,
                 new SgPntrArrRefExp ( fileinfo,
                   new SgVarRefExp ( fileinfo, params_symbol ),
                   new SgUnsignedIntVal ( fileinfo, index )
                  ), type
                ), type
              );
    }
    SgVariableDeclaration *var_decl = new SgVariableDeclaration ( fileinfo, SgName ( name ), type, init );
    SgInitializedName *var_name = new SgInitializedName ( SgName ( name ), type, init, var_decl );
    var_name->set_parent ( var_decl );

    // Warning: preprehension causes ugly code!
    func_body->prepend_statement ( var_decl );

    SgVariableSymbol *sym = new SgVariableSymbol ( var_name );
    func_body->insert_symbol ( SgName ( name ), sym );
    return sym;
  }

  TaskIR::tgVariable TaskIR::addArrayDefinition ( coreTaskGraph *taskgraph, tgStatementList list, const char *name, unsigned dimensions, const unsigned *sizes, bool parameter, tgDataType dataInfo ) {
    SgType* type = dataInfo;

    int last = parameter ? 1 : 0;
    for ( int d = dimensions - 1; d >= last; d-- ) {
      type = createArrayType ( type, sizes[d] );
    }

    return addVariableDefinition ( taskgraph, list, name, parameter, type );
  }

  TaskIR::tgExpression TaskIR::createBinaryMathExpression ( BinaryOperatorType operation, TaskIR::tgExpression lhs, TaskIR::tgExpression rhs, TaskIR::tgDataType dataType ) {
    switch (operation) {
    case opADD:
      return new SgAddOp ( fileinfo, lhs, rhs, dataType );
    case opSUB:
      return new SgSubtractOp ( fileinfo, lhs, rhs, dataType );
    case opMULT:
      return new SgMultiplyOp ( fileinfo, lhs, rhs, dataType );
    case opDIV:
      return new SgDivideOp ( fileinfo, lhs, rhs, dataType );
    case opBITWISEOR:
      return new SgBitOrOp ( fileinfo, lhs, rhs, dataType );
    case opBITWISEAND:
      return new SgBitAndOp ( fileinfo, lhs, rhs, dataType );
    case opLEFTSHIFT:
      return new SgLshiftOp ( fileinfo, lhs, rhs, dataType );
    case opRIGHTSHIFT:
      return new SgRshiftOp ( fileinfo, lhs, rhs, dataType );
    default: ;
    }
    throw TaskException ( "Operator not supported" );
  }

  TaskIR::tgExpression TaskIR::createBinaryComparisonExpression ( BinaryOperatorType operation, TaskIR::tgExpression lhs, TaskIR::tgExpression rhs, TaskIR::tgDataType dataType ) {
    switch (operation) {
    case opEQUALTO:
      return new SgEqualityOp ( fileinfo, lhs, rhs, dataType );
    case opNOTEQUALTO:
      return new SgNotEqualOp ( fileinfo, lhs, rhs, dataType );
    case opLESSTHAN:
      return new SgLessThanOp ( fileinfo, lhs, rhs, dataType );
    case opGREATERTHAN:
      return new SgGreaterThanOp ( fileinfo, lhs, rhs, dataType );
    case opLESSTHANOREQUALTO:
      return new SgLessOrEqualOp ( fileinfo, lhs, rhs, dataType );
    case opGREATERTHANOREQUALTO:
      return new SgGreaterOrEqualOp ( fileinfo, lhs, rhs, dataType );
    default: ;
    }
    // next line commented out, as a hack because createBinaryComparisonExpression called for logical operators as well  ( FIXME )
    // throw TaskException ( "Operator not supported" );
    return createBinaryLogicalBooleanExpression ( operation, lhs, rhs, dataType );
  }

  TaskIR::tgExpression TaskIR::createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, TaskIR::tgExpression lhs, TaskIR::tgExpression rhs, TaskIR::tgDataType dataType ) {
    switch (operation) {
    case opLOGICALOR:
      return new SgOrOp ( fileinfo, lhs, rhs, dataType );
    case opLOGICALAND:
      return new SgAndOp ( fileinfo, lhs, rhs, dataType );
    default: ;
    }
    throw TaskException ( "Operator not supported" );
  }

  TaskIR::tgExpression TaskIR::createUnaryExpression ( UnaryOperatorType operation, TaskIR::tgExpression expr, TaskIR::tgDataType dataType ) {
    switch (operation) {
    case opNEGATE:
      return new SgMinusOp ( fileinfo, expr, dataType );
    case opNOT:
      return new SgNotOp ( fileinfo, expr, dataType );
    case opINVERT:
      return new SgBitComplementOp ( fileinfo, expr, dataType );
    default: ;
    }
    throw TaskException ( "Operator not supported" );
  }

  TaskIR::tgExpression TaskIR::createCastExpression ( TaskIR::tgDataType dataType, TaskIR::tgExpression expr ) {
    return new SgCastExp ( fileinfo, expr, dataType );
  }

  TaskIR::tgExpression TaskIR::createLoadVariableExpression ( TaskIR::tgVariable variable, TaskIR::tgDataType dataInfo ) {
    return new SgVarRefExp ( fileinfo, variable );
  }

  TaskIR::tgExpression TaskIR::createLoadAndDereferenceVariableExpression ( TaskIR::tgVariable variable, TaskIR::tgDataType dataInfo ) {
    return new SgPointerDerefExp ( fileinfo, new SgVarRefExp ( fileinfo, variable ), dataInfo );
  }

  void TaskIR::fixupExprStatement ( SgExprStatement *es ) {
    SgExpressionRoot *er = es->get_expression_root ( );
    er->set_parent ( es );
    er->set_statement ( es );
  }

  TaskIR::tgStatement TaskIR::addStoreVariableStatement ( TaskIR::tgVariable variable, TaskIR::tgExpression expression ) {
    SgExprStatement *es = new SgExprStatement ( fileinfo,
                            new SgAssignOp ( fileinfo,
                              new SgVarRefExp ( fileinfo, variable ),
                              expression,
                              variable->get_type ( )
                            )
                          );
    fixupExprStatement ( es );
    return es;
  }

  TaskIR::tgStatement TaskIR::addDereferenceAndStoreVariableExpression ( tgVariable variable, tgExpression expression, tgDataType dataType ) {
    SgExprStatement *es = new SgExprStatement ( fileinfo,
                            new SgAssignOp ( fileinfo,
                              new SgPointerDerefExp ( fileinfo,
                                new SgVarRefExp ( fileinfo, variable ),
                                dataType
                              ),
                              expression,
                              dataType
                            )
                          );
    fixupExprStatement ( es );
    return es;
  }

  TaskIR::tgExpression TaskIR::createArrayAccessExpression ( tgStatementList scope, tgVariable variable, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter ) {
    /* We cheat in this method a bit and get the type of each array dereference by looking inside the tgVariable, instead of creating new ones from the sizes array (which may cause (another) memory leak) */
    SgType *type = variable->get_type ( );

    SgExpression *expr = new SgVarRefExp ( fileinfo, variable );
    
    for ( int i = dimensions-1; i >= 0; i-- ) {
      if ( isSgPointerType ( type ) ) {
        SgPointerType* ptype = isSgPointerType ( type );
        type = ptype->get_base_type ( );
      } else if ( isSgArrayType ( type ) ) {
        SgArrayType *atype = isSgArrayType ( type );
        type = atype->get_base_type ( );
      }
      expr = new SgPntrArrRefExp ( fileinfo, expr, indices[i], type );
    }

    return expr;
  }

  TaskIR::tgStatement TaskIR::addArrayStoreStatement ( tgStatementList scope, tgVariable variable, tgExpression rhs, unsigned dimensions, const unsigned *sizes, tgExpression *indices, tgDataType dataType, bool parameter ) {
    SgExpression *lhs = createArrayAccessExpression ( scope, variable, dimensions, sizes, indices, dataType, parameter );
    SgExprStatement *es = new SgExprStatement ( fileinfo, new SgAssignOp ( fileinfo, lhs, rhs, dataType ) );
    fixupExprStatement ( es );
    return es;
  }

  TaskIR::tgLoopStatement TaskIR::createForStatement ( tgStatementList scope, tgVariable symbol, bool varSigned, tgExpression from, tgExpression to, tgExpression step, tgStatementList list, BinaryOperatorType comparison ) {
    SgStatement *initStatement = addStoreVariableStatement ( symbol, from );
    SgTypeBool *typeBool = SgTypeBool::createType ( );
    SgExpression *testExpr = createBinaryComparisonExpression ( comparison, createLoadVariableExpression ( symbol, 0 ), to, typeBool );
    SgExpressionRoot *testExprRoot = new SgExpressionRoot ( fileinfo, testExpr, typeBool );
    SgTypeVoid *typeVoid = SgTypeVoid::createType ( );
    SgExpression *incrementExpr = new SgPlusAssignOp ( fileinfo, createLoadVariableExpression ( symbol, 0 ), step, typeVoid );
    SgExpressionRoot *incrementExprRoot = new SgExpressionRoot ( fileinfo, incrementExpr, typeVoid );
    SgForStatement *forLoop = new SgForStatement ( fileinfo, testExprRoot, incrementExprRoot, list->block );

    testExprRoot->set_parent ( forLoop );
    testExprRoot->set_statement ( forLoop );

    incrementExprRoot->set_parent ( forLoop );
    incrementExprRoot->set_statement ( forLoop );

    forLoop->append_init_stmt ( initStatement );
    return forLoop;
  }

  TaskIR::tgStatement TaskIR::createBreakStatement ( tgLoopStatement loop ) {
    return new SgBreakStmt ( );
  }

  TaskIR::tgStatement TaskIR::createContinueStatement ( tgLoopStatement loop ) {
    return new SgContinueStmt ( );
  }

  TaskIR::tgStatement TaskIR::finishForStatement ( tgLoopStatement loop ) {
    return loop;
  }

  TaskIR::tgStatement TaskIR::finishWhileStatement ( tgLoopStatement loop ) {
    return loop;
  }

  TaskIR::tgStatement TaskIR::createIfStatement ( tgStatementList scope, tgExpression ifpart, tgStatementList thenpart, tgStatementList elsepart ) {
    return new SgIfStmt ( fileinfo, new SgExprStatement ( fileinfo, ifpart ), thenpart->block, elsepart->block );
  }

  TaskIR::tgStatement TaskIR::createJumpStatement ( tgCodeLabel label ) {
    return new SgGotoStatement ( fileinfo, label );
  }

  TaskIR::tgStatement TaskIR::createReturnStatement ( tgExpression expr, tgDataType dataType ) {
    return new SgReturnStmt ( fileinfo, expr );
  }

  TaskIR::tgStatement TaskIR::createScopeStatement ( tgStatementList scope, tgStatementList list ) {
    return list->block;
  }

  void TaskIR::escapeString ( char *to, const char *from ) {
    char *top = to;
    for ( const char *fromp = from; *fromp; fromp++ ) {
      char fromc = *fromp;
      if ( fromc == '\n' ) {
    *top++ = '\\';
    *top++ = 'n';
    continue;
      }
      if ( fromc == '\r' ) {
    *top++ = '\\';
    *top++ = 'r';
    continue;
      }
      if ( fromc == '\t' ) {
    *top++ = '\\';
    *top++ = 't';
    continue;
      }
      if ( ( fromc == '"' ) || ( fromc == '\'' ) ) {
    *top++ = '\\';
      }
      *top++ = fromc;
    }
    *top = 0;
  }

  TaskIR::tgExpression TaskIR::createStringConstantExpression ( const char *text, tgDataType dataType ) {
  int len = strlen ( text );
  char *newtext =  ( char * )malloc ( len*2 + 1 );
  escapeString ( newtext, text );
    SgStringVal *s = new SgStringVal ( fileinfo, newtext  );
    free ( newtext );
    return s;
  }

  // This class is to hold all the various information about a function so we
  // can create a copy of it when desired.
  class RoseFunctionHolder {

  public:
    RoseFunctionHolder ( const char *_name, TaskIR::tgDataType _returnType, unsigned _numberArgs, TaskIR::tgDataType *_args, bool _varArgs ) :
      name ( _name ),
      returnType ( _returnType ),
      numberArgs ( _numberArgs ),
      args ( _args ),
      varArgs ( _varArgs ) { }

    const char *name;
    TaskIR::tgDataType returnType;
    unsigned numberArgs;
    TaskIR::tgDataType *args;
    bool varArgs;
    // map<TaskIR *, SgFunctionSymbol *> funcFor;

  };

  TaskIR::tgFunction TaskIR::createFunction ( const char *name, const Type *returnType, unsigned numberArgs, const Type **arguments, bool varArgs ) {
    SgType **args = new SgType*[numberArgs];
    for ( int i = 0; i < numberArgs; i++ ) {
      args[i] = arguments[i]->getIRType ( );
    }
    return new RoseFunctionHolder ( name, returnType->getIRType ( ), numberArgs, args, varArgs );
  }

  SgFunctionSymbol *TaskIR::declareFunction ( RoseFunctionHolder *f ) {
  if ( userFunctions.count ( f->name ) == 1 ) {
    return userFunctions[f->name];
  }
    /* Create function type */
    SgFunctionType *fType = new SgFunctionType ( f->returnType, f->varArgs );
    for ( int i = 0; i < f->numberArgs; i++ ) {
      fType->append_argument ( f->args[i] );
    }
    if ( f->varArgs ) {
      fType->append_argument ( SgTypeEllipse::createType ( ) );
    }

    /* Create function declaration */
    SgFunctionDeclaration *fDecl = new SgFunctionDeclaration ( fileinfo, SgName ( f->name ), fType );
    fDecl->setForward ( );
    for ( int i = 0; i < f->numberArgs; i++ ) {
      SgInitializedName *newname = new SgInitializedName ( SgName ( ), f->args[i] );
      fDecl->append_arg ( newname );
    }
    if ( f->varArgs ) { /* TODO: make this if statement  ( and the one above ) cleaner */
      SgInitializedName *newname = new SgInitializedName ( SgName ( ), SgTypeEllipse::createType ( ) );
      fDecl->append_arg ( newname );
    }

    /* add it */
    file->root ( ).prepend_declaration ( fDecl );

    SgFunctionSymbol *sym = new SgFunctionSymbol ( fDecl );
    userFunctions[f->name] = sym;
    return sym;
  }

  TaskIR::tgExpression TaskIR::callFunction ( tgFunction func, unsigned numberArgs, tgExpression *args  ) {
  SgFunctionSymbol *funcsym = declareFunction ( func );
    SgFunctionType *funcType = funcsym->get_declaration ( )->get_type ( );
    SgFunctionRefExp *functionRef = new SgFunctionRefExp ( fileinfo, funcsym, funcType );
    SgExprListExp *argList = new SgExprListExp ( fileinfo );
    for ( int i = 0; i < numberArgs; i++ ) {
      argList->append_expression ( args[i] );
    }
    return new SgFunctionCallExp ( fileinfo, functionRef, argList, funcType->get_return_type ( ) );
  }

  TaskIR::tgStatement TaskIR::callFunctionStatement (  TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args  ) {
    return new SgExprStatement ( fileinfo, callFunction ( func, numberArgs, args ) );
  }

  class AssumeNoAlias : public AliasAnalysisInterface
  {
 public:
  virtual bool
     may_alias ( AstInterface& fa, const AstNodePtr& r1, const AstNodePtr& r2 )
   { return false; }
  };

  // construct a definition of the "min" function and add it to the tree in the specified place
  void TaskIR::defineMin ( SgFunctionDeclaration *min_decl, SgVariableSymbol *a, SgVariableSymbol *b ) {
    SgStatement *stmt = new SgReturnStmt ( fileinfo, new SgConditionalExp ( fileinfo, new SgLessThanOp ( fileinfo, new SgVarRefExp ( fileinfo, a ), new SgVarRefExp ( fileinfo, b ) ), new SgVarRefExp ( fileinfo, a ), new SgVarRefExp ( fileinfo, b ), min_decl->get_type ( )->get_return_type ( ) ) );
    SgBasicBlock *min_body = new SgBasicBlock ( fileinfo, stmt );
    SgFunctionDefinition *min_def = new SgFunctionDefinition ( fileinfo, min_decl, min_body );
    min_decl->set_definition ( min_def );
    min_decl->unsetForward ( );
    min_decl->get_declarationModifier ( ).get_storageModifier ( ).setStatic ( );
    min_decl->get_functionModifier ( ).setInline ( );
  }

  void TaskIR::addPhantomDefinitions ( ) {
    for ( SgDeclarationStatementPtrList::iterator i = file->root ( ).get_declarations ( ).begin ( ); i != file->root ( ).get_declarations ( ).end ( ); i++ ) {
      SgFunctionDeclaration *fd = isSgFunctionDeclaration ( *i );
      if ( !fd ) continue;
      if ( fd->get_definition ( ) ) continue; // Don't redefine function that are already defined
      if ( fd->get_name ( ) == SgName ( "min" ) ) {
        SgInitializedNamePtrList::iterator i = fd->get_args ( ).begin ( );
        SgInitializedName *a_name = *i;
        SgInitializedName *new_a_name = new SgInitializedName ( SgName ( "a" ), a_name->get_typeptr ( ), a_name->get_initializer ( ), a_name->get_declaration ( ), a_name->get_itemptr ( ), a_name->get_prev_itemptr ( ) );
        *i = new_a_name;
        delete a_name;
        SgVariableSymbol *a = new SgVariableSymbol ( new_a_name );

        i++;
        SgInitializedName *b_name = *i;
        SgInitializedName *new_b_name = new SgInitializedName ( SgName ( "b" ), b_name->get_typeptr ( ), b_name->get_initializer ( ), b_name->get_declaration ( ), b_name->get_itemptr ( ), b_name->get_prev_itemptr ( ) );
        *i = new_b_name;
        delete b_name;
        SgVariableSymbol *b = new SgVariableSymbol ( new_b_name );
        defineMin ( fd, a, b );
      }
    }
  }

  void TaskIR::doOptimisation ( LoopTransformOptions &opt ) {
    LoopTransformOptions *saved_opt = LoopTransformOptions::GetInstance ( );
    LoopTransformOptions::SetInstance ( &opt );

    AstInterface fa ( func_body );
    AssumeNoAlias aliasInfo;
    LoopTransformTraverse ( fa, func_body, aliasInfo );

    addPhantomDefinitions ( );

    LoopTransformOptions::SetInstance ( saved_opt );
  }

  void TaskIR::loopInterchange ( ) {
    LoopTransformOptions opt;
    opt.SetInterchangeSel ( new ArrangeReuseOrder ( ) );

    doOptimisation ( opt );
  }
    
  void TaskIR::blockInner ( int blockSize ) {
    LoopTransformOptions opt;
    opt.SetBlockSel ( new InnerLoopReuseBlocking ( blockSize ) );

    doOptimisation ( opt );
  }

  void TaskIR::blockOuter ( int blockSize ) {
    LoopTransformOptions opt;
    opt.SetBlockSel ( new OuterLoopReuseBlocking ( 1, blockSize ) );

    doOptimisation ( opt );
  }

  void TaskIR::blockAll ( int blockSize ) {
    LoopTransformOptions opt;
    opt.SetBlockSel ( new AllLoopReuseBlocking ( blockSize ) );

    doOptimisation ( opt );
  }

  void TaskIR::unrollLoops ( int size, bool leftover, bool newvar ) {
    LoopTransformOptions opt;
    
    LoopUnrolling::UnrollOpt unrollopt = LoopUnrolling::DEFAULT;
    if ( leftover ) unrollopt = (LoopUnrolling::UnrollOpt) ( unrollopt | LoopUnrolling::COND_LEFTOVER );
    if ( newvar ) unrollopt = (LoopUnrolling::UnrollOpt) ( unrollopt | LoopUnrolling::USE_NEWVAR );
    
    opt.AddPostTransformation ( new LoopUnrolling ( size, unrollopt ), AstInterface::PostVisit );
    doOptimisation ( opt );
  }

  void TaskIR::allFission ( ) {
    LoopTransformOptions opt;
    opt.SetFusionSel ( new LoopNestFusion ( ) );

    doOptimisation ( opt );
  }

  void TaskIR::innerFission ( ) {
    LoopTransformOptions opt;
    opt.SetFusionSel ( new SameLevelFusion ( new InnermostLoopFision ( ) ) );

    doOptimisation ( opt );
  }

  void TaskIR::singleFusion ( ) {
    LoopTransformOptions opt;
    opt.SetFusionSel ( new SameLevelFusion ( new AnyReuseFusionAnal ( ) ) );

    doOptimisation ( opt );
  }

  void TaskIR::multiFusion ( ) {
    LoopTransformOptions opt;
    opt.SetFusionSel ( new MultiLevelFusion ( new AnyReuseFusionAnal ( ) ) );

    doOptimisation ( opt );
  }

  void TaskIR::outputPDF ( ) {
    AstPDFGeneration pdf;
    pdf.generateWithinFile ( file );
  }

  void TaskIRGroup::addToGroup ( TaskIR *taskIR ) {
    graphs.push_back ( taskIR );
  }

  void TaskIRGroup::removeFromGroup ( TaskIR *taskIR ) {
    graphs.remove ( taskIR );
  }

  void TaskIRGroup::outputToC ( const char *fileName ) {
    ofstream fileout ( fileName );
    for ( list<TaskIR *>::iterator i = graphs.begin ( ); i != graphs.end ( ); i++ ) {
       ( *i )->unparseTo ( fileout );
    }
  }

  void TaskIRGroup::print ( ) {
    for ( list<TaskIR *>::iterator i = graphs.begin ( ); i != graphs.end ( ); i++ ) {
       ( *i )->print ( );
    }
  }

} // namespace

