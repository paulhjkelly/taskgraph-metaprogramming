#ifndef TASKCOMPILERS_H__
#define TASKCOMPILERS_H__

#include "TaskDefines.h"
#include <string>

namespace tg {

// The base class for a C compiler
class TaskCompiler {
public:
  virtual ~TaskCompiler() { };
  virtual void compile( const char *sourceName, const char *objName, const char *asmName) = 0;
  virtual bool isSpuCompiler() const
  {
    return false;
  }
};

// Compiler for handling SPU code
class TaskSpuCompiler : public TaskCompiler
{
protected:
  std::string returnType;
  std::string functionName;
public:
  void setTaskGraphFunctionInfo(const std::string& _returnType, const std::string& _functionName)
  {
    returnType = _returnType;
    functionName = _functionName;
  }

  bool isSpuCompiler() const
  {
    return true;
  }
};

// Handles forking and execution of a command (currently used for all compilers)
class ForkExecuteHelper
{
public:
  void execute(const char* argv[]);
};

// ICC - Intel C Compiler
class IntelCCompiler : public TaskCompiler {
public:
  IntelCCompiler( ) : parallel( false ), debug( false ) { }
  
  void setDebug ( bool _debug ) {
    debug = _debug;
  }
  void setParellel ( bool _parallel ) {
    parallel = _parallel;
  }

  void compile( const char *sourceName, const char *objName, const char* asmName);

private:
  bool parallel;
  bool debug;
};

// GCC - Gnu C Compiler
class GnuCCompiler : public TaskCompiler {
public:
  GnuCCompiler( ) : debug(false) {}
  void setDebug ( bool _debug ) {
    debug = _debug;
  }

  void compile( const char *sourceName, const char *objName, const char* asmName);

private:
  bool debug;
};

// PPU-XLC - IBM XLC C Compiler for the PPU within the Cell Processor
class PpuXlcCCompiler : public TaskCompiler {
public:
  PpuXlcCCompiler( ) : debug(false) {}
  void setDebug ( bool _debug ) {
    debug = _debug;
  }

  void compile( const char *sourceName, const char *objName, const char* asmName);

private:
  bool debug;
};

// SPU-GCC - Gnu C Compiler for the SPUs within the Cell Processor
class SpuGnuCCompiler : public TaskSpuCompiler {
public:
  SpuGnuCCompiler() : debug(false) {}

  void setDebug (bool _debug) {
  	debug = _debug;
  }

  void compile( const char *sourceName, const char *objName, const char* asmName);

private:
  bool debug;
};

// SPU-XLC - IBM XLC C Compiler for the SPUs within the Cell Processor
class SpuXlcCCompiler : public TaskSpuCompiler {
public:
  SpuXlcCCompiler() : debug(false) {}

  void setDebug (bool _debug) {
  	debug = _debug;
  }

  void compile( const char *sourceName, const char *objName, const char* asmName);

private:
  bool debug;
};

}

#endif
