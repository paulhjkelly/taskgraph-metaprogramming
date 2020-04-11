#include <TaskCompilers.h>
#include <TaskException.h>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <string>

// Defines for various compilers to avoid compilation errors as they aren't going to
// used
#ifndef ICC_PATH
#define ICC_PATH ""
#endif

#ifndef GCC_PATH
#define GCC_PATH ""
#endif

#ifndef PPUXLC_PATH
#define PPUXLC_PATH ""
#endif

#ifndef SPUGCC_PATH
#define SPUGCC_PATH ""
#endif

#ifndef SPUXLC_PATH
#define SPUXLC_PATH ""
#endif

#define SPU_WRAPPER_NAME "spu_wrapper.c"

namespace tg {
  
  void ForkExecuteHelper::execute(const char* argv[])
  {
    const pid_t pid = fork();
    if (pid == 0) 
    {
      // Even glibc does a cast for the varargs versions of exec
      execv(argv[0], const_cast<char**>(&argv[0]));
      std::cerr << "Failed to execute: '" << argv[0] << "'" << std::endl;
      _exit (EXIT_FAILURE);
    } 
    else if (pid < 0) 
    {
      // Fork failed
      throw TaskCompileException ("Forking to execute compiler failed.");
    }
    else 
    {
      int status;

      // Wait for the compiler to finish
      if ( waitpid ( pid, &status, 0 ) != pid ) 
      {
	if (WIFSIGNALED (status)) 
	{
	  std::cerr << "WIFSIGNALED true WTERMSIG = " << WTERMSIG(status) << std::endl;
	  std::cerr << "Compiler unexpectedly interrupted, re-running." << std::endl;
	} 
	else 
	{
	  throw TaskCompileException("Compilation Failed");
	}
      }
    }
  }

  void IntelCCompiler::compile ( const char *sourceName, const char *objName, const char* asmName) {
    ForkExecuteHelper executeHelper;

    if( debug )
    {
      const char* args[] = {ICC_PATH, "-X", "-restrict", "-xc", 
	      "-g", "-mp", "-xHost", "-O3", "-shared", "-w", EXTRA_ICC_COMPILE_ARGS 
	      sourceName, "-o", objName,  NULL};
      executeHelper.execute(args); 
    }
    else 
    {
      if( !parallel ) {
	const char* args[] = {ICC_PATH, "-X", "-restrict", "-xc", 
		"-xHost", "-vec-report3", "-O3", "-ansi-alias", "-shared", "-w",
		EXTRA_ICC_COMPILE_ARGS sourceName, "-o", objName,  NULL};
        executeHelper.execute(args); 
      } 
      else 
      {
	const char* args[] = {ICC_PATH, "-X", "-restrict", "-xc", 
		"-xHost", "-vec-report3", "-O3", "-ansi-alias", "-shared", "-w", "-parallel",
		EXTRA_ICC_COMPILE_ARGS sourceName, "-o", objName,  NULL};
        executeHelper.execute(args); 
      }
    }
  }

  void GnuCCompiler::compile (const char* sourceName, const char* objName, const char* asmName) {
    ForkExecuteHelper executeHelper;
    if ( !debug ) 
    {
      const char* args[] = {GCC_PATH, "-O3", "-std=c99", "-pipe", "-xc", sourceName, "-shared", EXTRA_GCC_COMPILE_ARGS "-o", objName,  NULL};
      executeHelper.execute(args); 
    } 
    else 
    {
      const char* args[] = {GCC_PATH, "-xc", "-std=c99", "-ggdb", "-ffloat-store", sourceName, "-shared", EXTRA_GCC_COMPILE_ARGS "-o", objName,  NULL};
      executeHelper.execute(args); 
    }
  }
	
  void PpuXlcCCompiler::compile (const char* sourceName, const char* objName, const char* asmName) {
    ForkExecuteHelper executeHelper;
    if ( !debug ) 
    {
      const char* args[] = {PPUXLC_PATH, "-O5", "-qstrict", sourceName, "-qmkshrobj", "-o", objName, NULL};
      executeHelper.execute(args);
    }
    else 
    {
      const char* args[] = {PPUXLC_PATH, "-g", sourceName, "-qmkshrobj", "-o", objName, NULL};
      executeHelper.execute(args);
    }
  }

  std::string getWrapperPath()
  {
    struct stat buf;

    // If package has been installed, wrapper file should be here
    const std::string installDataPath = std::string(INSTALL_DATA_PATH) + SPU_WRAPPER_NAME;
    if (stat(installDataPath.c_str(), &buf) == 0)
      return installDataPath;

    // Otherwise, we iterate up through all directories, looking for a filename match 
    std::string searchDir("./");

    while (stat(searchDir.c_str(), &buf) == 0)
    {
      const std::string candidateFile(searchDir + PKG_DATA_DIR_NAME + "/" + SPU_WRAPPER_NAME);
      if (stat(candidateFile.c_str(), &buf) == 0)
        return candidateFile;
      else
        searchDir += "../";
    }

    throw TaskException("Unable to locate SPU wrapper file");
  }

  void SpuGnuCCompiler::compile (const char* sourceName, const char* objName, const char* asmName) {
    ForkExecuteHelper executeHelper;
    const std::string wrapper = getWrapperPath();
    const std::string returnDefine = std::string("-DRETURNTYPE=") + returnType;
    const std::string functionDefine = std::string("-DFUNCTIONNAME=") + functionName;
    const std::string defineVoid = (returnType.compare("void") ? "-URETURNTYPEVOID" : "-DRETURNTYPEVOID");

    if (!debug) 
    {
      const char* args[] = {SPUGCC_PATH, "-O3", "-std=c99", sourceName, wrapper.c_str(), "-lm", "-Xlinker", "--strip-all", "-o", objName, functionDefine.c_str(), defineVoid.c_str(), returnDefine.c_str(), NULL};
      executeHelper.execute(args);
    } 
    else 
    {
      const char* args[] = {SPUGCC_PATH, "-ggdb", "-std=c99", sourceName, wrapper.c_str(), "-lm", "-o", objName, functionDefine.c_str(), defineVoid.c_str(), returnDefine.c_str(), NULL};
      executeHelper.execute(args);
    }		
  }
	
  void SpuXlcCCompiler::compile(const char* sourceName, const char* objName, const char* asmName) {
    ForkExecuteHelper executeHelper;
    const std::string wrapper = getWrapperPath();
    const std::string returnDefine = std::string("-DRETURNTYPE=") + returnType;
    const std::string functionDefine = std::string("-DFUNCTIONNAME=") + functionName;
    const std::string defineVoid = (returnType.compare("void") ? "-URETURNTYPEVOID" : "-DRETURNTYPEVOID");

    /*
     * Some useful xlc compiler options:
     * -qreport : Print out a report of all loop optimisations performed
     * -qcompact : Make the code smaller by not replicating / expanding code inline
     * -qhot=noarraypad:level=1:simd:vector : Enables various higher order transformations
     * -O5 : Enables pretty much everything. If encountering problems, try -O3 -qhot instead.
     *  -s : Strips code of unused symbols (resulting in a far smaller binary)
     */
	  
    if (!debug) 
    {
      const char* args[] = {SPUXLC_PATH, "-O5", /*"-qreport", */"-s", wrapper.c_str(), sourceName, "-o", objName, functionDefine.c_str(), defineVoid.c_str(), returnDefine.c_str(), "-lm", NULL};
      executeHelper.execute(args);
    } 
    else 
    {
      const char* args[] = {SPUXLC_PATH, "-g", wrapper.c_str(), sourceName, "-o", objName, functionDefine.c_str(), defineVoid.c_str(), returnDefine.c_str(), "-lm", NULL};
      executeHelper.execute(args);
    }
  }
}
