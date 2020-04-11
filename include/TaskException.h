#ifndef TASKEXCEPTION_H__
#define TASKEXCEPTION_H__

#include "TaskDefines.h"
#include <stdexcept>
#include <string>

namespace tg 
{

class TaskException : public std::runtime_error 
{
public:
  TaskException ( const std::string &str );
};

class TaskCompileException : public TaskException 
{
public:
  TaskCompileException ( const std::string &str ) : TaskException ( str ) 
  {
  }
};

}

#endif
