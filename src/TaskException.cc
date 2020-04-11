#include <iostream>
#include <string>
#include "TaskDefines.h"
#include "TaskException.h"

namespace tg {

TaskException::TaskException ( const std::string &str )
	: std::runtime_error ( str ) {
#ifdef TASKGRAPH_VERBOSE_EXCEPTIONS
	std::cout << "Throwing Exception:" << str << std::endl;
#endif
}

}
