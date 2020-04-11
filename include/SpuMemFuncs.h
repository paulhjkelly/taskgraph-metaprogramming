#ifndef SPU_MEM_FUNCS_H__
#define SPU_MEM_FUNCS_H__

#include "TaskDefines.h"
#include <string>
#include <vector>
#include <map>
#include <cstddef>
#include <spu_args.h>

namespace tg
{  

class coreTaskGraph;
class ParameterInfo;

class MarshalledParameters
{
private:
  typedef std::map<std::string, ParameterInfo>::const_iterator T_ParamInfoIter;
  coreTaskGraph& taskGraph;
  const std::size_t parameterCount;
  std::vector<void*> originalBindings;
  spe_arg_t* spe_args;

  unsigned int calcDataSize(const std::map<std::string, ParameterInfo>& parameterInfo);
  void marshallParameters(const std::map<std::string, ParameterInfo>& parameterInfo, const std::vector<void*>& bindings);
  void cleanBindings();

public:
  MarshalledParameters(coreTaskGraph& taskGraph);
  void marshallParameters();
  void marshallCustomParameters(const std::vector<void*>& bindings);
  spe_arg_t* getSpeArg();
  void nullifySpeResultPtr();
  void writeBack();
  ~MarshalledParameters();
};

bool isAlignedPtr(void* const ptr);

void * _malloc_align(const std::size_t size, const unsigned int log2_align);

void _free_align(void* const ptr);

unsigned int round_multiple(const unsigned int value, const unsigned int multiple);

inline void* spu_malloc(const std::size_t size)
{
  return _malloc_align(size, SPU_ALIGN_LOG2);
}

inline void spu_free(void* const ptr)
{
  _free_align(ptr);
}

unsigned int get_padded_size(const unsigned int size);

} 

#endif
