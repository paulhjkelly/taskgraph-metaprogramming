#include<SpuMemFuncs.h>
#include<TaskGraph.h>
#include"spu_args.h"
#include<iostream>
#include<map>
#include<vector>
#include<string>
#include<cstring>
#include<cstddef>
#include<cassert>

namespace tg
{

unsigned int MarshalledParameters::calcDataSize(const std::map<std::string, ParameterInfo>& parameterInfo)
{
  unsigned int totalSize=0;

  for(T_ParamInfoIter paramInfoIter = parameterInfo.begin(); paramInfoIter != parameterInfo.end(); ++paramInfoIter)
  {
    const unsigned int size = paramInfoIter->second.size;
    const unsigned int padded_size = get_padded_size(size);
    const bool copyBack = paramInfoIter->second.isRef;

    if (copyBack)
      totalSize = round_multiple(totalSize, SPU_ALIGN);

    totalSize += padded_size;
  }
  totalSize = get_padded_size(totalSize);
  return totalSize;
}

void MarshalledParameters::marshallParameters(const std::map<std::string, ParameterInfo>& parameterInfo, const std::vector<void*>& bindings)
{
  assert(bindings.size() == parameterCount);
  unsigned int offset = 0;


  // Destroy any existing bindings then make a record of the current ones
  cleanBindings();
  originalBindings = bindings;

  for(T_ParamInfoIter paramInfoIter = parameterInfo.begin(); paramInfoIter != parameterInfo.end(); ++paramInfoIter)
  {
    const unsigned int index = paramInfoIter->second.index;
    const unsigned int size = paramInfoIter->second.size;
    const unsigned int padded_size = get_padded_size(size);
    const bool copyBack = paramInfoIter->second.isRef;

    if (copyBack)
      offset = round_multiple(offset, SPU_ALIGN);

    // Set up spe_args
    memcpy(spe_args->input + offset, bindings[index], size);
    spe_args->ptr_info[index].copy_back = copyBack;
    spe_args->ptr_info[index].offset = offset;
    spe_args->ptr_info[index].size = size;

    if (copyBack)
    {
      if (isAlignedPtr(bindings[index]))
      {
        spe_args->ptr_info[index].orig_loc = bindings[index];
      }
      else 
      {
        std::cerr << "TaskGraph parameter " << index+1 << " is not aligned to a 16-byte boundary. Forcing align now, but this will incur a runtime penalty." << std::endl;
        spe_args->ptr_info[index].orig_loc = spu_malloc(size);
      }
    } 
    else
    {
      spe_args->ptr_info[index].orig_loc = NULL;
    }

    offset += padded_size;
  }

  spe_args->input_size = get_padded_size(offset);
  spe_args->num_args = parameterCount;

}

MarshalledParameters::MarshalledParameters(coreTaskGraph& tg) : taskGraph(tg), parameterCount(taskGraph.parameterInfo.size()), originalBindings(parameterCount, NULL)
{
  const std::map<std::string, ParameterInfo>& paramInfo(taskGraph.parameterInfo);
  const int data_size = calcDataSize(paramInfo);

  spe_args =  static_cast<spe_arg_t*>(spu_malloc(sizeof(spe_arg_t)));
  spe_args->input = static_cast<char*>(spu_malloc(data_size));
  spe_args->ptr_info = static_cast<pointer_info_t*>(spu_malloc(sizeof(pointer_info_t) * parameterCount));

  // Make sure these are null so we don't try to accidentally deallocate them later
  for(unsigned int index=0; index<parameterCount; ++index)
    spe_args->ptr_info[index].orig_loc = NULL;

  // Allocate space for result if result pointer is not aligned.
  if (taskGraph.getReturnSize() > 0)
  {
    if (isAlignedPtr(taskGraph.result)) 
    {
      spe_args->result = taskGraph.result;
    }
    else
    {
      std::cerr << "The result destination of a member of a TaskFarm is not aligned, this may cause a slight performance degradation." << std::endl;
      spe_args->result = spu_malloc(taskGraph.getReturnSize());
    }
  }
}

void MarshalledParameters::marshallParameters()
{
  marshallParameters(taskGraph.parameterInfo, taskGraph.parameterBinding);
}

void MarshalledParameters::marshallCustomParameters(const std::vector<void*>& bindings)
{
  marshallParameters(taskGraph.parameterInfo, bindings);
}

spe_arg_t* MarshalledParameters::getSpeArg()
{
  return spe_args;
}

void MarshalledParameters::nullifySpeResultPtr()
{
  if (taskGraph.getReturnSize()>0 && !isAlignedPtr(taskGraph.result) && spe_args->result != NULL)
    spu_free(spe_args->result);

  spe_args->result=NULL;
}

void MarshalledParameters::writeBack()
{
  const std::map<std::string, ParameterInfo>& paramInfo(taskGraph.parameterInfo);
  // Write back result if necessary
  if (taskGraph.getReturnSize()>0 && !isAlignedPtr(taskGraph.result) && spe_args->result!=NULL) 
  {
    if (taskGraph.result != NULL)
    {
      memcpy(taskGraph.result, spe_args->result, taskGraph.getReturnSize());
    }
    else
    {
      std::cerr << "Failed to write result back for " << taskGraph.getName() << " perhaps you forgot to call setResultDest?" << std::endl;
    }
  }

  // Write back any unaligned parameters
  for(T_ParamInfoIter paramInfoIter = paramInfo.begin(); paramInfoIter != paramInfo.end(); ++paramInfoIter)
  {
    const unsigned int index = paramInfoIter->second.index;
    if (spe_args->ptr_info[index].copy_back && spe_args->ptr_info[index].orig_loc != originalBindings[index]) 
    {
      // Update pointers before we free the temporary data
      memcpy(originalBindings[index], spe_args->ptr_info[index].orig_loc, paramInfoIter->second.size);
    }
  }
}

void MarshalledParameters::cleanBindings()
{
  // Free any unaligned parameters
  const std::map<std::string, ParameterInfo>& paramInfo(taskGraph.parameterInfo);
  for(T_ParamInfoIter paramInfoIter = paramInfo.begin(); paramInfoIter != paramInfo.end(); ++paramInfoIter)
  {
    const unsigned int index = paramInfoIter->second.index;
    if (spe_args->ptr_info[index].copy_back && spe_args->ptr_info[index].orig_loc != originalBindings[index]) 
      spu_free(spe_args->ptr_info[index].orig_loc);

    originalBindings[index] = spe_args->ptr_info[index].orig_loc = NULL;
  }
}

MarshalledParameters::~MarshalledParameters()
{
  // Free holder for aligned result if necessary
  if (taskGraph.getReturnSize()>0 && !isAlignedPtr(taskGraph.result) && spe_args->result != NULL)
    spu_free(spe_args->result);

  cleanBindings();

  spu_free(spe_args->ptr_info);
  spu_free(spe_args->input);
  spu_free(spe_args);
} 


bool isAlignedPtr(void* const ptr) {
  return (reinterpret_cast<unsigned long>(ptr) % SPU_ALIGN) == 0;
}

void * _malloc_align(const std::size_t size, const unsigned int log2_align) {
  void *ret;
  char *real;
  unsigned long offset;
  unsigned long align;

  align = 1 << log2_align;
  real = static_cast<char*>(malloc(size + sizeof(void *) + (align-1)));
  if (real) {
    offset = (align - reinterpret_cast<unsigned long>(real + sizeof(void *))) & (align-1);
    ret = static_cast<void*>((real + sizeof(void *)) + offset);
    *(static_cast<void **>(ret)-1) = static_cast<void *>(real);
  } else {
    ret = static_cast<void*>(real);
  }
    
  return (ret);
}

void _free_align(void* const ptr) {
  if (ptr != NULL) {
    void* const real = *(static_cast<void **>(ptr)-1);
    free(real);
  }
}

unsigned int round_multiple(const unsigned int value, const unsigned int multiple)
{
  unsigned int return_val;
  if (value % multiple == 0)
    return_val = value;
  else
    return_val = value + multiple - (value % multiple);

  return return_val;
}

unsigned int get_padded_size(const unsigned int size)
{
  if (size > SPU_ALIGN)
    return round_multiple(size, SPU_ALIGN);
  else if (size > 8)
    return SPU_ALIGN;
  else if (size > 4)
    return 8;
  else if (size > 2)
    return 4;
  else if (size > 1)
    return 2;
  else
    return 1;
}

}
