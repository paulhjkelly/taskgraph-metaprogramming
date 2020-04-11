#ifndef TASK_CELL_H__
#define TASK_CELL_H__

#include "TaskDefines.h"

// Don't put Boost headers inside the CELL_SUPPORT #ifdef otherwise script
// to scan dependencies will miss them on non-Cell machines.
#include <boost/shared_ptr.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#ifdef CELL_SUPPORT

#include "TaskGraph.h"
#include <string>
#include <libspe2.h>
#include <pthread.h>
#include "spu_args.h"
#include "SpuMemFuncs.h"

namespace tg
{

class ThreadManager 
{
private:
  pthread_t threads[NUM_SPES];
  thread_args t_args[NUM_SPES];
  spe_program_handle_t *spe_objs[NUM_SPES];
  spe_context_ptr_t spes[NUM_SPES];
  std::string images[NUM_SPES];
  int spe_status[NUM_SPES];
  unsigned int argps[NUM_SPES];
  // Returns array index of destroyed thread
  int destroy_idle_thread();
  static void* run_spe(void *params);
public:
  ThreadManager();
  ~ThreadManager();
  void wait_for_all_to_terminate();
  // Run image with spe_args
  spe_context_ptr_t run(spe_arg_t *spe_args, const std::string& image);
};
extern ThreadManager theThreadManager;

class SpuTaskCompiledCode : public TaskCompiledCode 
{
private:
  static void error_function(void**);
  std::string image_path;

public:
  virtual void load(const char *fileName);
  virtual taskGraphFunction getFunctionAddress(const char *name);
  std::string get_image_path() const {
    return image_path;
  }
  virtual ~SpuTaskCompiledCode();  
};

class TaskContainer 
{
public:
  TaskContainer() {}
  virtual ~TaskContainer() {}
  virtual void execute()=0;
  virtual void add(coreTaskGraph*);
  virtual void reset();
protected:
  std::vector<coreTaskGraph *> tgs;
};

// A farm of TaskGraphs
class TaskFarm : public TaskContainer {
public:
  void execute();
private:
  void init();
  void terminate();
  boost::ptr_vector<MarshalledParameters> marshalledParams;
};

// A pipeline containing TaskGraphs
class TaskPipe : public TaskContainer 
{
public:
  TaskPipe() { /*for (int i = 0; i < 7; ++i) params.push_back((void *)NULL);*/ }
  void execute();
  void addResultDest(void* dst) { result_ptrs.push_back(dst); }
  void addParameters(std::vector<void*> params) { this->params = params; }
  void reset() { result_ptrs.clear(); params.clear(); }

private:
  std::vector<void*> result_ptrs;
  std::vector<void*> params;
  boost::shared_ptr<MarshalledParameters> marshalledParams;
  void init();
  void terminate();
};

// A single TaskGraph, that returns a value
class UnitTask : public TaskContainer 
{
public:
  UnitTask(coreTaskGraph* const tg) {
    tgs.push_back(tg);
  }

  void execute() {
    // Run as a singleton farm
    TaskFarm tFarm;
    tFarm.add(tgs.front());
    tFarm.execute();
  }

  template<typename TgReturn>
  TgReturn execute()
  {
    // Allocate an aligned return value so marshalling doesn't complain and irritate user
    TgReturn returnValue __attribute__((aligned(SPU_ALIGN)));
    TaskFarm tFarm;
    tgs.front()->setResultDest(&returnValue);
    tFarm.add(tgs.front());
    tFarm.execute();
    return returnValue;
  }
};

}

#endif
#endif
