#include <TaskCell.h>
#include <TaskGraph.h>
#include <SpuMemFuncs.h>
#include <spu_args.h>
#include <pthread.h>
#include <libspe2.h>
#include <string>
#include <vector>
#include <cstdio>
#include <iostream>

namespace tg
{

ThreadManager theThreadManager;

ThreadManager::ThreadManager() {
  for (int i = 0; i < NUM_SPES; ++i)
    spe_status[i] = SPU_IDLE_NO_TG;
}

ThreadManager::~ThreadManager() {
  while (destroy_idle_thread() != -1);
}

int ThreadManager::destroy_idle_thread() {
  for (int i = 0; i < NUM_SPES; ++i) {
    // Find an idle tg on an SPE
    if (spe_status[i] == SPU_IDLE_LOADED_TG) {
      // Tell the SPU to terminate
      unsigned int term = 0;
      spe_in_mbox_write(spes[i], &term, 1, SPE_MBOX_ANY_NONBLOCKING);
      // Wait for the thread to die, then clean up
      pthread_join(threads[i], NULL);
      spe_image_close(spe_objs[i]);
      // Set the status number
      spe_status[i] = SPU_IDLE_NO_TG;
      // Return the thread number we killed
      return i;
    }
  }
  // If we get here, we failed to destroy anything
  return -1;
}

void ThreadManager::wait_for_all_to_terminate() {
  for (int i = 0; i < NUM_SPES; ++i) {
    if (spe_status[i] == SPU_RUNNING) {
      unsigned int tmp;
      // Poll the mailbox (the SPE writes here on completion)
      while (!spe_out_mbox_status (spes[i]));
      spe_out_mbox_read (spes[i], &tmp, 1);
      spe_status[i] = SPU_IDLE_LOADED_TG;
    }
  }
}

spe_context_ptr_t ThreadManager::run(spe_arg_t *spe_args, const std::string& image) {
  bool found_tg = false;
  int spu = -1;
  
  if (!isAlignedPtr(spe_args))
    std::cerr << "ThreadManager received unaligned pointer. We're done for." << std::endl;
  
  for (int i = 0; i < NUM_SPES; ++i) {
    if (spe_status[i] == SPU_IDLE_LOADED_TG && !image.compare(images[i])) {
      // Found an idle thread running this tg!
      found_tg = true;
      spu = i;
      break;
    } else if (spe_status[i] == SPU_IDLE_NO_TG)
      spu = i;
  }
  if (!found_tg) {
    //std::cout << "SPU miss" << std::endl;
    if (spu == -1)
      spu = destroy_idle_thread();
    
    // Create a new thread with index = spu
    images[spu] = image;
    spe_program_handle_t *tmp = spe_image_open(image.c_str());
    if (tmp == NULL) {
      std::cerr << "Failed to load spe image " << image << std::endl;
      exit(1);
    }
    spe_objs[spu] = tmp;
      
    // Create context
    spes[spu] = spe_context_create(0, NULL);
    // Load object
    spe_program_load(spes[spu], spe_objs[spu]);
      
    // Create thread
    t_args[spu].spe = spes[spu];
    t_args[spu].argp = spe_args;
    t_args[spu].envp = NULL;
    pthread_create(&threads[spu], NULL, &run_spe, &t_args[spu]);
  } else {
    //std::cout << "SPU hit" << std::endl;
    // The TG is already running, give it a new argp
    argps[spu] = reinterpret_cast<unsigned int>(spe_args);
    spe_in_mbox_write(spes[spu], &(argps[spu]), 1, SPE_MBOX_ANY_NONBLOCKING);
  }
  
  spe_status[spu] = SPU_RUNNING;
  return spes[spu];
}

void SpuTaskCompiledCode::error_function(void**)
{
  std::cerr << "Something tried to call the function address returned by a SpuTaskCompiledCode object" << std::endl;
  std::cerr << "This is not supposed to happen. Please report it." << std::endl;
}

void SpuTaskCompiledCode::load(const char *fileName) {
    image_path = std::string(fileName);
}

taskGraphFunction SpuTaskCompiledCode::getFunctionAddress(const char *name ) {
  // This is a dummy method, the return value will (should...) never be dereferenced
  return &error_function;
}

SpuTaskCompiledCode::~SpuTaskCompiledCode()
{
  unlink ( image_path.c_str ( ) );
}

// The function to run the spe object in a thread
void* ThreadManager::run_spe(void *params) {
  struct thread_args *args = static_cast<struct thread_args *>(params);
  unsigned int runflags = 0;
  unsigned int entry = SPE_DEFAULT_ENTRY;
  // Finally run the spe context
  spe_stop_info_t status;
	
  if (spe_context_run(args->spe, &entry, runflags, args->argp, args->envp, &status) < 0) {
    perror("Failed to execute SPE context");
    exit(1);
  }
	  
  //std::cout << "SPE returned with status: " << status.result.spe_exit_code << std::endl;
  if (status.result.spe_exit_code) {
    perror("Aborting... ");
    exit(1);
  }
    
  // Destroy the spe context to free the spe
  spe_context_destroy(args->spe);
  pthread_exit(NULL);
}

void TaskContainer::add(coreTaskGraph *tg) {
  if (tg->isSpuCode)
    tgs.push_back(tg);
  else
    std::cerr << "Cannot add non-SPU taskgraphs to a TaskContainer type" << std::endl;
}

void TaskContainer::reset() {
  tgs.clear();
}

void TaskFarm::init()
{    
  if (tgs.size() > NUM_SPES) {
    std::cerr << "Executing more than " << NUM_SPES << " TaskGraphs in a farm will result in lost performance, aborting." << std::endl;
    exit(1);
  }

  marshalledParams.clear();
  
  for (unsigned threadNo = 0; threadNo < tgs.size(); ++threadNo) {
    marshalledParams.push_back(new MarshalledParameters(*tgs[threadNo]));
    marshalledParams.back().marshallParameters();
  }
}

void TaskFarm::terminate() 
{
  theThreadManager.wait_for_all_to_terminate();

  for (unsigned threadNo = 0; threadNo < tgs.size(); ++threadNo) {
    marshalledParams[threadNo].writeBack();
  }

  marshalledParams.clear();
}

struct TaskGraphComparator : public std::binary_function<bool, coreTaskGraph*, coreTaskGraph*> {
  bool operator()(coreTaskGraph* const a, coreTaskGraph* const b) const {
    return std::string(a->getName()) < std::string(b->getName());
  }
};

void TaskFarm::execute()
{
// A TaskFarm simply launches all the TaskGraphs at once

  std::vector<coreTaskGraph *> tgs_backup(tgs);
  std::vector<coreTaskGraph *> all_tgs(tgs);
 
  if (tgs.size() > NUM_SPES) {
    std::sort(all_tgs.begin(), all_tgs.end(), TaskGraphComparator());
  }

  while(!all_tgs.empty()) {
    if (all_tgs.size() < NUM_SPES) {
      tgs = all_tgs;
      all_tgs.clear();
    } else {
      tgs.clear();
      tgs.insert(tgs.begin(), all_tgs.begin(), all_tgs.begin()+NUM_SPES);
      all_tgs.erase(all_tgs.begin(), all_tgs.begin()+NUM_SPES);
    }    
    
    init();
  
    for (unsigned i = 0; i < tgs.size(); ++i)
      theThreadManager.run(marshalledParams[i].getSpeArg(), static_cast<SpuTaskCompiledCode*>(tgs[i]->code.get())->get_image_path());
  
    terminate();
  }
}

void TaskPipe::init() {
  marshalledParams = boost::shared_ptr<MarshalledParameters>(new MarshalledParameters(*tgs[0]));
  marshalledParams->nullifySpeResultPtr();
}

void TaskPipe::terminate() {
  theThreadManager.wait_for_all_to_terminate();
  marshalledParams.reset();
}


void TaskPipe::execute() {
  spe_context_ptr_t spes[NUM_SPES];
  std::vector<void*> paramHolder(1);
    
  if (result_ptrs.empty()) {
    std::cerr << "The result location has not been set for a TaskPipe. Please set the result location and try again." << std::endl;
    exit(1);
  }
  
  if (tgs.size() > NUM_SPES) {
    std::cerr << "TaskPipes have a maximum size of " << NUM_SPES << " items, and you're trying to create one of size " << tgs.size() << "!" << std::endl;
    exit(1);    
  }
  
  if (tgs.size() < 2) {
    std::cerr << "Singleton pipelines have no well defined semantics. Try adding another stage." << std::endl;
    exit(1);
  }
  
  if (result_ptrs.size() != params.size()) {
    std::cerr << "Number of inputs (" << params.size() << ") does not match number of outputs (" << result_ptrs.size() << ") for a TaskPipe!" << std::endl;
    exit(1);
  }
  
  // Create the single spe_arg_t we need for the pipeline
  init();

  // The first stage will run immediately
  paramHolder[0] = params[0];
  marshalledParams->marshallCustomParameters(paramHolder);
  spes[0] = theThreadManager.run(marshalledParams->getSpeArg(), static_cast<SpuTaskCompiledCode*>(tgs[0]->code.get())->get_image_path());
  
  // The other stages will ask for various things first
  for (unsigned i = 1; i < tgs.size(); ++i) {
    spes[i] = theThreadManager.run(NULL, static_cast<SpuTaskCompiledCode*>(tgs[i]->code.get())->get_image_path()); 
  }
  
  // Initialise the pipeline
  for(unsigned i = 0; i < tgs.size(); ++i) {
    // Base pointer
    unsigned int base_ptr;
    if (i == tgs.size() - 1)
      base_ptr = 0;
    else
      base_ptr = reinterpret_cast<unsigned int>(spe_ls_area_get(spes[i]));
    spe_in_mbox_write(spes[i], &base_ptr, 1, SPE_MBOX_ANY_NONBLOCKING);
    
    // Next spe_arg_t
    if (i < (tgs.size() - 1)) {
      while (!spe_out_mbox_status(spes[i+1]));

      unsigned int next_spe_args_ptr;
      spe_out_mbox_read(spes[i+1], &next_spe_args_ptr, 1);
      next_spe_args_ptr += reinterpret_cast<unsigned int>(spe_ls_area_get(spes[i+1]));
      spe_in_mbox_write(spes[i], &next_spe_args_ptr, 1, SPE_MBOX_ANY_NONBLOCKING);
    }
    
    // Previous result lock (if applicable)
    if (i > 0) {
      while (!spe_out_mbox_status(spes[i-1]));

      unsigned int prev_resultIsValid_ptr;
      spe_out_mbox_read(spes[i-1], &prev_resultIsValid_ptr, 1);
      prev_resultIsValid_ptr += reinterpret_cast<unsigned int>(spe_ls_area_get(spes[i-1]));
      spe_in_mbox_write(spes[i], &prev_resultIsValid_ptr, 1, SPE_MBOX_ANY_NONBLOCKING);
    }
    
    // Wait for the current stage to DMA a modified spe_arg_t to the next stage before proceeding
    if (i < tgs.size() - 1) {
      while (!spe_out_mbox_status(spes[i]));

      unsigned int tmp;
      spe_out_mbox_read(spes[i], &tmp, 1);
    }
  }

  // Ensure the first stage can initially receive input
  while (!spe_out_mbox_status(spes[0]));
  
  unsigned int count = 1, input_n = 1, result_n = 0;
  do {
    // Try to feed the pipeline
    if (input_n < params.size() && spe_out_mbox_status(spes[0])) {
      unsigned int tmp;
      spe_out_mbox_read(spes[0], &tmp, 1);
      paramHolder[0] = params[input_n];
      marshalledParams->marshallCustomParameters(paramHolder);
      unsigned int addr = reinterpret_cast<unsigned int>(marshalledParams->getSpeArg());
      spe_in_mbox_write(spes[0], &addr, 1, SPE_MBOX_ANY_NONBLOCKING);
      ++count;
      ++input_n;
    }
    // Try to drain the pipeline
    if (spe_out_mbox_status(spes[tgs.size() - 1])) {
      unsigned int tmp;
      spe_out_mbox_read(spes[tgs.size() - 1], &tmp, 1);
      spe_in_mbox_write(spes[tgs.size() - 1], reinterpret_cast<unsigned int*>(&result_ptrs[result_n]), 1, SPE_MBOX_ANY_NONBLOCKING);
      --count;
      ++result_n;
    }
  } while (count != 0);
  
  // Send term signals to pipe stages (so that they exit their inner loops)   
  for (unsigned i = 1; i < tgs.size(); ++i)
  {
    unsigned int tmp = 0;
    spe_in_mbox_write(spes[i], &tmp, 1, SPE_MBOX_ANY_NONBLOCKING); 
  }
    
  // Then tell the ThreadManager to clean up
  terminate();
}

}
