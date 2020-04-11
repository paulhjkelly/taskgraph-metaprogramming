#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <spu_mfcio.h>
#include "spu_args.h"
#include <assert.h>

extern RETURNTYPE FUNCTIONNAME(void **params);
unsigned int our_base_address = 0, prev_resultIsValid_ptr = 0, next_spe_args_ptr = 0;

#ifndef RETURNTYPEVOID
RETURNTYPE result_w __attribute__((aligned(SPU_ALIGN)));
RETURNTYPE result_r __attribute__((aligned(SPU_ALIGN)));
#endif

spe_arg_t spe_args __attribute__((aligned(SPU_ALIGN)));

// A simple lock variable to co-ordinate buffer access
volatile int resultIsValid __attribute__((aligned(SPU_ALIGN))) = 0;

void checkAlloc(void* const ptr, const size_t bytes)
{
  if (ptr == NULL && bytes>0)
  {
    fprintf(stderr, "A memory allocation of %d bytes failed on the SPE. Local store probably exhausted.\n", bytes);
    exit(EXIT_FAILURE);
  }
}

// Ripped straight from malloc_align.h
void* _malloc_align(const size_t size, const unsigned int log2_align)
{
  void * ret;

  const unsigned long align = 1 << log2_align;
  char* const real = (char *)malloc(size + sizeof(void *) + (align-1));
  if (real) {
    const unsigned long offset = (align - (unsigned long)(real + sizeof(void *))) & (align-1);
    ret = (void *)((real + sizeof(void *)) + offset);
    *((void **)(ret)-1) = (void *)(real);
  } else {
    ret = (void *)(real);
  }

  checkAlloc(ret, size); 
  return (ret);
}

// Ripped straight from free_align.h
void _free_align(void* const ptr)
{
  if (ptr != NULL) {
    void* const real = *((void **)(ptr)-1);
    free(real);
  }
}

void spu_mfcdma32_any_size(void* ls, unsigned ea, unsigned size, const unsigned tag_id, const unsigned cmd)
{
  // Both addresses must be aligned on a 16-byte boundary
  assert(ea % 16 == 0);
  assert((unsigned int)ls % 16 == 0);

  unsigned int multiple = 16;

  while(size>0)
  {
    const unsigned int maskedSize = size & ~(multiple-1);
    const unsigned int transferable = (SPU_MAX_DMA_SIZE < maskedSize ? SPU_MAX_DMA_SIZE : maskedSize);
    if (transferable>0)
    {
      spu_mfcdma32(ls, ea, transferable, tag_id, cmd);
      ls += transferable;
      ea += transferable;
      size -= transferable;
    }
    else
    {
      multiple >>= 1;
    }
  }
}

/*****************************************************************************/

int execute() {
  // Allocate space for arguments
  const unsigned totalsize = spe_args.input_size;
  
  void** const params = malloc(sizeof(void *) * spe_args.num_args);
  checkAlloc(params, sizeof(void*) * spe_args.num_args);

  void* const paramData = _malloc_align(totalsize, SPU_ALIGN_LOG2);
  
  // Request the data
  spu_mfcdma32_any_size(paramData, (unsigned int)spe_args.input, totalsize, 0, MFC_GET_CMD);

  // Copy in pointer information
  pointer_info_t* const ptr_info = _malloc_align(sizeof(pointer_info_t) * spe_args.num_args, SPU_ALIGN_LOG2);
  spu_mfcdma32_any_size(ptr_info, (unsigned int)spe_args.ptr_info, sizeof(pointer_info_t) * spe_args.num_args, 0, MFC_GET_CMD);
  
  // Wait for all the previous transfers to complete
  spu_writech(MFC_WrTagMask, 1 << 0);
  (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);

  for (int i = 0; i < spe_args.num_args; ++i) {
    // Set up the double pointer parameter list that the TaskGraph requires
    params[i] = paramData + ptr_info[i].offset;
  }
  
  // Call the taskGraph function
  #ifdef RETURNTYPEVOID
  FUNCTIONNAME(params);
  #else
  result_w = FUNCTIONNAME(params);
  #endif
	
		
  // Any pointer arguments to the TaskGraph now require writing back
  for (int i = 0; i < spe_args.num_args; ++i) {
    if (ptr_info[i].copy_back) {
      const unsigned current_item_size = ptr_info[i].size;
      spu_mfcdma32_any_size(params[i], (unsigned int)ptr_info[i].orig_loc, current_item_size, 0, MFC_PUT_CMD);
    }
  }

  spu_writech(MFC_WrTagMask, 1 << 0);
  (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
	
  _free_align(ptr_info);
  _free_align(paramData);
  free(params);
	
  return 0;
}

int execStd(unsigned long long argp) {
  // Retrieve our arguments using DMA
  spu_mfcdma32_any_size(&spe_args, (unsigned int)argp, sizeof(spe_arg_t), 0, MFC_GET_CMD);
  spu_writech(MFC_WrTagMask, 1 << 0);
  (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);	
	
  // This is required for initial pipeline stages which get executed using this method
  #ifndef RETURNTYPEVOID
  if (!(next_spe_args_ptr || spe_args.result)) {
    our_base_address = spu_readch(SPU_RdInMbox);
    next_spe_args_ptr = spu_readch(SPU_RdInMbox);
	  
    // Send spe_arg_t to second stage
    spe_arg_t next_spe_args __attribute__((aligned(SPU_ALIGN))) = spe_args;
    next_spe_args.input = (char *)((unsigned int)(&result_r) + our_base_address);
    
    spu_mfcdma32_any_size(&next_spe_args, next_spe_args_ptr, sizeof(spe_arg_t), 0, MFC_PUT_CMD);
    spu_writech(MFC_WrTagMask, 1 << 0);
    (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
    
    // Tell the PPU to initialise the rest of the pipe
    spu_writech(SPU_WrOutMbox, 1);
    
    // Write out our flag pointer
    spu_writech(SPU_WrOutMbox, (unsigned int)(&resultIsValid));
  }
  #endif
	  
  // Execute
  int stat;
  if (stat = execute())
	  return stat;
  
  #ifndef RETURNTYPEVOID  
  if (next_spe_args_ptr) {
    // Copy result when lock is free
    while (resultIsValid);
    result_r = result_w;
    resultIsValid = 1;
  } 
  else 
  {
    spu_mfcdma32(&result_w, (unsigned int)spe_args.result, sizeof(RETURNTYPE), 0, MFC_PUT_CMD);
    spu_writech(MFC_WrTagMask, 1 << 0);
    (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
  }
  #endif
	
  return 0;
}

int execPipe() {
  #ifndef RETURNTYPEVOID
  unsigned int isFinalStage = 0, tmp = 0;
  int stat;
  // Initialisation
  // Tell the world the addresses of our global variables
  spu_writech(SPU_WrOutMbox, (unsigned int)&spe_args);
  
  // Acquire addresses
  if (!our_base_address)
    our_base_address = spu_readch(SPU_RdInMbox);
    
  // Determine whether or not we're the final stage in the pipeline
  if (!our_base_address) {
    isFinalStage = 1;
    prev_resultIsValid_ptr = spu_readch(SPU_RdInMbox);
  } 
  else 
  {
    next_spe_args_ptr = spu_readch(SPU_RdInMbox);
    prev_resultIsValid_ptr = spu_readch(SPU_RdInMbox);
    
    // Update the spe_arg_t, send to next stage
    spe_arg_t next_spe_args __attribute__((aligned(SPU_ALIGN))) = spe_args;
    next_spe_args.input = (char *)((unsigned int)(&result_r) + our_base_address);

    spu_mfcdma32_any_size(&next_spe_args, next_spe_args_ptr, sizeof(spe_arg_t), 0, MFC_PUT_CMD);
    spu_writech(MFC_WrTagMask, 1 << 0);
    (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
    // Tell the PPU to initialise the next stage
    spu_writech(SPU_WrOutMbox, 1);
    // Write out flag pointer
    spu_writech(SPU_WrOutMbox, (unsigned int)&resultIsValid);
  }
  
  // Begin the pipeline loop
  while (1) {
    // Wait for the previous stage to complete
    int prev_resultIsValid __attribute__((aligned(SPU_ALIGN)));
    do {
      // Check for termination signal
      if (spu_readchcnt(SPU_RdInMbox)) {
        // Clear mbox
        spu_readch(SPU_RdInMbox);
        // Reset globals
        our_base_address = 0;
        prev_resultIsValid_ptr = 0;
        next_spe_args_ptr = 0;
        // Return to the main() loop
        return 0;
      }
      // If we're not terminating, poll the result from the previous stage    
      spu_mfcdma32(&prev_resultIsValid, prev_resultIsValid_ptr, sizeof(int), 0, MFC_GET_CMD);
      spu_writech(MFC_WrTagMask, 1 << 0);
      (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
    } while (!prev_resultIsValid);
    
    // Execute TaskGraph
    if (stat = execute())
      return stat;
      
    // Mark the previous result invalid (it has been 'used up')
    prev_resultIsValid = 0;
    spu_mfcdma32(&prev_resultIsValid, prev_resultIsValid_ptr, sizeof(int), 0, MFC_PUT_CMD);
    spu_writech(MFC_WrTagMask, 1 << 0);
    (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);

    // Pass on the result accordingly
    if (isFinalStage) {
      // Tell the PPU there's a result ready
      spu_writech(SPU_WrOutMbox, 1);
      // And read in the address we should write to
      unsigned int result_ptr = spu_readch(SPU_RdInMbox);
      // Write back result
      spu_mfcdma32(&result_w, result_ptr, sizeof(RETURNTYPE), 0, MFC_PUT_CMD);
      spu_writech(MFC_WrTagMask, 1 << 0);
      (void)spu_mfcstat(MFC_TAG_UPDATE_ALL);
    } 
    else 
    {
      // Wait for next stage to finish with the previous result
      while (resultIsValid);
      // Copy the result
      result_r = result_w;
      // Mark new result valid
      resultIsValid = 1;
    }
  }
  #else
  fprintf(stderr, "Cannot have a void TaskGraph as part of a pipeline!\n");  
  #endif
  return -1;
}

// The execution loop
int main(unsigned long long id, unsigned long long argp) {
  while (1) {
    // Determine which execute method to run
    if (argp) {
      if (execStd(argp))
        return -1;
    } 
    else 
    {
      if (execPipe())
        return -1;
    }
    // Write to SPU_WrOutMbox (signals completion)
    spu_writech(SPU_WrOutMbox, 1);
    // Read new parameter address from SPU_RdInMBox (stalls while waiting)
    unsigned int new_argp = spu_readch(SPU_RdInMbox);
    // NULL pointer means exit (this way the thread will be destroyed)
    if (new_argp == 0)
      return 0;
    else
      argp = new_argp;       
  }

  return 0;
}
