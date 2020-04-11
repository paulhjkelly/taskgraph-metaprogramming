#ifndef SPU_ARGS_H__
#define SPU_ARGS_H__

#define SPU_ALIGN_LOG2		4			// Align to a 2**ALIGN boundary
#define SPU_ALIGN		(1 << SPU_ALIGN_LOG2)
#define SPU_MAX_DMA_SIZE	16384

#define SPU_IDLE_NO_TG		0
#define SPU_IDLE_LOADED_TG	1
#define SPU_RUNNING		2

// Maintain pointer information
typedef struct {
  // The byte offset of this item in the parameter list
  unsigned offset;
  // The amount of data we point to (bytes)
  unsigned size;
  // The address that the pointer holds in main memory
  void *orig_loc;
  // Does this date need to be copied back?
  char copy_back;
}__attribute__((aligned(SPU_ALIGN))) pointer_info_t;

// Encapsulate spe arguments (args to the TaskGraph)
typedef struct {
	// Arguments go here
	// The arguments, concatenated together
	char *input;
	// Number of arguments supplied
	unsigned num_args;
	// Size of input
	unsigned input_size;
	// Pointer for output
	void *result;
	// Pointer info for input
	pointer_info_t *ptr_info;
}__attribute__((aligned(SPU_ALIGN))) spe_arg_t;

// Encapsulate thread arguments as a struct
struct thread_args {
  struct spe_context *spe;
  spe_arg_t *argp;
  void *envp;
};
#endif
