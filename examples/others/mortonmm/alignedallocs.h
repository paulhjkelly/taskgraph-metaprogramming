#ifndef __ALIGNED_ALLOCS_
#define __ALIGNED_ALLOCS_
#include<assert.h>
#include<malloc.h>

#ifndef _NO_INLINE_
#define INLINE inline
#else
#define INLINE
#endif

/* Maximum number of arrays we can allocate */

#define MAX_ALIGNED_ARRAYS 50

/* Now the posix compliant error messages */

#ifndef DEFAULT_ALIGN
#define DEFAULT_ALIGN 8
#endif

#ifndef ENOMEM
#define ENOMEM 0x0c
#endif


#ifndef EINVAL
#define EINVAL 22
#endif


/* A routine to check whether a given value is power of two or not */

static INLINE int isPowerOfTwo(long n)
{
 while (n>1L) {
    if (n%2L) break;
    n >>= 1L;
  }
  return(n==1L);
}


/*******************************************************
 * We need the following routine in the absence of the *
 * standard memalign routine , such as in OSF/Tru64    *
 ******************************************************/


#if defined NOMEMALIGN

static int nArraysAllocated=0;

/*
  Then we define an array to keep track of the arrays
  we allocated so far:
  arrayPointers[i][0] has the aligned pointer to be freed
  arrayPointers[i][1] has the actual  pointer to be freed

*/

static void* alignedArrayPointers[MAX_ALIGNED_ARRAYS][2];

#endif


#if defined NOMEMALIGN

int allocate_aligned(void ** memptr,size_t alignment, size_t size)
{
  void * ptr, *aligned_ptr;
  size_t extra_size;
  int i;

  /* If the alignment parameter is not a power of two , assert */
  assert(isPowerOfTwo(alignment));
  /*assert(alignment % sizeof (void *)!= 0);*/

  /* Now see whether the user is wishing for too much */

  if (alignment<DEFAULT_ALIGN)
    alignment=DEFAULT_ALIGN;


  /*
     Now , if the alignement is the default align,
     again we have nothing to do
  */

  if (alignment==DEFAULT_ALIGN){
    ptr=malloc(size);

    if(ptr==NULL)
      return ENOMEM;

    alignedArrayPointers[nArraysAllocated][0]=ptr;
    alignedArrayPointers[nArraysAllocated][1]=ptr;
    nArraysAllocated++;
    *memptr=ptr;
    return 0;
  }

  /* If we are lucky, we might hit the right address If not, the right
     address may be X/8-1 words far away from the current
     position. Thus, we need some extra memory so that we could shift
     to the right location.

     We should shift by simply masking away the last (n-1) bits of the address

     For simplicity, we would allocate 2^n+SZ bytes and we will close our
     eyes regarding the extra memory we allocated
  */


  extra_size=size+alignment;

  ptr=malloc(extra_size);
  if(ptr==NULL)
    return ENOMEM;


  /* Now find the shift to the aligned address */

  /*
    The reason why I am adding (alignment-1) before masking off the
    bits is to get the correct bits set up. For example, suppose that
    I would like to align at 16 byte boundaries. if my allocated
    address is 24, I can't simply mask off the last four bits -- if I
    do that, I will go back to the address of 16 --- which might fall
    into the previously allocated memory regions. So , to be on safe
    side, I should jump forward in the memory region (this is where I
    need to allocate extra memory).  The worst case would be I have to
    go forward by (alignment-1) bytes. I go forward by that amount and
    then I mask off the last n bits.  */

  aligned_ptr = (void *) (((size_t)ptr + (alignment - 1)) & ~(alignment - 1));

  /* Keep a record of what we are allocating */

  alignedArrayPointers[nArraysAllocated][0]=aligned_ptr;
  alignedArrayPointers[nArraysAllocated][1]=ptr;
  nArraysAllocated++;
  *memptr=aligned_ptr;

  return 0;
}

#else

int allocate_aligned(void ** memptr, size_t alignment, size_t size)
{
  void *ptr;
  /* Test that ALIGNMENT argument is valid.  It must be a power of two
     and multiple of sizeof (void *).  */
  if (!isPowerOfTwo(alignment))
    return EINVAL;

  ptr = static_cast<void *>(memalign (alignment, size));
  if (ptr!=NULL){
    *memptr=ptr;
    return 0;
  }

  return ENOMEM;

}
#endif


#if defined NOMEMALIGN
int free_aligned(void *ptr)
{
  int loc=0,i;
  int flag=0;
  void *mem;

  while((loc<nArraysAllocated) && (!flag)){
    if(alignedArrayPointers[loc][0]==ptr)
      flag=1;
    else
      loc++;
  }

  /*assert(flag!=0);*/
  if(!flag)
    return -1;

  /* Now free the actual pointer */
  mem=alignedArrayPointers[loc][1];
  free(mem);


  /* and then shift the array positions */
  for (i=loc;i<nArraysAllocated-1;i++){
    alignedArrayPointers[i][0]=alignedArrayPointers[i+1][0];
    alignedArrayPointers[i][1]=alignedArrayPointers[i+1][1];
  }
  nArraysAllocated--;
  return 0;
}

#else
int free_aligned(void *ptr)
{
  free(ptr);
  return 0;
}

#endif


#endif /* of __ALIGNED_ALLOCS__*/





















