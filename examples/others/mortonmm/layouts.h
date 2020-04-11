#include "alignedallocs.h"

/*
 * Layouts for  Arrays
 *
 * Paul Kelly , Olav and Jeyan Mar 2003
 *
 * And last modified on 27th March , 2003 - 1257 Hrs.
 */


/* Read the following before using this header file

   1. Some of the platforms (Sun/Alpha) do not require inline
      keyword. High level optimization like -xO4/xO5 or -fast includes
      all these. In these cases, make sure _NO_INLINE_ is defined.

   2. Also, posix_memalign is not implemented on Sun/Alpha machines
      for these platforms, define _NO_MEMALIGN_ as well.

   3. Again, some of the glibc versions are incorrectly imposing a
      constraint on the number of bytes we can allocate (as power of two
      sizes).  The code provided here will take care of this
      situation. Alternatively edit and fix the malloc/malloc.c code ,
      if you are brave enough.
*/

#ifndef FLOATTYPE
#define FLOATTYPE double
#endif


#define INT_WORDLENGTH 32

#define TRUE                1
#define FALSE               0

#define LAYOUT_ROW_MAJOR    0
#define LAYOUT_COL_MAJOR    1
#define LAYOUT_MORTON       2
#define LAYOUT_FOURDMORTON  3

#define mod(aaa, bbb) (((aaa%bbb)<0) ? (bbb+(aaa%bbb)) : (aaa%bbb))
#define divide(aaa, bbb) (aaa/bbb) /* Suggested by phjk instead of div */
#define max(aaa, bbb) ((aaa > bbb) ? (aaa) : (bbb))
#define min(aaa, bbb) ((aaa < bbb) ? (aaa) : (bbb))

#define Ones_1 0x55555555
#define Ones_0 0xaaaaaaaa

typedef unsigned int Layout_Type;

#ifndef MIN_TILE_SIZE
#define MIN_TILE_SIZE 32
#endif


#define ROWMAJOR_MAP(iii,jjj,szsz) (((iii)*(szsz))+(jjj))

#if defined ROWMAJOR
#define   array(M,i,j,sz)  M[(i)*(sz)+(j)]
#endif

#if defined COLMAJOR
#define   array(M,i,j,sz) M[(j)*(sz)+(i)]
#endif

#if defined MORTON
unsigned *Tab1;
unsigned *Tab0;
#define array(M,i,j,sz) M[Tab1[(j)]+Tab0[(i)]]
#endif

#if defined FOURDMORTON
unsigned *Tab1;
unsigned *Tab0;
#define array(M,i,j,sz) M[Tab1[(j)]+Tab0[(i)]]

#endif


/* Returns the log2 of something... - yeah that something should
   be a number
*/
double getLog2Of(unsigned sz)
{
  const double log2     = log( 2.0 );
  const double log2sz   = log( static_cast<double>(sz) ) / log2;
  return log2sz;
}


/* Returns the previous power of two value to the given
   value - e.g 16 for 18, 32 for 35 but 16 for 16
*/


unsigned getPreviousPowerOfTwo(unsigned sz)
{
  const double log2     = log( 2.0 );
  double log2sz   = log( static_cast<double>(sz) ) / log2;
  int padded_sz    = sz;


  if (!isPowerOfTwo(sz)){
  log2sz   = log( static_cast<double>(sz) ) / log2;
  padded_sz   = static_cast<int>(pow( 2.0, floor( log2sz ) ));
  }
  return padded_sz;
}

/* Returns the next  power of two value to the given
   value - e.g 16 for 12, 32 for 25 but 16 for 16
*/

unsigned getNextPowerOfTwo(unsigned sz)
{
  const double log2     = log( 2.0 );
  double log2sz;
  int padded_sz=sz ;


   if (!isPowerOfTwo(sz)){
  log2sz   = log( static_cast<double>(sz) ) / log2;
  padded_sz   = static_cast<int>(pow( 2.0, ceil( log2sz ) ));
}
  return padded_sz;
}

unsigned int SpreadBits(unsigned int bits)
{
  int i;
  unsigned int r = 0;
  for (i=0; i<=INT_WORDLENGTH/2; ++i) {
    unsigned int mask = 1 << i;
    unsigned int thisbit = mask & bits;
    unsigned int shiftedbit = thisbit << i;
    r |= shiftedbit;
  }
  return r;
}

void PrintBits(FILE *fd, unsigned int bits)
{
  int i;
  for (i=INT_WORDLENGTH-1; i>=0; --i) {
    unsigned int mask = 1 << i;
    unsigned int thisbit = mask & bits;
    fprintf(fd, "%d", (thisbit==0) ? 0 : 1 );
  }
  fprintf(fd, "(=%d=0x%x) ", bits, bits);
}
void FillMortonTables(unsigned  tab1[], unsigned tab0[],unsigned sz)
{
  /*
    The terms even and odd are very confusing... I mean, very confusing...
    The summary is, for an array access (i,j) = tab1[i] + tab0[j];

  */

  unsigned int i, oddeven;
  /* find the padded size of this matrix of size sz*/
  unsigned int szm = getNextPowerOfTwo(sz);
  unsigned int szn = getNextPowerOfTwo(sz);

  oddeven = 1;
  for( i = 0; i < szm; ++i ) {
    tab1[i] = SpreadBits(i) << oddeven;
  }
  oddeven=0;
  for (i=0; i<szn; ++i) {
    tab0[i] = SpreadBits(i) << oddeven;
  }

#ifdef _PRINT_TABLES_
 printf("\ni\t\tTab1[i]\n");
 printf("-\t\t------\n");
 for(i=0;i<sz;i++)
   printf("%d\t\t%d\n",i,tab1[i]);

 printf("\n\n");
 printf("\nj\t\tTab0[j]\n");
 printf("-\t\t------\n");
 for(i=0;i<sz;i++)
   printf("%d\t\t%d\n",i,tab0[i]);
#endif
}

void Fill4DMortonTables(unsigned Tab1[],unsigned Tab2[],unsigned sz)
{

  unsigned i,j,n_i_tiles,n_j_tiles,nTileElements;

  /* It is more uglier than it really looks!!.  Since we have decided
     to have the tile size of 'pagesize' we will set the
     MIN_TILE_SIZE=32
  */

  unsigned padded_sz,size_m,size_n,size_tm,size_tn;

  padded_sz=((sz+(MIN_TILE_SIZE-1)&~(MIN_TILE_SIZE-1)));

  /* Now to keep the previous code, untouched ... -- yes I am very lazy */
  size_m=size_n=padded_sz;
  size_tm=size_tn=MIN_TILE_SIZE;

  /* Fill up the standard morton table for the tile sizes first */
  FillMortonTables(Tab1,Tab2,MIN_TILE_SIZE);

  /* Now use  this to fill up the rest of the tables */

  nTileElements=size_tm*size_tn;
  n_i_tiles=size_m/size_tm;
  n_j_tiles=size_n/size_tn;

  /* Fill the j table */

  for(j=size_tn;j<size_n;j++)
    Tab2[j]=(j/size_tn)*nTileElements+Tab2[(j-(j/size_tn)*size_tn)];

  /* Fill the i table */
  for(i=size_tm;i<size_m;i++)
    Tab1[i]=(i/size_tm)*n_j_tiles*nTileElements+Tab1[(i-(i/size_tm)*size_tm)];


#ifdef _PRINT_TABLES_
 printf("\ni\t\tTab1[i]\n");
 printf("-\t\t------\n");
 for(i=0;i<size_m;i++)
   printf("%d\t\t%d\n",i,Tab1[i]);

 printf("\n\n");
 printf("\nj\t\tTab2[j]\n");
 printf("-\t\t------\n");
 for(i=0;i<size_n;i++)
   printf("%d\t\t%d\n",i,Tab2[i]);
#endif

}

/*
 * allocate_matrix
 * Function to allocate a matrix
 */

static INLINE FLOATTYPE *allocate_matrix( const unsigned int sz,
					  const int alignment,
					  const char name[],const Layout_Type pref_layout ) {
  void *ptr = 0;
  int res;

  switch(pref_layout){
  case  LAYOUT_ROW_MAJOR:
    {
      const int size = sz * sz * sizeof( FLOATTYPE );
      res = allocate_aligned( &ptr, alignment, size );

      /* If for some reason memalign doesn't work, then
	 either we can allocate the memory as usual - which is 8 bytes aligned
         or we can terminate as a disaster occured! - let's terminate*/

      assert(res==0);
      break;
    }

  case LAYOUT_COL_MAJOR:
    {
      const int size = sz * sz * sizeof( FLOATTYPE );
      res = allocate_aligned( &ptr, alignment, size );
      assert(res==0);
      break;
    }
  case LAYOUT_MORTON:
    {
      const int padded_sz   = getNextPowerOfTwo(sz);
      const int padded_size = padded_sz * padded_sz * sizeof( FLOATTYPE );
      res = allocate_aligned( &ptr, alignment, padded_size );
      assert(res==0);


#ifdef DEBUG
      printf( "Original Size = %d \n", sz );
      printf( "Padded Size   = %d \n", padded_sz );
      printf( "\n" );
#endif
      break;
    }
  case LAYOUT_FOURDMORTON:
    {
      unsigned int padded_sz;
      unsigned int padded_size;

      if (isPowerOfTwo(sz)){
	padded_sz = sz;
      }else{
	padded_sz=((sz+(MIN_TILE_SIZE-1)&~(MIN_TILE_SIZE-1)));
      }
      padded_size = padded_sz * padded_sz * sizeof( FLOATTYPE );
      res = allocate_aligned( &ptr, alignment, padded_size );
      assert(res==0);

#ifdef DEBUG
      printf( "Original Size = %d \n", sz );
      printf( "Padded Size   = %d \n", padded_sz );
      printf( "\n" );
#endif
      break;
    }
  }/* end of switch */

  return static_cast<FLOATTYPE *>(ptr);
}

static INLINE unsigned  *allocate_vector( const unsigned int sz,
					  const char name[] ) {
  unsigned *ptr;

  ptr = static_cast<unsigned  *>(calloc(sz,sizeof(unsigned)));
  return ptr;
}


/*
 * free_matrix
 * Function to free a matrix
 */

static INLINE void free_matrix(void *ptr)
{
  free_aligned(ptr);
}
