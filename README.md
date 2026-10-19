# The TaskGraph Meta-programming Library

The TaskGraph Meta-programming Library is a C++ package that supports

* Run-time code generation - programs that generate code on the fly, then execute it (the first step towards "multi-stage" programming)

* Meta-programming - having built a piece of code, you can apply various transformations to it (such as loop interchange and tiling).

The library is presented in the following paper (however this distribution has a tidied and more typesafe design):

> Olav Beckmann, Alastair Houghton, Michael R. Mellor, Paul H. J. Kelly:
> Runtime Code Generation in C++ as a Foundation for Domain-Specific Optimisation. Domain-Specific Program Generation 2003: 291-306

```bibtex
@inproceedings{Beckmann2003RuntimeCG,
  title={Runtime Code Generation in C++ as a Foundation for Domain-Specific Optimisation},
  author={Olav Beckmann and Alastair Houghton and Michael R. Mellor and Paul H. J. Kelly},
  booktitle={Domain-Specific Program Generation},
  editor = "hristian Lengauer and Don Batory and Charles Consel and Martin Odersky",
  publisher = "Springer LNCS",
  volume = 3016,
  year={2003}
}
```
Available from [Springer](https://link.springer.com/chapter/10.1007/978-3-540-25935-0_17) or freely from [here](https://www.doc.ic.ac.uk/~phjk/Publications/DagstuhlDSPGBookPaperOnTaskGraphs.pdf).

The core idea here, that we use types to distinguish between binding times instead of quasi-quotation (as seen in Lisp, tick-C and MetaOcaml), is common to Lightweight Modular Staging (LMS), as proposed by Rompf and Odersky in 2010 (the paper is [here](https://dl.acm.org/doi/10.1145/1868294.1868314)). 


## Example (from examples/introductory/simpleinterchange)
```cpp 
#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

typedef TaskGraph<int, int> TG_IntResIntArg;

int main( int argc, char *argv[] ) {
  TG_IntResIntArg T;

  int n = atoi( argv[1] );

  taskgraph( TG_IntResIntArg, T, tuple1(a) ) {
    tVar(int, i);
    tVar(int, j);

    tFor(i, 0, n-1) {
      tFor(j, 0, n-1) {
        tPrintf("Iteration i=%d, j=%d\n", i, j);
      }
    }
    tReturn(a + n);
  }

  InterchangeSettings inter;
  inter.firstLoop = LoopIdentifier ( 1 );
  inter.secondLoop = LoopIdentifier ( 1, 1 );

  T.applyOptimisation ( "interchange", &inter );

  T.compile( tg::GCC, true );

  printf( "T(%d) = %d\n", n, T(n) );
}
```
If you run this with input 2, it prints:
``` 
Iteration i=0, j=0
Iteration i=1, j=0
Iteration i=0, j=1
Iteration i=1, j=1
T(2) = 4
```
At runtime it generates the following code:
```cpp
extern int printf(char *, ...);
extern int taskGraph_0(void **);
 
extern int taskGraph_0(void **params)
  {
    int *a;
    int i;
    int j;
    static char string0_[23] = "Iteration i=%d, j=%d\n";
 
    a = *params;
    for (j = 0; j <= 1; j++)
      {
        for (i = 0; i <= 1; i++)
          {
            printf(string0_, i, j);
          }
      }
    return *a + 2;
  }
```
The distribution includes examples that demonstrate performance improvements thanks to two Taskgraph features:
* Specialisation:
* Convolution filtering
* Interpreter for simple functional language
* JIT for a RISC instruction set
* Ray tracing
* Transformation:
* Tiling and loop interchange for matrix multiply
* Skewing and tiling for a Gauss-Seidel smoother
* Design-space exploration: searching for optimum tiling factor

There are also some more complex generators, for example producing recursively-tiled loops to walk arrays in Morton order - code that is very hard to write by hand.
Taskgraphs are typed and taskgraph parameters are type-checked.

## Authors and Contributors
* Alastair Houghton
* Michael Mellor
* Paul Kelly
* Peter Fordham
* Olav Beckmann
* Konstantinos Spyropoulos
* Peter Collingbourne
* Francis Russell
* Will Deacon

## Installation
* Requires gcc (not just at build time but also at runtime) and boost (and probably more)
    * Create empty files that are required to exist by the `autoreconf` tool:
    
        ```
        touch NEWS; touch README ; touch ChangeLog
        ```

    * Regenerate the `configure` file using `autoreconf`: 
    
        ```
        autoreconf -fi -I m4
        ```

    * Regenerate `Makefile.in` files from `Makefile.am` templates:
        
        ```
        automake
        ```

    * Finally configure and build the project:

        ```
        ./configure; make
        ```

## System requirements
Tested in Ubuntu 20.04.  It has worked under cygwin in the past.  

## Getting Started
Take a look at the examples/introductory/simple directory.  Try to run:
```
./addc 1
```
This should print `T(b) = 2`.

There are additional examples included in this package, showing various algorithms or techniques that the TaskGraph library is best suited at. They typically contain a C++ implementation of the algorithm or technique, as well as a TaskGraph implementation, for easy comparison of speeds. More details on each example can be found in the examples/README file.
