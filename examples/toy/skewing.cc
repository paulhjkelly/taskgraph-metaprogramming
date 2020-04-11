#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

const int MATRIXSIZE = 16;

typedef float matrix[MATRIXSIZE][MATRIXSIZE];

typedef TaskGraph<void, matrix> skew_TaskGraph;

void initMatrix(matrix m) {
	for (int i = 0; i < MATRIXSIZE; i++) {
		for (int j = 0; j < MATRIXSIZE; j++) {
			m[i][j] = static_cast<float>(rand()) / RAND_MAX;
		}
	}
}

void cSkew(matrix m) {
	for (int i = 1; i < MATRIXSIZE; i++) {
		for (int j = 1; j < MATRIXSIZE; j++) {
			m[i][j] = m[i][j] + ((m[i-1][j]+m[i][j-1])/2);
		}
	}
}

void printMatrix(matrix m) {
	for (int y = 0; y < MATRIXSIZE; y++) {
		for (int x = 0; x < MATRIXSIZE; x++) {
			printf("%.3f ", m[x][y]);
		}
		puts("\n");
	}
}

const float DELTA = 0.0001f;

void compMatrices(matrix a, matrix b) {
	for (int i = 0; i < MATRIXSIZE; i++) {
		for (int j = 0; j < MATRIXSIZE; j++) {
			if (a[i][j] - b[i][j] > DELTA) {
				puts("Matrices differ!");
				printf("a[%d][%d] = %f, b[%d][%d] = %f\n", i, j, a[i][j], i, j, b[i][j]);
				return;
			}
		}
	}
	puts("Matrices match");
}

int main( int argc, char *argv[] ) {
  skew_TaskGraph T;
  matrix a, b;
  initMatrix(a);
  puts("-- Original Matrix -- ");
//  printMatrix(a);

  memcpy(b, a, sizeof(matrix));
  cSkew(b);
  puts("-- C-Skew Method --");
//  printMatrix(b);

  taskgraph( skew_TaskGraph, T, tuple1(m) ) {
	  tVar(int, i);
	  tVar(int, j);
	tFor (i, 1, MATRIXSIZE-1) {
		tFor (j, 1, MATRIXSIZE-1) {
			m[i][j] = m[i][j] + ((m[i-1][j]+m[i][j-1])/2);
		}
	}
  }
  SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1 ), 1 );
  T.applyOptimisation ( "skew", &skew );
  T.print();
  T.compile( tg::GCC, true );
  T.execute( a );
  puts("-- TG-Skew Method --");
//  printMatrix(a);

  compMatrices(a, b);
}

