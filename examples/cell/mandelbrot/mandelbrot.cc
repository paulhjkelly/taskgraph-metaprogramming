#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <png.h>
#include <TaskGraph>
#include <TaskUserFunctions.h>
#include <boost/static_assert.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include "mandelbrot.hpp"

using namespace tg;

const int IMG_X = 12800;
const int IMG_Y ((IMG_X/(NUM_SPES*4))*3*NUM_SPES); // Approximately 4:3 aspect ratio with number of rows multiple of NUM_SPES

const int MAX_ITERATIONS = 1000;
const char* OUTPUT_FILE = "mandelbrot_set.png";
const float GAMMA_EXPONENT = 3.5f;

const int  UNROLL_FACTOR = 8; // MUST BE A DIVISOR OF IMG_X!
BOOST_STATIC_ASSERT(IMG_X % UNROLL_FACTOR == 0);

typedef TaskGraph<void, int, char[IMG_X]> mandel_tg;

int main(int argc, char* argv[]) {
  int threads;
  const Options options = getOptions("mandelbrot", argc, argv);

  if (options.threads < 1 || options.threads > NUM_SPES)
  {
    std::cerr << "Please select a number of threads between 1 and " << NUM_SPES << " (inclusive)." << std::endl;
    exit(EXIT_FAILURE);
  }
  else if (IMG_Y % options.threads != 0)
  {
    std::cerr << "Please choose a number of threads that is a factor of the image height (" << IMG_Y << ")." << std::endl;
    exit(EXIT_FAILURE);
  }
  else
  {
    threads = options.threads;
  }

  FILE *output_file;
  boost::ptr_vector<mandel_tg> taskGraphs; 
  TaskFarm tFarm;

  mandel_tg T; 
  taskgraph(mandel_tg, T, tuple2(y_pos, line)) {
    tVar(float, x);
    tVar(float, y);
    tVar(float, x0);
    tVar(float, y0);
    tVar(float, xtemp);
    tVar(int, x_pos);
    tVar(int, iteration);
    tVar(float, pixel);
  
    y0 = (y_pos * (3.0f / IMG_Y)) - 1.5f;
    
    // Iterate over the scanline
    tFor (x_pos, 0, IMG_X - 1) {
      x0 = (x_pos * (3.5f / IMG_X)) - 2.5f;
      
      // Unroll the loop
      for (int i = 0; i < UNROLL_FACTOR; ++i) {
        x = x0;
        y = y0;
        iteration = 0;
      
        // Test if this location is in the set
        tWhile ((x*x + y*y) < 4 && iteration < MAX_ITERATIONS) {
          xtemp = (x*x) - (y*y) + x0;
          y = 2*x*y + y0;
          x = xtemp;
          iteration+=1;
        }

        // Calculate and set the value of the pixel
        tIf(iteration == MAX_ITERATIONS)
          pixel = 0.0f;
        tElse
          pixel = 8.0f * (iteration + 1.0f - tLogf(tLogf(tSqrtf(x*x + y*y))) / std::log(2.0f)) / MAX_ITERATIONS;

        line[x_pos] = tPowf(pixel, (1.0f / GAMMA_EXPONENT)) * 255.0f;
        x_pos+=1;
      }
      x_pos-=1;
    }
  }

  T.compile(tg::SPU_GCC, false);

  for(int index=0; index<threads; ++index)
  {
    taskGraphs.push_back(new mandel_tg(T.duplicate()));
    tFarm.add(&taskGraphs.back());
  }

  png_structp png_ptr;
  png_infop info_ptr;
  
  if (!(output_file = fopen(OUTPUT_FILE, "wb"))) {
    std::cerr << "Unable to open " << OUTPUT_FILE << " for writing. Aborting now." << std::endl;
    exit(1);
  }
  
  if (!(png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL))) {
    std::cerr << "Failed to create png write struct. Aborting now." << std::endl;
    exit(1);
  }
  
  if (!(info_ptr = png_create_info_struct(png_ptr))) {
    std::cerr << "Failed to create png info struct. Aborting now." << std::endl;
    // Clean up
    png_destroy_write_struct(&png_ptr,static_cast<png_infopp>(NULL));
    exit(1);
  }
  
  png_init_io(png_ptr, output_file);  
  png_set_IHDR(png_ptr, info_ptr, IMG_X, IMG_Y, 8,
               PNG_COLOR_TYPE_PALETTE,
               PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  png_set_gamma(png_ptr, 0.5, 0.45455);

  std::vector<png_color> palette(generateBlueYellowColourMap());
  png_set_PLTE(png_ptr, info_ptr, &palette[0], palette.size());
  png_write_info(png_ptr, info_ptr);
 
  std::vector<char*> scanlines(threads); 

  for (int spe = 0; spe < threads; ++spe)
    scanlines[spe] = static_cast<char*>(spu_malloc(IMG_X));

  for (int line = 0; line < IMG_Y; line+=threads)
  {
    std::vector<int> lineNumbers(threads);

    for(int spe=0; spe<threads; ++spe)
    {
      lineNumbers[spe] = line+spe;
      taskGraphs[spe].setParameters(lineNumbers[spe], scanlines[spe]);
    }

    tFarm.execute();

    for (int spe = 0; spe < threads; ++spe) 
    {
      png_bytep png_row_ptr = reinterpret_cast<png_bytep>(scanlines[spe]);
      png_write_row(png_ptr, png_row_ptr);
    }
  }
  
  for (int spe = 0; spe < threads; ++spe)
    spu_free(scanlines[spe]);
    
  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  exit(EXIT_SUCCESS);
}

