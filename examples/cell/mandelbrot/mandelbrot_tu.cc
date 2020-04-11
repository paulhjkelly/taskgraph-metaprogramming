#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <vector>
#include <map>
#include <png.h>
#include <TaskGraph>
#include <TaskUserFunctions.h>
#include <boost/ptr_container/ptr_vector.hpp>
#include "mandelbrot.hpp"

using namespace tg;

const float gamma_exponent = 3.5;
const int max_iterations = 1000;

const int max_alloc = 1024*176; // Allocate up to 176K
const int width = 12800;
const int height = 9600;
const char* output_filename = "mandelbrot_set_tu.png";

struct TgInfo
{
  int startRow;
  int rowCount;
  char* data; 
};

int main(int argc, char* argv[])
{
  int threads;
  const Options options = getOptions("mandelbrot_tu", argc, argv);

  if (options.threads < 1 || options.threads > NUM_SPES)
  {
    std::cerr << "Please select a number of threads between 1 and " << NUM_SPES << " (inclusive)." << std::endl;
    exit(EXIT_FAILURE);
  }
  else
  {
    threads = options.threads;
  }

  const int rowSize = width * sizeof(char);
  const int rowMultiple = max_alloc/rowSize;
  unsigned int arraySize[2] = {rowMultiple, width};

  std::cout << "Calculating mandelbrot of size " << width << " x " << height << "." << std::endl;
  std::cout << "Will transfer data in blocks of " << rowSize*rowMultiple << " bytes." << std::endl;

  tuTaskGraph mandel;
  tu_taskgraph(mandel)
  {
    tParameter(tVarNamed(int, startRow, "startRow"));
    tParameter(tVarNamed(int, rowCount, "rowCount"));
    tParameter(tArrayFromListNamed(char, imageData, 2, arraySize, "imageData"));

    tVar(float, x);
    tVar(float, y);
    tVar(float, x0);
    tVar(float, y0);
    tVar(float, xtemp);
    tVar(int, x_pos);
    tVar(int, y_pos);
    tVar(int, iteration);
    tVar(float, pixel);
  
    tFor(y_pos, startRow, startRow+rowCount-1)
    {
      y0 = (y_pos * (3.0f / height)) - 1.5f;
      
      tFor(x_pos, 0, width-1)
      {
        // Scale the pixel location to be +- 2 from the origin of the complex plane
        x0 = (x_pos * (3.5f / width)) - 2.5f;
        x = x0;
        y = y0;
        iteration = 0;
      
        // Test if this location is in the set
        tWhile ((x*x + y*y) < 4.0f && iteration  < max_iterations) {
          xtemp = (x*x) - (y*y) + x0;
          y = 2*x*y + y0;
          x = xtemp;
          iteration+=1;
        }
        
        // Calculate and set the value of the pixel
        tIf(iteration == max_iterations)
          pixel = 0.0f;
        tElse
          pixel = 8.0f * (iteration + 1.0f - tLogf(tLogf(tSqrtf(x*x + y*y))) / std::log(2.0f)) / max_iterations;

        imageData[y_pos-startRow][x_pos] = tPowf(pixel, (1.0f / gamma_exponent)) * 255.0f;
      }
    }
  }
 
  mandel.compile(tg::SPU_GCC, false);

  boost::ptr_vector<tuTaskGraph> taskGraphs; 
  std::map<tuTaskGraph*, TgInfo> tgInfo;
  TaskFarm farm;

  for(int thread=0; thread<threads; ++thread)
  {
    taskGraphs.push_back(new tuTaskGraph(mandel)); 
    farm.add(&taskGraphs.back());
    tgInfo[&taskGraphs.back()].data = static_cast<char*>(spu_malloc(rowSize*rowMultiple));
  }
  
  FILE *output_file;
  png_structp png_ptr;
  png_infop info_ptr;
  
  if (!(output_file = fopen(output_filename, "wb"))) {
    std::cerr << "Unable to open " << output_filename << " for writing. Aborting now." << std::endl;
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
  png_set_IHDR(png_ptr, info_ptr, width, height, 8,
               PNG_COLOR_TYPE_PALETTE,
               PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  png_set_gamma(png_ptr, 0.5, 0.45455);

  std::vector<png_color> palette(generateRedYellowColourMap());
  png_set_PLTE(png_ptr, info_ptr, &palette[0], palette.size());
  png_write_info(png_ptr, info_ptr);

  int currentRow = 0;

  while(currentRow < height)
  {
    for(int thread=0; thread<threads; ++thread)
    {
      tgInfo[&taskGraphs[thread]].startRow = currentRow;
      tgInfo[&taskGraphs[thread]].rowCount = std::min(rowMultiple, height-currentRow);
      currentRow+=tgInfo[&taskGraphs[thread]].rowCount;
    
      taskGraphs[thread].setParameter("startRow", &tgInfo[&taskGraphs[thread]].startRow);
      taskGraphs[thread].setParameter("rowCount", &tgInfo[&taskGraphs[thread]].rowCount);
      taskGraphs[thread].setParameter("imageData", tgInfo[&taskGraphs[thread]].data);
    }

    farm.execute();
    
    for(int thread=0; thread<threads; ++thread)
    {
      for (int line = 0; line<tgInfo[&taskGraphs[thread]].rowCount; ++line) 
      {
        png_bytep png_row_ptr = reinterpret_cast<png_bytep>(tgInfo[&taskGraphs[thread]].data + rowSize*line);
        png_write_row(png_ptr, png_row_ptr);
      }
    }
  }
  
  std::cout << "Done." << std::endl;

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  for(int thread=0; thread<threads; ++thread)
    spu_free(tgInfo[&taskGraphs[thread]].data);

  exit(EXIT_SUCCESS);
}
