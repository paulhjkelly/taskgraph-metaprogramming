#ifndef TASKGRAPH_MANDELBROT_HPP
#define TASKGRAPH_MANDELBROT_HPP

#include<png.h>
#include<unistd.h>
#include<cstdlib>
#include<cstdio>
#include<vector>
#include<string>
#include<cassert>
#include<iostream>

struct Options
{
  int threads;
};

void printUsage(const std::string& name)
{
  std::cerr << "Usage: " << name << " -n threads" << std::endl;
}

Options getOptions(const std::string& name, int argc, char* argv[])
{
  Options options;

  if (argc < 2)
  {
    printUsage(name);
    exit(EXIT_FAILURE);
  }

  int opt;
  while((opt = getopt(argc, argv, "n:")) != -1) 
  {
    switch(opt) {
    case 'n':
      options.threads = atoi(optarg);
      break;
    default:
      printUsage(name);
      exit(EXIT_FAILURE);
    }
  }

  return options;
}


std::vector<png_color> generateBlueYellowColourMap()
{
  std::vector<png_color> map;

  for(int i=0; i<128; ++i)
  {
    png_color colour;
    colour.red = 0;
    colour.green = 0;
    colour.blue = i; 
    map.push_back(colour);
  }

  for(int offset_i=128; offset_i<256; ++offset_i)
  {
    const int i = offset_i - 128;
    png_color colour;
    colour.red = 2*i;
    colour.green = 2*i;
    colour.blue =  128-i; 
    map.push_back(colour);
  }

  assert(map.size() == 256);
  return map;
}

std::vector<png_color> generateRedYellowColourMap()
{
  std::vector<png_color> map;

  for(int i=0; i<128; ++i)
  {
    png_color colour;
    colour.red = i;
    colour.green = 0;
    colour.blue = 0; 
    map.push_back(colour);
  }

  for(int offset_i=128; offset_i<256; ++offset_i)
  {
    const int i = offset_i - 128;
    png_color colour;
    colour.red = 128 + i;
    colour.green = 2*i;
    colour.blue =  0; 
    map.push_back(colour);
  }

  assert(map.size() == 256);
  return map;
}

#endif
