/***************************************************************************
Timing.h  -  description
-------------------
begin                : Thu Jul 9 2001
copyright            : (C) 2001 by Jeyan
email                :
***************************************************************************/

/***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************/
#ifndef _TIMING_H
#define _TIMING_H


#ifdef _WIN32

#include<windows.h>							//If we are under Win32 System

static LARGE_INTEGER _tstart, _tend;
static LARGE_INTEGER freq;

void tstart(void)
{
	static int first = 1;

	if(first) {
		QueryPerformanceFrequency(&freq);
		first = 0;
	}
	QueryPerformanceCounter(&_tstart);
}
void tend(void)
{
	QueryPerformanceCounter(&_tend);
}

double tval()
{
	return (static_cast<double>(_tend.QuadPart) -
		static_cast<double>(_tstart.QuadPart))/static_cast<double>(freq.QuadPart);
}


#else											//or under the unix/linux susbsystem

#include <time.h>
#include <sys/time.h>

static struct timeval _tstart, _tend;
static struct timezone tz;

void tstart(void)
{
	gettimeofday(&_tstart, &tz);
}
void tend(void)
{
	gettimeofday(&_tend,&tz);
}

double tval()
{
	double t1, t2;

	t1 =  _tstart.tv_sec + static_cast<double>(_tstart.tv_usec)/(1000*1000);
	t2 =  _tend.tv_sec + static_cast<double>(_tend.tv_usec)/(1000*1000);
	return t2-t1;
}
#endif

#endif											//end of header file definition
