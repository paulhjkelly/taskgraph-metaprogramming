#ifndef TASKTIMER_H__
#define TASKTIMER_H__

#include "TaskDefines.h"

namespace tg {

class Timer {
public:
  Timer ( bool autoStart = true );
  void start ( );
  double stop ( );
  double split ( );

  double getTime ( ) const {
  	return time;
  }
  operator double ( ) const {
  	return time;
  }
private:
  double time;
};

class AccumTimer {
public:
  AccumTimer ( ) : time ( 0.0 ), restarts ( 0 ) {
  }

  void reset ( ) {
  	time = 0.0;
  	restarts = 0;
  }

  void start ( ) {
  	timer.start();
  }
  void stop ( ) {
  	time += timer.stop();
  	++restarts;
  }
  double getTime ( ) {
  	return time;
  }
  unsigned getRestarts ( ) {
  	return restarts;
  }
private:
  Timer timer;
  double time;
  unsigned restarts;
};

template<unsigned numberLaps>
class StopWatch {
public:
  StopWatch() : currentSplit ( 0 ) {
  }

  void start ( ) {
  	timer.start();
  }
  void split ( ) {
  	time[currentSplit++] = timer.split();
  }
  void stop ( ) {
  	timer.stop ( );
  }
  double getTime ( ) {
  	return timer.getTime();
  }
private:
  Timer timer;
  double time[numberLaps];
  unsigned currentSplit;
};

}

#endif
