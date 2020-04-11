#include <TaskTimer.h>
#include <sys/time.h>

namespace {

inline double utime () {
	struct timeval tv;
	gettimeofday ( &tv, 0 );

	return (tv.tv_sec + double (tv.tv_usec) * 1e-6);
}

}

namespace tg {

Timer::Timer ( bool autoStart ) {
	if ( autoStart )
		start ( );
}

void Timer::start ( ) {
	time = -utime ( );
}

double Timer::split ( ) {
	return time + utime ( );
}

double Timer::stop ( ) {
	time += utime ( );

	// To avoid small rounding issues
	if ( time < 0 )
		time = 0;
	return time;
}

}
