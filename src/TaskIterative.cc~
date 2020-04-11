#include "TaskIterative.h"
#include "TaskGraph.h"
#include <functional>
#include <iostream>

// FIXME: I leak lots of memory, use auto-pointer or something
namespace tg {

OneVariableIterative::OneVariableIterative ( function_type _creationFunction, unsigned min, unsigned max, unsigned _steps, unsigned _clusterSize )
	: creationFunction ( _creationFunction),
	  steps ( _steps ),
	  finished ( false ),
	  clusterSize ( _clusterSize ),
	  compiler ( new GnuCCompiler ) {
	points.insert ( min );
	addRange ( min, max - min );
	points.insert ( max );
}

coreTaskGraph *OneVariableIterative::getNext ( unsigned *point ) {
	if ( finished ) {
		*point = (*resultsByTime.begin()).first;
		return taskGraph;
	}
	if ( points.empty ( ) ) {
		findMore ( );
		if ( finished ) {
			*point = (*resultsByTime.begin()).first;
			return taskGraph;
		}
	}

	if ( precompiled.empty ( ) ) {
		compileCluster ( );
	}

	taskGraph = precompiled.front();
	precompiled.pop_front ( );
	*point = *points.begin ( );
	points.erase ( points.begin ( ) );

	return taskGraph;
}

void OneVariableIterative::setResult ( unsigned point, float time ) {
	if ( finished )
		return;
	num_type::iterator iter = resultsByNum.find ( std::make_pair<unsigned,float>(point, time) );

	// If this point has been done before erase it
	if ( iter != resultsByNum.end ( ) ) {
		resultsByNum.erase ( iter );
		resultsByTime.erase ( find_if ( resultsByTime.begin(),
		                                resultsByTime.end(),
		                                std::bind2nd ( tg::pair_first_equal<pair_type>(), point ) ) );
	}

	// Insert new point
	resultsByNum.insert ( pair_type (point, time) );
	resultsByTime.insert ( pair_type (point, time) );
}

void OneVariableIterative::addRange ( unsigned min, unsigned range ) {
	float stepSize = range / static_cast<float>(steps - 1);
	for ( unsigned a = 1; a < (steps - 1); ++a ) {
		points.insert ( static_cast<unsigned>(min + stepSize * a ) );
	}
}

void OneVariableIterative::compileCluster ( ) {
	TaskGraphGroup group;

	std::size_t num = std::min ( clusterSize, points.size () );
	std::set<unsigned>::const_iterator iter = points.begin ( );

	for ( std::size_t a = 0; a < num; ++a ) {
		coreTaskGraph *t;
		(*creationFunction) ( t, *iter++ );

		group.add ( t );
		precompiled.push_back ( t );
	}
	group.compile ( compiler.get(), true );
}

// TODO: Needs cleaning up and parameterising
void OneVariableIterative::findMore ( ) {
	unsigned done = 0;
	unsigned num = std::max<unsigned> ( static_cast<unsigned>(resultsByNum.size ( ) * 0.25f), 1 );

	time_type::const_iterator it = resultsByTime.begin();
	for ( unsigned a = 0; a < num; ++a, ++it ) {
		num_type::const_iterator iter = resultsByNum.find ( *it );
		points.insert ( (*iter).first );
		if ( iter != resultsByNum.begin ( ) ) {
			unsigned lowest = (*prev ( iter ) ).first;
			unsigned range = (*iter).first - lowest;
			addRange ( lowest, range );
			if ( a == 0 && range == 1 )
				++done;
		} else if ( a == 0 ) {
			++done;
		}
		if ( (++iter) != resultsByNum.end ( ) ) {
			unsigned lowest = (*prev (iter)).first;
			unsigned range = (*iter).first - lowest;
			addRange ( lowest, range );
			if ( a == 0 && range == 1 )
				++done;
		} else if ( a == 0 ) {
			++done;
		}
	}
	if ( done == 2 ) {
		finished = true;
		(*creationFunction) ( taskGraph, (*resultsByTime.begin()).first );
		taskGraph->compile ( compiler.get(), true );
	}
}

}
