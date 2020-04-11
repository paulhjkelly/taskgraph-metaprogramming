#ifndef TASKITERATIVE_H__
#define TASKITERATIVE_H__

#include "TaskDefines.h"
#include "TaskUtilities.h"
#include "TaskCompilers.h"
#include <set>
#include <deque>
#include <utility>
#include <algorithm>
#include <functional>
#include <memory>

namespace tg {

class coreTaskGraph;

class TaskGraphGroup;

class OneVariableIterative {
private:
	typedef std::pair<unsigned, float> pair_type;
	typedef std::set<pair_type, tg::pairless<unsigned, pair_type, tg::select1st<pair_type> > >  num_type;
	typedef std::multiset<pair_type, tg::pairless<float, pair_type, tg::select2nd<pair_type> > >  time_type;
	typedef void (*function_type) ( coreTaskGraph *&graph, unsigned point);

public:
	OneVariableIterative ( function_type creationFunction, unsigned min, unsigned max, unsigned steps = 3, unsigned clusterSize = 4 );
	coreTaskGraph *getNext ( unsigned *point );
	void setResult ( unsigned point, float time );
	void setCompiler ( TaskCompiler *_compiler ) {
		compiler.reset ( _compiler );
	}

protected:
	void addRange ( unsigned min, unsigned range );
	void findMore ( );
	void compileCluster ( );

private:
	num_type resultsByNum;
	time_type resultsByTime;
	function_type creationFunction;

	std::set<unsigned> points;

	unsigned steps;
	bool finished;
	coreTaskGraph *taskGraph;
	std::deque<coreTaskGraph *> precompiled;
	std::size_t clusterSize;
	std::auto_ptr<TaskCompiler> compiler;
};

}

#endif
