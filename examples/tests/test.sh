#!/bin/bash

TESTS="simple variable return array loops"

mkdir -p results 

let "FAIL = 0"
for test in $TESTS
do
	./$test | diff - $test.exp &> results/$test
	DC=`cat results/$test | wc -l`
	if [ "$DC" -eq "0" ]
	then
		echo "$test: OK"
	else
		echo "$test: FAILED"
		let "FAIL++"
	fi
done
echo "Failures:$FAIL"
