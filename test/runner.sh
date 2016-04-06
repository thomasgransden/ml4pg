#!/bin/sh

if [ -z "$TESTS" ]
then
    TESTS="coq ssreflect"
fi

cd "$ML4PG_HOME" || {
    echo "Couldn't cd to '$ML4PG_HOME'" >> /dev/stderr
    exit 1
}

for TEST_SUITE in $TESTS
do
    export TEST_SUITE
    echo "Running $TEST_SUITE tests"
    emacs --quick --debug-init --script test/runner.el 2>&1 | grep -v "^Loading.*\.\.\.$"
done
