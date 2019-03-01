#!/bin/bash

TEST=endpoints
TESTPID=/tmp/$TEST.pid
TESTENDPOINTS=/tmp/$TEST.json
TESTSRV=./$TEST.exe

rm -f $TESTENDPOINTS $TESTPID $TESTSRV
../bin/urweb -debug -boot -noEmacs -endpoints $TESTENDPOINTS  "$TEST" || exit 1

$TESTSRV -q -a 127.0.0.1 &
echo $! >> $TESTPID
sleep 1
python3 $TEST.py
kill `cat $TESTPID`
