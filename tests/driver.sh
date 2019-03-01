#!/bin/bash

if [[ $# -eq 0 ]] ; then
    echo 'Supply at least one argument'
    exit 1
fi

TESTDB=/tmp/$1.db
TESTSQL=/tmp/$1.sql
TESTPID=/tmp/$1.pid
TESTSRV=./$1.exe

rm -f $TESTDB $TESTSQL $TESTPID $TESTSRV
../bin/urweb -debug -boot -noEmacs -dbms sqlite -db $TESTDB -sql $TESTSQL "$1" || exit 1

if [ -e $TESTSQL ]
then
    sqlite3 $TESTDB < $TESTSQL
fi

$TESTSRV -q -a 127.0.0.1 &
echo $! >> $TESTPID
sleep 1
if [[ $# -eq 1 ]] ; then
    python3 -m unittest $1.py
else
    python3 -m unittest $1.Suite.$2
fi
kill `cat $TESTPID`
