#!/bin/sh

set -e

cd `dirname $0`

urweb -dbms sqlite dbupload2

rm -rf dbupload2.db || true
sqlite3 dbupload2.db < dbupload2.sql

./dbupload2.exe -p 8083 &
sleep 1

touch /tmp/empty
curl --verbose -F"operation=upload" -F"filename=@/tmp/empty" http://localhost:8083/Blabla/bla

