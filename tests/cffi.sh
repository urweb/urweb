#!/bin/bash

CCOMP=gcc

$CCOMP -pthread -Wimplicit -Werror -Wno-unused-value -I ..include/urweb   -c "test.c" -o "test.o" -g
./driver.sh cffi
