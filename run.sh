#!/usr/bin/env bash

rm -f compiler *.o
for f in *.c
do
    gcc -Wall -Wno-unused-function -std=c11 -g -c "$f" &
done

wait

gcc -rdynamic -g -o compiler *.o -lm
#rm -f *.o

LD_PRELOAD=/lib/x86_64-linux-gnu/libSegFault.so ./compiler
