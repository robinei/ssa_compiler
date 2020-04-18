#!/usr/bin/env bash

rm -f compiler *.o
for f in *.c
do
    (gcc -Wall -Wno-unused-function -std=c11 -g -O -c "$f"; echo "Compiled $f") &
done

wait

gcc -o compiler *.o -lm &&
rm -f *.o

./compiler
