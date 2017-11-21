#!/bin/bash

clang -emit-llvm -o queue.bc -c c/queue.c
clang -emit-llvm -o stack.bc -c c/stack.c

./strux.native <$1> a.ll
llvm-link queue.bc stack.bc a.ll -S > run.ll
clang run.ll
./a.out
rm a.ll
rm run.ll
rm ./a.out
