#!/bin/bash

clang -emit-llvm -o queue.bc -c c/queue.c
clang -emit-llvm -o stack.bc -c c/stack.c


