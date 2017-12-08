#!/bin/bash
clang -emit-llvm -o queue.bc -c queue.c
clang -emit-llvm -o linkedlist.bc -c linkedlist.c
clang -emit-llvm -o stack.bc -c stack.c

