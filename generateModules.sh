#!/bin/bash
clang -emit-llvm -o queue.bc -c queue.c
clang -emit-llvm -o linkedlist.bc -c linkedlist.c
clang -emit-llvm -o bstree.bc -c c/bstree.c
clang -emit-llvm -o stack.bc -c stack.c
clang -emit-llvm -o quicksort.bc -c quicksort.c
