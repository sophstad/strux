#!/bin/bash
clang -emit-llvm -o queue.bc -c c/queue.c
clang -emit-llvm -o linkedlist.bc -c c/linkedlist.c
clang -emit-llvm -o bstree.bc -c c/bstree.c
clang -emit-llvm -o stack.bc -c c/stack.c
clang -emit-llvm -o quicksort.bc -c c/quicksort.c
