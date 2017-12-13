#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

struct Stack {
	int size;
	struct Node *top;
};

struct Node {
	struct Node *next;
	void *data;
};

struct Stack* initStack(unsigned capacity) {
	struct Stack* stack = (struct Stack*) malloc(sizeof(struct Stack));
	stack->size = 0;
	stack->top = NULL;
	return stack;
}

int stack_size(struct Stack* stack) {
	return stack->size;
}

void push(struct Stack *stack, void *data) {
	struct Node* node = (struct Node*)malloc(sizeof(struct Node));
	node->data = data;
	node->next = stack->top;
	stack->top = node;
	stack->size++;
}

void pop(struct Stack *stack) {
	if (stack->size == 0) {
		return;
	}

	struct Node* node = stack->top;
	if (stack->size == 1) {
		stack->top = NULL;
		stack->size--;
	} else {
		stack->top = stack->top->next;
		stack->size--;
	}
	free(node);
}

void *top(struct Stack *stack) {
	if (stack->size == 0) {
		return NULL;
	}
	return stack->top->data;
}

void printLine(int size) {
        printf("+");
        int i;
        for (i = 0; i < size; i++) {
            printf("-");
        }
	printf("+");
}

void stack_show(struct Stack *stack, int typ) {
        int INTEGER_MAX = 13;
        int FLOATING_MAX = 20;
        int STRING_MAX = 48;

        if (typ == INTEGER) {
            printLine(INTEGER_MAX);
        } else if (typ == FLOATING) {
            printLine(FLOATING_MAX);
        } else if (typ == STRING) {
            printLine(STRING_MAX);
        }
        printf(" <- Top\n");

        int i;
        int size = stack->size;
        struct Node *curr = stack->top;
        for (i = 0; i < size; i++) {
            if (typ == INTEGER) {
                printf("| %-*d |\n", INTEGER_MAX - 2, *(int *) curr->data);
                printLine(INTEGER_MAX);
            } else if (typ == FLOATING) {
                printf("| %-*f |\n", FLOATING_MAX - 2, *(double *) curr->data);
                printLine(FLOATING_MAX);
            } else if (typ == STRING) {
                printf("| %-*s |\n", STRING_MAX - 2, *(char **) curr->data);
                printLine(STRING_MAX);
            }
            printf("\n");
            curr = curr->next;
        }
}

void stack_show_int(struct Stack* stack)
{	
        stack_show(stack, INTEGER);
}

void stack_show_float(struct Stack* stack)
{	
        stack_show(stack, FLOATING);
}

void stack_show_string(struct Stack* stack)
{	
        stack_show(stack, STRING);
}
