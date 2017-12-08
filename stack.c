#include <stdio.h>
#include <stdlib.h>

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


// int main()
// {
//     struct Stack* stack = initStack(10);
 
//     push(stack, 10);
//     push(stack, 20);
//     push(stack, 30);
 
//     printf("Front item is %p\n", front(stack));
 
//     return 0;
// }
