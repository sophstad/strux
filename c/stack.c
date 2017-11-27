#include <stdio.h>
#include <stdlib.h>

struct Stack {
	unsigned capacity;
	int size;
	struct Node *front;
	struct Node *rear;
};

struct Node {
	struct Node *next;
	void *data;
};


int stack_isFull(struct Stack *stack) {
	return (stack->size >= stack->capacity);
}

struct Stack* initStack(unsigned capacity) {
	struct Stack* stack = (struct Stack*) malloc(sizeof(struct Stack));
	stack->capacity = capacity;
	stack->size = 0;
	stack->front = 0;
	stack->rear = 0;
	return stack;
}

int stack_size(struct Stack* stack) {
	return stack->size;
}

void push(struct Stack *stack, void *data) {
	if (stack_isFull(stack)) {
		return;
	}
	struct Node* node = (struct Node*)malloc(sizeof(struct Node));
	node->data = data;
	node->next = stack->front;
	if (stack->size == 0) {
		stack->front = stack->rear = node;
		stack->size++;
		return;
	}
	stack->front = node;
	stack->size++;
}

void pop(struct Stack *stack) {
	if (stack->size == 0) {
		return;
	}
	struct Node* node = stack->front;
	if (stack->size == 1) {
		stack->front = NULL;
		stack->rear = NULL;
		stack->size--;
	} 
	else {
		stack->front = stack->front->next;
		stack->size--;
	}
	free(node);
}

void *peek(struct Stack *stack) {
	if(stack->size == 0) {
		return NULL;
	}
	return stack->front->data;
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