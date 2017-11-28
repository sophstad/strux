#include <stdio.h>
#include <stdlib.h>

struct Queue {
	unsigned capacity;
	int size;
	struct Node *front;
	struct Node *rear;
};

struct Node {
	struct Node *next;
	void *data;
};


int queue_isFull(struct Queue *q) {
	return (q->size >= q->capacity);
}

struct Queue* initQueue(unsigned capacity) {
	struct Queue* q = (struct Queue*) malloc(sizeof(struct Queue));
	q->capacity = capacity;
	q->size = 0;
	q->front = 0;
	q->rear = 0;
	return q;
}

int queue_size(struct Queue* queue) {
	return queue->size;
}

void enqueue(struct Queue *q, void *data) {
	if (queue_isFull(q)) {
		return;
	}
	struct Node* node = (struct Node*)malloc(sizeof(struct Node));
	node->data = data;
	node->next = NULL;
	if (q->size == 0) {
		q->front = q->rear = node;
		q->size++;
		return;
	}
	q->rear = node;
	q->size++;
}

void dequeue(struct Queue *q) {
	if (q->size == 0) {
		return;
	}
	struct Node* node = q->front;
	if (q->size == 1) {
		q->front = NULL;
		q->rear = NULL;
		q->size--;
	} 
	else {
		q->front = q->front->next;
		q->size--;
	}
	free(node);
}

void *front(struct Queue *q) {
	if(q->size == 0) {
		return NULL;
	}
	return q->front->data;
}

// int main()
// {
//     struct Queue* queue = initQueue(10);
//  	int ten = 10;
//  	int twenty = 20;
//  	int thirty = 30;
//     enqueue(queue, &ten);
//     enqueue(queue, &twenty);
//     enqueue(queue, &thirty);
 
//     printf("Front item is %p\n", front(queue));
 
//     return 0;
// }