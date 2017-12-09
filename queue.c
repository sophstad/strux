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
        q->rear->next = node;
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

void *peek(struct Queue *q) {
	if(q->size == 0) {
		return NULL;
	}
	return q->front->data;
}

void show_int(struct Queue* q)
{	
	printf("show int \n");
}

void show_string(struct Queue* q)
{	
	printf("show string \n");
}

void show_float(struct Queue* q)
{	
	printf("show float \n");
}

// void show(struct LinkedList* list)
// {
// 	for (int i = 0; i < list->size; i++) {
// 		printf("%s", "| " );
// 		void* current = get(list, i);
// 		printf("%p", current);
// 		printf("%s", " |" );

// 		if (access(list, i) -> next != NULL) {
// 			printf("%s", " -> ");
// 		}
// 	}
// 	printf("\n");
// }


// int main()
// {
//     struct Queue* queue = initQueue(10);
//     enqueue(queue, 10);
//     enqueue(queue, 20);
//     enqueue(queue, 30);
 
//     printf("Front item is %d\n", peek(queue));
 
//     return 0;
// }
