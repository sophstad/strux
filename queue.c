#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int INTEGER = 0;
int FLOATING = 1;
int STRING = 2;

struct Queue {
	int size;
	struct Node *front;
	struct Node *rear;
};

struct Node {
	struct Node *next;
	void *data;
};

struct Queue *initQueue() {
	struct Queue *q = (struct Queue*) malloc(sizeof(struct Queue));
	q->size = 0;
	q->front = 0;
	q->rear = 0;
	return q;
}

int queue_size(struct Queue *queue) {
	return queue->size;
}

void enqueue(struct Queue *q, void *data) {
	struct Node *node = (struct Node*)malloc(sizeof(struct Node));
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
	struct Node *node = q->front;
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

int printBorder(struct Queue *q, int typ) {
	int totalChars = 0;
	int size = q->size;

	int i;

	totalChars += printf("%s", "+");
	struct Node *curr = q->front;
	for (i = 0; i < size; i++) {
	    int len = 0;
	    char tmp[1000];
	    if (typ == INTEGER) {
		len = sprintf(tmp, " %d ", *(int *) curr->data);
	    } else if (typ == FLOATING) {
		len = sprintf(tmp, " %f ", *(double *) curr->data);
	    } else if (typ == STRING) {
		len = sprintf(tmp, " %s ", *(char **) curr->data);
	    }

	    int j;
	    for (j = 0; j < len; j++) {
		totalChars += printf("-");
	    }
	    totalChars += printf("+");
	    
	    curr = curr->next;
	}
	printf("\n");

	return totalChars;
}

void printHeadTail(int len) {
        int padding = len - strlen("Tail");
	printf("%-*s%s\n", padding, "Head", "Tail");
}

void queue_show(struct Queue *q, int typ) {
	// Print top border
	printBorder(q, typ);

	int i;
	int size = q->size;
	struct Node *curr = q->front;
	printf("%s", "|");
	for (i = 0; i < size; i++) {
            if (typ == INTEGER) {
                printf(" %d |", *(int *) curr->data);
            } else if (typ == FLOATING) {
	        printf(" %f |", *(double *) curr->data);
            } else if (typ == STRING) {
                printf(" %s |", *(char **) curr->data);
            }
	    curr = curr-> next;
	}
	printf("\n");

	int totalChars = printBorder(q, typ);
	printHeadTail(totalChars);
}

void queue_show_int(struct Queue *q)
{	
        queue_show(q, INTEGER);
}

void queue_show_float(struct Queue *q)
{	
        queue_show(q, FLOATING);
}

void queue_show_string(struct Queue *q)
{	
        queue_show(q, STRING);
}
