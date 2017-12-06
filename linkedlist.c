#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>

struct LinkedList {
	struct ListNode *head;
	int size;
};

struct ListNode {
	void *data;
	struct ListNode *next;
};

struct LinkedList* initList() {
	struct LinkedList* list = (struct LinkedList*) malloc(sizeof(struct LinkedList));
	list->size = 0;
	list->head = NULL;
	return list;
}

void add(struct LinkedList *list, void *data) {
	struct ListNode* newNode = (struct ListNode*) malloc(sizeof(struct ListNode));
	if (newNode == NULL)
		return;

	newNode->data = data;
	newNode->next = NULL;

	if (list->head == NULL) {
		list->head = newNode;
		list->size++;
	} else {
		struct ListNode* temp = list->head;
		while (temp->next != NULL) {
			temp = temp->next;
		}
		temp->next = newNode;
		list->size++;
	}
}

int isEmpty(struct LinkedList *list) {
	return (list->head == NULL);
}

void delete(struct LinkedList* list, int index) {
	if (isEmpty(list)) {
		return;
	}
	if (index == 0) {
		struct ListNode* temp = list->head;
		list->head = list->head->next;
		list->size--;
		free(temp);
		return;
	}

	int ctr = 0;
	struct ListNode* temp = list->head;
	while(ctr < index-1) {
		temp = temp->next;
		ctr++;
	}
	temp->next = temp->next->next;
	temp = temp->next;
	free(temp);
	list->size--;
}

void* get(struct LinkedList* list, int index) {
	struct ListNode* node = list->head;
	while (index > 0) {
		node = node->next;
		index--;
	}
	return node->data;
}

struct ListNode* access(struct LinkedList* list, int index) {
	struct ListNode* temp = list->head;
	while (index > 0) {
		temp = temp->next;
		index--;
	}
	return temp;
}

int size(struct LinkedList *list) {
	return (list->size);
}

void show_int(struct LinkedList* list)
{	
	void* head = get(list, 0);
	for (int i = 0; i < list->size; i++) {
		printf("%s", "| " );
		printf("%s", "[ ");
		printf("%d", *(int*)get(list, i));
		printf("%s", " ]");
		printf("%s", " |" );

		if (access(list, i) -> next != NULL) {
			printf("%s", " -> ");
		}
	}
	printf("\n");
}

void show_string(struct LinkedList* list)
{	
	void* head = get(list, 0);
	for (int i = 0; i < list->size; i++) {
		printf("%s", "| " );
		printf("%s", "[ ");
		printf("%c", *(char*)get(list, i));
		printf("%s", " ]");
		printf("%s", " |" );

		if (access(list, i) -> next != NULL) {
			printf("%s", " -> ");
		}
	}
	printf("\n");
}

void show_float(struct LinkedList* list)
{	
	void* head = get(list, 0);
	for (int i = 0; i < list->size; i++) {
		printf("%s", "| " );
		printf("%s", "[ ");
		printf("%f", *(float*)get(list, i));
		printf("%s", " ]");
		printf("%s", " |" );

		if (access(list, i) -> next != NULL) {
			printf("%s", " -> ");
		}
	}
	printf("\n");
}

// Function to print an integer
void printInt(void *n)
{
   printf("%d", *(int *)n);
}

// Function to print a float
void printFloat(void *f)
{
   printf("%f", *(float *)f);
}

// Function to print a string
void printString(void *s)
{
   printf("%s", (char *)s);
}

// int main()
// {
//     struct LinkedList* list = initList();
 
//     add(list, 10);
//     add(list, 20);
//     add(list, 30);
 
//     printf("Front item is %d\n", get(list, 0));
//	   show(list);
//     return 0;
// }
