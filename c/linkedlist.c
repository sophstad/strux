#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

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

	int pointer = 0;
	struct ListNode* temp = list->head;
	while(pointer < index-1) {
		temp = temp->next;
		pointer++;
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

void printLlBorder(struct LinkedList *list, int typ) {
    int i;
    for (i = 0; i < list->size; i++) {
        int len = 0;
        char tmp[256];
        if (typ == INTEGER) {
            len = sprintf(tmp, " %d ", *(int *) get(list, i));
        } else if (typ == FLOATING) {
            len = sprintf(tmp, " %f ", *(double *) get(list, i));
        } else if (typ == STRING) {
            len = sprintf(tmp, " %s ", *(char **) get(list, i));
        }

        int j;
        printf("+");
        for (j = 0; j < len; j++) {
            printf("-");
        }
        printf("+  ");
    }
    printf("+------+\n");
}

void printIndexes(struct LinkedList *list, int typ) {
    int i;
    for (i = 0; i < list->size; i++) {
        int len = 0;
        char tmp[256];
        if (typ == INTEGER) {
            len = sprintf(tmp, "| %d |->", *(int *) get(list, i));
        } else if (typ == FLOATING) {
            len = sprintf(tmp, "| %f |->", *(double *) get(list, i));
        } else if (typ == STRING) {
            len = sprintf(tmp, "| %s |->", *(char **) get(list, i));
        }
        printf("%-*d", len, i);
    }
    printf("<- Index\n");
}

void ll_simple_show(struct LinkedList *list, int typ) {
    int i;
    for (i = 0; i < list->size; i++) {
        printf("[ ");
        if (typ == INTEGER) {
            printf("%d", *(int *) get(list, i));
        } else if (typ == FLOATING) {
            printf("%f", *(double *) get(list, i));
        } else if (typ == STRING) {
            printf("%s", *(char **) get(list, i));
        }
        printf(" ]");

        if (access(list, i) -> next != NULL) {
            printf("%s", " -> ");
        }
    }
    printf(" -> [ NULL ]\n");
}

void ll_show(struct LinkedList *list, int typ) {
    if (list->size == 0) {
        printf("LinkedList is empty!");
        return;
    }

    if (list->size > 10) {
        ll_simple_show(list, typ);
        return;
    }

    printLlBorder(list, typ);

    int i;
    for (i = 0; i < list->size; i++) {
        if (typ == INTEGER) {
            printf("| %d |->", *(int *) get(list, i));
        } else if (typ == FLOATING) {
            printf("| %f |->", *(double *) get(list, i));
        } else if (typ == STRING) {
            printf("| %s |->", *(char **) get(list, i));
        }
    }
    printf("| NULL |\n");
    printLlBorder(list, typ);
    printIndexes(list, typ);
}

void ll_show_int(struct LinkedList* list)
{
    ll_show(list, INTEGER);
}

void ll_show_string(struct LinkedList* list)
{
    ll_show(list, STRING);
}

void ll_show_float(struct LinkedList* list)
{
    ll_show(list, FLOATING);
}
