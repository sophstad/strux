#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

struct BSTreeNode
{
    void *data;
    struct BSTreeNode *left;
    struct BSTreeNode *right;
    struct BSTreeNode *parent;
};

struct BSTree
{
    struct BSTreeNode *root;
};

struct BSTree *initBSTree()
{
    struct BSTree *tree = (struct BSTree*) malloc(sizeof(struct BSTree));
    tree->root = NULL;
    return tree;
}

/* Allocates memory for a new BSTreeNode and returns its pointer */
struct BSTreeNode *createNode(void *data)
{
    struct BSTreeNode *newNode = (struct BSTreeNode*) malloc(sizeof(struct BSTreeNode));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    newNode->parent = NULL;
    return newNode;
}

/* Adds the given data to the tree */
struct BSTreeNode *addIntToTreeHelper(struct BSTreeNode *node, void *data)
{
    /* Node is empty, place the new node here */
    if (node == NULL)
        return createNode(data);

    if (*(int *) data < *(int *) node->data) 
        node->left = addIntToTreeHelper(node->left, data);
    else if (*(int *) data > *(int *) node->data)
        node->right = addIntToTreeHelper(node->right, data);
    
    return node;
}

void addIntToTree(struct BSTree *tree, void *data)
{
    if (tree->root == NULL) {
        tree->root = createNode(data);
    }
    else {
        addIntToTreeHelper(tree->root, data);
    }
}

/* Adds the given data to the tree */
struct BSTreeNode *addNumToTreeHelper(struct BSTreeNode *node, void *data)
{
    /* Node is empty, place the new node here */
    if (node == NULL)
        return createNode(data);

    if (*(double *) data < *(double *) node->data) 
        node->left = addNumToTreeHelper(node->left, data);
    else if (*(double *) data > *(double *) node->data)
        node->right = addNumToTreeHelper(node->right, data);
    
    return node;
}

void addNumToTree(struct BSTree *tree, void *data)
{
    if (tree->root == NULL) {
        tree->root = createNode(data);
    }
    else {
        addNumToTreeHelper(tree->root, data);
    }
}

struct BSTreeNode *getMin(struct BSTreeNode *node)
{
    /* Invalid node given */
    if (node == NULL)
        return NULL;

    if (node->left)
        return getMin(node->left);
    else
        return node;
}

struct BSTreeNode *deleteIntFromTreeHelper(struct BSTreeNode *node, int data)
{
    struct BSTreeNode *temp;

    /* element not found */
    if (node == NULL)
        return NULL;

    if (data < *(int *) node->data)
        node->left = deleteIntFromTreeHelper(node->left, data);
    else if (data > *(int *) node->data)
        node->right = deleteIntFromTreeHelper(node->right, data);

    /* element found */
    else{

        /* two children present, handle accordingly */
        if (node->left && node->right) {

            temp = getMin(node->right);
            node->data = temp->data;
            node->right = deleteIntFromTreeHelper(node->right, *(int *) temp->data);
        }

        /* zero or one child present, handle accordingly */
        else {

            temp = node;

            if (node->left == NULL)
                node = node->right;
            else if (node->right == NULL)
                node = node->left;

            free(temp);
        }
    }

    return node;

}

void deleteIntFromTree(struct BSTree *tree, int data)
{
    deleteIntFromTreeHelper(tree->root, data);
}

struct BSTreeNode *deleteNumFromTreeHelper(struct BSTreeNode *node, double data)
{
    struct BSTreeNode *temp;

    /* element not found */
    if (node == NULL)
        return NULL;

    if (data < *(double *) node->data)
        node->left = deleteNumFromTreeHelper(node->left, data);
    else if (data > *(double *) node->data)
        node->right = deleteNumFromTreeHelper(node->right, data);

    /* element found */
    else{

        /* two children present, handle accordingly */
        if (node->left && node->right) {

            temp = getMin(node->right);
            node->data = temp->data;
            node->right = deleteNumFromTreeHelper(node->right, *(int *) temp->data);
        }

        /* zero or one child present, handle accordingly */
        else {

            temp = node;

            if (node->left == NULL)
                node = node->right;
            else if (node->right == NULL)
                node = node->left;

            free(temp);
        }
    }

    return node;

}

void deleteNumFromTree(struct BSTree *tree, double data)
{
    deleteNumFromTreeHelper(tree->root, data);
}



int treeContains(struct BSTreeNode *node, void *data)
{
    /* Not found */
    if (node == NULL)
        return 0;

    /* Keep searching */
    if (data < node->data)
        return treeContains(node->left, data);
    else if (data > node->data)
        return treeContains(node->right, data);

    /* Element found */
    else
        return 1;
}

int _print_t(struct BSTreeNode *tree, int is_left, int offset, int depth, char s[20][255], int typ)
{
    char b[20];
    int width = 6;

    if (!tree) return 0;

    if (typ == FLOATING)
        sprintf(b, "(%.2lf)", *(double *)tree->data);
    else
        sprintf(b, "(%04d)", *(int *)tree->data);

    int left  = _print_t(tree->left,  1, offset,                depth + 1, s, typ);
    int right = _print_t(tree->right, 0, offset + left + width, depth + 1, s, typ);

    for (int i = 0; i < width; i++)
        s[2 * depth][offset + left + i] = b[i];

    if (depth && is_left) {

        for (int i = 0; i < width + right; i++)
            s[2 * depth - 1][offset + left + width/2 + i] = '-';

        s[2 * depth - 1][offset + left + width/2] = '*';
        s[2 * depth - 1][offset + left + width + right + width/2] = '*';

    } else if (depth && !is_left) {

        for (int i = 0; i < left + width; i++)
            s[2 * depth - 1][offset - width/2 + i] = '-';

        s[2 * depth - 1][offset + left + width/2] = '*';
        s[2 * depth - 1][offset - width/2 - 1] = '*';
    }

    return left + width + right;
}

void showTree(struct BSTreeNode *tree, int typ)
{
    char s[20][255];
    for (int i = 0; i < 20; i++)
        sprintf(s[i], "%80s", " ");

    _print_t(tree, 0, 0, 0, s, typ);

    for (int i = 0; i < 20; i++)
        printf("%s\n", s[i]);
}

void showIntTree(struct BSTree *tree)
{
    showTree(tree -> root, INTEGER);
}

void showNumTree(struct BSTree *tree)
{
    showTree(tree -> root, FLOATING);
}