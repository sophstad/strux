#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "BSTree.h"

/* Allocates memory for a new BSTreeNode and returns its pointer */
struct BSTreeNode *createNode(int data)
{
    struct BSTreeNode *newNode = (struct BSTreeNode*) malloc(sizeof(struct BSTreeNode));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    newNode->parent = NULL;
    return newNode;
}

/* Basic inorder printing of the tree */
void printTree(struct BSTreeNode *node)
{
    if (node == NULL) {
        return;
    }
    printTree(node->left);
    printf("%d\n", node->data);
    printTree(node->right);
}


void postOrder(struct BSTreeNode *node, int indent)
{
    char out[7] = "%4s  ";

    char number[3];
    sprintf(number, "%d", indent);

    if(node != NULL) {

        if(node->right) {
            postOrder(node->right, indent+4);
        }

        if (indent) {
            printf(out, "");
        }

        if (node->right) {
            printf("%4s  /\n ", "");
        }

        printf("%d\n ", node->data);

        if(node->left) {
            printf("%4s\n  \\\n", "");
            postOrder(node->left, indent+4);
        }
    }
}

void postorder(struct BSTreeNode *node, int indent)
{
    if(node != NULL) {

        char number[4];
        sprintf(number, "%d", indent);
        char out[20] = "%";
        strcat(out, number);
        strcat(out, "s");

        if(node->left)
            postorder(node->left, indent+4);
        if(node->right)
            postorder(node->right, indent+4);
        
        if (indent) {
            //std::cout << std::setw(indent) << ' ';
            printf(out, " ");
        }
        //cout<< p->data << "\n ";
        printf("%d\n ", node->data);
    }
}

/* Adds the given data to the tree */
struct BSTreeNode *addElementToTree(struct BSTreeNode *node, int data)
{
    /* Node is empty, place the new node here */
    if (node == NULL)
        return createNode(data);

    if (data < node->data) 
        node->left = addElementToTree(node->left, data);
    else if (data > node->data)
        node->right = addElementToTree(node->right, data);
    
    return node;
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

struct BSTreeNode *removeFromTree(struct BSTreeNode *node, int data)
{
    struct BSTreeNode *temp;

    /* element not found */
    if (node == NULL)
        return NULL;

    if (data < node->data)
        node->left = removeFromTree(node->left, data);
    else if (data > node->data)
        node->right = removeFromTree(node->right, data);

    /* element found */
    else{

        /* two children present, handle accordingly */
        if (node->left && node->right) {

            temp = getMin(node->right);
            node->data = temp->data;
            node->right = removeFromTree(node->right, temp->data);
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

int treeContains(struct BSTreeNode *node, int data)
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