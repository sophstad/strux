#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTreeNode *root = createNode(5);
    addElementToTree(root, 2);
    addElementToTree(root, 4);
    addElementToTree(root, 1);
    printTree(root);
    free(root->left->left);
    free(root->left->right);
    free(root->left);
    free(root);
}