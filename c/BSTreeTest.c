#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTreeNode *root = createNode(5);
    addElementToTree(root, 3);
    addElementToTree(root, 4);
    printTree(root);
    free(root->right);
    free(root->left);
    free(root);
}