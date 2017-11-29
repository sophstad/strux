#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTreeNode *root = createNode(5);
    addElementToTree(root, 6);
    addElementToTree(root, 4);
    postorder(root, 0);
    free(root->right);
    free(root->left);
    free(root);
}