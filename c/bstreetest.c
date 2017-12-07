#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTree *tree = initBSTree();
    struct BSTreeNode *root = createNode(5);
    tree->root = root;
    addElementToTree(tree->root, 4);
    addElementToTree(tree->root, 2);
    addElementToTree(tree->root, 3);
    addElementToTree(tree->root, 1);
    //postorder(tree->root, 0);
    showTree(root);
    free(root->right);
    free(root->left);
    free(root);
    free(tree);
}