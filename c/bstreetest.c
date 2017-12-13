#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTree *tree = initBSTree();
    int a = 5;
    int b = 4;
    int c = 2;
    int d = 3;
    int e = 1;
    addElementToTree(tree, (void *) &a);
    addElementToTree(tree, (void *) &b);
    addElementToTree(tree, (void *) &c);
    addElementToTree(tree, (void *) &d);
    addElementToTree(tree, (void *) &e);
    printf("%d\n", *(int *) tree->root->data);
    //postorder(tree->root, 0);
    //showTree(tree->root);
}