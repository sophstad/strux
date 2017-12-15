#include <stdlib.h>
#include <stdio.h>
#include "BSTree.h"

int main()
{
    struct BSTree *tree = initBSTree();
    double a = 5.3;
    double b = 4.2421;
    double c = 2.53;
    double d = 3.43;
    double e = 1.642;
    addIntToTree(tree, (void *) &a);
    addIntToTree(tree, (void *) &b);
    addIntToTree(tree, (void *) &c);
    addIntToTree(tree, (void *) &d);
    addIntToTree(tree, (void *) &e);
    //printf("%d\n", *(int *) tree->root->data);
    //postorder(tree->root, 0);
    showIntTree(tree);
}