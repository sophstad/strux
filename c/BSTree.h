#ifndef __BSTREE_H__
#define __BSTREE_H__

struct BSTreeNode
{
    int data;
    struct BSTreeNode *left;
    struct BSTreeNode *right;
    struct BSTreeNode *parent;
};

struct BSTreeNode *createNode(int data);
struct BSTreeNode *addElementToTree(struct BSTreeNode *node, int data);
struct BSTreeNode *removeFromTree(struct BSTreeNode *node, int data);
int treeContains(struct BSTreeNode *node, int data);
void printTree(struct BSTreeNode *node);

#endif