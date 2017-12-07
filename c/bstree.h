#ifndef __BSTREE_H__
#define __BSTREE_H__

struct BSTreeNode
{
    int data;
    struct BSTreeNode *left;
    struct BSTreeNode *right;
    struct BSTreeNode *parent;
};

struct BSTree
{
    struct BSTreeNode *root;
};

struct BSTree *initBSTree();
void showTree(struct BSTreeNode *tree);
struct BSTreeNode *createNode(int data);
struct BSTreeNode *addElementToTree(struct BSTreeNode *node, int data);
struct BSTreeNode *removeFromTree(struct BSTreeNode *node, int data);
int treeContains(struct BSTreeNode *node, int data);
void printTree(struct BSTreeNode *node);
void postOrder(struct BSTreeNode *node, int indent);
void postorder(struct BSTreeNode *node, int indent);

#endif