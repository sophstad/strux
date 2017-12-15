#ifndef __BSTREE_H__
#define __BSTREE_H__

int DATA_WIDTH = 6;

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

struct BSTree *initBSTree();
struct BSTreeNode *createNode(void *data);
struct BSTreeNode *addIntToTreeHelper(struct BSTreeNode *node, void *data);
void addIntToTree(struct BSTree *tree, void *data);
struct BSTreeNode *addNumToTreeHelper(struct BSTreeNode *node, void *data);
void addNumToTree(struct BSTree *tree, void *data);
struct BSTreeNode *getMin(struct BSTreeNode *node);
struct BSTreeNode *deleteIntFromTreeHelper(struct BSTreeNode *node, int data);
void deleteIntFromTree(struct BSTree *tree, int data);
struct BSTreeNode *deleteNumFromTreeHelper(struct BSTreeNode *node, double data);
void deleteNumFromTree(struct BSTree *tree, double data);
int treeContainsInt(struct BSTree *tree, int data);
int treeContainsNum(struct BSTree *tree, double data);
int printLeftChild(struct BSTreeNode *tree, int offset, int depth, char s[20][255], int typ);
int printRightChild(struct BSTreeNode *tree, int offset, int depth, char s[20][255], int typ);
void showTree(struct BSTreeNode *tree, int typ);
void showIntTree(struct BSTree *tree);
void showNumTree(struct BSTree *tree);

#endif