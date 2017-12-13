#ifndef __BSTREE_H__
#define __BSTREE_H__

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
void showTree(struct BSTreeNode *tree);
struct BSTreeNode *createNode(void *data);
void addElementToTree(struct BSTree *tree, void *data);
void removeFromTree(struct BSTree *tree, void *data);
int treeContains(struct BSTreeNode *node, void *data);
void printTree(struct BSTreeNode *node);
void postOrder(struct BSTreeNode *node, int indent);
void postorder(struct BSTreeNode *node, int indent);

#endif