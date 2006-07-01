#include <gobtree.h>

int main ()
{
    goBTreeElement<goFloat>* el = 0;

    el = new goBTreeElement<goFloat> (1.0f);
    goBTree<goFloat>* tree = new goBTree<goFloat> (el);
    el->leftChild = new goBTreeElement<goFloat> (0.9);
    el->rightChild = new goBTreeElement<goFloat> (0.8);
    el = el->leftChild;
    el->leftChild = new goBTreeElement<goFloat> (0.7);
    el->rightChild = new goBTreeElement<goFloat> (0.6);
    el = tree->getRoot()->rightChild;
    el->leftChild = new goBTreeElement<goFloat> (0.5);
    el->rightChild = new goBTreeElement<goFloat> (0.4);
    class PrintTree : public goBTreeAlgorithm<goFloat>
    {
        public:
            virtual bool action (goBTree<goFloat>::Element* node) 
            {
                printf ("Value: %f\n", node->value);
                return true;
            };
    };
    PrintTree pt;
    printf ("depth first:\n");
    pt.depthFirst(tree->getRoot());
    printf ("breadth first:\n");
    pt.breadthFirst(tree->getRoot());
    delete tree;
    tree = 0;
    exit(1);
}
