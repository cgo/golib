#include <gobtree.h>

int main ()
{
    goBTree<goFloat>::ElementPtr el;

    el = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (1.0f));
    goBTree<goFloat>* tree = new goBTree<goFloat> (el);
    el->leftChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.9));
    el->rightChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.8));
    el = el->leftChild;
    el->leftChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.7));
    el->rightChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.6));
    el = tree->getRoot()->rightChild;
    el->leftChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.5));
    el->rightChild = goBTree<goFloat>::ElementPtr (new goBTreeElement<goFloat> (0.4));
    class PrintTree : public goBTreeAlgorithm<goFloat>
    {
        public:
            virtual bool action (goBTree<goFloat>::ElementPtr node) 
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

    tree->erase ();

    delete tree;
    tree = 0;
    exit(1);
}
