#ifndef GOBTREE_HPP
#define GOBTREE_HPP

#include <gobtree.h>

template <class T>
goBTreeElement<T>::goBTreeElement (const T& v)
    : goObjectBase (),
      leftChild (0),
      rightChild (0),
      parent (0),
      value (v)
{
    this->setClassName ("goBTreeElement");
}

template<class T>
goBTreeElement<T>::~goBTreeElement ()
{
}

//============================================


/** --------------------------------------------------------------------------
 * @brief Runs depth-first (left to right).
 * 
 * @param root Node at which to start.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
template<class T>
bool goBTreeAlgorithm<T>::run (typename goBTree<T>::Element* root)
{
    static bool ok = true;
    if (!root)
        return false;
    if (root->leftChild)
        ok = ok && this->run (root->leftChild);
    if (root->rightChild)
        ok = ok && this->run (root->rightChild);
    return ok && this->action(root);
}

//============================================

template<class T>
goBTree<T>::goBTree ()
    : goObjectBase (),
      myRoot (0)
{
}

template<class T>
goBTree<T>::goBTree (typename goBTree<T>::Element* root)
    : goObjectBase (),
      myRoot (root)
{
}

template<class T>
goBTree<T>::~goBTree ()
{
    class DeleteTree : public goBTreeAlgorithm<T>
    {
        public:
            virtual bool action (goBTree<T>::Element* node)
            {
                printf ("Deleting node with value %f\n", node->value);
                delete node;
                return true;
            };
    };

    DeleteTree del;
    del.run (myRoot);
}

template<class T>
bool goBTree<T>::isEmpty () const
{
    return (myRoot == 0);
}

template<class T>
typename goBTree<T>::Element* goBTree<T>::getRoot ()
{
    return myRoot;
}

template<class T>
typename goBTree<T>::ConstElement* goBTree<T>::getRoot () const
{
    return myRoot;
}

#endif
