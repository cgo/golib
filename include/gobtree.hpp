#ifndef GOBTREE_HPP
#define GOBTREE_HPP

#include <gobtree.h>
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif
#ifndef GODEFS_H
# include <godefs.h>
#endif

template <class T>
goBTreeElement<T>::goBTreeElement ()
    : goObjectBase (),
      leftChild (0),
      rightChild (0),
      parent (0),
      value ()
{
    this->setClassID(GO_BTREEELEMENT);
}

template <class T>
goBTreeElement<T>::goBTreeElement (const T& v)
    : goObjectBase (),
      leftChild (0),
      rightChild (0),
      parent (0),
      value (v)
{
    this->setClassID(GO_BTREEELEMENT);
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
bool goBTreeAlgorithm<T>::depthFirst (typename goBTree<T>::Element* root)
{
    static bool ok = true;
    if (!root)
        return false;
    if (root->leftChild)
        ok = ok && this->depthFirst (root->leftChild);
    if (root->rightChild)
        ok = ok && this->depthFirst (root->rightChild);
    return ok && this->action(root);
}

template<class T>
bool goBTreeAlgorithm<T>::depthFirst (typename goBTree<T>::ConstElement* root) const
{
    static bool ok = true;
    if (!root)
        return false;
    if (root->leftChild)
        ok = ok && this->depthFirst (root->leftChild);
    if (root->rightChild)
        ok = ok && this->depthFirst (root->rightChild);
    return ok && this->action(root);
}

/* --------------------------------------------------------------------------
 * @brief Runs breadth-first (left to right).
 * 
 * @param root Node at which to start.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
template<class T>
bool goBTreeAlgorithm<T>::breadthFirst (typename goBTree<T>::Element* root)
{
    bool ok = true;
    goList<void*> Q;
    Q.append (root);

    goList<void*>::Element* Qhead = Q.getFrontElement();

    typename goBTree<T>::Element* node = 0;
    while (!Q.isEmpty())
    {
        node = static_cast<typename goBTree<T>::Element*>(Qhead->elem);
        assert (node);
        if (node->leftChild)
        {
            Q.append (node->leftChild);
        }
        if (node->rightChild)
        {
            Q.append (node->rightChild);
        }
        ok = ok & this->action (node);
        Qhead = Q.remove (Qhead);
    }
    return ok;
}

template<class T>
bool goBTreeAlgorithm<T>::breadthFirst (typename goBTree<T>::ConstElement* root) const
{
    bool ok = true;
    goList<const void*> Q;
    Q.append (root);

    goList<const void*>::Element* Qhead = Q.getFrontElement();

    typename goBTree<T>::ConstElement* node = 0;
    while (!Q.isEmpty())
    {
        node = static_cast<typename goBTree<T>::ConstElement*>(Qhead->elem);
        assert (node);
        if (node->leftChild)
        {
            Q.append (node->leftChild);
        }
        if (node->rightChild)
        {
            Q.append (node->rightChild);
        }
        ok = ok & this->action (node);
        Qhead = Q.remove (Qhead);
    }
    return ok;
}

//============================================

template<class T>
goBTree<T>::goBTree ()
    : goObjectBase (),
      myRoot (0)
{
    this->setClassID(GO_BTREE);
}

template<class T>
goBTree<T>::goBTree (typename goBTree<T>::Element* root)
    : goObjectBase (),
      myRoot (root)
{
    this->setClassID(GO_BTREE);
}

template<class T>
goBTree<T>::~goBTree ()
{
    this->erase ();
}

template <class T>
void goBTree<T>::erase ()
{
    class DeleteTree : public goBTreeAlgorithm<T>
    {
        public:
            virtual bool action (goBTree<T>::Element* node)
            {
                // printf ("Deleting node with value %f\n", node->value);
                delete node;
                return true;
            };
    };

    DeleteTree del;
    del.depthFirst (myRoot);
    this->setRoot (0);
}

template<class T>
bool goBTree<T>::isEmpty () const
{
    return (myRoot == 0);
}

template <class T>
void goBTree<T>::setRoot (typename goBTree<T>::Element* e)
{
    this->myRoot = e;
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

template <class T>
bool goBTree<T>::writeDOT (FILE* f) const
{
    if (!f)
    {
        return false;
    }

    class dotWriter : public goBTreeAlgorithm<T>
    {
        public:
            dotWriter (FILE* f_) : goBTreeAlgorithm<T> (), f(f_) {};
            virtual ~dotWriter() {};

            virtual bool action (typename goBTree<T>::ConstElement* node) const
            {
                goString pointer;
                pointer.resize(256);
                sprintf(pointer.getPtr(),"%p",node);
                goString nodeName = "node_";
                nodeName += pointer.toCharPtr();
                goString leftName = "";
                goString rightName = "";
                goString command = nodeName;
                command += ";\n";
                goFileIO::writeASCII(this->f, command);
                if (node->leftChild)
                {
                    sprintf(pointer.getPtr(),"%p",node->leftChild);
                    leftName = "node_";
                    leftName += pointer.toCharPtr();
                    command = nodeName;
                    command += " -> "; command += leftName; command += ";\n";
                    goFileIO::writeASCII(this->f, command);
                }
                if (node->rightChild)
                {
                    sprintf(pointer.getPtr(),"%p",node->rightChild);
                    rightName = "node_";
                    rightName += pointer.toCharPtr();
                    command = nodeName;
                    command += " -> "; command += rightName; command += ";\n";
                    goFileIO::writeASCII(this->f, command);
                }
                return true;
            };

            FILE* f;
    };

    goFileIO::writeASCII(f,goString("digraph {\n"));
    dotWriter writer(f);
    bool ok = writer.depthFirst(this->getRoot());
    goFileIO::writeASCII(f,goString("}\n"));
    return ok;
}

#endif
