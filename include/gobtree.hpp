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
      leftChild (),
      rightChild (),
      parent (),
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
    //this->parent.reset ();
    //this->leftChild.reset ();
    //this->rightChild.reset ();
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
bool goBTreeAlgorithm<T>::depthFirst (typename goBTree<T>::ElementPtr root)
{
    static bool ok = true;
    if (root.isNull())
        return false;
    if (root->leftChild)
        ok = ok && this->depthFirst (root->leftChild);
    if (root->rightChild)
        ok = ok && this->depthFirst (root->rightChild);
    return ok && this->action(root);
}

#if 0
template<class T>
bool goBTreeAlgorithm<T>::depthFirst (typename goBTree<T>::ElementConstPtr root) const
{
    static bool ok = true;
    if (root.isNull())
        return false;
    if (root->leftChild)
        ok = ok && this->depthFirst (root->leftChild);
    if (root->rightChild)
        ok = ok && this->depthFirst (root->rightChild);
    return ok && this->action(root);
}
#endif

/* --------------------------------------------------------------------------
 * @brief Runs breadth-first (left to right).
 * 
 * @param root Node at which to start.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
template<class T>
bool goBTreeAlgorithm<T>::breadthFirst (typename goBTree<T>::ElementPtr root)
{
    bool ok = true;
    goList<typename goBTree<T>::ElementPtr> Q;
    Q.append (root);

    typename goList<typename goBTree<T>::ElementPtr>::Element* Qhead = Q.getFrontElement();

    typename goBTree<T>::ElementPtr node;
    while (!Q.isEmpty())
    {
        node = Qhead->elem;
        assert (!node.isNull());
        if (!node->leftChild.isNull())
        {
            Q.append (node->leftChild);
        }
        if (!node->rightChild.isNull())
        {
            Q.append (node->rightChild);
        }
        ok = ok & this->action (node);
        Qhead = Q.remove (Qhead);
    }
    return ok;
}

#if 0
template<class T>
bool goBTreeAlgorithm<T>::breadthFirst (typename goBTree<T>::ElementConstPtr root) const
{
    bool ok = true;
    goList<typename goBTree<T>::ElementConstPtr> Q;
    Q.append (root);

    typename goList<typename goBTree<T>::ElementConstPtr>::Element* Qhead = Q.getFrontElement();

    typename goBTree<T>::ElementConstPtr node;
    while (!Q.isEmpty())
    {
        node = Qhead->elem;
        assert (!node.isNull());
        if (!node->leftChild.isNull())
        {
            Q.append (node->leftChild);
        }
        if (!node->rightChild.isNull())
        {
            Q.append (node->rightChild);
        }
        ok = ok & this->action (node);
        Qhead = Q.remove (Qhead);
    }
    return ok;
}
#endif

//============================================

template<class T>
goBTree<T>::goBTree ()
    : goObjectBase (),
      myRoot (0)
{
    this->setClassID(GO_BTREE);
}

template<class T>
goBTree<T>::goBTree (typename goBTree<T>::ElementPtr root)
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
            virtual bool action (goAutoPtr<goBTree<T>::Element> node)
            {
                // printf ("Deleting node with value %f\n", node->value);
                // delete node;
                node->parent.reset ();
                node->leftChild.reset ();
                node->rightChild.reset ();
                return true;
            };
    };

    DeleteTree del;
    del.depthFirst (myRoot);
    myRoot.reset ();
}

template<class T>
bool goBTree<T>::isEmpty () const
{
    return (myRoot.isNull());
}

template <class T>
void goBTree<T>::setRoot (typename goBTree<T>::ElementPtr e)
{
    this->myRoot = e;
}

template<class T>
typename goBTree<T>::ElementPtr goBTree<T>::getRoot ()
{
    return myRoot;
}

#if 0
template<class T>
typename goBTree<T>::ElementConstPtr goBTree<T>::getRoot () const
{
    return myRoot;
}
#endif

template <class T>
bool goBTree<T>::writeDOT (FILE* f) //const
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

            virtual bool action (typename goBTree<T>::ElementPtr node) // const
            {
                goString pointer;
                pointer.resize(256);
                sprintf(pointer.getPtr(),"%p",&*node);
                goString nodeName = "node_";
                nodeName += pointer.toCharPtr();
                goString leftName = "";
                goString rightName = "";
                goString command = nodeName;
                command += ";\n";
                goFileIO::writeASCII(this->f, command);
                if (!node->leftChild.isNull())
                {
                    sprintf(pointer.getPtr(),"%p",&*node->leftChild);
                    leftName = "node_";
                    leftName += pointer.toCharPtr();
                    command = nodeName;
                    command += " -> "; command += leftName; command += ";\n";
                    goFileIO::writeASCII(this->f, command);
                }
                if (!node->rightChild.isNull())
                {
                    sprintf(pointer.getPtr(),"%p",&*node->rightChild);
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
