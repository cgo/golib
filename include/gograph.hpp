#ifndef GOGRAPH_HPP
#define GOGRAPH_HPP

#include <gograph.h>
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif
#ifndef GODEFS_H
# include <godefs.h>
#endif


//============================================


//= depth first appears to work (28.10.06)
/** --------------------------------------------------------------------------
 * @brief Runs depth-first (left to right).
 * 
 * @param root Node at which to start.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
#if 0
template<class T>
bool goGraphAlgorithm<T>::depthFirst (typename goGraph<T>::Node* root, goGraph<T>& graph)
{
    static bool ok = true;
    if (!root)
        return false;
    typename goList<goGraphNode<T>*>::Element* el = graph.getNodes().getFrontElement ();
    while (el)
    {
        el->elem->status = goGraphNode<T>::NORMAL;
        el = el->next;
    }
    root->status = goGraphNode<T>::VISITED;
    return this->depthFirstVisit (root);
}

template<class T>
bool goGraphAlgorithm<T>::depthFirstVisit (typename goGraph<T>::Node* root)
{
    static bool ok = true;
    if (!root)
        return false;
    typename goList<goGraphNode<T>*>::Element* el = root->adj.getFrontElement();
    while (el)
    {
        if (el->elem->status != goGraphNode<T>::VISITED)
        {
            el->elem->status = goGraphNode<T>::VISITED;
            ok = ok && this->depthFirstVisit (el->elem);
        }
        el = el->next;
    }
    return ok && this->action (root);
}

/* --------------------------------------------------------------------------
 * @brief TO BE IMPLEMENTED Runs breadth-first (left to right).
 * 
 * @param root Node at which to start.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
template<class T>
bool goGraphAlgorithm<T>::breadthFirst (typename goGraph<T>::Node* root, goGraph<T>& graph)
{
#if 0
    bool ok = true;
    goList<void*> Q;
    Q.append (root);

    goList<void*>::Node* Qhead = Q.getFrontNode();

    typename goGraph<T>::Node* node = 0;
    while (!Q.isEmpty())
    {
        node = static_cast<typename goGraph<T>::Node*>(Qhead->elem);
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
#endif
}
#endif

//============================================

template<class T>
goGraph<T>::goGraph ()
    : goObjectBase ()
{
    this->setClassID(GO_GRAPH);
}

template<class T>
goGraph<T>::~goGraph ()
{
    this->erase ();
}

template <class T>
void goGraph<T>::erase ()
{
    myNodes.erase ();
}

template<class T>
bool goGraph<T>::isEmpty () const
{
    return (myNodes.getSize() == 0);
}

template<class T>
goList<goGraphNode<T>*>& goGraph<T>::getNodes ()
{
    return myNodes;
}

//template<class T>
//const goList<goGraphNode<T>*> goGraph<T>::getNodes () const
//{
//    return myNodes;
//}

template <class T>
goGraphNode<T>* goGraph<T>::newNode ()
{
    goGraphNode<T>* node = new goGraphNode<T>;
    myNodes.append (node);
    return node;
}

/** 
 * @brief TO BE IMPLEMENTED
 * 
 * @param f 
 * 
 * @return 
 */
template <class T>
bool goGraph<T>::writeDOT (FILE* f) const
{
    if (!f)
    {
        return false;
    }

    class dotWriter : public goGraphAlgorithm<T>
    {
        public:
            dotWriter (FILE* f_) : goGraphAlgorithm<T> (), f(f_) {};
            virtual ~dotWriter() {};

            virtual bool action (typename goGraph<T>::ConstNode* node) const
            {
//                goString pointer;
//                pointer.resize(256);
//                sprintf(pointer.getPtr(),"%p",node);
//                goString nodeName = "node_";
//                nodeName += pointer.toCharPtr();
//                goString leftName = "";
//                goString rightName = "";
//                goString command = nodeName;
//                command += ";\n";
//                goFileIO::writeASCII(this->f, command);
//                if (node->leftChild)
//                {
//                    sprintf(pointer.getPtr(),"%p",node->leftChild);
//                    leftName = "node_";
//                    leftName += pointer.toCharPtr();
//                    command = nodeName;
//                    command += " -> "; command += leftName; command += ";\n";
//                    goFileIO::writeASCII(this->f, command);
//                }
//                if (node->rightChild)
//                {
//                    sprintf(pointer.getPtr(),"%p",node->rightChild);
//                    rightName = "node_";
//                    rightName += pointer.toCharPtr();
//                    command = nodeName;
//                    command += " -> "; command += rightName; command += ";\n";
//                    goFileIO::writeASCII(this->f, command);
//                }
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

#ifndef GOLIST_HPP
# include <golist.hpp>
#endif

#endif
