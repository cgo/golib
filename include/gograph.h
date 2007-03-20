#ifndef GOGRAPH_H
#define GOGRAPH_H

#include <goobjectbase.h>
#include <golist.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

/** @addtogroup data
 * @{
 */
/** 
 * @brief TO BE IMPLEMENTED Node of a Graph.
 */
template<class T>
class goGraphNode : public goObjectBase
{
    public:
        goGraphNode ()
            : adj(), value(), status(NORMAL), pass(0)
            {
            };
        goGraphNode (const T&)
            : adj(), value(), status(NORMAL), pass(0)
            {
            };

        //= Using the standard copy operator.
        
        virtual ~goGraphNode () {};

		goList< goGraphNode<T>* > adj;   //= Adjacency list
        T                         value; //= Node value
        goSize_t                  pass;  //= Counter for passes (how often was this node visited)

        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goGraphNode<T>::Status status;
};

template<class T> class goGraph;

/** 
 * @brief Running through a binary Graph.
 *
 * Currently only offers depth first order.
 *
 * NodeType must be derived from goGraphNode
 * 
 * @note Algorithms currently work for graphs with a single connected component
 * 
 * @todo Add support for multiple connected components
 */
template<class T, class NodeType>
class goGraphAlgorithm
{
    public:
        goGraphAlgorithm () {};
        virtual ~goGraphAlgorithm () {};

        bool breadthFirst (NodeType* root, goList< goAutoPtr< NodeType > >& nodeList)
        {
            if (!root)
                return false;

            //= Set all nodes to NORMAL (not visited)
            typename goList< goAutoPtr< NodeType > >::Element* el = nodeList.getFrontElement();
            while (el)
            {
                el->elem->status = goGraphNode<T>::NORMAL;
                el = el->next;
            }

            goList<NodeType*> Q;
            Q.append (root);

            typename goList<NodeType*>::Element* Qhead = Q.getFrontElement();

            bool ok = true;
            NodeType* node = 0;
            while (!Q.isEmpty())
            {
                node = Qhead->elem;
                assert (node);
                typename goList<goGraphNode<T>*>::Element* adjNodeEl = node->adj.getFrontElement();
                while (adjNodeEl)
                {
                    if (adjNodeEl->elem->status != goGraphNode<T>::VISITED)
                    {
                        Q.append (dynamic_cast<NodeType*>(adjNodeEl->elem));
                    }
                    adjNodeEl = adjNodeEl->next;
                }
                ok = ok & this->action (node);
                node->status = goGraphNode<T>::VISITED;
                Qhead = Q.remove (Qhead);
            }
            return ok;
        };

        // bool breadthFirst (typename goGraph<T>::Node* root, goGraph<T>& graph) { return false; };
        // bool breadthFirst (typename goGraph<T>::ConstNode* root) const;
        
        //= New version
        bool depthFirstRecursive (NodeType* root, goList< goAutoPtr< NodeType > >& nodeList)
        {
            // static bool ok = true;
            if (!root)
                return false;
            typename goList< goAutoPtr< NodeType > >::Element* el = nodeList.getFrontElement();
            while (el)
            {
                el->elem->status = goGraphNode<T>::NORMAL;
                el = el->next;
            }
            root->status = goGraphNode<T>::VISITED;
            return this->depthFirstVisit (root);
        };

        bool depthFirst (NodeType* root, goList< goAutoPtr< NodeType > >& nodeList)
        {
            if (!root)
                return false;
            goList<NodeType*> stack;
            typename goList< goAutoPtr< NodeType > >::Element* el = nodeList.getFrontElement();
            while (el)
            {
                el->elem->status = goGraphNode<T>::NORMAL;
                //if (&*el->elem != root)
                //    stack.append (&*el->elem);
                el = el->next;
            }
            stack.append (root);
            while (!stack.isEmpty())
            {
                typename goList<NodeType*>::Element* nodeEl = stack.getTailElement();
                if (nodeEl->elem->status != goGraphNode<T>::VISITED)
                {
                    nodeEl->elem->status = goGraphNode<T>::VISITED;
                    typename goList<goGraphNode<T>*>::Element* el = nodeEl->elem->adj.getFrontElement();
                    while (el)
                    {
                        if (el->elem->status == goGraphNode<T>::NORMAL && el->elem != (goGraphNode<T>*)nodeEl->elem)
                        {
                            stack.append (dynamic_cast<NodeType*>(el->elem));
                        }
                        el = el->next;
                    }
                }
                else
                {
                    this->action (nodeEl->elem);
                    stack.remove (nodeEl);
                }
            }
            return true;
        };

        // bool depthFirst (typename goGraph<T>::ConstNode* root) const;
        virtual bool action (goGraphNode<T>* node) { return false; };
        virtual bool action (const goGraphNode<T>* node) const { return false; };

    private:
        //= New version
        bool depthFirstVisit (goGraphNode<T>* root)
        {
            static bool ok = true;
            if (!root)
                return false;
            typename goList< goGraphNode<T>* >::Element* el = root->adj.getFrontElement();
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
        };
};


/** 
 * @brief Graph.
 */
template<class T>
class goGraph : public goObjectBase
{
    public:
        typedef goGraphNode<T> Node;
        typedef const goGraphNode<T> ConstNode;
        typedef goList<goGraphNode<T>*> NodeList;

    public:
        goGraph ();
        virtual ~goGraph ();

        goGraphNode<T>*          newNode ();
        goList<goGraphNode<T>*>& getNodes ();
        
        bool          isEmpty () const;
        void          erase   ();
        bool          writeDOT (FILE* f) const;

    private:
        goList<goGraphNode<T>*> myNodes;
};
/** @} */
#endif
