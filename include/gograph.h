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

#if 0
template <class T> class goGraphNode;

template <class T>
class goGraphEdge
{
    public:
        goGraphEdge (goGraphNode<T>* n1 = 0, goGraphNode<T>* n2 = 0) 
            : myNode1(n1), myNode2(n2) {};
        virtual ~goGraphEdge() {};

        inline void setNodes (goGraphNode<T>* n1, goGraphNode<T>* n2) 
        { 
            myNode1 = n1;
            myNode2 = n2;
        };
        
        inline goGraphNode<T>* getOtherNode (const goGraphNode<T>* askingNode)
        {
            if (askingNode == myNode1)
            {
                return myNode2;
            }
            else
            {
                assert (askingNode == myNode2);
                return myNode1;
            }
        };
        
    protected:
        goGraphNode<T>* myNode1;
        goGraphNode<T>* myNode2;
};

/** 
 * @brief TO BE IMPLEMENTED Node of a Graph.
 */
template<class T>
class goGraphNode : public goObjectBase
{
    public:
        goGraphNode ()
            : adj(), parent(0), children(), value(), status(NORMAL), pass(0)
            {
            };
        goGraphNode (const T&)
            : adj(), parent(0), children(), value(), status(NORMAL), pass(0)
            {
            };

        //= Using the standard copy operator.
        
        virtual ~goGraphNode () {};

        void reset ()
        {
            this->adj.erase();
            this->parent = 0;
            this->children.erase();
            this->pass = 0;
            this->status = NORMAL;
        };

		goList< EdgeType* >       adj;      //= Adjacency list
        EdgeType*                 parent;   //= Set only if an algorithm has set it. Else NULL.
        goList< EdgeType* >       children; //= Set only if an algorithm has set it. Else NULL.
        T                         value;    //= Node value

        goSize_t                  pass;     //= Counter for passes (how often was this node visited)

        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goGraphNode<T>::Status status;
};

template<class T> class goGraph;
#endif

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
template<class T, class NodeType, class EdgeType>
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
                el->elem->status = NodeType::NORMAL;
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
                //typename goList<EdgeType*>::Element* adjNodeEl = node->adj.getFrontElement();
                //while (adjNodeEl)
                goSize_t adjCount = node->adj.getSize();
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                //    NodeType* adjNode = adjNodeEl->elem->getOtherNode (node);
                    NodeType* adjNode = node->adj[i]->getOtherNode (node);
                    if (adjNode->status != NodeType::VISITED)
                    {
                        Q.append (adjNode);
                    }
                //    adjNodeEl = adjNodeEl->next;
                }
                ok = ok & this->action (node);
                node->status = NodeType::VISITED;
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
                el->elem->status = NodeType::NORMAL;
                el = el->next;
            }
            root->status = NodeType::VISITED;
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
                el->elem->status = NodeType::NORMAL;
                //if (&*el->elem != root)
                //    stack.append (&*el->elem);
                el = el->next;
            }
            stack.append (root);
            while (!stack.isEmpty())
            {
                typename goList<NodeType*>::Element* nodeEl = stack.getTailElement();
                if (nodeEl->elem->status != NodeType::VISITED)
                {
                    nodeEl->elem->status = NodeType::VISITED;
                    // typename goList<EdgeType*>::Element* el = nodeEl->elem->adj.getFrontElement();
                    //while (el)
                    goSize_t adjCount = nodeEl->elem->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        // NodeType* adjNode = el->elem->getOtherNode (nodeEl->elem);
                        NodeType* adjNode = nodeEl->elem->adj[i]->getOtherNode (nodeEl->elem);
                        if (adjNode->status == NodeType::NORMAL && adjNode != (NodeType*)nodeEl->elem)
                        {
                            stack.append (adjNode);
                        }
                    //    el = el->next;
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

        /** 
         * @brief Same as depthFirst(), plus fills parent fields in the nodes.
         * 
         * @param root     Root node (to start with).
         * @param nodeList Node list of the complete graph.
         *
         * @todo Find a better name for this.
         * 
         * @return True if successful, false otherwise.
         */
        bool depthFirstTree (NodeType* root, goList< goAutoPtr< NodeType > >& nodeList)
        {
            if (!root)
                return false;
            goList<NodeType*> stack;
            typename goList< goAutoPtr< NodeType > >::Element* el = nodeList.getFrontElement();
            while (el)
            {
                el->elem->status = NodeType::NORMAL;
                //if (&*el->elem != root)
                //    stack.append (&*el->elem);
                el = el->next;
            }
            stack.append (root);
            while (!stack.isEmpty())
            {
                typename goList<NodeType*>::Element* nodeEl = stack.getTailElement();
                if (nodeEl->elem->status != NodeType::VISITED)
                {
                    nodeEl->elem->status = NodeType::VISITED;
                    // nodeEl->elem->children.erase ();
                    //typename goList<EdgeType*>::Element* el = nodeEl->elem->adj.getFrontElement();
                    //while (el)
                    goSize_t adjCount = nodeEl->elem->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        //NodeType* adjNode = el->elem->getOtherNode (nodeEl->elem);
                        NodeType* adjNode = nodeEl->elem->adj[i]->getOtherNode (nodeEl->elem);
                        if (adjNode->status == NodeType::NORMAL && adjNode != nodeEl->elem)
                        {
                            stack.append (adjNode);
                            // adjNode->parent = el;
                            adjNode->parent = nodeEl->elem->adj[i]->getIndex (adjNode);
                            // nodeEl->elem->children.append (el->elem);
                        }
                    //    el = el->next;
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
        virtual bool action (NodeType* node) { return false; };
        virtual bool action (const NodeType* node) const { return false; };

    private:
        //= New version
        bool depthFirstVisit (NodeType* root)
        {
            static bool ok = true;
            if (!root)
                return false;
            //typename goList< EdgeType* >::Element* el = root->adj.getFrontElement();
            //while (el)
            goSize_t adjCount = root->adj.getSize();
            for (goSize_t i = 0; i < adjCount; ++i)
            {
                //NodeType* adjNode = el->elem->getOtherNode (root);
                NodeType* adjNode = root->adj[i]->getOtherNode (root);
                if (adjNode->status != NodeType::VISITED)
                {
                    adjNode->status = NodeType::VISITED;
                    ok = ok && this->depthFirstVisit (adjNode);
                }
                //el = el->next;
            }
            return ok && this->action (root);
        };
};


#if 0
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

#endif

/** @} */
#endif
