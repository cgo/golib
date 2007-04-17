#ifndef GOGRAPH_H
#define GOGRAPH_H

#include <goobjectbase.h>
#include <golist.h>
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif

/** @addtogroup data
 * @{
 */
/** 
 * @brief Graph node base class.
 *
 * Use derived classes with goGraphAlgorithm.
 */
template <class EdgeType>
class goGraphNode 
{
    public:
        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goGraphNode<EdgeType>::Status status;

		goFixedArray< EdgeType* >  adj;      //= Adjacency list
        goIndex_t parent;                    //= Index into adj of the parent. 
                                             //= Negative if not net, set only if 
                                             //= an algorithm has set it.

    public:
        virtual ~goGraphNode () {};
        
        virtual void reset ()
        {
            this->adj.setSize(0);
            this->parent = -1;
            this->status = NORMAL;
        };

    protected:
        goGraphNode (goSize_t edgeCount)
            : status(NORMAL), adj(edgeCount), parent(-1)
        {
            adj.fill (0);
        };
};

/** 
 * @brief Graph edge base class.
 *
 * Use derived classes with goGraphAlgorithm.
 */
template <class NodeType>
class goGraphEdge
{
    public:
        virtual ~goGraphEdge () {};

        inline void setNodes (NodeType* n1, NodeType* n2) 
        { 
            myNode1 = n1;
            myNode2 = n2;
        };
        
        inline NodeType* getOtherNode (const NodeType* askingNode)
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

        /** 
         * @brief Get index of this edge in adj array.
         * 
         * @param askingNode Node that is asking.
         * 
         * @return The index of this edge in the adj array of \c askingNode.
         */
        inline goIndex_t getIndex (const NodeType* askingNode)
        {
            if (askingNode == myNode1)
            {
                return myIndex1;
            }
            else
            {
                assert (askingNode == myNode2);
                return myIndex2;
            }
        };

        /** 
         * @brief Set index of this edge in \c adj elements of nodes.
         * 
         * @see getIndex()
         * 
         * @param i1 Index1
         * @param i2 Index2
         */
        inline void setIndex (goIndex_t i1, goIndex_t i2)
        {
            myIndex1 = i1;
            myIndex2 = i2;
        };

    protected:
        goGraphEdge (NodeType* n1 = 0, NodeType* n2 = 0)
            : myNode1(n1), myNode2(n2), myIndex1(-1), myIndex2(-1)
        { };


        NodeType*           myNode1;
        NodeType*           myNode2;
        goIndex_t           myIndex1;          //= Index of this edge in myNode1->adj
        goIndex_t           myIndex2;          //= Index of this edge in myNode2->adj
};

/** 
 * @brief Running through a Graph.
 *
 * \c NodeType should be derived from \c goGraphNode or provide the same functionality.
 * \c EdgeType should be derived from \c goGraphEdge or provide the same functionality.
 * 
 * @note Algorithms currently work for graphs with a single connected component
 * 
 * @todo Add support for multiple connected components (simply by supplying a set of all nodes).
 */
template<class NodeType, class EdgeType>
class goGraphAlgorithm
{
    public:
        goGraphAlgorithm () {};
        virtual ~goGraphAlgorithm () {};

        /** 
         * @brief BFS.
         * 
         * @note All nodes need to be reset to NodeType::NORMAL state before calling this.
         *
         * @param root Node to start at.
         * 
         * @return True if successful, false otherwise.
         */
        bool breadthFirst (NodeType* root) //, goFixedArray< goAutoPtr< NodeType > >& nodes)
        {
            if (!root)
                return false;

            //= Set all nodes to NORMAL (not visited)
            //typename goList< goAutoPtr< NodeType > >::Element* el = nodeList.getFrontElement();
            //while (el)
            //{
            //    el->elem->status = NodeType::NORMAL;
            //    el = el->next;
            //}

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
                    if (node->adj[i])
                    {
                        NodeType* adjNode = node->adj[i]->getOtherNode (node);
                        if (adjNode->status != NodeType::VISITED)
                        {
                            Q.append (adjNode);
                        }
                    }
                //    adjNodeEl = adjNodeEl->next;
                }
                ok = ok & this->action (node);
                node->status = NodeType::VISITED;
                Qhead = Q.remove (Qhead);
            }
            return ok;
        };

        bool breadthFirstTree (NodeType* root) //, goFixedArray< goAutoPtr< NodeType > >& nodes)
        {
            if (!root)
                return false;

            goList<NodeType*> Q;
            Q.append (root);

            typename goList<NodeType*>::Element* Qhead = Q.getFrontElement();

            bool ok = true;
            NodeType* node = 0;
            while (!Q.isEmpty())
            {
                node = Qhead->elem;
                assert (node);
                goSize_t adjCount = node->adj.getSize();
                //= Append all adjacent nodes to queue
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    if (node->adj[i])
                    {
                        NodeType* adjNode = node->adj[i]->getOtherNode (node);
                        if (adjNode->status != NodeType::VISITED)
                        {
                            Q.append (adjNode);
                            adjNode->parent = node->adj[i]->getIndex (adjNode);
                        }
                    }
                }
                ok = ok & this->action (node);
                node->status = NodeType::VISITED;
                Qhead = Q.remove (Qhead);
            }
            return ok;
        };

        //= New version
        bool depthFirstRecursive (NodeType* root) // , goFixedArray< goAutoPtr< NodeType > >& nodes)
        {
            if (!root)
                return false;

            root->status = NodeType::VISITED;
            return this->depthFirstVisit (root);
        };

        bool depthFirst (NodeType* root) // , goFixedArray< goAutoPtr< NodeType > >& nodes)
        {
            if (!root)
                return false;

            goList<NodeType*> stack;
            stack.append (root);
            while (!stack.isEmpty())
            {
                typename goList<NodeType*>::Element* nodeEl = stack.getTailElement();
                if (nodeEl->elem->status != NodeType::VISITED)
                {
                    nodeEl->elem->status = NodeType::VISITED;
                    goSize_t adjCount = nodeEl->elem->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        if (nodeEl->elem->adj[i])
                        {
                            NodeType* adjNode = nodeEl->elem->adj[i]->getOtherNode (nodeEl->elem);
                            if (adjNode->status == NodeType::NORMAL && adjNode != (NodeType*)nodeEl->elem)
                            {
                                stack.append (adjNode);
                            }
                        }
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
         * In order for all nodes to be visited, their status field must be set to NORMAL
         * prior to calling any of the BFS or DFS methods.
         *
         * @param root     Root node (to start with).
         * @param nodeList Node list of the complete graph.
         *
         * @todo Find a better name for this.
         * 
         * @return True if successful, false otherwise.
         */
        bool depthFirstTree (NodeType* root) // , goFixedArray< goAutoPtr< NodeType > >& nodes)
        {
            if (!root)
                return false;

            goList<NodeType*> stack;
            stack.append (root);
            while (!stack.isEmpty())
            {
                typename goList<NodeType*>::Element* nodeEl = stack.getTailElement();
                if (nodeEl->elem->status != NodeType::VISITED)
                {
                    nodeEl->elem->status = NodeType::VISITED;
                    goSize_t adjCount = nodeEl->elem->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        if (nodeEl->elem->adj[i])
                        {
                            NodeType* adjNode = nodeEl->elem->adj[i]->getOtherNode (nodeEl->elem);
                            if (adjNode->status == NodeType::NORMAL && adjNode != nodeEl->elem)
                            {
                                stack.append (adjNode);
                                adjNode->parent = nodeEl->elem->adj[i]->getIndex (adjNode);
                            }
                        }
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
                if (root->adj[i])
                {
                    NodeType* adjNode = root->adj[i]->getOtherNode (root);
                    if (adjNode->status != NodeType::VISITED)
                    {
                        adjNode->status = NodeType::VISITED;
                        ok = ok && this->depthFirstVisit (adjNode);
                    }
                }
                //el = el->next;
            }
            return ok && this->action (root);
        };
};

template <class NodeType, class EdgeType>
static bool goGraphWriteDOT (NodeType* root, FILE* f) //const
{
    if (!f)
    {
        return false;
    }

    class dotWriter : public goGraphAlgorithm<NodeType,EdgeType>
    {
        public:
            dotWriter (FILE* f_) : goGraphAlgorithm<NodeType,EdgeType> (), f(f_) {};
            virtual ~dotWriter() {};

            virtual bool action (NodeType* node) // const
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

                goSize_t adjCount = node->adj.getSize();
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    if (node->adj[i])
                    {
                        sprintf(pointer.getPtr(),"%p",node->adj[i]->getOtherNode(node));
                        goString adjName = "node_";
                        adjName += pointer.toCharPtr();
                        command = nodeName;
                        command += " -- "; command += adjName; command += ";\n";
                        goFileIO::writeASCII(this->f, command);
                    }
                }
                return true;
            };

            FILE* f;
    };

    goFileIO::writeASCII(f,goString("graph {\n"));
    dotWriter writer(f);
    bool ok = writer.depthFirst(root);
    goFileIO::writeASCII(f,goString("}\n"));
    return ok;
}

/** @} */

#endif
