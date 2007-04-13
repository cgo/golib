#ifndef GOFACTORGRAPH_H
#define GOFACTORGRAPH_H

#ifndef GOGRAPH_H
# include <gograph.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

template <class T, class MessageType> class goFGEdge;

/** 
 * \addtogroup gm
 * @{
 */

/** 
 * @brief Node base class for variable and factor nodes for goFactorGraph.
 */
template <class T, class Tfloat>
class goFGNode : public goGraphNode < goFGEdge<T,Tfloat> >
{
    public:
        virtual ~goFGNode () {};
        
        enum Type
        {
            VARIABLE,
            FACTOR
        };

        Type getType () const
        {
            return this->myType;
        };

        virtual void reset ()
        {
            goGraphNode< goFGEdge<T,Tfloat> >::reset ();
            this->pass = 0;
        };

        T         value;    //= Node value
        goSize_t  pass;     //= UNUSED Counter for passes (how often was this node visited)

    protected:
        void setType (Type t)
        {
            this->myType = t;
        };

        goFGNode (goSize_t edgeCount)
            : goGraphNode< goFGEdge<T,Tfloat> > (edgeCount), 
              value(T(0)), pass(0), myType (VARIABLE)
        { };

    private:
        Type     myType;
};

/** 
 * @brief Factor node for goFactorGraph.
 */
template <class T, class Tfloat>
class goFGNodeFactor : public goFGNode<T,Tfloat>
{
    public:
        goFGNodeFactor (goSize_t edgeCount) 
            : goFGNode<T,Tfloat> (edgeCount),
              functor (0),
              maxX (0,0)
        {
            this->setType (goFGNode<T,Tfloat>::FACTOR);
            this->dummyFunctor = 
                    goMemberFunction<goFGNodeFactor<T,Tfloat>,Tfloat,const goVector<T>& > (this, &goFGNodeFactor<T,Tfloat>::dummyFactor);
            this->functor = &*dummyFunctor;
        };
        virtual ~goFGNodeFactor ()
        {
        };

        inline Tfloat operator () (const goVector<T>& X)
        {
            return (*this->functor) (X);
        };

        inline Tfloat operator () () const
        {
            goSize_t sz = this->adj.getSize();
            goVector<T> X (sz);
            for (goSize_t i = 0; i < sz; ++i)
            {
                assert (this->adj[i]);
                X[i] = this->adj[i]->getOtherNode(this)->value;
            }
            return (*this->functor) (X);
        };

        Tfloat dummyFactor (const goVector<T>& X)
        {
            return 1.0;
        };

        inline goFunctorBase1< Tfloat, const goVector<T>& >* getFunctor ()
        {
            return &*this->functor;
        };

        inline void setFunctor (goFunctorBase1< Tfloat, const goVector<T>& >* f)
        {
            this->functor = f;
        };

        inline goMatrix<T>& getMaxX ()
        {
            return maxX;
        };
        
    private:
        goFunctorBase1< Tfloat, const goVector<T>& >*            functor;
        goAutoPtr <goFunctorBase1< Tfloat, const goVector<T>&> > dummyFunctor;
        //= For max-sum: keep the variable values that lead to the max message (f->x) in the forward pass.
        goMatrix<T> maxX; //= TODO: implement max-sum filling this in the forward step and using it to set the
                          //= variable values in the backtracking step.

};

/** 
 * @brief Variable class for goFactorGraph.
 */
template <class T, class Tfloat>
class goFGNodeVariable : public goFGNode<T,Tfloat>
{
    public:
        goFGNodeVariable (goSize_t edgeCount) : goFGNode<T,Tfloat> (edgeCount)
        {
            this->setType (goFGNode<T,Tfloat>::VARIABLE);
        };
        
        virtual ~goFGNodeVariable ()
        {
        };
};

/** 
 * @brief Edge class for goFactorGraph.
 */
template <class T, class Tfloat>
class goFGEdge : public goGraphEdge < goFGNode<T,Tfloat> >
{
    public:
        typedef goVector<Tfloat> MessageType;
    
    public:
        goFGEdge (goFGNode<T,Tfloat>* n1 = 0, goFGNode<T,Tfloat>* n2 = 0) 
            : goGraphEdge< goFGNode<T,Tfloat> > (n1,n2), myMsg12(), myMsg21() {};
        virtual ~goFGEdge () {};

        /** 
         * @brief Get message coming in to \c askingNode.
         * 
         * @param askingNode Must be one of the nodes this edge is connected to.
         * 
         * @return The incoming message for \c askingNode.
         */
        inline MessageType& getInMsg (const goFGNode<T,Tfloat>* askingNode)
        {
            if (askingNode == this->myNode1)
            {
                return myMsg21;
            }
            else
            {
                assert (askingNode == this->myNode2);
                return myMsg12;
            }
        };

        /** 
         * @brief Get message going out from \c askingNode.
         * 
         * @param askingNode Must be one of the nodes this edge is connected to.
         * 
         * @return The outgoing message from \c askingNode.
         */
        inline MessageType& getOutMsg (const goFGNode<T,Tfloat>* askingNode)
        {
            if (askingNode == this->myNode1)
            {
                return myMsg12;
            }
            else
            {
                assert (askingNode == this->myNode2);
                return myMsg21;
            }
        };

        //=
        //= Needed for initialising messages in the flooding scheme.
        //=
        inline MessageType& getMsg12 ()
        {
            return myMsg12;
        };
        inline MessageType& getMsg21 ()
        {
            return myMsg21;
        };

    private:
        MessageType myMsg12;
        MessageType myMsg21;
};

/** 
 * @brief Factor graph for use with goMaxSum and goSumProduct.
 *
 * Directly resize the arrays myVariables and myFactors, and connect them using the \c connect()
 * member function.
 */
template <class T, class Tfloat>
class goFactorGraph
{
    public:
        typedef goFGNode<T,Tfloat> NodeType;
        typedef goFGEdge<T,Tfloat> EdgeType;
        typedef goFixedArray< goAutoPtr< goFGNodeVariable<T,Tfloat> > > VariableArray;
        typedef goFixedArray< goAutoPtr< goFGNodeFactor<T,Tfloat> > >   FactorArray;
        typedef goList< goAutoPtr< goFGEdge<T, Tfloat > > >  EdgeList;

        
        /** 
         * @brief Indicates whether the used factors are exponential or logarithmic.
         *
         * If they are exponential, \c operator() uses multiplications to calculate the function value,
         * otherwise it uses sums.
         */
        enum FactorType
        {
            EXP,
            LOG
        };

    public:
        goFactorGraph ()
            : myVariables(), myFactors(), myEdges() {};
        ~goFactorGraph () {};
        
        /** 
         * @brief Connect \c n1, "slot" \c adjIndex1 to \c n2, "slot" \c adjIndex2.
         * 
         * @param n1            Node 1
         * @param adjIndex1     Index of the edge in node 1
         * @param n2            Node 2
         * @param adjIndex2     Index of the edge in node 2
         */
        void connect (goFGNode<T,Tfloat>* n1, goSize_t adjIndex1, goFGNode<T,Tfloat>* n2, goSize_t adjIndex2)
        {
            //= Create a new edge
            myEdges.append (goAutoPtr< goFGEdge<T, Tfloat > > (new goFGEdge<T, Tfloat > (n1,n2)));
            //= Append edge to both nodes and index all edges at each node.
            n1->adj[adjIndex1] = &*myEdges.getTail();
            n2->adj[adjIndex2] = &*myEdges.getTail();
            myEdges.getTail()->setIndex (adjIndex1, adjIndex2);
        };

        /** 
         * @brief Set all nodes to \c NodeType::NORMAL state.
         */
        void setNormal ()
        {
            goSize_t sz = myVariables.getSize();
            for (goSize_t i = 0; i < sz; ++i)
            {
                myVariables[i]->status = NodeType::NORMAL;
            }
            sz = myFactors.getSize();
            for (goSize_t i = 0; i < sz; ++i)
            {
                myFactors[i]->status = NodeType::NORMAL;
            }
        };

        goDouble operator () (const goVector<T>& X, FactorType ft = EXP) 
        {
            assert (X.getSize() == myVariables.getSize());
            goSize_t sz = X.getSize();
            for (goSize_t i = 0; i < sz; ++i)
            {
                myVariables[i]->value = X[i];
            }
            sz = myFactors.getSize();
            if (ft == EXP)
            {
                goDouble ret = 1.0;
                for (goSize_t i = 0; i < sz; ++i)
                {
                    ret *= (*myFactors[i])();
                }
                return ret;
            }
            else
            {
                goDouble ret = 0.0;
                for (goSize_t i = 0; i < sz; ++i)
                {
                    ret += (*myFactors[i])();
                }
                return ret;
            }
        };

        VariableArray myVariables;
        FactorArray   myFactors;
        EdgeList      myEdges;
};

template <class T, class Tfloat>
static bool goFGGraphWriteDOT (goFGNode<T,Tfloat>* root, FILE* f) //const
{
    typedef goFGNode<T,Tfloat> NodeType;
    typedef goFGEdge<T,Tfloat> EdgeType;

    if (!f)
    {
        return false;
    }

    class dotWriter : public goGraphAlgorithm<NodeType,EdgeType>
    {
        public:
            dotWriter (FILE* f_) : goGraphAlgorithm<NodeType,EdgeType> (), visitedEdges(), f(f_) {};
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
                switch (node->getType())
                {
                    case NodeType::VARIABLE:
                        command += " [label=Variable];\n";
                        break;
                    case NodeType::FACTOR:
                        {
                            goString s = " [label=Factor,shape=box,style=filled];\n";
                            command += s.toCharPtr();
                        }
                        break;
                    default:
                        command += " [label=UNKNOWN];\n";
                        break;
                }
                goFileIO::writeASCII(this->f, command);

                goSize_t adjCount = node->adj.getSize();
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    bool visited = false;
                    {
                        goList<void*>::Element* el = visitedEdges.getFrontElement();
                        while (el)
                        {
                            if ((void*)node->adj[i] == el->elem)
                            {
                                visited = true;
                                break;
                            }
                            el = el->next;
                        }
                    }
                    if (node->adj[i] && !visited)
                    {
                        visitedEdges.append ((void*)node->adj[i]);
                        sprintf(pointer.getPtr(),"%p",node->adj[i]->getOtherNode(node));
                        goString adjName = "node_";
                        adjName += pointer.toCharPtr();
                        command = nodeName;
                        command += " -- "; command += adjName; 
                        command += "\n";
                        //goSize_t inSize = node->adj[i]->getInMsg(node).getSize();
                        //goSize_t outSize = node->adj[i]->getOutMsg(node).getSize();
                        // command += " [label=\"msg_size:";
                        //command += (int)(inSize);
                        //command += ",";
                        //command += (int)(outSize);
                        //command += "\"];\n";
                        goFileIO::writeASCII(this->f, command);
                    }
                }
                return true;
            };

            goList<void*> visitedEdges;

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
