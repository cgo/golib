#ifndef GOSUMPRODUCT_H
#define GOSUMPRODUCT_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOGRAPH_H
# include <gograph.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

template <class T, class MessageType> class goFGEdge;

template <class T, class Tfloat>
class goFGNode
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

        void reset ()
        {
            this->adj.setSize(0);
            this->parent = -1;
            this->pass = 0;
            this->status = NORMAL;
        };

		goFixedArray< goFGEdge<T, Tfloat >* >  adj;      //= Adjacency list
        goIndex_t parent; // Index into adj of the parent. Negative if not net, set only if an algorithm has set it.
        T         value;    //= Node value

        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goFGNode<T,Tfloat>::Status status;

        goSize_t                  pass;     //= Counter for passes (how often was this node visited)

    protected:
        void setType (Type t)
        {
            this->myType = t;
        };

        goFGNode (goSize_t edgeCount)
            : adj(edgeCount), parent(-1), value(T(0)), status(NORMAL), pass(0), 
              myType (VARIABLE)
        {
            adj.fill (0);
        };

    private:
        Type     myType;
};

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

template <class T, class Tfloat>
class goFGEdge
{
    public:
        typedef goVector<Tfloat> MessageType;
    
    public:
        goFGEdge (goFGNode<T,Tfloat>* n1 = 0, goFGNode<T,Tfloat>* n2 = 0) 
            : myNode1(n1), myNode2(n2), myIndex1(-1), myIndex2(-1), myMsg12(), myMsg21() {};
        virtual ~goFGEdge () {};

        inline void setNodes (goFGNode<T,Tfloat>* n1, goFGNode<T,Tfloat>* n2) 
        { 
            myNode1 = n1;
            myNode2 = n2;
        };
        
        inline goFGNode<T,Tfloat>* getOtherNode (const goFGNode<T,Tfloat>* askingNode)
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
        inline goIndex_t getIndex (const goFGNode<T,Tfloat>* askingNode)
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
        
        inline MessageType& getInMsg (const goFGNode<T,Tfloat>* askingNode)
        {
            if (askingNode == myNode1)
            {
                return myMsg21;
            }
            else
            {
                assert (askingNode == myNode2);
                return myMsg12;
            }
        };

        inline MessageType& getOutMsg (const goFGNode<T,Tfloat>* askingNode)
        {
            if (askingNode == myNode1)
            {
                return myMsg12;
            }
            else
            {
                assert (askingNode == myNode2);
                return myMsg21;
            }
        };

    private:
        goFGNode<T,Tfloat>* myNode1;
        goFGNode<T,Tfloat>* myNode2;
        goIndex_t           myIndex1;          //= Index of this edge in myNode1->adj
        goIndex_t           myIndex2;          //= Index of this edge in myNode2->adj
        MessageType         myMsg12;
        MessageType         myMsg21;
};

template <class T, class Tfloat>
class goFactorGraph
{
    public:
        typedef goFixedArray< goAutoPtr< goFGNodeVariable<T,Tfloat> > > VariableArray;
        typedef goFixedArray< goAutoPtr< goFGNodeFactor<T,Tfloat> > >   FactorArray;
        // typedef goList< goAutoPtr< goFGNode<T, Tfloat> > >   NodeList;
        typedef goList< goAutoPtr< goFGEdge<T, Tfloat > > >  EdgeList;

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
            //n1->adj.append (&*myEdges.getTail());
            //n1->adj.getTailElement()->index = n1->adj.getSize()-1;
            //n2->adj.append (&*myEdges.getTail());
            //n2->adj.getTailElement()->index = n2->adj.getSize()-1;
            n1->adj[adjIndex1] = &*myEdges.getTail();
            n2->adj[adjIndex2] = &*myEdges.getTail();
            myEdges.getTail()->setIndex (adjIndex1, adjIndex2);
        };

        //NodeList myNodes;
        //EdgeList myEdges;
        VariableArray myVariables;
        FactorArray   myFactors;
        EdgeList      myEdges;
};


//==========================================================

template<class T, class Tfloat> class goSumProductPrivate;

template <class T, class Tfloat>
class goSumProduct : public goObjectBase
{
    public:
        // typedef goList< goAutoPtr< goFGNode<T,Tfloat> > > NodeList;
    
    public:
        goSumProduct ();
        virtual ~goSumProduct ();

        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);

    private:
        goSumProductPrivate<T,Tfloat>* myPrivate;
};

template <class T, class Tfloat>
class goMaxSum : public goObjectBase
{
    public:
        // typedef goList< goAutoPtr< goFGNode<T,Tfloat> > > NodeList;
    
    public:
        goMaxSum ();
        virtual ~goMaxSum ();

        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);
};

#endif
