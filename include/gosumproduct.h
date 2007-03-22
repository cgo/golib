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

template <class T, class MessageType> class goFGEdge;

template <class T>
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
            this->adj.erase();
            this->parent = 0;
            this->pass = 0;
            this->status = NORMAL;
        };

		goList< goFGEdge<T, goVector<T> >* >       adj;      //= Adjacency list
        typename goList< goFGEdge<T, goVector<T> >* >::Element*  parent;   //= Set only if an algorithm has set it. Else NULL.
        T                                          value;    //= Node value

        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goFGNode<T>::Status status;

        goSize_t                  pass;     //= Counter for passes (how often was this node visited)

    protected:
        void setType (Type t)
        {
            this->myType = t;
        };

        goFGNode ()
            : adj(), parent(0), value(T(0)), status(NORMAL), pass(0), 
              myType (VARIABLE)
        {
        };

    private:
        Type     myType;
};

template <class T>
class goFGNodeFactor : public goFGNode<T>
{
    public:
        goFGNodeFactor () 
            : goFGNode<T> (),
              functor (0)
        {
            this->setType (goFGNode<T>::FACTOR);
            this->dummyFunctor = 
                    goMemberFunction<goFGNodeFactor<T>,goDouble,const goVector<T>& > (this, &goFGNodeFactor<T>::dummyFactor);
            this->functor = &*dummyFunctor;
        };
        virtual ~goFGNodeFactor ()
        {
        };

        virtual goDouble operator () (const goVector<T>& X)
        {
            return (*this->functor) (X);
        };

        goDouble dummyFactor (const goVector<T>& X)
        {
            return 1.0;
        };

        goFunctorBase1< goDouble, const goVector<T>& >* getFunctor ()
        {
            return &*this->functor;
        };
        
    private:
        goFunctorBase1< goDouble, const goVector<T>& >*  functor;

        goAutoPtr <goFunctorBase1< goDouble, const goVector<T>&> > dummyFunctor;
};

template <class T>
class goFGNodeVariable : public goFGNode<T>
{
    public:
        goFGNodeVariable () : goFGNode<T> ()
        {
            this->setType (goFGNode<T>::VARIABLE);
        };
        
        virtual ~goFGNodeVariable ()
        {
        };
};

template <class T, class MessageType>
class goFGEdge
{
    public:
        goFGEdge (goFGNode<T>* n1 = 0, goFGNode<T>* n2 = 0) 
            : myNode1(n1), myNode2(n2), myMsg12(), myMsg21() {};
        virtual ~goFGEdge () {};

        inline void setNodes (goFGNode<T>* n1, goFGNode<T>* n2) 
        { 
            myNode1 = n1;
            myNode2 = n2;
        };
        
        inline goFGNode<T>* getOtherNode (const goFGNode<T>* askingNode)
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
        
        inline MessageType& getInMsg (const goFGNode<T>* askingNode)
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

        inline MessageType& getOutMsg (const goFGNode<T>* askingNode)
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
        goFGNode<T>* myNode1;
        goFGNode<T>* myNode2;
        MessageType  myMsg12;
        MessageType  myMsg21;
};

template <class T>
class goFactorGraph
{
    public:
        typedef goList< goAutoPtr< goFGNode<T> > >                NodeList;
        typedef goList< goAutoPtr< goFGEdge<T, goVector<T> > > >  EdgeList;

    public:
        goFactorGraph ()
            : myNodes(), myEdges() {};
        ~goFactorGraph () {};
        
        void connect (goFGNode<T>* n1, goFGNode<T>* n2)
        {
            //= Create a new edge
            myEdges.append (goAutoPtr< goFGEdge<T, goVector<T> > > (new goFGEdge<T, goVector<T> > (n1,n2)));
            //= Append edge to both nodes and index all edges at each node.
            n1->adj.append (&*myEdges.getTail());
            n1->adj.getTailElement()->index = n1->adj.getSize()-1;
            n2->adj.append (&*myEdges.getTail());
            n2->adj.getTailElement()->index = n2->adj.getSize()-1;
        };

        NodeList myNodes;
        EdgeList myEdges;
};


//==========================================================

template<class T> class goSumProductPrivate;

template <class T>
class goSumProduct : public goObjectBase
{
    public:
        // typedef goList< goAutoPtr< goFGNode<T> > > NodeList;
    
    public:
        goSumProduct ();
        virtual ~goSumProduct ();

        virtual bool run (goFactorGraph<T>& fg);

    private:
        goSumProductPrivate<T>* myPrivate;
};

#endif
