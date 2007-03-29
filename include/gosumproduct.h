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
#ifndef GOGRAPH_H
# include <gograph.h>
#endif

template <class T, class MessageType> class goFGEdge;

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
        typedef goFixedArray< goAutoPtr< goFGNodeVariable<T,Tfloat> > > VariableArray;
        typedef goFixedArray< goAutoPtr< goFGNodeFactor<T,Tfloat> > >   FactorArray;
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
            n1->adj[adjIndex1] = &*myEdges.getTail();
            n2->adj[adjIndex2] = &*myEdges.getTail();
            myEdges.getTail()->setIndex (adjIndex1, adjIndex2);
        };

        VariableArray myVariables;
        FactorArray   myFactors;
        EdgeList      myEdges;
};


//==========================================================

/** 
 * @brief The sum-product algorithm.
 * @verbatim
    Bishop, C.M., Pattern Recognition and Machine Learning, Springer, 2006, chapter 8
   @endverbatim
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goSumProduct : public goObjectBase
{
    public:
        goSumProduct ();
        virtual ~goSumProduct ();

        /** 
         * @brief Run the sum-product algorithm.
         * 
         * @param fg Factor graph to run the algorithm on.
         * @param valueCount The values of the variables are in the range [0,valueCount-1].
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);

        bool marginal (
                goFGNodeVariable<T,Tfloat>* variable, 
                goSize_t                    valueCount, 
                goVector<Tfloat>&           marginalRet);
};

/** 
 * @brief The max-sum algorithm.
 * @verbatim
    Bishop, C.M., Pattern Recognition and Machine Learning, Springer, 2006, chapter 8
   @endverbatim
 * @note The factors in the factor graph must return logarithms, no extra log() is called.
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goMaxSum : public goObjectBase
{
    public:
        goMaxSum ();
        virtual ~goMaxSum ();

        /** 
         * @brief Run the max-sum algorithm.
         * 
         * @param fg Factor graph to run the algorithm on.
         * @param valueCount The values of the variables are in the range [0,valueCount-1].
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);
};

#endif
