#include <gosumproduct.h>
#include <gograph.h>
#include <gofunctor.h>
// 1. Nodes: Factor, variable.
// (2. Array of all nodes, to access them by index)
// 3. Each node carries adjacencies as list or array of indices .. or pointers directly.
// 


template <class T, class Tfloat>
class goSumProductPrivate
{
    public:
        goSumProductPrivate () {};
        ~goSumProductPrivate () {};
};

template <class T, class Tfloat>
goSumProduct<T,Tfloat>::goSumProduct ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goSumProductPrivate<T,Tfloat>;
}

template <class T, class Tfloat>
goSumProduct<T,Tfloat>::~goSumProduct ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}


        //=
        //= TODO: * [ scheint zu funktionieren ] Hinweg / Rueckweg, 
        //        - sum-product -> max-sum, 
        //        testen, 
        //        dokumentieren,
        //=       * [ scheint ok ] message-Typ != T, message-Typ = float|double, T nur Variablen-Typ (idR integer) 
        //=
/** 
 * @brief Message passing in trees (sum-product)
 *
 * \li \c T      Type for the variables (currently recommended to be integer, since [0,myValueCount-1] is used as values).
 * \li \c Tfloat Floating point type for the "message" vectors. Determines the accuracy. Set to goFloat or goDouble.
 */
template <class T, class Tfloat>
class goMessagePassing : public goGraphAlgorithm< T, goFGNode<T,Tfloat>, goFGEdge<T, Tfloat > >
{
    public:
        typedef goList< goFGEdge<T,Tfloat>* > EdgeList;
        typedef goList< goFGNode<T,Tfloat> >  NodeList;

        enum Direction
        {
            FORWARD,
            BACKWARD
        };

        //=
        //= Currently, two algorithms are chosen by an enumerator.
        //= This could more OO be done by more classes, but for quickness' sake, 
        //= do it like this now.
        //=
        enum Algorithm
        {
            SUM_PRODUCT,
            MAX_SUM
        };

    public:
        goMessagePassing () 
            : goGraphAlgorithm< T, goFGNode<T,Tfloat>, goFGEdge<T, Tfloat > > (),
              myValueCount (3),
              myDirection (FORWARD),
              myAlgorithm (SUM_PRODUCT)
        {};
        virtual ~goMessagePassing () {};
        
        bool run (goFGNode<T,Tfloat>* root, goFactorGraph<T,Tfloat>& fg, Algorithm algo = SUM_PRODUCT)
        {
            myAlgorithm = algo;
            //= Test for the sum() code. Tests show it seems to work fine.
#if 0
            goVector<T> X (5);
            X.fill (T(0));
            X[1] = T(10);
            goFunctor1 <Tfloat, goMessagePassing<T,Tfloat>, const goVector<T>&> f (this, &goMessagePassing<T,Tfloat>::testf);
            goDouble s = this->sum (&f, X, 0, 2);
            printf ("Sum: %lf\n", s);
#endif
            //= Depth first search plus fill parent fields in the nodes so we know
            //= which is which.
            this->myDirection = FORWARD;
            bool ok = this->depthFirstTree (root, fg.myNodes);
            if (!ok)
            {
                goLog::warning ("goMessagePassing::run(): Forward pass failed.");
                return false;
            }
            this->myDirection = BACKWARD;
            ok = this->breadthFirst (root, fg.myNodes);

            return ok;
        };

        //=
        //= Dummy function for testing sum()
        //=
        Tfloat testf (const goVector<T>& X)
        {
            return 1.0;
        };

        /** 
         * @brief Create and "send" message from a variable node \c fgn along a given edge.
         * 
         * @param fgn           Variable node to send from.
         * @param parentEdgeEl  Edge to send along (the other connected node is a factor and "receives" the message).
         * 
         * @return True if successful, false otherwise.
         */
        inline bool variableSend (goFGNode<T,Tfloat>* fgn, typename EdgeList::Element* parentEdgeEl)
        {
            //= Send message \mu_{x->f} along parent edge
            goVector<Tfloat>& mu = parentEdgeEl->elem->getOutMsg (fgn);
            // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T,Tfloat>::FACTOR);
            if (mu.getSize() != this->myValueCount)
                mu.resize (this->myValueCount);
            typename EdgeList::Element* el = fgn->adj.getFrontElement();
            switch (myAlgorithm) //= This may be done differently, but this prevents copy-paste. I know
                                 //= that is may slow things down.
            {
                case SUM_PRODUCT:
                {
                    mu.fill (Tfloat(1));
                    while (el)
                    {
                        if (el->elem != parentEdgeEl->elem)
                            mu *= el->elem->getInMsg (fgn);  //= Element-wise multiply
                        el = el->next;
                    }
                }
                break;
                case MAX_SUM:
                {
                    mu.fill (Tfloat(0));
                    while (el)
                    {
                        if (el->elem != parentEdgeEl->elem)
                            mu += el->elem->getInMsg (fgn);  //= Element-wise multiply
                        el = el->next;
                    }
                }
            }

            return true;
        };
        /** 
         * @brief Create and "send" message from a factor node \c fgn along a given edge.
         * 
         * @param fgn           Factor node to send from.
         * @param parentEdgeEl  Edge to send along (the other connected node is a variable and "receives" the message).
         * 
         * @return True if successful, false otherwise.
         */
        inline bool factorSend (goFGNode<T,Tfloat>* fgn, typename EdgeList::Element* parentEdgeEl)
        {
            //= Make this a function of parentEdge -- the same will be needed on the way back.
            goVector<Tfloat>& mu = parentEdgeEl->elem->getOutMsg (fgn);
            // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T,Tfloat>::VARIABLE);
            if (mu.getSize() != this->myValueCount)
                mu.resize (this->myValueCount);

            //= Marginalise locally for this factor and multiply by all incoming 
            //= messages (from variables).

            //= First, marginalise.
            //= Problem: Which variable is which ... the factor needs
            //= to know. Use arrays of links, the order of which is important?
            //= -> Enumerate all edges in the order they appear in the adj list (using the goList index
            //=    member), and
            //=    use the number as index into the
            //=    vector X that goes into the factor function f(X)
            //=    Note: Currently, all variables take values [0,myValueCount-1].
            //=          When there are individual possible values, use those instead.
            if (fgn->adj.getSize() > 1)
            {
                typename EdgeList::Element* el = fgn->adj.getFrontElement();
                while (el)
                {
                    if (el->elem != parentEdgeEl->elem)   
                    {
                        //= Sum over all other variables connected to this 
                        //= factor except for the parent.
                        goSize_t x_index = parentEdgeEl->index;
                        printf ("parent x index == %d\n", x_index);
                        goVector<T> X (fgn->adj.getSize());
                        goFGNodeFactor<T,Tfloat>* factornode = dynamic_cast<goFGNodeFactor<T,Tfloat>*> (fgn);
                        assert (factornode);
                        goFunctorBase1< Tfloat, const goVector<T>& >* f = factornode->getFunctor();
                        for (goSize_t x = 0; x < this->myValueCount; ++x)
                        {
                            X.fill (T(0));
                            X[x_index] = T(x);
                            //= Sum over all other variables .. this is recursive, but as long
                            //= as the number of connections (variables) is not overly large, 
                            //= that should work.
                            mu[x] = this->sum (f, X, 0, x_index);
                        }
                    }
                    el = el->next;
                }
                //= Multiply with all incoming messages except the one from the parent variable node.
                //= TODO: Das ist wahrscheinlich falsch. (8.66) im Buch, Produkt ist Funktion von den
                //=       Variablen x_m --> Produkt in sum() einbauen.
                el = fgn->adj.getFrontElement();
                while (el)
                {
                    if (el->elem != parentEdgeEl->elem)   
                    {
                        mu *= el->elem->getInMsg (fgn);
                    }
                    el = el->next;
                }
            }
            else
            {
                //= This must be a leaf node. Send f(x).
                goVector<T> X(1);
                goFGNodeFactor<T,Tfloat>* f = dynamic_cast<goFGNodeFactor<T,Tfloat>*> (fgn);
                for (goSize_t i = 0; i < this->myValueCount; ++i)
                {
                    X[0] = T(i);
                    mu[i] = (*f)(X);
                }
            }

            printf ("Sending mu = ");
            for (goSize_t i = 0; i < mu.getSize(); ++i)
                printf ("%f ", (float)mu[i]);
            printf ("\n");

            return true;
        };

        
        /** 
         * @brief Recursive sum.
         *
         * Sums f over all variables except fixed_index. Currently just uses the indices as input functions
         * for the function f(). If other values should be used, change this function accordingly.
         * 
         * @param f Functor to sum over.
         * @param X Input vector for f.
         * @param i Current index into X to sum over (start the recursion with 0).
         * @param fixed_index Fixed index: do not iterate over this index into X (remains fixed).
         * 
         * @return The sum of f over all variables except fixed_index.
         */
        inline goDouble sum (goFunctorBase1 <Tfloat, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index)
        {
            if (i >= X.getSize())
            {
                //printf ("X: ");
                //for (goSize_t k = 0; k < X.getSize(); ++k)
               // {
               //     printf ("%f ", X[k]);
               // }
               // printf ("\n");
                return (*f)(X);
            }
            if (i == fixed_index)
            {
                return sum (f, X, i+1, fixed_index);
            }
            goDouble s = 0.0;
            for (goSize_t j = 0; j < this->myValueCount; ++j)
            {
                X[i] = T(j);
                s += sum (f, X, i+1, fixed_index);
            }
            return s;
        };

        inline goDouble sumproduct (goFGNodeFactor<T,Tfloat>* factorNode,
                goFunctorBase1 <Tfloat, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index)
        {
            if (i >= X.getSize())
            {
                //printf ("X: ");
                //for (goSize_t k = 0; k < X.getSize(); ++k)
               // {
               //     printf ("%f ", X[k]);
               // }
               // printf ("\n");
               Tfloat prodIncoming = Tfloat(1);
               //= TODO: Jetzt waere ein array mit allen eingehenden Kanten praktisch. Indexierung die gleiche wie in X.
               //= \prod mu_{x_m->f_s}(x_m) ausrechnen
               return (*f)(X);
            }
            if (i == fixed_index)
            {
                return sum (f, X, i+1, fixed_index);
            }
            goDouble s = 0.0;
            for (goSize_t j = 0; j < this->myValueCount; ++j)
            {
                X[i] = T(j);
                s += sum (f, X, i+1, fixed_index);
            }
            return s;
        };

        inline bool forwardPass (goFGNode<T,Tfloat>* node)
        {
            //=
            //= This is pass 1 (from leaves to root).
            //=

            //=
            //= Send a message to the parent of node fgn.
            //=
            typename EdgeList::Element* parentEdge = node->parent;
            if (!parentEdge)
            {
                // goLog::warning ("goMessagePassing::action(): parent == 0");
                //= This means we are at the root.
                return true;
            }

            printf ("Would send from %d to %d\n", node->value, parentEdge->elem->getOtherNode(node)->value);
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    this->variableSend (node, parentEdge);
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    this->factorSend (node, parentEdge);
                    break;
                default:
                    goLog::error ("goMessagePassing::action(): Unknown node type.");
                    return false;
                    break;
            }

            return true;
        };

        inline bool backwardPass (goFGNode<T,Tfloat>* node)
        {
            //=
            //= This is pass 2 (from root back to leaves).
            //=
            typename EdgeList::Element* parentEdge = node->parent;
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    {
                        typename EdgeList::Element* el = node->adj.getFrontElement();
                        //= Send messages to all "children" (away from root)
                        while (el)
                        {
                            if (!parentEdge || el->elem != parentEdge->elem)
                            {
                                printf ("Backward pass: would send from %d to %d\n", node->value, el->elem->getOtherNode(node)->value);
                                this->variableSend (node, el);
                            }
                            el = el->next;
                        }
                    }
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    {
                        typename EdgeList::Element* el = node->adj.getFrontElement();
                        //= Send messages to all "children" (away from root)
                        while (el)
                        {
                            if (!parentEdge || el->elem != parentEdge->elem)
                            {
                                printf ("Backward pass: would send from %d to %d\n", node->value, el->elem->getOtherNode(node)->value);
                                this->factorSend (node, el);
                            }
                            el = el->next;
                        }
                    }
                    break;
                default:
                    goLog::error ("goMessagePassing::action(): Unknown node type.");
                    return false;
                    break;
            }

            return true;
        };

        virtual bool action (goFGNode<T,Tfloat>* node) 
        { 
            if (!node)
                return false;

            switch (this->myDirection)
            {
                case FORWARD:
                    {
                        return this->forwardPass (node);
                    }
                    break;
                case BACKWARD:
                    {
                        return this->backwardPass (node);
                    }
                    break;
                default:
                    goLog::error ("goMessagePassing::action(): Unknown value for direction.");
                    return false;
                    break;
            }

            return true;
        };

        void     setValueCount (goSize_t c) { myValueCount = c; };
        goSize_t getValueCount () const { return myValueCount; };
        
    private:
        //= Variables must all have the same number of possible values,
        //= so that the message vectors have same lengths. This is this length.
        goSize_t  myValueCount;
        Direction myDirection;
        Algorithm myAlgorithm;
};

template <class T, class Tfloat>
bool goSumProduct<T,Tfloat>::run (goFactorGraph<T,Tfloat>& fg)
{
    /*
     * Take any one variable node as root
     * Find the leaves
     * At the leaves, start the message passing
     */

    //= 
    //= Find first variable node
    //=
    typename goFactorGraph<T,Tfloat>::NodeList& nodeList = fg.myNodes;
    typename goFactorGraph<T,Tfloat>::NodeList::Element* el = nodeList.getFrontElement();
    while (el && el->elem->getType() != goFGNode<T,Tfloat>::VARIABLE)
    {
        el = el->next;
    }
    if (!el)
    {
        goLog::warning ("goSumProduct::run(): el == 0");
        return false;
    }

    //=
    //= Find leaves
    //=
#if 0
    class FindLeaves : public goGraphAlgorithm< T, goFGNode<T,Tfloat> >
    {
        public:
            FindLeaves () : goGraphAlgorithm< T, goFGNode<T,Tfloat> > () {};
            virtual ~FindLeaves () {};

            virtual bool action (goGraphNode<T>* node) 
            { 
                //= Hmm .. how expensive are dynamic casts? -- FIXME
                goFGNode<T,Tfloat>* fgn = dynamic_cast< goFGNode<T,Tfloat>* > (node);
                if (!fgn)
                    return false;
                printf ("%s node visited\n", (fgn->getType() == goFGNode<T,Tfloat>::VARIABLE) ? "Variable" : "Factor");
                if (fgn->getType() == goFGNode<T,Tfloat>::VARIABLE)
                {
                    printf ("\tValue: %f\n", fgn->value);
                }

                if (fgn->adj.getSize() == 1)
                    leaves.append (fgn);

                return true;
            };

            goList<goFGNode<T,Tfloat>*> leaves;
    };

    FindLeaves findLeaves;

    printf ("Breadth first:\n");
    findLeaves.breadthFirst ((goFGNode<T,Tfloat>*)el->elem, nodeList);

    findLeaves.leaves.erase ();
    printf ("Depth first:\n");
    findLeaves.depthFirst ((goFGNode<T,Tfloat>*)el->elem, nodeList);

    findLeaves.leaves.erase ();
    printf ("Depth first:\n");
    findLeaves.depthFirstRecursive ((goFGNode<T,Tfloat>*)el->elem, nodeList);
    {
        typename goList<goFGNode<T,Tfloat>*>::Element* el = findLeaves.leaves.getFrontElement();
        printf ("Leaves: ");
        while (el)
        {
            if (el->elem->getType() == goFGNode<T,Tfloat>::VARIABLE)
                printf ("%f ", el->elem->value);
            else
                printf ("f(x) ");
            el = el->next;
        }
        printf ("\n");
    }
#endif

    goMessagePassing<T,Tfloat> mp;
    mp.run ((goFGNode<T,Tfloat>*)el->elem, fg);

    return true;
}

template class goSumProduct<goSize_t,goFloat>;
