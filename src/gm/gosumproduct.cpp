#include <gosumproduct.h>
#include <gograph.h>
#include <gofunctor.h>
// 1. Nodes: Factor, variable.
// (2. Array of all nodes, to access them by index)
// 3. Each node carries adjacencies as list or array of indices .. or pointers directly.
// 


template <class T>
class goSumProductPrivate
{
    public:
        goSumProductPrivate () {};
        ~goSumProductPrivate () {};
};

template <class T>
goSumProduct<T>::goSumProduct ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goSumProductPrivate<T>;
}

template <class T>
goSumProduct<T>::~goSumProduct ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class T>
class goMessagePassing : public goGraphAlgorithm< T, goFGNode<T>, goFGEdge<T, goVector<T> > >
{
    public:
        typedef goList< goFGEdge<T, goVector<T> >* > EdgeList;
        typedef goList< goFGNode<T> >                NodeList;

    public:
        goMessagePassing () 
            : goGraphAlgorithm< T, goFGNode<T>, goFGEdge<T, goVector<T> > > (),
              myValueCount (3)
        {};
        virtual ~goMessagePassing () {};

        bool run (goFGNode<T>* root, goFactorGraph<T>& fg)
        {
            //= Test for the sum() code. Tests show it seems to work fine.
#if 0
            goVector<T> X (5);
            X.fill (T(0));
            X[1] = 10.0;
            goFunctor1 <goDouble, goMessagePassing<T>, const goVector<T>&> f (this, &goMessagePassing<T>::testf);
            goDouble s = this->sum (&f, X, 0, 2);
            printf ("Sum: %lf\n", s);
#endif
            //= Depth first search plus fill parent/children fields in the nodes so we know
            //= which is which.
            return this->depthFirstTree (root, fg.myNodes);
        };

        //=
        //= Dummy function for testing sum()
        //=
        goDouble testf (const goVector<T>& X)
        {
            return 1.0;
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
        goDouble sum (goFunctorBase1 <goDouble, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index)
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

        virtual bool action (goFGNode<T>* node) 
        { 
            //= Hmm .. how expensive are dynamic casts? -- FIXME
            goFGNode<T>* fgn = node;

            if (!fgn)
                return false;

            //=
            //= This is pass 1 (from leaves to root).
            //=

            //=
            //= Send a message to the parent of node fgn.
            //=
            typename EdgeList::Element* parentEdge = fgn->parent;
            if (!parentEdge)
            {
                // goLog::warning ("goMessagePassing::action(): parent == 0");
                //= This means we are at the root.
                return true;
            }
            
            printf ("Would send from %f to %f\n", fgn->value, parentEdge->elem->getOtherNode(fgn)->value);
            switch (fgn->getType())
            {
                case goFGNode<T>::VARIABLE:
                    {
                        //= Send message \mu_{x->f} along parent edge
                        goVector<T>& mu = parentEdge->elem->getOutMsg (fgn);
                        // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T>::FACTOR);
                        if (mu.getSize() != this->myValueCount)
                            mu.resize (this->myValueCount);
                        mu.fill (T(1));
                        typename goList< goFGEdge<T, goVector<T> >* >::Element* el = fgn->adj.getFrontElement();
                        while (el)
                        {
                            if (el->elem != fgn->parent->elem)
                                mu *= el->elem->getInMsg (fgn);  //= Element-wise multiply
                            el = el->next;
                        }
                    }
                    break;
                case goFGNode<T>::FACTOR:
                    {
                        //= Make this a function of parentEdge -- the same will be needed on the way back.
                        goVector<T>& mu = parentEdge->elem->getOutMsg (fgn);
                        // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T>::VARIABLE);
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
                        
                        //goVector<T> margin (mu.getSize());
                        //margin.fill (T(0));
                        if (fgn->adj.getSize() > 1)
                        {
                            typename goList< goFGEdge<T, goVector<T> >* >::Element* el = fgn->adj.getFrontElement();
                            while (el)
                            {
                                if (el->elem != fgn->parent->elem)   
                                {
                                    mu *= el->elem->getInMsg (fgn);  //= Element-wise multiply

                                    //= CONTINUE HERE
                                    //= Loop over parentEdge.getOtherNode() and sum over all other variables connected to this 
                                    //= factor.
                                    goSize_t x_index = parentEdge->index;
                                    printf ("parent x index == %d\n", x_index);
                                    goVector<T> X (fgn->adj.getSize());
                                    goSize_t x = 0;
                                    for (x = 0; x < this->myValueCount; ++x)
                                    {
                                        X.fill (T(0));
                                        X[x_index] = T(x);
                                        //= Sum over all other variables .. this is tricky non-recursively. (?)
                                        // mu[x] = this->sum (fgn->
                                    }
                                }
                                el = el->next;
                            }
                        }
                        else
                        {
                            //= This must be a leaf node. Send f(x).
                            goVector<T> X(1);
                            goFGNodeFactor<T>* f = dynamic_cast<goFGNodeFactor<T>*> (fgn);
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
                    }
                    break;
                default:
                    goLog::error ("goMessagePassing::action(): Unknown node type.");
                    return false;
                    break;
            }

            //printf ("%s node visited\n", (fgn->getType() == goFGNode<T>::VARIABLE) ? "Variable" : "Factor");
            //if (fgn->getType() == goFGNode<T>::VARIABLE)
            //{
            //    printf ("\tValue: %f\n", fgn->value);
            //}

            //if (fgn->adj.getSize() == 1)
            //    leaves.append (fgn);

            return true;
        };

        void     setValueCount (goSize_t c) { myValueCount = c; };
        goSize_t getValueCount () const { return myValueCount; };
        
    private:
        //= Variables must all have the same number of possible values,
        //= so that the message vectors have same lengths. This is this length.
        goSize_t myValueCount;
};

template <class T>
bool goSumProduct<T>::run (goFactorGraph<T>& fg)
{
    /*
     * Take any one variable node as root
     * Find the leaves
     * At the leaves, start the message passing
     */

    //= 
    //= Find first variable node
    //=
    typename goFactorGraph<T>::NodeList& nodeList = fg.myNodes;
    typename goFactorGraph<T>::NodeList::Element* el = nodeList.getFrontElement();
    while (el && el->elem->getType() != goFGNode<T>::VARIABLE)
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
    class FindLeaves : public goGraphAlgorithm< T, goFGNode<T> >
    {
        public:
            FindLeaves () : goGraphAlgorithm< T, goFGNode<T> > () {};
            virtual ~FindLeaves () {};

            virtual bool action (goGraphNode<T>* node) 
            { 
                //= Hmm .. how expensive are dynamic casts? -- FIXME
                goFGNode<T>* fgn = dynamic_cast< goFGNode<T>* > (node);
                if (!fgn)
                    return false;
                printf ("%s node visited\n", (fgn->getType() == goFGNode<T>::VARIABLE) ? "Variable" : "Factor");
                if (fgn->getType() == goFGNode<T>::VARIABLE)
                {
                    printf ("\tValue: %f\n", fgn->value);
                }

                if (fgn->adj.getSize() == 1)
                    leaves.append (fgn);

                return true;
            };

            goList<goFGNode<T>*> leaves;
    };

    FindLeaves findLeaves;

    printf ("Breadth first:\n");
    findLeaves.breadthFirst ((goFGNode<T>*)el->elem, nodeList);

    findLeaves.leaves.erase ();
    printf ("Depth first:\n");
    findLeaves.depthFirst ((goFGNode<T>*)el->elem, nodeList);

    findLeaves.leaves.erase ();
    printf ("Depth first:\n");
    findLeaves.depthFirstRecursive ((goFGNode<T>*)el->elem, nodeList);
    {
        typename goList<goFGNode<T>*>::Element* el = findLeaves.leaves.getFrontElement();
        printf ("Leaves: ");
        while (el)
        {
            if (el->elem->getType() == goFGNode<T>::VARIABLE)
                printf ("%f ", el->elem->value);
            else
                printf ("f(x) ");
            el = el->next;
        }
        printf ("\n");
    }
#endif

    goMessagePassing<T> mp;
    mp.run ((goFGNode<T>*)el->elem, fg);

    //=
    //= Start message passing at the leaves (they are in findLeaves.leaves).
    //= Nur Implementierungsfrage: wie genau messages realisieren? 
    //=  - "send"-Methode an jedem Knoten (intuitiv, aber blaeht Objekte auf und ist langsamer)
    //=  - zentrale Funktion (wahrsch. schneller, aber weniger intuitiv, weniger objektorientiert)
    //= Per DFS durch graph laufen und Nachrichten schicken -- geht nur fuer Baeume!
    //= Nachrichten: Variable -> Faktor: Reelle Zahl ; Faktor -> Variable: Vektor (f(x))
   
    //= Einfuehren: Marker fuer "messages sent" in nodes.
    
    //= Use leaves as queue.

    return true;
    
    //goList<goFGNode<T>*>& Q = findLeaves.leaves;
    //while (!findLeaves.leaves.isEmpty())
   // {
   //     goFGNode<T>* node = Q.getFront();
   //             
   // }
    
    return true; 
}

template class goSumProduct<goFloat>;
