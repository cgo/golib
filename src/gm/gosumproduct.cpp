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

            //=
            //= Set the maximising value of the root node in the case of the max-sum algorithm.
            //=
            if (this->myAlgorithm == MAX_SUM)
            {
                assert ((root->getType() == goFGNode<T,Tfloat>::VARIABLE));
                goIndex_t adjCount = static_cast<goIndex_t>(root->adj.getSize());
                goVector<Tfloat> sum (this->myValueCount);
                sum.fill (Tfloat(0));
                for (goIndex_t i = 0; i < adjCount; ++i)
                {
                    sum += root->adj[i]->getInMsg (root);
                }
                goSize_t max_i = 0;
                assert (this->myValueCount > 0);
                Tfloat   max_value = sum[0];
                for (goSize_t i = 0; i < this->myValueCount; ++i)
                {
                    if (sum[i] > max_value)
                    {
                        max_value = sum[i];
                        max_i = i;
                    }
                }
                root->value = T(max_i);
            }
            
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
        //inline bool variableSend (goFGNode<T,Tfloat>* fgn, typename EdgeList::Element* parentEdgeEl)
        inline bool variableSend (goFGNode<T,Tfloat>* fgn, goIndex_t parentIndex)
        {
            //= Send message \mu_{x->f} along parent edge
            //goVector<Tfloat>& mu = parentEdgeEl->elem->getOutMsg (fgn);
            goIndex_t mu_index = parentIndex;
            if (mu_index < 0)
                mu_index = 0;
            goVector<Tfloat>& mu = fgn->adj[mu_index]->getOutMsg (fgn);
            // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T,Tfloat>::FACTOR);
            if (mu.getSize() != this->myValueCount)
                mu.resize (this->myValueCount);
            // typename EdgeList::Element* el = fgn->adj.getFrontElement();
            goIndex_t adjCount = static_cast<goIndex_t>(fgn->adj.getSize());
            switch (myAlgorithm) //= This may be done differently, but this prevents copy-paste. I know
                                 //= that is may slow things down.
            {
                case SUM_PRODUCT:
                {
                    mu.fill (Tfloat(1));
                    //while (el)
                    for (goIndex_t i = 0; i < adjCount; ++i)
                    {
                        //if (el->elem != parentEdgeEl->elem)
                        if (i != parentIndex)
                            //mu *= el->elem->getInMsg (fgn);  //= Element-wise multiply
                            mu *= fgn->adj[i]->getInMsg (fgn);
                        //el = el->next;
                    }
                }
                break;
                case MAX_SUM:
                {
                    mu.fill (Tfloat(0));
                    //while (el)
                    for (goIndex_t i = 0; i < adjCount; ++i)
                    {
                        //if (el->elem != parentEdgeEl->elem)
                        if (i != parentIndex)
                            //mu += el->elem->getInMsg (fgn);  //= Element-wise multiply
                            mu += fgn->adj[i]->getInMsg (fgn);  
                        //el = el->next;
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
        //inline bool factorSend (goFGNode<T,Tfloat>* fgn, typename EdgeList::Element* parentEdgeEl)
        inline bool factorSend (goFGNode<T,Tfloat>* fgn, goSize_t parentIndex)
        {
            //= Make this a function of parentEdge -- the same will be needed on the way back.
            //goVector<Tfloat>& mu = parentEdgeEl->elem->getOutMsg (fgn);
            goVector<Tfloat>& mu = fgn->adj[parentIndex]->getOutMsg (fgn);
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
                //typename EdgeList::Element* el = fgn->adj.getFrontElement();
                goSize_t adjCount = fgn->adj.getSize();
                //while (el)
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    //if (el->elem != parentEdgeEl->elem)   
                    if (i != parentIndex)
                    {
                        //= Sum over all other variables connected to this 
                        //= factor except for the parent.
                        // goSize_t x_index = parentEdgeEl->index;
                        goSize_t x_index = parentIndex;
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
                            mu[x] = this->sumproduct (factornode, f, X, 0, x_index);
                        }
                    }
                    //el = el->next;
                }
                //= Multiply with all incoming messages except the one from the parent variable node.
                //= TODO: Das ist wahrscheinlich falsch. (8.66) im Buch, Produkt ist Funktion von den
                //=       Variablen x_m --> Produkt in sum() einbauen.
                //el = fgn->adj.getFrontElement();
                //while (el)
#if 0
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    //if (el->elem != parentEdgeEl->elem)   
                    if (i != parentIndex)
                    {
                        mu *= fgn->adj[i]->getInMsg (fgn);
                        //mu *= el->elem->getInMsg (fgn);
                    }
                    //el = el->next;
                }
#endif
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

        inline bool factorSendMaxSum (goFGNode<T,Tfloat>* fgn, goSize_t parentIndex)
        {
            //= Make this a function of parentEdge -- the same will be needed on the way back.
            goVector<Tfloat>& mu = fgn->adj[parentIndex]->getOutMsg (fgn);
            // assert (parentEdge->getOtherNode(fgn)->getType() == goFGNode<T,Tfloat>::VARIABLE);
            if (mu.getSize() != this->myValueCount)
                mu.resize (this->myValueCount);

            goFGNodeFactor<T,Tfloat>* factornode = dynamic_cast<goFGNodeFactor<T,Tfloat>*> (fgn);

            //= 
            //= Reference and optionally resize storage for maximising variable values.
            //=
            goMatrix<T>& maxX = factornode->getMaxX();
            assert (factornode);
            if (maxX.getRows() != this->myValueCount || 
                maxX.getColumns() != fgn->adj.getSize())
            {
                maxX.resize (this->myValueCount, fgn->adj.getSize());
                maxX.fill (T(0));
            }
            
            //=
            //= Marginalise locally for this factor and multiply by all incoming 
            //= messages (from variables).
            //=
            if (fgn->adj.getSize() > 1)
            {
                goSize_t adjCount = fgn->adj.getSize();
                for (goSize_t i = 0; i < adjCount; ++i)
                {
                    if (i != parentIndex)
                    {
                        //= Sum over all other variables connected to this 
                        //= factor except for the parent.
                        goSize_t x_index = parentIndex;
                        printf ("parent x index == %d\n", x_index);
                        goVector<T> X (fgn->adj.getSize());
                        goFunctorBase1< Tfloat, const goVector<T>& >* f = factornode->getFunctor();
                        goVector<T> maxXv;
                        for (goSize_t x = 0; x < this->myValueCount; ++x)
                        {
                            X.fill (T(0));
                            X[x_index] = T(x);
                            //= Maximise over all other variables .. this is recursive, but as long
                            //= as the number of connections (variables) is not overly large, 
                            //= that should work.
                            //= maxXv should contain the variable values that maximise the function for
                            //= each x and
                            //= will be used to set the variables in the backtracking pass.
                            maxX.refRow (x, maxXv);
                            mu[x] = this->maxsum (factornode, f, X, 0, x_index, Tfloat(0), maxXv);
                        }
                    }
                }
            }
            else
            {
                //= This must be a leaf node. Send f(x).
                goVector<T> X(1);
                for (goSize_t i = 0; i < this->myValueCount; ++i)
                {
                    X[0] = T(i);
                    mu[i] = (*factornode)(X);
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

# if 1
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
               goSize_t M = factorNode->adj.getSize();
               for (goSize_t j = 0; j < M; ++j)
               {
                   //= This can be made faster with an array containing simply the incoming messages. But what the heck.
                   if (j != fixed_index)
                       prodIncoming *= factorNode->adj[j]->getInMsg(factorNode)[X[i]];
               }
               return (*f)(X) * prodIncoming;
            }
            if (i == fixed_index)
            {
                return sumproduct (factorNode, f, X, i+1, fixed_index);
            }
            goDouble s = 0.0;
            for (goSize_t j = 0; j < this->myValueCount; ++j)
            {
                X[i] = T(j);
                s += sumproduct (factorNode, f, X, i+1, fixed_index);
            }
            return s;
        };
# endif
        inline Tfloat maxsum (goFGNodeFactor<T,Tfloat>* factorNode,
                goFunctorBase1 <Tfloat, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index, Tfloat currentMax, goVector<T>& maxX)
        {
            if (i >= X.getSize())
            {
                //printf ("X: ");
                //for (goSize_t k = 0; k < X.getSize(); ++k)
               // {
               //     printf ("%f ", X[k]);
               // }
               // printf ("\n");
               Tfloat sumIncoming = Tfloat(0);
               goSize_t M = factorNode->adj.getSize();
               for (goSize_t j = 0; j < M; ++j)
               {
                   //= This can be made faster with an array containing simply the incoming messages. But what the heck.
                   if (j != fixed_index)
                       sumIncoming += factorNode->adj[j]->getInMsg(factorNode)[X[i]];
               }
               Tfloat temp = (*f)(X) + sumIncoming;
               if (temp > currentMax)
               {
                   maxX = X;
                   return temp;
               }
               return currentMax;
            }
            if (i == fixed_index)
            {
                return maxsum (factorNode, f, X, i+1, fixed_index, currentMax, maxX);
            }
            goDouble s = currentMax;
            for (goSize_t j = 0; j < this->myValueCount; ++j)
            {
                X[i] = T(j);
                s = maxsum (factorNode, f, i+1, fixed_index, s, maxX);
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
            // typename EdgeList::Element* parentEdge = node->parent;
            goIndex_t parentIndex = node->parent;
            if (parentIndex < 0)
            {
                // goLog::warning ("goMessagePassing::action(): parent == 0");
                //= This means we are at the root.
                return true;
            }

            //printf ("Would send from %d to %d\n", node->value, parentEdge->elem->getOtherNode(node)->value);
            printf ("Would send from %d to %d\n", node->value, node->adj[parentIndex]->getOtherNode(node)->value);
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    this->variableSend (node, parentIndex);
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    this->factorSend (node, parentIndex);
                    break;
                default:
                    goLog::error ("goMessagePassing::action(): Unknown node type.");
                    return false;
                    break;
            }

            return true;
        };

        inline bool forwardPassMaxSum (goFGNode<T,Tfloat>* node)
        {
            //=
            //= This is pass 1 (from leaves to root).
            //=

            //=
            //= Send a message to the parent of node fgn.
            //=
            // typename EdgeList::Element* parentEdge = node->parent;
            goIndex_t parentIndex = node->parent;
            if (parentIndex < 0)
            {
                // goLog::warning ("goMessagePassing::action(): parent == 0");
                //= This means we are at the root.
                return true;
            }

            //printf ("Would send from %d to %d\n", node->value, parentEdge->elem->getOtherNode(node)->value);
            printf ("Would send from %d to %d\n", node->value, node->adj[parentIndex]->getOtherNode(node)->value);
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    this->variableSend (node, parentIndex);
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    this->factorSendMaxSum (node, parentIndex);
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
            //typename EdgeList::Element* parentEdge = node->parent;
            goIndex_t parentIndex = node->parent;
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    {
                        //typename EdgeList::Element* el = node->adj.getFrontElement();
                        //= Send messages to all "children" (away from root)
                        //while (el)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            // if (!parentEdge || el->elem != parentEdge->elem)
                            if (i != parentIndex)
                            {
                                printf ("Backward pass: would send from %d to %d\n", node->value, node->adj[i]->getOtherNode(node)->value);
                                this->variableSend (node, parentIndex);
                            }
                            //el = el->next;
                        }
                    }
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    {
                        //typename EdgeList::Element* el = node->adj.getFrontElement();
                        //= Send messages to all "children" (away from root)
                        //while (el)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            //if (!parentEdge || el->elem != parentEdge->elem)
                            if (i != parentIndex)
                            {
                                printf ("Backward pass: would send from %d to %d\n", node->value, node->adj[i]->getOtherNode(node)->value);
                                this->factorSend (node, parentIndex);
                            }
                            //el = el->next;
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

        inline bool backwardPassMaxSum (goFGNode<T,Tfloat>* node)
        {
            //=
            //= This is pass 2 (from root back to leaves).
            //=
            //typename EdgeList::Element* parentEdge = node->parent;
            goIndex_t parentIndex = node->parent;
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    {
                        //= We actually only need to set the value at the root and then use backtracking to set
                        //= the maximising configuration from the factor nodes.
#if 0
                        //= Send messages to all "children" (away from root)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            if (i != parentIndex)
                            {
                                printf ("Backward pass: would send from %d to %d\n", node->value, node->adj[i]->getOtherNode(node)->value);
                                this->variableSend (node, parentIndex);
                            }
                        }
#endif
                    }
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    {
                        //= Send messages to all "children" (away from root)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        goFGNodeFactor<T,Tfloat>* fn = dynamic_cast<goFGNodeFactor<T,Tfloat>*> (node);
                        assert (fn);
                        //= Get the maximising configuration of the other variable nodes given the value of the parent variable:
                        goVector<T> maxX;
                        goSize_t parent_value = static_cast<goSize_t>(fn->adj[fn->parent]->getOtherNode(fn)->value);
                        fn->getMaxX().refRow (parent_value, maxX);
                        //= 
                        //= Set values of all other adjacent nodes:
                        //=
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            if (i != parentIndex)
                            {
                                fn->adj[i]->getOtherNode(fn)->value = maxX[i];
                                printf ("Maxsum backward pass: set child %d to %d\n", i, maxX[i]);
                                //= this->factorSend (node, parentIndex);
                            }
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
                        switch (this->myAlgorithm)
                        {
                            case SUM_PRODUCT:
                                return this->forwardPass (node);
                                break;
                            case MAX_SUM:
                                return this->forwardPassMaxSum (node);
                                break;
                            default:
                                goLog::error ("goMessagePassing::action(): unknown algorithm.");
                                return false;
                                break;
                        }
                    }
                    break;
                case BACKWARD:
                    {
                        switch (this->myAlgorithm)
                        {
                            case SUM_PRODUCT:
                                return this->backwardPass (node);
                                break;
                            case MAX_SUM:
                                return this->backwardPassMaxSum (node);
                                break;
                            default:
                                goLog::error ("goMessagePassing::action(): unknown algorithm.");
                                return false;
                                break;
                        }
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
