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
#ifndef GOFACTORGRAPH_H
# include <gofactorgraph.h>
#endif
#ifndef GOMESSAGEPASSING_H
# include <gomessagepassing.h>
#endif

/** 
 * \addtogroup gm
 * @{
 */

/** 
 * @brief The sum-product algorithm.
 * \par References:
   \verbatim
     Bishop, C.M. 
     Pattern Recognition and Machine Learning 
     Springer, 2006 \endverbatim
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goSumProduct : public goMessagePassing <T,Tfloat>
{
    public:
        goSumProduct ()
            : goMessagePassing <T,Tfloat> ()
        {
        };
        virtual ~goSumProduct () {};

        /** 
         * @brief Run the sum-product algorithm.
         *
         * Use setValueCount() to set the values of the variables (they will be in the range [0,valueCount-1]).
         * 
         * @param root Node to start the algorithm on.
         * @param fg Factor graph to run the algorithm on.
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFGNode<T,Tfloat>* root, goFactorGraph<T,Tfloat>& fg)
        {
            //= Depth first search plus fill parent fields in the nodes so we know
            //= which is which.
            this->setDirection (goMessagePassing<T,Tfloat>::FORWARD);
            {
                //=
                //= Reset factor and variable nodes 
                //=
                goSize_t sz = fg.myFactors.getSize();
                for (goSize_t i = 0; i < sz; ++i)
                    fg.myFactors[i]->status = goFGNode<T,Tfloat>::NORMAL;
                sz = fg.myVariables.getSize();
                for (goSize_t i = 0; i < sz; ++i)
                    fg.myVariables[i]->status = goFGNode<T,Tfloat>::NORMAL;
            }
            bool ok = this->depthFirstTree (root);
            if (!ok)
            {
                goLog::warning ("goSumProduct::run(): Forward pass failed.");
                return false;
            }

            this->setDirection (goMessagePassing<T,Tfloat>::BACKWARD);

            {
                //=
                //= Reset factor and variable nodes 
                //=
                goSize_t sz = fg.myFactors.getSize();
                for (goSize_t i = 0; i < sz; ++i)
                    fg.myFactors[i]->status = goFGNode<T,Tfloat>::NORMAL;
                sz = fg.myVariables.getSize();
                for (goSize_t i = 0; i < sz; ++i)
                    fg.myVariables[i]->status = goFGNode<T,Tfloat>::NORMAL;
            }
            ok = this->breadthFirst (root);

            return ok;
        };

        //=
        //= Wed Apr 11 11:57:08 CEST 2007
        //= Flooding appears to work. Will terminate for loops if maxPasses is set to some value > 0.
        //=
        /** 
         * @brief "Flooding" type scheme for graphs with loops.
         *
         * Uses a sort of flooding message passing schedule (not parallel, but driven by a queue of pending messages).
         *
         * @param startNode "Root" node. Does not serve any purpose and may be removed.
         * @param fg Factor graph to work on.
         * @param maxPasses Maximal number of passes each node is allowed to send messages for. If 0 (default),
         * the algorithm will terminate when no more messages are pending. If there are loops in the graph and \c maxPasses is 0,
         * the algorithm will run indefinitely.
         * 
         * @return True if successful, false otherwise.
         */
        bool flooding (goFGNode<T,Tfloat>* startNode, goFactorGraph<T,Tfloat>& fg, goSize_t maxPasses = 0)
        {
            goSize_t vc = this->getValueCount();

            //=
            //= Step 1: Initialisation.
            //=

            //=
            //= Set all messages to the unit message.
            //=
            {
                typename goFactorGraph<T,Tfloat>::EdgeList::Element* el = fg.myEdges.getFrontElement();
                while (el)
                {
                    el->elem->getMsg12().resize (vc);
                    el->elem->getMsg12().fill (Tfloat(1));
                    el->elem->getMsg21().resize (vc);
                    el->elem->getMsg21().fill (Tfloat(1));
                    el = el->next;
                }
            }

            goList<goFGNode<T,Tfloat>*> nodeQueue;         //= Nodes with outgoing messages
            goList<goSize_t>            edgeQueue;         //= Edge indices in node->adj on which the outgoing messages need to be sent

            //= Append all messages as pending messages
            {
                goSize_t nodeCount = fg.myVariables.getSize();
                for (goSize_t j = 0; j < nodeCount; ++j)
                {
                    fg.myVariables[j]->pass = 0;
                    goSize_t adjCount = fg.myVariables[j]->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        if (fg.myVariables[j]->adj[i])
                        {
                            nodeQueue.append (fg.myVariables[j]);
                            edgeQueue.append (i);
                        }
                    }
                }
                nodeCount = fg.myFactors.getSize();
                for (goSize_t j = 0; j < nodeCount; ++j)
                {
                    fg.myFactors[j]->pass = 0;
                    goSize_t adjCount = fg.myFactors[j]->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        if (fg.myFactors[j]->adj[i])
                        {
                            nodeQueue.append (fg.myFactors[j]);
                            edgeQueue.append (i);
                        }
                    }
                }
            }

            //=
            //= Step 2: Flooding.
            //=
            typename goList<goFGNode<T,Tfloat>*>::Element* nodeEl = 0;
            goList<goSize_t>::Element* edgeEl = 0;
            goFGNode<T,Tfloat>* currentNode = 0;
            goSize_t            currentEdge = 0;
            //= FIXME Change this if you want to stop in cyclic graphs. This works for trees only (as a test).
            while (!nodeQueue.isEmpty())
            {
                //= Get first element from queue
                nodeEl = nodeQueue.getFrontElement();
                currentNode = nodeEl->elem;
                nodeQueue.remove (nodeEl);
                nodeEl = 0;
                edgeEl = edgeQueue.getFrontElement();
                currentEdge = edgeEl->elem;
                edgeQueue.remove (edgeEl);
                edgeEl = 0;
                
                //= Compute outgoing messages for all outgoing edges and send them.
                //= Note: Only add nodes to the queue if their previous parent index was invalid.
                //= If they already have a valid parent (incoming message), they are already in the queue.
                //= After initialisation, all nodes will be added to the queue again. (Try: do not add nodes to the queue in initialisation,
                //= just set the parent to -1).
                //
                //= This is principally backwardPass() from the two-way schedule (message passing in a tree), but we additionally
                //= need to set the "parent" (current incoming message) here.
                {
                    switch (currentNode->getType())
                    {
                        case goFGNode<T,Tfloat>::VARIABLE: this->variableSend (currentNode, currentEdge); break;
                        case goFGNode<T,Tfloat>::FACTOR:   this->factorSend (currentNode, currentEdge); break;
                        default:
                            goLog::error ("goSumProduct::flooding(): Unknown node type.");
                            return false;
                            break;
                    }
                    goFGNode<T,Tfloat>* otherNode = currentNode->adj[currentEdge]->getOtherNode (currentNode);
                    goSize_t otherIndex = currentNode->adj[currentEdge]->getIndex (otherNode);
                    //= Add adjacent nodes of the one we just sent to to pending messages queue
                    goSize_t adjCount = otherNode->adj.getSize();
                    for (goSize_t i = 0; i < adjCount; ++i)
                    {
                        if (i != otherIndex && otherNode->adj[i] && (maxPasses == 0 || otherNode->pass < maxPasses))
                        {
                            // printf ("Adding from %p over egde %d\n", otherNode->adj[i]->getOtherNode(otherNode), i);
                            nodeQueue.append (otherNode);
                            edgeQueue.append (i);
                            // otherNode->parent = currentNode->adj[i]->getIndex(otherNode);
                        }
                    }

                    ++currentNode->pass;
                }

                // currentNode->parent = -1;  //= Reset parent to -1, since this node was removed from the queue.
            }
            return true;
        };

#if 0
        //= Flooding scheme
        bool flooding (goFGNode<T,Tfloat>* startNode, goFactorGraph<T,Tfloat>& fg)
        {
            goSize_t vc = this->getValueCount();

            //=
            //= Step 1: Initialisation.
            //=

            //=
            //= Set all messages to the unit message.
            //=
            {
                typename goFactorGraph<T,Tfloat>::EdgeList::Element* el = fg.myEdges.getFrontElement();
                while (el)
                {
                    el->elem->getMsg12().resize (vc);
                    el->elem->getMsg12().fill (Tfloat(1));
                    el->elem->getMsg21().resize (vc);
                    el->elem->getMsg21().fill (Tfloat(1));
                    el = el->next;
                }
            }

            goList<goFGNode<T,Tfloat>*> nodeQueue;  //= Nodes with incoming messages
            //=
            //= Add all nodes to the queue with incoming messages and set the
            //= incoming ("parent") edge index to "none" (negative).
            //=
            {
                goSize_t nodeCount = fg.myVariables.getSize();
                for (goSize_t i = 0; i < nodeCount; ++i)
                {
                    nodeQueue.append (fg.myVariables[i]);
                    fg.myVariables[i]->parent = -1; //= Set "incoming edge" to "none".
                }
                nodeCount = fg.myFactors.getSize();
                for (goSize_t i = 0; i < nodeCount; ++i)
                {
                    nodeQueue.append (fg.myFactors[i]);
                    fg.myFactors[i]->parent = -1; //= Set "incoming edge" to "none".
                }
            }

            //=
            //= Step 2: Flooding.
            //=
            typename goList<goFGNode<T,Tfloat>*>::Element* Qel = 0;
            goFGNode<T,Tfloat>* currentNode = 0;
            //= FIXME Change this if you want to stop in cyclic graphs. This works for trees only (as a test).
            while (!nodeQueue.isEmpty())
            {
                //= Get first element from queue
                Qel = nodeQueue.getFrontElement();
                currentNode = Qel->elem;
                nodeQueue.remove (Qel);
                Qel = 0;
                
                //= Compute outgoing messages for all outgoing edges and send them.
                //= Note: Only add nodes to the queue if their previous parent index was invalid.
                //= If they already have a valid parent (incoming message), they are already in the queue.
                //= After initialisation, all nodes will be added to the queue again. (Try: do not add nodes to the queue in initialisation,
                //= just set the parent to -1).
                //
                //= This is principally backwardPass() from the two-way schedule (message passing in a tree), but we additionally
                //= need to set the "parent" (current incoming message) here.
                {
                    goIndex_t parentIndex = currentNode->parent;
                    //= Check if the node is a leaf without outgoing edges, if yes simply "absorb" the message
                    if (parentIndex >= 0 && currentNode->adj.getSize() <= 1)
                    {
                        continue;
                    }
                    switch (currentNode->getType())
                    {
                        case goFGNode<T,Tfloat>::VARIABLE:
                            {
                                //= Send messages to all "outgoing" edges ("parent" denotes the currently incoming edge)
                                goIndex_t adjCount = static_cast<goIndex_t>(currentNode->adj.getSize());
                                for (goIndex_t i = 0; i < adjCount; ++i)
                                {
                                    if (i != parentIndex && currentNode->adj[i])
                                    {
                                        this->variableSend (currentNode, i);
                                        goFGNode<T,Tfloat>* otherNode = currentNode->adj[i]->getOtherNode(currentNode);
                                        if (otherNode->parent < 0)
                                            nodeQueue.append (otherNode);
                                        otherNode->parent = currentNode->adj[i]->getIndex(otherNode);
                                    }
                                }
                            }
                            break;
                        case goFGNode<T,Tfloat>::FACTOR:
                            {
                                //= Send messages to all "outgoing" edges ("parent" denotes the currently incoming edge)
                                goIndex_t adjCount = static_cast<goIndex_t>(currentNode->adj.getSize());
                                for (goIndex_t i = 0; i < adjCount; ++i)
                                {
                                    if (i != parentIndex && currentNode->adj[i])
                                    {
                                        this->factorSend (currentNode, i);
                                        goFGNode<T,Tfloat>* otherNode = currentNode->adj[i]->getOtherNode(currentNode);
                                        if (otherNode->parent < 0)
                                            nodeQueue.append (otherNode);
                                        otherNode->parent = currentNode->adj[i]->getIndex(otherNode);
                                    }
                                }
                            }
                            break;
                        default:
                            goLog::error ("goSumProduct::flooding(): Unknown node type.");
                            return false;
                            break;
                    }
                }

                currentNode->parent = -1;  //= Reset parent to -1, since this node was removed from the queue.
            }

        };
#endif

        /** 
         * @brief Create and "send" message from a variable node \c fgn along a given edge.
         * 
         * @param fgn           Variable node to send from.
         * @param parentIndex   Index into fgn->adj of edge to send the message along 
         *                      (the other connected node is a variable and "receives" the message).
         *                      Think of it as "outgoingIndex".
         * 
         * @return True if successful, false otherwise.
         */
        inline bool variableSend (goFGNode<T,Tfloat>* fgn, goIndex_t parentIndex)
        {
            //= Send message \mu_{x->f} along parent edge
            goIndex_t mu_index = parentIndex;
            if (mu_index < 0)
                mu_index = 0;
            goVector<Tfloat>& mu = fgn->adj[mu_index]->getOutMsg (fgn);
            if (mu.getSize() != this->getValueCount())
                mu.resize (this->getValueCount());

            goIndex_t adjCount = static_cast<goIndex_t>(fgn->adj.getSize());

            mu.fill (Tfloat(1));
            for (goIndex_t i = 0; i < adjCount; ++i)
            {
                if (i != parentIndex && fgn->adj[i])
                    mu *= fgn->adj[i]->getInMsg (fgn);
            }

            return true;
        };
        /** 
         * @brief Create and "send" message from a factor node \c fgn along a given edge.
         * 
         * @param fgn           Factor node to send from.
         * @param parentIndex   Index into fgn->adj of edge to send the message along 
         *                      (the other connected node is a variable and "receives" the message).
         * 
         * @return True if successful, false otherwise.
         */
        inline bool factorSend (goFGNode<T,Tfloat>* fgn, goSize_t parentIndex)
        {
            goVector<Tfloat>& mu = fgn->adj[parentIndex]->getOutMsg (fgn);
            if (mu.getSize() != this->getValueCount())
                mu.resize (this->getValueCount());

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
                //= Sum over all other variables connected to this 
                //= factor except for the parent.
                goSize_t x_index = parentIndex;
                goVector<T> X (fgn->adj.getSize());
                //= Dynamic cast funktioniert in manchen Faellen nicht (??) -- daher mit Gewalt.
                goFGNodeFactor<T,Tfloat>* factornode = (goFGNodeFactor<T,Tfloat>*)fgn;
                assert (factornode);
                goFunctorBase1< Tfloat, const goVector<T>& >* f = factornode->getFunctor();
                for (goSize_t x = 0; x < this->getValueCount(); ++x)
                {
                    X.fill (T(0));
                    X[x_index] = T(x);
                    //= Sum over all other variables .. this is recursive, but as long
                    //= as the number of connections (variables) is not overly large, 
                    //= that should work.
                    mu[x] = this->sumproduct (factornode, f, X, 0, x_index);
                }
            }
            else
            {
                //= 
                //= Send message from leaf only in forward mode (from leaves to root).
                //=
                if (this->getDirection() == goMessagePassing<T,Tfloat>::FORWARD)
                {
                    //= This must be a leaf node. Send f(x).
                    goVector<T> X(1);
                    //= Dynamic cast funktioniert in manchen Faellen nicht (??) -- daher mit Gewalt.
                    goFGNodeFactor<T,Tfloat>* f = (goFGNodeFactor<T,Tfloat>*)fgn;
                    for (goSize_t i = 0; i < this->getValueCount(); ++i)
                    {
                        X[0] = T(i);
                        mu[i] = (*f)(X);
                    }
                }
            }

            return true;
        };

        /** 
         * @brief Recursive sum/product for the sum-product algorithm.
         *
         * Calculates
         * \f$ \sum\limits_{x_1} \ldots \sum\limits_{x_M} f(x,x_1,\ldots,x_M) \prod\limits_{m \in neighbours(f)\backslash x} \mu_{x_m \mapsto f}(x_m) \f$
         * with x being the variable with \c fixed_index, \f$x_1,\ldots,x_M\f$ the other variables connected to \c factorNode,
         * and \f$\mu\f$ the messages stored in the edges between the variable nodes and \c factorNode.
         * The above term constitutes the messages \f$ \mu_{f \mapsto x}(x) \f$ for the sum-product algorithm.
         * 
         * @param factorNode Factor node to calculate on.
         * @param f Functor of \c factorNode (usually, but can also be a different one).
         * @param X Input for \c f
         * @param i Current index into \c X we are summing over.
         * @param fixed_index Index into \c X of the variable that is held fixed (not summed over).
         * 
         * @return The sum/product value.
         */
        inline Tfloat sumproduct (goFGNodeFactor<T,Tfloat>* factorNode,
                goFunctorBase1 <Tfloat, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index)
        {
            goSize_t vc = this->getValueCount();
            if (i >= X.getSize())
            {
               Tfloat prodIncoming = Tfloat(1);
               goSize_t M = factorNode->adj.getSize();
               for (goSize_t j = 0; j < M; ++j)
               {
                   if (j == fixed_index || !factorNode->adj[j])
                   {
                       continue;
                   }
                   goVector<Tfloat>& inMsg = factorNode->adj[j]->getInMsg(factorNode);
                   if (inMsg.getSize() != vc)
                   {
                       goLog::warning ("goSumProduct::sumproduct(): inMsg size mismatch, continuing -- loopy graph?");
                       continue;
                   }
                   //prodIncoming *= factorNode->adj[j]->getInMsg(factorNode)[X[j]];
                   prodIncoming *= inMsg[X[j]];
               }

               return (*f)(X) * prodIncoming;
            }
            if (i == fixed_index)
            {
                return sumproduct (factorNode, f, X, i+1, fixed_index);
            }
            Tfloat s = Tfloat(0);
            for (goSize_t j = 0; j < vc; ++j)
            {
                X[i] = T(j);
                s += sumproduct (factorNode, f, X, i+1, fixed_index);
                //printf ("Added sumproduct for ");
                //for (goSize_t k = 0; k < X.getSize(); ++k)
                //{
                //    printf ("%d ", X[k]);
                //}
                //printf ("\n");
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
            goIndex_t parentIndex = node->parent;
            if (parentIndex < 0)
            {
                //= This means we are at the root.
                return true;
            }

            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    this->variableSend (node, parentIndex);
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    this->factorSend (node, parentIndex);
                    break;
                default:
                    goLog::error ("goSumProduct::forwardPass(): Unknown node type.");
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
            goIndex_t parentIndex = node->parent;
            switch (node->getType())
            {
                case goFGNode<T,Tfloat>::VARIABLE:
                    {
                        //= Send messages to all "children" (away from root)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            if (i != parentIndex && node->adj[i])
                            {
                                this->variableSend (node, i);
                            }
                        }
                    }
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    {
                        //= Send messages to all "children" (away from root)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            if (i != parentIndex && node->adj[i])
                            {
                                this->factorSend (node, i);
                            }
                        }
                    }
                    break;
                default:
                    goLog::error ("goSumProduct::backwardPass(): Unknown node type.");
                    return false;
                    break;
            }

            return true;
        };

        virtual bool action (goFGNode<T,Tfloat>* node) 
        { 
            if (!node)
                return false;

            switch (this->getDirection())
            {
                case goMessagePassing<T,Tfloat>::FORWARD:  return this->forwardPass (node); break;
                case goMessagePassing<T,Tfloat>::BACKWARD: return this->backwardPass (node); break;
                default:
                    goLog::error ("goSumProduct::action(): Unknown value for direction.");
                    return false;
                    break;
            }

            return true;
        };

        bool marginal (
                goFGNodeVariable<T,Tfloat>* variable, 
                goSize_t                    valueCount, 
                goVector<Tfloat>&           marginalRet)
        {
            goSize_t adjCount = variable->adj.getSize();
            if (marginalRet.getSize() != valueCount)
                marginalRet.resize (valueCount);
            marginalRet.fill (Tfloat(1));
            for (goSize_t i = 0; i < adjCount; ++i)
            {
                if (variable->adj[i])
                {
                    marginalRet *= variable->adj[i]->getInMsg(variable);
                }
            }
            return true;
        };
        
        Tfloat norm (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount)
        {
            goVector<Tfloat> marginal (valueCount);
            this->marginal (fg.myVariables[0], valueCount, marginal);
            return marginal.sum ();
        };
};

/** @} */

#endif
