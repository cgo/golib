/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOMAXSUM_H
#define GOMAXSUM_H

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

#include <limits>

/** 
 * \addtogroup gm
 * @{
 */
/** 
 * @brief The max-sum algorithm.
 * @note The factors in the factor graph must return logarithms, no extra log() is called.
 * \par References:
   \verbatim
     Kschischang, F.R. & Frey, B.J. 
     Iterative Decoding of Compound Codes by Probability Propagation in Graphical Models 
     IEEE Journal on Selected Areas in Communications, 1998, 16, 219-230
   \endverbatim
   \verbatim
     F.R. Kschischang; B.J. Frey & H. Loeliger 
     Factor Graphs and the Sum-Product Algorithm 
     IEEE Transactions on Information Theory, 2001, 47, 498-508
   \endverbatim
   \verbatim
     Bishop, C.M. 
     Pattern Recognition and Machine Learning 
     Springer, 2006
   \endverbatim
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goMaxSum : public goMessagePassing <T,Tfloat>
{
    public:
        goMaxSum ()
            : goMessagePassing<T,Tfloat> () {};
        virtual ~goMaxSum () {};

        /** 
         * @brief Run the max-sum algorithm.
         * 
         * @param fg Factor graph to run the algorithm on.
         * @param valueCount The values of the variables are in the range [0,valueCount-1].
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFGNode<T,Tfloat>* root, goFactorGraph<T,Tfloat>& fg)
        {
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
                goLog::warning ("goMaxSum::run(): Forward pass failed.");
                return false;
            }

            this->setDirection (goMessagePassing<T,Tfloat>::BACKWARD);

            //=
            //= Set the maximising value of the root node in the case of the max-sum algorithm
            //= by taking \arg\max_x (\sum_{s \in ne(x)} \mu_{f_s -> x}(x)) (8.98 in Bishop's book).
            //=
            assert ((root->getType() == goFGNode<T,Tfloat>::VARIABLE));
            goIndex_t adjCount = static_cast<goIndex_t>(root->adj.getSize());
            goSize_t vc = this->getValueCount();
            goMath::Vector<Tfloat> sum (vc);
            sum.fill (Tfloat(0));
            for (goIndex_t i = 0; i < adjCount; ++i)
            {
                if (!root->adj[i])
                    continue;
                goMath::Vector<Tfloat>& inMsg = root->adj[i]->getInMsg (root);
                if (inMsg.getSize() != vc)
                {
                    goLog::warning ("goMaxSum::run(): inMsg to ROOT of wrong size -- loopy graph?");
                    continue;
                }
                sum += inMsg;
            }
            goSize_t max_i = 0;
            assert (vc > 0);
            Tfloat   max_value = sum[0];
            for (goSize_t i = 0; i < vc; ++i)
            {
                if (sum[i] > max_value)
                {
                    max_value = sum[i];
                    max_i = i;
                }
            }
            root->value = T(max_i);
            
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

        /** 
         * @brief Create and "send" message from a variable node \c fgn along a given edge.
         * 
         * @param fgn           Variable node to send from.
         * @param parentIndex   Index into fgn->adj of edge to send the message along 
         *                      (the other connected node is a variable and "receives" the message).
         * 
         * @return True if successful, false otherwise.
         */
        inline bool variableSend (goFGNode<T,Tfloat>* fgn, goIndex_t parentIndex)
        {
            //= Send message \mu_{x->f} along parent edge
            goIndex_t mu_index = parentIndex;
            if (mu_index < 0)
                mu_index = 0;
            goMath::Vector<Tfloat>& mu = fgn->adj[mu_index]->getOutMsg (fgn);
            if (mu.getSize() != this->getValueCount())
                mu.resize (this->getValueCount());
            goIndex_t adjCount = static_cast<goIndex_t>(fgn->adj.getSize());

            mu.fill (Tfloat(0));
            for (goIndex_t i = 0; i < adjCount; ++i)
            {
                if (i != parentIndex && fgn->adj[i])
                    mu += fgn->adj[i]->getInMsg (fgn);  
            }

            return true;
        };

        /** 
         * @brief Calculate outgoing message for a factor node in the max-sum algorithm.
         * 
         * @param fgn Factor node.
         * @param parentIndex Index into fgn->adj pointing to the "parent" link,
         * i.e. the link where the message is sent to.
         * 
         * @return True if successful, false otherwise.
         */
        inline bool factorSend (goFGNode<T,Tfloat>* fgn, goSize_t parentIndex)
        {
            //= Make this a function of parentEdge -- the same will be needed on the way back.
            goMath::Vector<Tfloat>& mu = fgn->adj[parentIndex]->getOutMsg (fgn);
            if (mu.getSize() != this->getValueCount())
                mu.resize (this->getValueCount());

            //= Dynamic cast funktionierte in einem Fall nicht (nicht ersichtlich warum) ...
            //= daher mit Gewalt.
            goFGNodeFactor<T,Tfloat>* factornode = (goFGNodeFactor<T,Tfloat>*)fgn;
            assert (factornode);

            goSize_t vc = this->getValueCount ();

            //= 
            //= Reference and optionally resize storage for maximising variable values.
            //=
            goMath::Matrix<T>& maxX = factornode->getMaxX();
            if (maxX.getRows() != vc || 
                maxX.getColumns() != fgn->adj.getSize())
            {
                maxX.resize (vc, fgn->adj.getSize());
                maxX.fill (T(0));
            }
            
            //=
            //= Maximise locally for this factor and add all incoming 
            //= messages (from variables).
            //=
            static Tfloat float_min = -std::numeric_limits<Tfloat>::max(); //= Start value for maxsum()
            if (fgn->adj.getSize() > 1)
            {
                //= Sum over all other variables connected to this 
                //= factor except for the parent.
                goSize_t x_index = parentIndex;
                goMath::Vector<T> X (fgn->adj.getSize());
                goFunctorBase1< Tfloat, const goMath::Vector<T>& >* f = factornode->getFunctor();
                goMath::Vector<T> maxXv;
                for (goSize_t x = 0; x < vc; ++x)
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
                    mu[x] = this->maxsum (factornode, f, X, 0, x_index, float_min, maxXv);
                }
            }
            else
            {
                //= This must be a leaf node. Send f(x).
                goMath::Vector<T> X(1);
                for (goSize_t i = 0; i < vc; ++i)
                {
                    X[0] = T(i);
                    mu[i] = (*factornode)(X);
                }
            }

            return true;
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
                    goLog::error ("goMaxSum::action(): Unknown node type.");
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
                        //= We actually only need to set the value at the root and 
                        //= then use backtracking to set
                        //= the maximising configuration from the factor nodes.
                    }
                    break;
                case goFGNode<T,Tfloat>::FACTOR:
                    {
                        //= Send messages to all "children" (away from root)
                        goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());

                        //= Dynamic cast funktionierte in einem Fall nicht (nicht ersichtlich
                        //= warum .. vielleicht compiler bug??) .. daher mit Gewalt.
                        goFGNodeFactor<T,Tfloat>* fn = (goFGNodeFactor<T,Tfloat>*)node;
                        assert (fn);
                        //= Get the maximising configuration of the other variable nodes given the value of the parent variable:
                        goMath::Vector<T> maxX;
                        goSize_t parent_value = static_cast<goSize_t>(fn->adj[fn->parent]->getOtherNode(fn)->value);
                        fn->getMaxX().refRow (parent_value, maxX);
                        //= 
                        //= Set values of all other adjacent nodes:
                        //=
                        for (goIndex_t i = 0; i < adjCount; ++i)
                        {
                            if (i != parentIndex && fn->adj[i])
                            {
                                fn->adj[i]->getOtherNode(fn)->value = maxX[i];
                            }
                        }
                    }
                    break;
                default:
                    goLog::error ("goMaxSum::backwardPass(): Unknown node type.");
                    return false;
                    break;
            }

            return true;
        };

        /** 
         * @brief Recursive max/sum for the max-sum algorithm.
         * 
         * Calculates \f$ \max\limits_{x_1,\ldots,x_M} \left[ \ln f(x,x_1,\ldots,x_M) + \sum\limits_{m \in neighbours(f)\backslash x} \mu_{x_m \mapsto f}(x_m) \right] \f$
         * with x being the variable with \c fixed_index, \f$x_1,\ldots,x_M\f$ the other variables connected to \c factorNode,
         * and \f$\mu\f$ the messages stored in the edges between the variable nodes and \c factorNode.
         * The above term constitutes the messages \f$ \mu_{f \mapsto x}(x) \f$ for the max-sum algorithm.
         *
         * @param factorNode Factor node to calculate on.
         * @param f Functor of \c factorNode (usually, but can also be a different one).
         * @param X Input for \c f
         * @param i Current index into \c X we are summing over.
         * @param fixed_index Index into \c X of the variable that is held fixed (not summed over).
         * @param currentMax Current maximum -- start with a value lower than all other occuring values.
         * @param maxX Maximising variable values -- set by this function.
         * @return The max/sum value.
         */
        Tfloat maxsum (goFGNodeFactor<T,Tfloat>* factorNode,
                       goFunctorBase1 <Tfloat, const goMath::Vector<T>& >* f, 
                       goMath::Vector<T>& X, 
                       goSize_t     i, 
                       goSize_t     fixed_index, 
                       Tfloat       currentMax, 
                       goMath::Vector<T>& maxX)
        {
            goSize_t vc = this->getValueCount();
            if (i >= X.getSize())
            {
               Tfloat sumIncoming = Tfloat(0);
               goSize_t M = factorNode->adj.getSize();
               for (goSize_t j = 0; j < M; ++j)
               {
                   if (!factorNode->adj[j] || j == fixed_index)
                       continue;
                   goMath::Vector<Tfloat>& inMsg = factorNode->adj[j]->getInMsg(factorNode);
                   if (inMsg.getSize() != vc)
                   {
                       goLog::warning ("goMaxSum::maxsum(): inMsg size mismatch, continuing -- loopy graph?");
                       continue;
                   }
                   //sumIncoming += factorNode->adj[j]->getInMsg(factorNode)[X[j]];
                   sumIncoming += inMsg[X[j]];
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
                return this->maxsum (factorNode, f, X, i+1, fixed_index, currentMax, maxX);
            }
            Tfloat s = currentMax;
            for (goSize_t j = 0; j < vc; ++j)
            {
                X[i] = T(j);
                s = this->maxsum (factorNode, f, X, i+1, fixed_index, s, maxX);
            }
            return s;
        };

        /** 
         * @brief "Flooding" type scheme for graphs with loops.
         *
         * Uses a sort of flooding message passing schedule (not parallel, but driven by a queue of pending messages).
         *
         * @note Notice that this will fail to give a consistent maximising configuration in case there are multiple
         * maximising configurations. We are simply applying the maximisation to all variable nodes locally instead of using 
         * a backtracking step (which does not work here .. is that a bug or does it generally not work?).
         *
         * @param startNode "Root" node. Does not serve any purpose as long as no backtracking is implemented.
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
                    el->elem->getMsg12().fill (Tfloat(0));
                    el->elem->getMsg21().resize (vc);
                    el->elem->getMsg21().fill (Tfloat(0));
                    el = el->next;
                }
            }

            //=
            //= Queues for pending messages
            //=
            goList<goFGNode<T,Tfloat>*> nodeQueue;         //= Nodes with outgoing messages
            goList<goSize_t>            edgeQueue;         //= Edge indices in node->adj on which the outgoing messages need to be sent

            //= Append all messages as pending messages
            {
                goSize_t nodeCount = fg.myVariables.getSize();
                for (goSize_t j = 0; j < nodeCount; ++j)
                {
                    fg.myVariables[j]->parent = -1;
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
                    fg.myFactors[j]->parent = -1;
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
                //printf ("Size of nodeQueue is %d\n", nodeQueue.getSize());
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
                    // printf ("Sending from %p over edge %d\n", currentNode, currentEdge);
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
                            nodeQueue.append (otherNode);
                            edgeQueue.append (i);
                        }
                    }

                    if (maxPasses > 0)
                        ++currentNode->pass;
                }
            }

            //=
            //= Assing maximising configuration.
            //=

            //=
            //= NOTE:
            //= Notice that this will fail to give a consistent maximising configuration in case there are multiple
            //= maximising configurations. We are simply applying the maximisation to all variable nodes locally instead of using
            //= a backtracking step (which does not work here .. is that a bug or does it generally not work?).
            //=

            goSize_t nodeCount = fg.myVariables.getSize();
            for (goSize_t i = 0; i < nodeCount; ++i)
            {
                goFGNode<T,Tfloat>* node = fg.myVariables[i];
                //=
                //= Set the maximising value of the root node in the case of the max-sum algorithm
                //= by taking \arg\max_x (\sum_{s \in ne(x)} \mu_{f_s -> x}(x)) (8.98 in Bishop's book).
                //=
                assert ((node->getType() == goFGNode<T,Tfloat>::VARIABLE));
                goIndex_t adjCount = static_cast<goIndex_t>(node->adj.getSize());
                goSize_t vc = this->getValueCount();
                goMath::Vector<Tfloat> sum (vc);
                sum.fill (Tfloat(0));
                for (goIndex_t i = 0; i < adjCount; ++i)
                {
                    if (!node->adj[i])
                        continue;
                    goMath::Vector<Tfloat>& inMsg = node->adj[i]->getInMsg (node);
                    if (inMsg.getSize() != vc)
                    {
                        goLog::warning ("goMaxSum::flooding(): inMsg to node of wrong size -- loopy graph?");
                        continue;
                    }
                    sum += inMsg;
                }
                goSize_t max_i = 0;
                assert (vc > 0);
                Tfloat   max_value = sum[0];
                for (goSize_t i = 0; i < vc; ++i)
                {
                    if (sum[i] > max_value)
                    {
                        max_value = sum[i];
                        max_i = i;
                    }
                }
                node->value = T(max_i);
            }
            return true;

#if 0
            //=
            //= backtracking
            //=
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

            printf ("Running backtracking for startNode == %p\n", startNode);

            {
                //=
                //= Set the maximising value of the root node in the case of the max-sum algorithm
                //= by taking \arg\max_x (\sum_{s \in ne(x)} \mu_{f_s -> x}(x)) (8.98 in Bishop's book).
                //=
                assert ((startNode->getType() == goFGNode<T,Tfloat>::VARIABLE));
                goIndex_t adjCount = static_cast<goIndex_t>(startNode->adj.getSize());
                goSize_t vc = this->getValueCount();
                goMath::Vector<Tfloat> sum (vc);
                sum.fill (Tfloat(0));
                for (goIndex_t i = 0; i < adjCount; ++i)
                {
                    if (!startNode->adj[i])
                        continue;
                    goMath::Vector<Tfloat>& inMsg = startNode->adj[i]->getInMsg (startNode);
                    if (inMsg.getSize() != vc)
                    {
                        goLog::warning ("goMaxSum::flooding(): inMsg to startNode of wrong size -- loopy graph?");
                        continue;
                    }
                    sum += inMsg;
                }
                goSize_t max_i = 0;
                assert (vc > 0);
                Tfloat   max_value = sum[0];
                for (goSize_t i = 0; i < vc; ++i)
                {
                    if (sum[i] > max_value)
                    {
                        max_value = sum[i];
                        max_i = i;
                    }
                }
                startNode->value = T(max_i);
            }

            printf ("Running back tracking\n");
            return this->breadthFirstTree (startNode);
#endif
        };

        virtual bool action (goFGNode<T,Tfloat>* node) 
        { 
            if (!node)
                return false;

            switch (this->getDirection())
            {
                case goMessagePassing<T,Tfloat>::FORWARD:  return this->forwardPass (node); break;
                case goMessagePassing<T,Tfloat>::BACKWARD: this->backwardPass (node); break;
                default:
                    goLog::error ("goMaxSum::action(): Unknown value for direction.");
                    return false;
                    break;
            }

            return true;
        };

        // virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);
};

/** @} */

#endif
