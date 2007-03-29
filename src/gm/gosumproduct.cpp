#include <gosumproduct.h>
#include <gograph.h>
#include <gofunctor.h>
#include <gomessagepassing.h>

// 1. Nodes: Factor, variable.
// (2. Array of all nodes, to access them by index)
// 3. Each node carries adjacencies as list or array of indices .. or pointers directly.
// 


template <class T, class Tfloat>
goSumProduct<T,Tfloat>::goSumProduct ()
    : goObjectBase ()
{
}

template <class T, class Tfloat>
goSumProduct<T,Tfloat>::~goSumProduct ()
{
}

        //=
        //= TODO: * [ scheint zu funktionieren ] Hinweg / Rueckweg, 
        //        * [ scheint ok, testen in partialshape! ] sum-product -> max-sum, 
        //        * [ scheint ok beim ersten Test in goPartialShapeModel ] testen, 
        //        dokumentieren,
        //        sum-product ist nicht ganz komplett, Summe ueber alle eingehenden 
        //          Nachrichten in einem Knoten
        //          muss noch gebildet werden, um die Randverteilung zu bekommen.
        //=       * [ scheint ok ] message-Typ != T, message-Typ = float|double, T nur Variablen-Typ (idR integer) 
        //=


template <class T, class Tfloat>
bool goSumProduct<T,Tfloat>::run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount)
{
    /*
     * Take any one variable node as root
     * Find the leaves
     * At the leaves, start the message passing
     */

    //= 
    //= Find first variable node
    //=
    //typename goFactorGraph<T,Tfloat>::NodeList& nodeList = fg.myNodes;
    //typename goFactorGraph<T,Tfloat>::NodeList::Element* el = nodeList.getFrontElement();
    goSize_t nodes_sz = fg.myVariables.getSize();
    goSize_t i = 0;
    for (i = 0; i < nodes_sz && fg.myVariables[i]->getType() != goFGNode<T,Tfloat>::VARIABLE; ++i)
    // while (el && el->elem->getType() != )
    {
        //el = el->next;
    }
    // if (!el)
    if (i >= nodes_sz)
    {
        goLog::warning ("goSumProduct::run(): no variable node found. Not running.");
        return false;
    }

    goMessagePassing<T,Tfloat> mp;
    mp.setValueCount (valueCount);
    mp.run ((goFGNode<T,Tfloat>*)fg.myVariables[i], fg);

    return true;
}

template <class T, class Tfloat>
bool goSumProduct<T,Tfloat>::marginal (
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
}

/** 
* @brief Calculate the normalisation constant Z for the given factor graph.
*
* @note the sum-product algorithm must have run before, so that a marginal can be 
* calculated.
* 
* @param fg Factor graph to calculate the normalisation constant for.
* @param valueCount Number of possible values for the input to the factors.
* 
* @return Normalisation constant Z.
*/
template <class T, class Tfloat>
Tfloat goSumProduct<T,Tfloat>::norm (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount)
{
    goVector<Tfloat> marginal (valueCount);
    this->marginal (fg.myVariables[0], valueCount, marginal);
    return marginal.sum ();
}

//====================================================================

template <class T, class Tfloat>
goMaxSum<T,Tfloat>::goMaxSum ()
    : goObjectBase ()
{
}

template <class T, class Tfloat>
goMaxSum<T,Tfloat>::~goMaxSum ()
{
}

template <class T, class Tfloat>
bool goMaxSum<T,Tfloat>::run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount)
{
    /*
     * Take any one variable node as root
     * Find the leaves
     * At the leaves, start the message passing
     */

    //= 
    //= Find first variable node
    //=

    //typename goFactorGraph<T,Tfloat>::NodeList& nodeList = fg.myNodes;
    //typename goFactorGraph<T,Tfloat>::NodeList::Element* el = nodeList.getFrontElement();
    goSize_t nodes_sz = fg.myVariables.getSize();
    goSize_t i = 0;
    for (i = 0; i < nodes_sz && fg.myVariables[i]->getType() != goFGNode<T,Tfloat>::VARIABLE; ++i)
    // while (el && el->elem->getType() != )
    {
        //el = el->next;
    }
    // if (!el)
    if (i >= nodes_sz)
    {
        goLog::warning ("goMaxSum::run(): no variable node found. Not running.");
        return false;
    }

    goMessagePassing<T,Tfloat> mp;
    mp.setValueCount (valueCount);
    mp.run ((goFGNode<T,Tfloat>*)fg.myVariables[i], fg, goMessagePassing<T,Tfloat>::MAX_SUM);

    return true;
}

//==============================================================

template class goMaxSum<goSize_t,goFloat>;
template class goMaxSum<goSize_t,goDouble>;
template class goSumProduct<goSize_t,goFloat>;
template class goSumProduct<goSize_t,goDouble>;
