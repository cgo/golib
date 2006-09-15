#include <gomath.h>
#include <golist.h>

/** 
 * @brief Find the roots (zero crossings) of a function curve
 * approximated as polygonal curve.
 * 
 * @param fX X coordinates of the points of the polygonal approximation of
 * a function curve.
 * @param fY X coordinates of the points of the polygonal approximation of
 * a function curve.
 * @param retX X coordinates of the roots.
 * @param retIndex If not 0, contains the indices of the Y input points
 * which are closer to zero for each root. If 0, this is ignored.
 * 
 * Instantiated for <goVectord, goDouble>
 *
 * @return True if successful, false otherwise.
 */
template <class vectorT, class T>
bool goMath::getRoots (const vectorT& fX, 
                       const vectorT& fY,
                       vectorT&       retX,
                       goVector<goIndex_t>* retIndex)
{
    assert (fX.getSize() == fY.getSize());

    goList<T> rootList;
    goList<goIndex_t> rootIndexList;
    goSize_t i;
    goSize_t N = fX.getSize();
    for (i = 0; i < N-1; ++i)
    {
        if (fY[i] * fY[i+1] < T(0) || fY[i] == T(0))
        {
            rootList.append(fX[i] - fY[i] / ((fY[i+1] - fY[i]) / (fX[i+1]-fX[i])));
            rootIndexList.append(fabs(fY[i]) < fabs(fY[i+1]) ? i : i + 1);
        }
    }
    typename goList<T>::Element* el = rootList.getFrontElement();
    goList<goIndex_t>::Element* iel = rootIndexList.getFrontElement();
    goSize_t rootCount = rootList.getSize();
    retX.resize(rootCount);
    assert (rootList.getSize() == rootIndexList.getSize());
    if (retIndex)
    {
        retIndex->resize(rootCount);
    }
    for (i = 0; i < rootCount && el; ++i, el = el->next, iel = iel->next)
    {
        retX[i] = el->elem;
        if (retIndex)
        {
            (*retIndex)[i] = iel->elem;
        }
    }
    return true;
}

template bool goMath::getRoots<goVectord, goDouble> (const goVectord&, const goVectord&, goVectord&, goVector<goIndex_t>*);
