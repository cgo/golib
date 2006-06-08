#include <gokmeansspatial.h>
#include <golog.h>

class goKMeansSpatialPrivate
{
    public:
        goKMeansSpatialPrivate() : positions () {};
        ~goKMeansSpatialPrivate() {};

        goList<goVectord>       positions;
        goFixedArray<goVectord> meanPositions;
};

template <class elementT>
goKMeansSpatial<elementT>::goKMeansSpatial()
    : goKMeans<elementT>(), myPrivate(0)
{
    this->setClassName("goKMeansSpatial");
    myPrivate = new goKMeansSpatialPrivate;
}

template <class elementT>
goKMeansSpatial<elementT>::~goKMeansSpatial()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class elementT>
bool goKMeansSpatial<elementT>::initialisePositions (const goFixedArray<goVectord>& initMeanPos)
{
    myPrivate->meanPositions = initMeanPos;
    return true;
}

template <class elementT>
goSize_t goKMeansSpatial<elementT>::assignment ()
{
    assert (this->getElements().getSize() == myPrivate->positions.getSize());
    goIndex_t K = this->getMeans().getSize();
    goIndex_t sz = this->getElements().getSize();
    if (this->getCluster().getSize() != static_cast<goSize_t>(sz))
    {
        this->getCluster().setSize(sz);
        this->getCluster().fill(0);
    }
    goSize_t changedElements = 0;
    goIndex_t i = 0;
    typename goList<elementT>::ConstElement* el = this->getElements().getFrontElement();
    goList<goVectord>::ConstElement* elPos = myPrivate->positions.getFrontElement();
    while (el && elPos && i < sz)
    {
        goIndex_t c = 0;
        goIndex_t c_min = this->getCluster()[i];
        goDouble spatialFactor = (elPos->elem - myPrivate->meanPositions[c_min]).abs();
        spatialFactor = exp(-spatialFactor); // spatialFactor * spatialFactor * spatialFactor;
        goDouble minDist = this->distance(el->elem,this->getMeans()[c_min])
            * spatialFactor;
        for (c = 0; c < K; ++c)
        {
            spatialFactor = (elPos->elem - myPrivate->meanPositions[c]).abs();
            spatialFactor = exp(-spatialFactor); // spatialFactor * spatialFactor * spatialFactor;
            goDouble dist = this->distance(el->elem,this->getMeans()[c])
                * spatialFactor;
            if (dist < minDist)
            {
                minDist = dist;
                c_min = c;
            }
        }
        if (c_min != this->getCluster()[i])
        {
            ++changedElements;
            this->getCluster()[i] = c_min;
        }
        ++i;
        el = el->next;
        elPos = elPos->next;
    }
    return changedElements;
}

template <class elementT>
bool goKMeansSpatial<elementT>::update ()
{
    if (this->getCluster().getSize() != static_cast<goSize_t>(this->getElements().getSize()))
    {
        goLog::error("update(): cluster indices array is of different size than element list.",this);
        return false;
    }

    if (myPrivate->meanPositions.getSize() != this->getMeans().getSize())
    {
        goLog::error("update(): meanPositions and means must be of the same size.",this);
        return false;
    }
    
    goIndex_t sz = this->getElements().getSize();
    goIndex_t K  = this->getMeans().getSize();
    goFixedArray<elementT>  sums(K);
    goFixedArray<goVectord> sumPositions(K);
    goFixedArray<goSize_t>  counts(K);
    counts.fill(0);
    sums.fill(elementT(0));
    typename goList<elementT>::ConstElement* el = this->getElements().getFrontElement();
    goList<goVectord>::ConstElement* elPos = myPrivate->positions.getFrontElement();
    goIndex_t i = 0;
    while (el && elPos && i < sz)
    {
        goIndex_t c = this->getCluster()[i];
        sums[c] += el->elem;
        sumPositions[c] += elPos->elem;
        ++counts[c];
        ++i;
        el = el->next;
        elPos = elPos->next;
    }
    for (i = 0; i < K; ++i)
    {
        this->getMeans()[i] = sums[i];
        myPrivate->meanPositions[i] = sumPositions[i];
        if (counts[i] > 0)
        {
            goFloat f = 1.0f / static_cast<goFloat>(counts[i]);
            this->getMeans()[i] *= f;
            myPrivate->meanPositions[i] *= f;
        }
    }

    return true;
}

template <>
bool goKMeansSpatial<goVectord>::update ()
{
    if (this->getCluster().getSize() != static_cast<goSize_t>(this->getElements().getSize()))
    {
        goLog::error("update(): cluster indices array is of different size than element list.",this);
        return false;
    }

    if (myPrivate->meanPositions.getSize() != this->getMeans().getSize())
    {
        goLog::error("update(): meanPositions and means must be of the same size.",this);
        return false;
    }
    
    goIndex_t sz = this->getElements().getSize();
    goIndex_t K  = this->getMeans().getSize();
    goFixedArray<goVectord>  sums(K);
    goFixedArray<goVectord>  sumPositions(K);
    goFixedArray<goSize_t>   counts(K);
    counts.fill(0);
    {
        goVectord zeroVec (this->getMeans()[0].getSize());
        zeroVec.fill(0.0);
        sums.fill(zeroVec);
        zeroVec.setSize(myPrivate->meanPositions[0].getSize());
        sumPositions.fill(zeroVec);
    }
    goList<goVectord>::ConstElement* el = this->getElements().getFrontElement();
    goList<goVectord>::ConstElement* elPos = myPrivate->positions.getFrontElement();
    goIndex_t i = 0;
    while (el && elPos && i < sz)
    {
        goIndex_t c = this->getCluster()[i];
        sums[c] += el->elem;
        sumPositions[c] += elPos->elem;
        ++counts[c];
        ++i;
        el = el->next;
        elPos = elPos->next;
    }
    for (i = 0; i < K; ++i)
    {
        this->getMeans()[i] = sums[i];
        myPrivate->meanPositions[i] = sumPositions[i];
        if (counts[i] > 0)
        {
            goFloat f = 1.0f / static_cast<goFloat>(counts[i]);
            this->getMeans()[i] *= f;
            myPrivate->meanPositions[i] *= f;
        }
    }

    return true;
}
template <class elementT>
bool goKMeansSpatial<elementT>::addPosition (const goVectord& p)
{
    return myPrivate->positions.append(p);
}

template <class elementT>
const goList<goVectord>& goKMeansSpatial<elementT>::getPositions () const
{
    return myPrivate->positions;
}

template class goKMeansSpatial <goVectord>;
