/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gokmeansspatial.h>
#include <golog.h>
#include <godefs.h>

class goKMeansSpatialPrivate
{
    public:
        goKMeansSpatialPrivate() : positions () {};
        ~goKMeansSpatialPrivate() {};

        goList<goMath::Vector<goDouble> >       positions;
        goFixedArray<goMath::Vector<goDouble> > meanPositions;
};

template <class elementT>
goKMeansSpatial<elementT>::goKMeansSpatial()
    : goKMeans<elementT>(), myPrivate(0)
{
    this->setClassID(GO_KMEANSSPATIAL);
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
bool goKMeansSpatial<elementT>::initialisePositions (const goFixedArray<goMath::Vector<goDouble> >& initMeanPos)
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
    goList<goMath::Vector<goDouble> >::ConstElement* elPos = myPrivate->positions.getFrontElement();
    while (el && elPos && i < sz)
    {
        goIndex_t c = 0;
        goIndex_t c_min = this->getCluster()[i];
        goDouble spatialFactor = (elPos->elem - myPrivate->meanPositions[c_min]).abs();
        spatialFactor = spatialFactor * spatialFactor * spatialFactor;
        goDouble minDist = this->distance(el->elem,this->getMeans()[c_min])
            * spatialFactor;
        for (c = 0; c < K; ++c)
        {
            spatialFactor = (elPos->elem - myPrivate->meanPositions[c]).abs();
            spatialFactor = spatialFactor * spatialFactor * spatialFactor;
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
    goFixedArray<goMath::Vector<goDouble> > sumPositions(K);
    goFixedArray<goSize_t>  counts(K);
    counts.fill(0);
    sums.fill(elementT(0));
    typename goList<elementT>::ConstElement* el = this->getElements().getFrontElement();
    goList<goMath::Vector<goDouble> >::ConstElement* elPos = myPrivate->positions.getFrontElement();
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
bool goKMeansSpatial<goMath::Vector<goDouble> >::update ()
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
    goFixedArray<goMath::Vector<goDouble> >  sums(K);
    goFixedArray<goMath::Vector<goDouble> >  sumPositions(K);
    goFixedArray<goSize_t>   counts(K);
    counts.fill(0);
    {
        goMath::Vector<goDouble>  zeroVec (this->getMeans()[0].getSize());
        zeroVec.fill(0.0);
        sums.fill(zeroVec);
        zeroVec.setSize(myPrivate->meanPositions[0].getSize());
        sumPositions.fill(zeroVec);
    }
    goList<goMath::Vector<goDouble> >::ConstElement* el = this->getElements().getFrontElement();
    goList<goMath::Vector<goDouble> >::ConstElement* elPos = myPrivate->positions.getFrontElement();
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
bool goKMeansSpatial<elementT>::addPosition (const goMath::Vector<goDouble> & p)
{
    return myPrivate->positions.append(p);
}

template <class elementT>
const goList<goMath::Vector<goDouble> >& goKMeansSpatial<elementT>::getPositions () const
{
    return myPrivate->positions;
}

template class goKMeansSpatial <goMath::Vector<goDouble> >;
