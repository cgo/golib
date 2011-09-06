/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gokmeans.h>
#include <golog.h>
#include <godefs.h>
#include <gofixedarray.h>
#include <golist.h>
#include <govector.h>

#include <assert.h>

template <class elementT> class goKMeansPrivate
{
    public:
        goKMeansPrivate ()
            : means (), elementList ()
        {
        };

        ~goKMeansPrivate ()
        {
        };

        goFixedArray<elementT>  means;
        goList<elementT>        elementList;
        goFixedArray<goIndex_t> cluster;
};

template <class elementT>
goKMeans<elementT>::goKMeans()
    : myPrivate (0)
{
    this->setClassID(GO_KMEANS);
    myPrivate = new goKMeansPrivate<elementT>;
    assert(myPrivate);
}

template <class elementT>
goKMeans<elementT>::~goKMeans()
{
    if (this->myPrivate)
    {
        delete this->myPrivate;
        this->myPrivate = 0;
    }
}

template <class elementT>
bool goKMeans<elementT>::initialise (const goFixedArray<elementT>& initMeans)
{
    myPrivate->means = initMeans;
    return true;
}

template <class elementT>
goSize_t goKMeans<elementT>::assignment ()
{
    goIndex_t K = myPrivate->means.getSize();
    goIndex_t sz = myPrivate->elementList.getSize();
    if (myPrivate->cluster.getSize() != static_cast<goSize_t>(sz))
    {
        myPrivate->cluster.setSize(sz);
        myPrivate->cluster.fill(0);
    }
    goSize_t changedElements = 0;
    goIndex_t i = 0;
    typename goList<elementT>::ConstElement* el = myPrivate->elementList.getFrontElement();
    while (el && i < sz)
    {
        goIndex_t c = 0;
        goIndex_t c_min = myPrivate->cluster[i];
        goDouble minDist = this->distance(el->elem,myPrivate->means[c_min]);
        for (c = 0; c < K; ++c)
        {
            goDouble dist = this->distance(el->elem,myPrivate->means[c]);
            if (dist < minDist)
            {
                minDist = dist;
                c_min = c;
            }
        }
        if (c_min != myPrivate->cluster[i])
        {
            ++changedElements;
            myPrivate->cluster[i] = c_min;
        }
        ++i;
        el = el->next;
    }
    return changedElements;
}

template <class elementT>
bool goKMeans<elementT>::update () 
{
    if (myPrivate->cluster.getSize() != myPrivate->elementList.getSize())
    {
        goLog::error("means(): cluster indices array is of different size than element list.",this);
        return false;
    }

    goIndex_t sz = myPrivate->elementList.getSize();
    goIndex_t K  = myPrivate->means.getSize();
    goFixedArray<elementT> sums(K);
    goFixedArray<goSize_t> counts(K);
    counts.fill(0);
    sums.fill(elementT(0));
    typename goList<elementT>::ConstElement* el = myPrivate->elementList.getFrontElement();
    goIndex_t i = 0;
    while (el && i < sz)
    {
        sums[myPrivate->cluster[i]] += el->elem;
        ++counts[myPrivate->cluster[i]];
        ++i;
        el = el->next;
    }
    for (i = 0; i < K; ++i)
    {
        myPrivate->means[i] = sums[i];
        if (counts[i] > 0)
        {
            goFloat f = 1.0f / static_cast<goFloat>(counts[i]);
            myPrivate->means[i] *= f;
        }
    }

    return true;
}

template <>
bool goKMeans<goMath::Vector<goDouble> >::update () 
{
    if (myPrivate->cluster.getSize() != static_cast<goSize_t>(myPrivate->elementList.getSize()))
    {
        goLog::error("update(): cluster indices array is of different size than element list.",this);
        return false;
    }

    goIndex_t sz = myPrivate->elementList.getSize();
    goIndex_t K  = myPrivate->means.getSize();
    goFixedArray<goMath::Vector<goDouble> > sums(K);
    goFixedArray<goSize_t> counts(K);
    counts.fill(0);
    goMath::Vector<goDouble> zeroVec (myPrivate->means[0].getSize());
    zeroVec.fill(0.0);
    sums.fill(zeroVec);
    goList<goMath::Vector<goDouble> >::ConstElement* el = myPrivate->elementList.getFrontElement();
    goIndex_t i = 0;
    while (el && i < sz)
    {
        sums[myPrivate->cluster[i]] += el->elem;
        ++counts[myPrivate->cluster[i]];
        ++i;
        el = el->next;
    }
    for (i = 0; i < K; ++i)
    {
        myPrivate->means[i] = sums[i];
        if (counts[i] > 0)
        {
            goFloat f = 1.0f / static_cast<goFloat>(counts[i]);
            myPrivate->means[i] *= f;
        }
    }

    return true;
}

template <class elementT>
goDouble goKMeans<elementT>::distance (const elementT& e1, const elementT& e2) const
{
    return fabs(e1 - e2);
}

template <>
goDouble goKMeans<goMath::Vector<goDouble> >::distance (const goMath::Vector<goDouble>& e1, const goMath::Vector<goDouble>& e2) const
{
    return (e1-e2).abs();
}


template <class elementT>
bool goKMeans<elementT>::addElement (const elementT& e)
{
    return myPrivate->elementList.append(e);
}

template <class elementT>
const goList<elementT>& goKMeans<elementT>::getElements() const
{
    return myPrivate->elementList;
}

template <class elementT>
goList<elementT>& goKMeans<elementT>::getElements() 
{
    return myPrivate->elementList;
}

template <class elementT>
const goFixedArray<elementT>& goKMeans<elementT>::getMeans () const
{
    return myPrivate->means;
}

template <class elementT>
goFixedArray<elementT>& goKMeans<elementT>::getMeans () 
{
    return myPrivate->means;
}

template <class elementT>
const goFixedArray<goIndex_t>& goKMeans<elementT>::getCluster () const
{
    return myPrivate->cluster;
}

template <class elementT>
goFixedArray<goIndex_t>& goKMeans<elementT>::getCluster () 
{
    return myPrivate->cluster;
}

template class goKMeansPrivate<goMath::Vector<goDouble> >;
template class goKMeans<goMath::Vector<goDouble> >;
