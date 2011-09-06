/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOKMEANS_H
#define GOKMEANS_H
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
template <class elementT> class goKMeansPrivate;

/*!
 * \addtogroup learning
 * @{
 */
/** 
 * @brief K-means clustering
 */
template <class elementT>
class goKMeans : public goObjectBase
{
    public:
        goKMeans();
        virtual ~goKMeans();

        bool     initialise (const goFixedArray<elementT>& initMeans);
        //= Specialise this for a distance measure.
        virtual goDouble distance   (const elementT& e1, const elementT& e2) const;
        virtual goSize_t assignment ();

        //= Specialise this when standard mean calculation is not appropriate.
        virtual bool update ();

        bool                           addElement  (const elementT& e);
        const goList<elementT>&        getElements () const;
        const goFixedArray<elementT>&  getMeans    () const;
        const goFixedArray<goIndex_t>& getCluster  () const;

        goList<elementT>&        getElements ();
        goFixedArray<elementT>&  getMeans    ();
        goFixedArray<goIndex_t>& getCluster  ();

    private:
        goKMeansPrivate<elementT>* myPrivate;
};
/*!
 * @} 
 */
#endif
