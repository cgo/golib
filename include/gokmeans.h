#ifndef GOKMEANS_H
#define GOKMEANS_H
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
template <class elementT> class goKMeansPrivate;

template <class elementT>
class goKMeans : public goObjectBase
{
    public:
        goKMeans();
        virtual ~goKMeans();

        bool     initialise (const goFixedArray<elementT>& initMeans);
        //= Specialise this for a distance measure.
        goDouble distance   (const elementT& e1, const elementT& e2) const;
        virtual goSize_t assignment ();

        //= Specialise this when standard mean calculation is not appropriate.
        virtual bool update ();

        bool                           addElement  (const elementT& e);
        const goList<elementT>&        getElements () const;
        const goFixedArray<elementT>&  getMeans    () const;
        const goFixedArray<goIndex_t>& getCluster  () const;

    protected:
        goFixedArray<elementT>&  getMeans   ();
        goFixedArray<goIndex_t>& getCluster ();

    private:
        goKMeansPrivate<elementT>* myPrivate;
};

#endif
