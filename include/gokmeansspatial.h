#ifndef GOKMEANSSPATIAL_H
#define GOKMEANSSPATIAL_H

#include <gokmeans.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

class goKMeansSpatialPrivate;

template <class elementT>
class goKMeansSpatial : public goKMeans<elementT>
{
    public:
        goKMeansSpatial();
        virtual ~goKMeansSpatial();
        
        bool             initialisePositions (const goFixedArray<goVectord>& initMeanPos);
        virtual goSize_t assignment ();
        virtual bool     update     ();

        //= For each element, add a position too.
        bool addPosition (const goVectord& p);
        
        const goList<goVectord>& getPositions () const;

    private:
        goKMeansSpatialPrivate* myPrivate;
};

#endif
