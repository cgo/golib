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
        
        bool             initialisePositions (const goFixedArray<goMath::Vectord>& initMeanPos);
        virtual goSize_t assignment ();
        virtual bool     update     ();

        //= For each element, add a position too.
        bool addPosition (const goMath::Vectord& p);
        
        const goList<goMath::Vectord>& getPositions () const;

    private:
        goKMeansSpatialPrivate* myPrivate;
};

#endif
