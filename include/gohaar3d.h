#ifndef GOHAAR3D_H
#define GOHAAR3D_H

#include <gosignal3dbase.h>
#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOSUBSIGNAL3D_H
# include <gosubsignal3d.h>
#endif

class goHaar3DPrivate;

class
goHaar3D : public goObjectBase
{
    public:
        goHaar3D ();
        virtual ~goHaar3D ();

        enum
        {
            LOW = 0,
            HIGH = 1
        };
        
        bool haar (const goSignal3DBase<void>* sig, int axes, goTypeEnum dwtType);
        bool haar (goHaar3D& parentDWT, int axes);
        // bool haar   (goSignal3DBase<void>* sig, goIndex_t stages);
        bool unHaar ();
        
        goSignal3DBase<void>* getTransform  ();
        bool getBand (goSubSignal3D<void>* retBand, int x_band = LOW, int y_band = LOW, int z_band = LOW);
      
    private:
        bool calculateHaar ();
        
    private:
        goHaar3DPrivate* myPrivate;
};

#endif
