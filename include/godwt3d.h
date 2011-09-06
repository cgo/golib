/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GODWT3D_H
#define GODWT3D_H

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
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

class goDWT3DPrivate;

/**
 * \addtogroup signal
 * @{
 */
/** 
 * @brief 3D discrete wavelet transform.
 *
 * @author Christian Gosch
 */
class
goDWT3D : public goObjectBase
{
    public:
        goDWT3D ();
        virtual ~goDWT3D ();

        enum
        {
            LOW = 0,
            HIGH = 1
        };
        
        enum
        {
            HAAR,
            D4,
            D8
        };
        
        bool dwt            (goSignal3DBase<void>* sig, int axes, goTypeEnum dwtType);
        bool dwt            (goDWT3D& parentDWT, int axes = GO_X|GO_Y|GO_Z);
        bool idwt           (goSignal3D<void>* target);
        bool idwt           (goDWT3D& parentDWT);
        bool setFilter      (int filterEnum, int upsample = 0);
        void setFrameMode   (bool m = true);

        goFixedArray<goAutoPtr<goSignal3D<void> > >& getDWT ();
      
    private:
        bool calculateDWT (goSignal3DBase<void>& sig, goTypeEnum dwtType);
        // For regular dwt operation
        bool upsampleFilter (goSignal3DBase<void>& L, goSignal3DBase<void>& H, goSignal3D<void>& target);
        bool downsampleFilter (goSignal3DBase<void>& sig, goSignal3D<void>& L, goSignal3D<void>& H);
        // For pyramid operation
        bool reconstruct (goSignal3DBase<void>& L, goSignal3DBase<void>& H, goSignal3D<void>& target);
        bool filter (goSignal3DBase<void>& sig, goSignal3D<void>& L, goSignal3D<void>& H);
        
    private:
        goDWT3DPrivate* myPrivate;
};
/** @} */

#endif
