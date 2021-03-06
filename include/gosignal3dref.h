/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSIGNAL3DREF_H
#define GOSIGNAL3DREF_H

#include <gosignal3dbase.h>

/**
 * @addtogroup signal
 * @{
 */
/** 
 * @brief Wrapper for any memory area suitable for
 * representation through a goSignal3DBase type object
 * (e.g. simply linearly stored data)
 */
class goSignal3DRef : public goSignal3DBase<void>
{
    public:
        goSignal3DRef ();
        goSignal3DRef (void* data_ptr, 
                       goTypeEnum data_type, 
                       const goSize3D& size, 
                       const goSize3D& blockSize, 
                       const goSize3D& borderSize, 
                       goSize_t channelCount);
        goSignal3DRef (void* data_ptr,
                       goTypeEnum data_type,
                       goSize_t sx, goSize_t sy = 1, goSize_t sz = 1,
                       goSize_t channelCount = 1);
        virtual ~goSignal3DRef ();
        void ref (void* data_ptr, 
                  goTypeEnum data_type, 
                  const goSize3D& size, 
                  const goSize3D& blockSize, 
                  const goSize3D& borderSize, 
                  goSize_t channelCount);
        void ref (goSignal3DBase<void>& s);
        void ref (goSignal3DBase<void>& s, const goSize3D& borderSize);
};
/** @} */

#endif
