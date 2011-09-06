/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSIGNAL_H
#define GOSIGNAL_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNALHELPER_H
# include <gosignalhelper.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

/** 
 * @addtogroup signal
 * @{
 */
namespace goSignal
{
    extern const goSize3D _defaultBlockSize3D;
    extern const goSize3D _defaultBlockSize2D;

    const goSize3D& defaultBlockSize3D ();
    const goSize3D& defaultBlockSize2D ();

    bool canny (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goDouble thresh1 = 80.0, goDouble thresh2 = 40.0);
    bool smooth (goSignal3DBase<void>& sig, goSize_t width = 3);

    bool convert (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, const int* source_chan, const int* target_chan, int channelCount);

    bool RGB2BGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target);
    bool toBGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target, goFloat alpha = -1.0);
    goAutoPtr<goSignal3D<void> > toBGRA (goSignal3DBase<void>& source, goFloat alpha = -1.0);
};
/** @} */

#endif
