#ifndef GOSIGNAL_H
#define GOSIGNAL_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#include <gosignalhelper.h>

namespace goSignal
{
    extern const goSize3D _defaultBlockSize3D;
    extern const goSize3D _defaultBlockSize2D;

    const goSize3D& defaultBlockSize3D ();
    const goSize3D& defaultBlockSize2D ();

    bool canny (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret);

    bool convert (goSignal3DBase<void>& source, goSignal3DBase<void>& target, ptrdiff_t* source_chan, ptrdiff_t* target_chan, int channelCount);

    bool RGB2BGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target);
};

#endif
