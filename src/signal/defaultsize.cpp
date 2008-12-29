#include <gosignal.h>

namespace goSignal
{
    const goSize3D _defaultBlockSize3D (32, 32, 32);
    const goSize3D _defaultBlockSize2D (32, 32, 1);

    const goSize3D& defaultBlockSize3D ()
    {
        return _defaultBlockSize3D;
    }

    const goSize3D& defaultBlockSize2D ()
    {
        return _defaultBlockSize2D;
    }
};
