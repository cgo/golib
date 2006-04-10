#include <gomath.h>
#include <golog.h>
#include <gosignal3dgenericiterator.h>
#include <gotype.h>

template <class T>
static bool divergence_ (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue)
{
    // calculate central difference derivatives of x and y
    // and add them.
    goSignal3D<void> ddy_y;
    ddy_y.setDataType (y.getDataType().getID());
    if (!goMath::centralDifferences (x, retValue, 0, hx))
        return false;
    if (!goMath::centralDifferences (y, ddy_y, 1, hy))
        return false;
    retValue += ddy_y;
    return true;
}

bool goMath::divergence (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue)
{
    if (x.getDataType().getID() != y.getDataType().getID())
    {
        goLog::error ("goMath::divergence(): x and y must be of the same type.");
        return false;
    }
    if (x.getSizeX() != y.getSizeX() ||
        x.getSizeY() != y.getSizeY())
    {
        goLog::error ("goMath::divergence(): x and y must be of the same size.");
        return false;
    }
    retValue.setDataType (x.getDataType().getID());
    retValue.make (x.getSizeX(), x.getSizeY(), x.getSizeZ(),
                   x.getBlockSizeX(), x.getBlockSizeY(), x.getBlockSizeZ(),
                   4, 4, 4);

    switch (x.getDataType().getID())
    {
        case GO_FLOAT: return divergence_<goFloat> (x,y,hx,hy,retValue); break;
        case GO_DOUBLE: return divergence_<goDouble> (x,y,hx,hy,retValue); break;
        default: 
            {
                goLog::error ("goMath::divergence(): Unsupported data type.");
                return false;
            }
    }
}
