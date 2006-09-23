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

/** 
 * @brief Calculate the divergence of a 2D vector field given by 2 goSignal3DBase.
 * 
 * Calculates \f$ D = \nabla V \f$, with V a two dimensional vector field over 
 * real numbers, using central differences. So 
 * \f$ D \approx \frac{\partial V}{\partial x} + \frac{\partial V}{\partial y} \f$
 *
 * The input must be of data type GO_FLOAT or GO_DOUBLE and
 * both components must be of the same data type and size.
 *
 * @param x X components of the vector field.
 * @param y Y components of the vector field.
 * @param hx  Grid spacing in X direction (set this to 1.0 for now).
 * @param hy  Grid spacing in Y direction (set this to 1.0 for now).
 * @param retValue  Divergence, will be allocated to the same data type 
 *   and size as the input data only if necessary.
 * 
 * @return True if successful, false otherwise.
 */
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
    if (retValue.getSize() != x.getSize() ||
        retValue.getDataType().getID() != x.getDataType().getID())
    {
        retValue.setDataType (x.getDataType().getID());
        retValue.make (x.getSizeX(), x.getSizeY(), x.getSizeZ(),
                x.getBlockSizeX(), x.getBlockSizeY(), x.getBlockSizeZ(),
                4, 4, 4);
    }

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
