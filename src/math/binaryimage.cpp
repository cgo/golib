#include <gomath.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>

template <class T, class sigT>
static inline void comp_point (int j, int jp1, const goMatrix<T>& MM, goSignal3D<void>& binimg)
{
    int y1 = int(floor(MM(j,1)));
    int y2 = int(floor(MM(jp1,1)));
    int x1 = int(floor(MM(j,0)));
    int x2 = int(floor(MM(jp1,0)));

    int ymax = binimg.getSizeY () - 1;
    int xmax = binimg.getSizeX () - 1;
    if (y1 < 0)
        y1 = 0;
    else if (y1 > ymax)
        y1 = ymax;
    if (y2 < 0)
        y2 = 0;
    else if (y2 > ymax)
        y2 = ymax;
    if (x1 < 0)
        x1 = 0;
    else if (x1 > xmax)
        x1 = xmax;
    if (x2 < 0)
        x2 = 0;
    else if (x2 > xmax)
        x2 = xmax;

    if (y1 > y2)
    {
        int temp = y1;
        y1 = y2;
        y2 = temp;
        temp = x1;
        x1 = x2;
        x2 = temp;
    }
    for (int y = y1; y < y2; ++y)
    {
        float t = float(y - y2) / float(y1 - y2);
        int x = 0;
        if (t >= 0.0 && t <= 1.0)
        {
            x = int(t*float(x1) + (1-t)*float(x2));
            sigT* v = (sigT*)binimg.getPtr(x,y,0);
            if (*v == sigT(0))
            {
                *v = sigT(1);
            }
            else
            {
                *v = sigT(0);
            }
        }
    }
}

template <class T, class sigT>
static inline bool binaryImage2 (const goMatrix<T>& boundary, goSignal3D<void>& ret, goSize_t width, goSize_t height)
{
    if (ret.getSizeX() != width || ret.getSizeY() != height)
    {
        ret.make (goSize3D(width,height,1), goSize3D(width,height,1), goSize3D(4,4,0), 1);
    }
    ret.fill (0.0f);
    
    for (unsigned int j = 0; j < boundary.getRows() - 1; ++j)
    {
        comp_point<T,sigT> (j, j+1, boundary, ret);
    }
    comp_point<T,sigT> (boundary.getRows() - 1, 0, boundary, ret);

    goSignal3DGenericIterator it (&ret);

    sigT temp;
    sigT value = sigT(0);
    while (!it.endY())
    {
        it.resetX();
        while (!it.endX())
        {
            temp = *(sigT*)*it;
            *(sigT*)*it = value;
            if (temp != sigT(0))
            {
                if (value == sigT(0))
                    value = sigT(1);
                else
                    value = sigT(0);
            }
            it.incrementX();
        }
        it.incrementY();
    }
    return true;

    // return goCopySignalArray (binimg.getPtr(), &ret);
}

/** \addtogroup math
 * @{
 */
/** 
 * @brief Calculate a binary image bounded by the closed curve
 * described by the Nx2 point configuration matrix \c boundary.
 * 
 * @note No bounds check is done, so if the curve points fall out of the 
 * bounds of the image size, (e.g. negative values), you will get segfaults.
 *
 * @param boundary Closed curve point configuration matrix.
 * @param ret      On return, contains the binary image.
 *                 The type must be set by the user.
 * @param width    Width of the binary image
 * @param height   Width of the binary image
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::binaryImage (const goMatrix<T>& boundary, goSignal3D<void>& ret, goSize_t width, goSize_t height)
{
    switch (ret.getDataType().getID())
    {
        case GO_UINT8: return binaryImage2<T,goUInt8> (boundary, ret, width, height); break;
        case GO_INT8: return binaryImage2<T,goInt8> (boundary, ret, width, height); break;
        case GO_UINT16: return binaryImage2<T,goUInt16> (boundary, ret, width, height); break;
        case GO_INT16: return binaryImage2<T,goInt16> (boundary, ret, width, height); break;
        case GO_UINT32: return binaryImage2<T,goUInt32> (boundary, ret, width, height); break;
        case GO_INT32: return binaryImage2<T,goInt32> (boundary, ret, width, height); break;
        case GO_FLOAT: return binaryImage2<T,goFloat> (boundary, ret, width, height); break;
        case GO_DOUBLE: return binaryImage2<T,goDouble> (boundary, ret, width, height); break;
        default: return false; break;
    }
    return false;
}
/** @} */

template bool goMath::binaryImage<goFloat> (const goMatrix<goFloat>&, goSignal3D<void>&, goSize_t, goSize_t);
template bool goMath::binaryImage<goDouble> (const goMatrix<goDouble>&, goSignal3D<void>&, goSize_t, goSize_t);
