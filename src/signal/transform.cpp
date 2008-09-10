#include <gomath.h>
#include <gosignal3d.h>
#include <gosignal3dgenericiterator.h>
#include <gosubsignal3d.h>
#include <gomathsignal.h>

//= Bilinear sampling at x,y
template <class T>
static inline T sample (const goSignal3DBase<void>& s, goFloat x, goFloat y)
{
    goIndex_t x1 = goIndex_t(x);
    goIndex_t y1 = goIndex_t(y);

    goUInt8* Ap = (goUInt8*)s.getPtr (x1,y1,0);
    goFloat A = *(T*)Ap;
    goPtrdiff_t xd = s.getXDiff()[x1];
    goPtrdiff_t yd = s.getYDiff()[y1];
    goFloat B = *(T*)(Ap + xd);
    goFloat C = *(T*)(Ap + yd);
    goFloat D = *(T*)(Ap + xd + yd);
    goFloat __px = (x - float(x1));
    goFloat __py = (y - float(y1));
    goFloat __p1 = A + ((B - A)*__px);
    goFloat __p2 = C + ((D - C)*__px);
    return static_cast<T>((__p1 + ((__p2 - __p1)*__py)));
}

template <class T>
static bool transformSignal2D2 (goDouble scale, goDouble theta, goDouble t_x, goDouble t_y,
        const goSignal3DBase<void>& phi, goSignal3D<void>& target)
{
//    if (target.getDataType().getID() != phi.getDataType().getID() ||
//        target.getSize() != phi.getSize())
//    {
//        target.setDataType (phi.getDataType().getID());
//        target.make (phi.getSize(), phi.getBlockSize(), phi.getBorderSize(), 1);
//    }

    // printf ("transformSignal2D2(): Got parameters scale %f, angle %f, tx %f, ty %f\n", scale, theta, t_x, t_y);

    goDouble rot_mat[] = {::cos(theta), -::sin(theta), 
                          ::sin(theta),  ::cos(theta)};
    //goDouble width = goDouble(target.getSizeX());
    //goDouble height = goDouble(target.getSizeY());
    goDouble source_width = phi.getSizeX();
    goDouble source_height = phi.getSizeY();
    // goDouble centre[] = {(float)width * 0.5, (float)height * 0.5};
    goDouble source_centre[] = {(float)source_width * 0.5, (float)source_height * 0.5};

    goSignal3DGenericIterator targetIt (&target);

    while (!targetIt.endZ())
    {
        targetIt.resetY();
        goDouble y = 0.0;
        while (!targetIt.endY())
        {
            targetIt.resetX();
            goDouble x = 0.0;
            while (!targetIt.endX())
            {
                goDouble trans_x = (scale * ((x - source_centre[0]) * rot_mat[0] + (y - source_centre[1]) * rot_mat[1]) + source_centre[0] + t_x);
                goDouble trans_y = (scale * ((x - source_centre[0]) * rot_mat[2] + (y - source_centre[1]) * rot_mat[3]) + source_centre[1] + t_y);

                if (trans_x < 0.0 || trans_x >= source_width ||
                    trans_y < 0.0 || trans_y >= source_height)
                {
                    // Ausserhalb des Zielbildes ---
                    if (trans_x < 0.0)
                        trans_x = 0;
                    if (trans_x >= source_width)
                        trans_x = source_width - 1;
                    if (trans_y < 0.0)
                        trans_y = 0;
                    if (trans_y >= source_height)
                        trans_y = source_height - 1;
                }
                // Samplen
                *(T*)*targetIt = sample<T> (phi, trans_x, trans_y);

                targetIt.incrementX();
                x += 1.0;
            }
            targetIt.incrementY();
            y += 1.0;
        }
        targetIt.incrementZ();
    }
    return true;
}

template <class T>
static bool paste2D2 (goDouble scale, goDouble theta, goDouble t_x, goDouble t_y,
        const goSignal3DBase<void>& phi, goSignal3DBase<void>& target, T bgColour)
{
//    if (target.getDataType().getID() != phi.getDataType().getID() ||
//        target.getSize() != phi.getSize())
//    {
//        target.setDataType (phi.getDataType().getID());
//        target.make (phi.getSize(), phi.getBlockSize(), phi.getBorderSize(), 1);
//    }

    goDouble st = ::sin (theta), ct = ::cos (theta);
    goDouble rot_mat[] = {ct, -st, 
                          st,  ct};
    //goDouble width = goDouble(target.getSizeX());
    //goDouble height = goDouble(target.getSizeY());
    goDouble source_width = phi.getSizeX();
    goDouble source_height = phi.getSizeY();
    // goDouble centre[] = {(float)width * 0.5, (float)height * 0.5};
    goDouble source_centre[] = {(float)source_width * 0.5, (float)source_height * 0.5};

    goSignal3DGenericIterator targetIt (&target);

    T value = T(0);
    while (!targetIt.endZ())
    {
        targetIt.resetY();
        goDouble y = 0.0;
        while (!targetIt.endY())
        {
            targetIt.resetX();
            goDouble x = 0.0;
            while (!targetIt.endX())
            {
                goDouble trans_x = (scale * ((x - source_centre[0]) * rot_mat[0] + (y - source_centre[1]) * rot_mat[1]) + source_centre[0] + t_x);
                goDouble trans_y = (scale * ((x - source_centre[0]) * rot_mat[2] + (y - source_centre[1]) * rot_mat[3]) + source_centre[1] + t_y);

                if (!(trans_x < 0.0 || trans_x >= source_width ||
                     trans_y < 0.0 || trans_y >= source_height))
                {
                    // Innerhalb des Zielbildes ---
                    // Samplen
                    value = sample<T> (phi, trans_x, trans_y);
                    if (value != bgColour)
                    {
                        *(T*)*targetIt = value;
                    }
                }
                targetIt.incrementX();
                x += 1.0;
            }
            targetIt.incrementY();
            y += 1.0;
        }
        targetIt.incrementZ();
    }
    return true;
}

template <class T>
static bool scale2D2 (const goSignal3DBase<void>& image, goSignal3DBase<void>& target, bool keep_aspect)
{
    if (target.getDataType().getID() != image.getDataType().getID())
        return false;

    goDouble step[] = {target.getSizeX() > 1 ? ((float)(image.getSizeX()-1) / (float)(target.getSizeX()-1)) : 1.0, 
                       target.getSizeY() > 1 ? ((float)(image.getSizeY()-1) / (float)(target.getSizeY()-1)) : 1.0,
                       target.getSizeZ() > 1 ? ((float)(image.getSizeZ()-1) / (float)(target.getSizeZ()-1)) : 1.0};

    if (keep_aspect)
    {
        // goDouble temp = goMath::min<goDouble> (step[0], goMath::min<goDouble>(step[1],step[2]));
        goDouble temp = goMath::min<goDouble> (step[0], step[1]);
        step[0] = temp;
        step[1] = temp;
        step[2] = temp;
    }

    goSignal3DGenericIterator targetIt (&target);

    goDouble z = 0.0;
    while (!targetIt.endZ())
    {
        targetIt.resetY();
        goDouble y = 0.0;
        while (!targetIt.endY())
        {
            targetIt.resetX();
            goDouble x = 0.0;
            while (!targetIt.endX())
            {
                // Samplen
                *(T*)*targetIt = sample<T> (image, x, y);

                targetIt.incrementX();
                x += step[0];
            }
            targetIt.incrementY();
            y += step[1];
        }
        targetIt.incrementZ();
        z += step[2];
    }
    return true;
}

/** \addtogroup math
 * @{
 */
/**
 * @brief 2D euclidean transformation of an image.
 * 
 * Calculate
 * \f$ target(x) = source(s\cdot\Gamma\cdot x + T) \f$
 *
 * @param source Source image (2D)
 * @param scale  Scale factor
 * @param angle  Rotation angle
 * @param t_x    Translation in x
 * @param t_y    Translation in y
 * @param target Target, will be resized and set to the data type of \c source if necessary.
 * @param setsize Automatically set size of target if it does not fit. Default: true.
 */
void goMath::transform2D (
        const goSignal3DBase<void>& source, 
        goDouble scale,
        goDouble angle,
        goDouble t_x,
        goDouble t_y,
        goSignal3D<void>& target,
        bool setsize)
{
    if (!setsize && source.getDataType().getID() != target.getDataType().getID())
    {
        goLog::error ("goMath::transform2D(): setsize is false but data types differ.");
        return;
    }
    if (source.getDataType().getID() != target.getDataType().getID() ||
            source.getSize() != target.getSize() && setsize)
    {
        target.setDataType (source.getDataType().getID());
        target.make (&source);
    }
    int ch = source.getChannel();
    goSize_t channels = goMath::max (source.getChannelCount(), target.getChannelCount());
    for (goSize_t i = 0; i < channels; ++i)
    {
        const_cast<goSignal3DBase<void>*>(&source)->setChannel(i);
        target.setChannel(i);
        switch (target.getDataType().getID())
        {
            case GO_UINT8: transformSignal2D2<goUInt8> (scale, angle, t_x, t_y, source, target); break;
            case GO_INT8: transformSignal2D2<goInt8> (scale, angle, t_x, t_y, source, target); break;
            case GO_UINT16: transformSignal2D2<goUInt16> (scale, angle, t_x, t_y, source, target); break;
            case GO_INT16: transformSignal2D2<goInt16> (scale, angle, t_x, t_y, source, target); break;
            case GO_UINT32: transformSignal2D2<goUInt32> (scale, angle, t_x, t_y, source, target); break;
            case GO_INT32: transformSignal2D2<goInt32> (scale, angle, t_x, t_y, source, target); break;
            case GO_FLOAT: transformSignal2D2<goFloat> (scale, angle, t_x, t_y, source, target); break;
            case GO_DOUBLE: transformSignal2D2<goDouble> (scale, angle, t_x, t_y, source, target); break;
            default: goLog::error ("goMath::transform2D(): unsupported data type.");
        }
    }
    const_cast<goSignal3DBase<void>*>(&source)->setChannel(ch);
    target.setChannel(0);
}

/** 
 * @brief Scale \c source into \c target
 * 
 * @todo Extend to 3D -- this only requires to write a 3D sampling function.
 *
 * @param source Original 2D signal
 * @param target Target 2D signal
 * @param keep_aspect If true, keep aspect ratio
 */
void goMath::scale2D (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, bool keep_aspect)
{
    if (source.getDataType().getID() != target.getDataType().getID())
    {
        goLog::error ("goMath::scale2D(): data types must match.");
        return;
    }
    if (target.getSizeX() < 2 || target.getSizeY() < 2 ||
        source.getSizeX() < 2 || source.getSizeY() < 2)
    {
        goLog::error ("goMath::scale2D(): sizes must be >= 2.");
        return;
    }

    int ch = source.getChannel();
    goSize_t channels = goMath::max (source.getChannelCount(), target.getChannelCount());
    for (goSize_t i = 0; i < channels; ++i)
    {
        const_cast<goSignal3DBase<void>*>(&source)->setChannel(i);
        target.setChannel(i);
        switch (target.getDataType().getID())
        {
            case GO_UINT8: scale2D2<goUInt8> (source, target, keep_aspect); break;
            case GO_INT8: scale2D2<goInt8> (source, target, keep_aspect); break;
            case GO_UINT16: scale2D2<goUInt16> (source, target, keep_aspect); break;
            case GO_INT16: scale2D2<goInt16> (source, target, keep_aspect); break;
            case GO_UINT32: scale2D2<goUInt32> (source, target, keep_aspect); break;
            case GO_INT32: scale2D2<goInt32> (source, target, keep_aspect); break;
            case GO_FLOAT: scale2D2<goFloat> (source, target, keep_aspect); break;
            case GO_DOUBLE: scale2D2<goDouble> (source, target, keep_aspect); break;
            default: goLog::error ("goMath::scale2D(): unsupported data type."); break;
        }
    }
    const_cast<goSignal3DBase<void>*>(&source)->setChannel(ch);
    target.setChannel(0);
}

/** 
 * @brief Paste \c source at under some euclidean transformations into \c target.
 * 
 * @param source Source patch
 * @param scale  scale factor
 * @param angle  rotation angle
 * @param t_x    Translation x
 * @param t_y    Translation y
 * @param target target signal (is left unchanged except for the pasted \c source)
 * @param bgColour Transparent colour in \c source. Only points which have different colour values are
 *                 pasted into \c target.
 */
void goMath::paste2D (
        const goSignal3DBase<void>& source, 
        goDouble scale,
        goDouble angle,
        goDouble t_x,
        goDouble t_y,
        goSignal3D<void>& target,
        goFloat bgColour)
{
    if (source.getDataType().getID() != target.getDataType().getID())
    {
        goLog::error ("goMath::paste2D(): data types differ.");
        return;
    }

#if 0
    goDouble st = ::sin (angle), ct = ::cos (angle);
    goDouble rot_mat[] = {ct, -st, 
                          st,  ct};
    goDouble width = goDouble(target.getSizeX());
    goDouble height = goDouble(target.getSizeY());
    goDouble source_width = source.getSizeX();
    goDouble source_height = source.getSizeY();
    // goDouble centre[] = {(float)width * 0.5, (float)height * 0.5};
    goDouble source_centre[] = {(float)source_width * 0.5, (float)source_height * 0.5};
    goMath::Vectord sc (source_centre, 2, 1);
    goMath::Matrixd R (rot_mat, 2, 2);
    goDouble x0_[] = {0.0, 0.0};
    goDouble x1_[] = {source_width-1, 0.0};
    goDouble y0_[] = {0.0, 0.0};
    goDouble y1_[] = {0.0, source_height-1};
    goDouble corners_[] = {0.0, source_width-1, 0.0, 0.0,
                          0.0, 0.0           , 0.0, source_height-1};
    goMath::Matrixd corners (corners_, 2, 4);
    for (goSize_t i = 0; i < 4; ++i)
    {
        goMath::Vectord temp(0);
        corners.refRow (i, temp);
        temp -= sc;
    }
    
    goMath::Matrixd trans_corners = R * corners * scale;
    goMath::Vectord X(0), Y(0);
    trans_corners.refRow (0, X);
    trans_corners.refRow (1, Y);
    goDouble max_x = goMath::max(X);
    goDouble min_x = goMath::min(X);
    goDouble max_y = goMath::max(Y);
    goDouble min_y = goMath::min(Y);

    goSize3D target_patch_size (max_x - min_x, max_y - min_y, 1);
    goDouble patch_position[] = {-t_x, -t_y, 0};
    patch_position[0] = goMath::max (0.0, goMath::min (patch_position[0], width - 1));
    patch_position[1] = goMath::max (0.0, goMath::min (patch_position[1], height - 1));
    target_patch_size.x = goMath::min<goDouble> (width - 1 - patch_position[0], target_patch_size.x);
    target_patch_size.y = goMath::min<goDouble> (height - 1 - patch_position[1], target_patch_size.y);

    printf ("patch size: %f , %f\n", target_patch_size.x, target_patch_size.x);
    printf ("patch position: %f , %f\n", patch_position[0], patch_position[1]);

    goSubSignal3D<void> target_patch (&target, target_patch_size);
    target_patch.setPosition (patch_position[0], patch_position[1], patch_position[2]);
#endif
    int ch = source.getChannel();
    goSize_t channels = goMath::min (source.getChannelCount(), target.getChannelCount());
    for (goSize_t i = 0; i < channels; ++i)
    {
        const_cast<goSignal3DBase<void>*>(&source)->setChannel(i);
        target.setChannel(i);
        // target_patch.setChannel(i);
        switch (target.getDataType().getID())
        {
            case GO_UINT8: paste2D2<goUInt8> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_INT8: paste2D2<goInt8> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_UINT16: paste2D2<goUInt16> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_INT16: paste2D2<goInt16> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_UINT32: paste2D2<goUInt32> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_INT32: paste2D2<goInt32> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_FLOAT: paste2D2<goFloat> (scale, angle, t_x, t_y, source, target, bgColour); break;
            case GO_DOUBLE: paste2D2<goDouble> (scale, angle, t_x, t_y, source, target, bgColour); break;
            // case GO_UINT8: paste2D2<goUInt8> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_INT8: paste2D2<goInt8> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_UINT16: paste2D2<goUInt16> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_INT16: paste2D2<goInt16> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_UINT32: paste2D2<goUInt32> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_INT32: paste2D2<goInt32> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_FLOAT: paste2D2<goFloat> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            // case GO_DOUBLE: paste2D2<goDouble> (scale, angle, 0.0, 0.0, source, target_patch, bgColour); break;
            default: goLog::error ("goMath::transform2D(): unsupported data type.");
        }
    }
    const_cast<goSignal3DBase<void>*>(&source)->setChannel(ch);
    target.setChannel(0);
}
/** @} */
