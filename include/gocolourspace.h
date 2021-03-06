/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOCOLOURSPACE_H
#define GOCOLOURSPACE_H

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOSIGNAL3DGENERICITERATOR_H
# include <gosignal3dgenericiterator.h>
#endif
#ifndef GOMATH_H
# include <gomath.h>
#endif

//= The goYUV422_* functions are not used and probably don't have an application.
//= They were made on the way while figuring out how our camera
//= stores YUV data.
//= goYUV420P_RGB() is actually doing the job.

/*!
 * \addtogroup signal
 * \addtogroup video
 * @{
 */
static inline goFloat goYUV422_Red (goUInt8 yuv)
{
    // Y + 1.14 * V
    return (static_cast<float>(yuv >> 4) + 1.140 * static_cast<float>(yuv & 3)) / 255.0;
}
static inline goFloat goYUV422_Green (goUInt8 yuv)
{
    // Y - 0.394 * U - 0.581 * V
    return (static_cast<float>(yuv >> 4) - 0.581 * static_cast<float>(yuv & 3) - 0.394 * static_cast<float>((yuv >> 2) & 3)) / 255.0;
}
static inline goFloat goYUV422_Blue (goUInt8 yuv)
{
    // Y - 2.032 * U
    return (static_cast<float>(yuv >> 4) - 
            2.032 * static_cast<float>((yuv >> 2) & 3)) / 255.0;
}

static inline goFloat goYUV422_Red (goUInt8 y, goUInt8 u, goUInt8 v)
{
    // Y + 1.14 * V
    return (static_cast<float>(y) + 1.140 * static_cast<float>(u)) / 255.0;
}
static inline goFloat goYUV422_Green (goUInt8 y, goUInt8 u, goUInt8 v)
{
    // Y - 0.394 * U - 0.581 * V
    return (static_cast<float>(y) - 0.581 * static_cast<float>(v) - 0.394 * static_cast<float>(u)) / 255.0;
}
static inline goFloat goYUV422_Blue (goUInt8 y, goUInt8 u, goUInt8 v)
{
    // Y - 2.032 * U
    return (static_cast<float>(y) - 
            2.032 * static_cast<float>(u)) / 255.0;
}
/*! @} */

/** 
 * \addtogroup video
 * @{
 */
/*!
 * @brief Converts a planar YUV420 image to RGB.
 *
 * Converts a planar YUV420 image, stored linearly in memory,
 * to an RGB goSignal3D (goUInt8, 3 channels).
 *     @note: This does not really upsample the downsampled 
 *          U/V (Cr/Cb) values but just uses the left neighbour for
 *          non-existent samples.
 * @param yuv Pointer to the planar yuv420 data, e.g. from the video4linux driver
 * of Philips webcams (and probably others).
 * @param width  Width of the image
 * @param height Height of the image
 * @param ret    The goSignal3D<void> containing the RGB image after 
 * @param rgb_offsets Contains 3 integers each denoting the channel number in \c ret of
 *                    r, g, and b channels respectively.
 * the function returned. If it is not of appropriate size/type, nothing will be converted.
 *
 * @author Christian Gosch
 */
static inline bool goYUV420P_RGB_base (goUInt8* yuv, goSize_t width, goSize_t height, goSignal3DBase<void>& ret, const int* rgb_offsets)
{
    if (ret.getSizeX() != width || ret.getSizeY() != height || ret.getChannelCount() < 3 || ret.getDataType().getID() != GO_UINT8)
    {
        return false;
//        ret.setDataType(GO_UINT8);
//        ret.make (goSize3D(width,height,1),goSize3D(32,32,1),goSize3D(4,4,0),3);
    }
    assert(yuv);
 
    ret.setChannel(0);
    goSignal3DGenericIterator it(&ret);
    goUInt8* y_p = yuv;
    goIndex_t cr_width = width >> 1;
    goIndex_t cr_height = height >> 1;
    goUInt8* cb_py = yuv + (width * height);
    goUInt8* cr_py = cb_py + (cr_width * cr_height);

    goIndex_t y = 0;

    int r_chan = rgb_offsets[0];
    int g_chan = rgb_offsets[1];
    int b_chan = rgb_offsets[2];
    //= Note: FIXME This does not really upsample the downsampled 
    //=       U/V (Cr/Cb) values but just uses the left neighbour for
    //=       non-existent samples.
    while(!it.endY())
    {
        goUInt8* cr_p = cr_py;
        goUInt8* cb_p = cb_py;
        goIndex_t x = 0;
        it.resetX();
        while (!it.endX())
        {
            goFloat cr = (static_cast<float>(*cr_p) - 128.0f);
            goFloat cb = (static_cast<float>(*cb_p) - 128.0f);
            goFloat Y  = (static_cast<float>(*y_p) - 16.0f);
            goFloat R = Y * 1.164f + cr * 1.596f;
            goFloat G = Y * 1.164f - cr * 0.813f - cb * 0.392f;
            goFloat B = Y * 1.164f + cb * 2.017f;
            //= Clamp values to [0,255]
            R = goMath::min<goFloat>(goMath::max<goFloat>(0.0f,R),255.0f);
            G = goMath::min<goFloat>(goMath::max<goFloat>(0.0f,G),255.0f);
            B = goMath::min<goFloat>(goMath::max<goFloat>(0.0f,B),255.0f);
            *((goUInt8*)*it + r_chan) = static_cast<goUInt8>(R);
            //= G = 1.164x(Y-16) - 0.813�(Cr-128) - 0.392�(Cb-128)
            *((goUInt8*)*it + g_chan) = static_cast<goUInt8>(G);
            //= B = 1.164�(Y-16) + 2.017�(Cb-128)
            *((goUInt8*)*it + b_chan) = static_cast<goUInt8>(B);
            cr_p += x;
            cb_p += x;
            x = x ^ 1;
            ++y_p;
            it.incrementX();
        }
        cr_py += y * (cr_width);
        cb_py += y * (cr_width);
        y = y ^ 1;
        it.incrementY();
    }
    return true;
}

/*!
 * @brief Converts a planar YUV420 image to RGB.
 *
 * Converts a planar YUV420 image, stored linearly in memory,
 * to an RGB goSignal3D (goUInt8, 3 channels).
 *     @note: This does not really upsample the downsampled 
 *          U/V (Cr/Cb) values but just uses the left neighbour for
 *          non-existent samples.
 * @param yuv Pointer to the planar yuv420 data, e.g. from the video4linux driver
 * of Philips webcams (and probably others).
 * @param width  Width of the image
 * @param height Height of the image
 * @param ret    The goSignal3D<void> containing the RGB image after 
 * the function returned. If it is not of appropriate size/type, it will be 
 * reallocated.
 * @param rgb_channels Contains 3 integers each denoting the channel number in \c ret of
 *                    r, g, and b channels respectively.
 *
 * @author Christian Gosch
 */
static inline bool goYUV420P_RGB (goUInt8* yuv, goSize_t width, goSize_t height, goSignal3D<void>& ret, const int* rgb_channels)
{
    if (ret.getSizeX() != width || ret.getSizeY() != height || ret.getChannelCount() < 3 || ret.getDataType().getID() != GO_UINT8)
    {
        ret.setDataType(GO_UINT8);
        ret.make (goSize3D(width,height,1),goSize3D(32,32,1),goSize3D(8,8,0),3);
    }
    return goYUV420P_RGB_base (yuv, width, height, ret, rgb_channels);
}
/*! @} */
#endif
