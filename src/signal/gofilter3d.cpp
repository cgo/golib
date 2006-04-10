/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log: gofilter3d.cpp,v $
 * Revision 1.1  2002/10/26 15:45:32  christian
 * 3d filter class
 *
 */

#include <gofilter3d.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <math.h>
#include <assert.h>

#include <gotype.h>
#ifndef GOTYPE_HPP
# include <gotype.hpp>
#endif

template <class T_IN, class T_OUT>
goFilter3D<T_IN, T_OUT>::goFilter3D ()
    : goObjectBase (),
      myMask       (),
      myMaskCenterX  (0),
      myMaskCenterY  (0),
      myMaskCenterZ  (0)
{
    setClassName ("goFilter3D");
    myMask.setDataType (GO_FLOAT);
    myMask.make (3, 3, 3);
    GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goFloat*)__ptr = 0.33333333f, myMask);
    myMaskCenterX = 1;
    myMaskCenterY = 1;
    myMaskCenterZ = 1;
}

template <class T_IN, class T_OUT>
goFilter3D<T_IN, T_OUT>::~goFilter3D ()
{
}

template <class T_IN, class T_OUT>
goFilter3D<T_IN, T_OUT>::goFilter3D (const goFilter3D<T_IN, T_OUT>& other)
{
    setMask (other.getMask ());
    setMaskCenter (other.getMaskCenterX (),
                   other.getMaskCenterY (),
                   other.getMaskCenterZ ());
}

/*!
 * Sets the filter mask.
 * @param mask A goSignal3DBase containing the filter mask. Must be
 *             of data type GO_FLOAT.
 * @param normalize If true, the filter coefficients are normalized
 *        so that their sum equals one (default)
 * @return True on success, false otherwise
 */
template <class T_IN, class T_OUT>
bool
goFilter3D<T_IN, T_OUT>::setMask (const goSignal3DBase<void>& mask,
                                  bool normalize)
{
    myMask.destroy ();
    myMask.make (mask.getSizeX      (),
                 mask.getSizeY      (),
                 mask.getSizeZ      (),
                 mask.getBlockSizeX (),
                 mask.getBlockSizeY (),
                 mask.getBlockSizeZ (),
                 mask.getBorderX    (),
                 mask.getBorderY    (),
                 mask.getBorderZ    ());
  
    goDouble normalizationFactor = 1.0f;
    
    if (normalize)
    {
        normalizationFactor = 0.0f;
        GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (normalizationFactor += (fabs(*(const goFloat*)__ptr)), mask);

        assert (normalizationFactor != 0.0f);

        normalizationFactor = 1.0f / normalizationFactor;
    }
    
   GO_SIGNAL3D_EACHELEMENT_2_GENERIC (*(goFloat*)__ptr = normalizationFactor * *(const goFloat*)__ptr_target, myMask, mask);

   return true;
}

template <class T_IN, class T_OUT>
void
goFilter3D<T_IN, T_OUT>::setMaskCenter (goIndex_t x, goIndex_t y, goIndex_t z)
{
   myMaskCenterX = x; 
   myMaskCenterY = y; 
   myMaskCenterZ = z; 
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterX () const
{
    return myMaskCenterX;
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterY () const
{
    return myMaskCenterY;
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterZ () const
{
    return myMaskCenterZ;
}

template <class T_IN, class T_OUT>
const goSignal3D<void>&
goFilter3D<T_IN, T_OUT>::getMask () const
{
    return this->myMask;
}

template <class in_T, class out_T, class mask_T>
static bool filterFunction (goSignal3DBase<void>& inSignal, goSignal3DBase<void>& outSignal, const goSignal3DBase<void>& mask, goIndex_t myMaskCenterX, goIndex_t  myMaskCenterY, goIndex_t myMaskCenterZ)
{
    assert (outSignal.getDataType().getID() == GO_FLOAT);
    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
    
    goSubSignal3D<void> inCoeff (&inSignal, 
                                 mask.getSizeX(),
                                 mask.getSizeY(),
                                 mask.getSizeZ());

    goSize_t x, y, z;
    x = 0; y = 0; z = 0;

//    goUInt8*     xPtrIn;
//    goUInt8*     yPtrIn;
//    goUInt8*     zPtrIn;
    goUInt8*     xPtrOut;
    goUInt8*     yPtrOut;
//    goUInt8*     zPtrOut;
//    goPtrdiff_t* xDiffIn;
//    goPtrdiff_t* yDiffIn;
    goPtrdiff_t* xDiffOut;
    goPtrdiff_t* yDiffOut;
    
    goDouble     cumulationBuffer = 0.0f;
    
    for (z = 0; z < outSignal.getSizeZ(); ++z)
    {
        yPtrOut  = (goUInt8*)outSignal.getPtr   (0, 0, z);
        yDiffOut = outSignal.getYDiff ();

        for (y = 0; y < outSignal.getSizeY(); ++y)
        {
            xPtrOut  = (goUInt8*)yPtrOut;
            xDiffOut = outSignal.getXDiff ();

            for (x = 0; x < outSignal.getSizeX(); ++x)
            {
                inCoeff.setPosition (x - myMaskCenterX, y - myMaskCenterY, z - myMaskCenterZ);
                cumulationBuffer = 0.0f;
                GO_SIGNAL3D_EACHELEMENT_2_GENERIC (cumulationBuffer += *(in_T*)(__ptr) * *(const mask_T*)__ptr_target, inCoeff, mask);
                *(out_T*)xPtrOut = (out_T)cumulationBuffer;
                xPtrOut += *xDiffOut;
                ++xDiffOut;
            }
            yPtrOut += *yDiffOut;
            ++yDiffOut;
        }
    }
    
    return true;
}


/**
 * @brief Filters a signal.
 *
 * @note Only the implementation for <void,void> type is tested.
 * 
 * @param inSignal  The signal to be filtered, may be of any type 
 *                  in case it is a giSignal3DBase<void>.
 * @param outSignal  Will contain the filtered signal
 *                   after the method returned true.
 *                   Must currently be of type GO_FLOAT in case of
 *                   input and output types are void and must be
 *                   of the same size as inSignal.
 *
 * @return True if successful, false otherwise.
 **/
template<> bool
goFilter3D<void, void>::filter (goSignal3DBase<void>& inSignal,
                                goSignal3DBase<void>& outSignal)
{
    assert (outSignal.getDataType().getID() == GO_FLOAT);
    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
    
    if (outSignal.getDataType().getID() != GO_FLOAT)
    {
        return false;
    }
   
    if (inSignal.getDataType().getID() == GO_FLOAT)
    {
        return filterFunction<goFloat,goFloat,goFloat> (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ);
    }
    if (inSignal.getDataType().getID() == GO_DOUBLE)
    {
        return filterFunction<goDouble,goFloat,goFloat> (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ);
    }
    
    
    goSubSignal3D<void> inCoeff (&inSignal, 
                                 myMask.getSizeX(),
                                 myMask.getSizeY(),
                                 myMask.getSizeZ());

    goSize_t x, y, z;
    x = 0; y = 0; z = 0;

//    goUInt8*     xPtrIn;
//    goUInt8*     yPtrIn;
//    goUInt8*     zPtrIn;
    goUInt8*     xPtrOut;
    goUInt8*     yPtrOut;
//    goUInt8*     zPtrOut;
//    goPtrdiff_t* xDiffIn;
//    goPtrdiff_t* yDiffIn;
    goPtrdiff_t* xDiffOut;
    goPtrdiff_t* yDiffOut;
    
    goDouble     cumulationBuffer = 0.0f;

    goIndexFunction  indexFunction = inSignal.getDataType().getIndexFunction();
    goIndex_t minIndex             = inSignal.getDataType().getMinIndex();
    goIndex_t maxIndex             = inSignal.getDataType().getMaxIndex();
    
    goArray<goFloat> floatLUT;
    goFloat*         LUTOrigin = NULL;
    LUTOrigin = goCreateQuantizationTable (inSignal.getDataType(),
                                           0.0f, 1.0f, minIndex, maxIndex, floatLUT);
    if (!LUTOrigin)
    {
        return false;
    }
//    {
//        goDouble delta = 1.0 / (goDouble)(maxIndex - minIndex);
//        floatLUT.resize (maxIndex - minIndex + 1);
//        LUTOrigin = floatLUT.getPtr() - minIndex;
//        goIndex_t i;
//        goDouble value = 0.0;
//        for (i = minIndex; i <= maxIndex; ++i)
//        {
//            LUTOrigin[i] = (goFloat)value;
//            value += delta;
//        }
//    }
    
    for (z = 0; z < outSignal.getSizeZ(); ++z)
    {
        yPtrOut  = (goUInt8*)outSignal.getPtr   (0, 0, z);
        yDiffOut = outSignal.getYDiff ();

        for (y = 0; y < outSignal.getSizeY(); ++y)
        {
            xPtrOut  = (goUInt8*)yPtrOut;
            xDiffOut = outSignal.getXDiff ();

            for (x = 0; x < outSignal.getSizeX(); ++x)
            {
                inCoeff.setPosition (x - myMaskCenterX, y - myMaskCenterY, z - myMaskCenterZ);
                cumulationBuffer = 0.0f;
                GO_SIGNAL3D_EACHELEMENT_2_GENERIC (cumulationBuffer += LUTOrigin[indexFunction(__ptr)] * *(goFloat*)__ptr_target, inCoeff, myMask);
                *(goFloat*)xPtrOut = (goFloat)cumulationBuffer;
                xPtrOut += *xDiffOut;
                ++xDiffOut;
            }
            yPtrOut += *yDiffOut;
            ++yDiffOut;
        }
    }
    
    return true;
}

/*
 * @brief Start filter operation.
 *
 * @param inSignal   Input signal of any type.
 * @param outSignal  Output signal. Must be of same size as inSignal
 *                   and must be of type GO_FLOAT in case of
 *                   template parameters <void, void>.
 *
 * @return True if successful, false otherwise.
 */
#if 1
template <class T_IN, class T_OUT>
bool
goFilter3D<T_IN, T_OUT>::filter (goSignal3DBase<T_IN>&  inSignal,
                                 goSignal3DBase<T_OUT>& outSignal)
{
    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
    
    goSubSignal3D<T_IN> inCoeff (&inSignal, 
                                 myMask.getSizeX(),
                                 myMask.getSizeY(),
                                 myMask.getSizeZ());

    goSize_t x, y, z;
    x = 0; y = 0; z = 0;

//    T_IN*        xPtrIn;
//    T_IN*        yPtrIn;
//    T_IN*        zPtrIn;
    T_OUT*       xPtrOut;
    T_OUT*       yPtrOut;
//    T_OUT*       zPtrOut;
//    goPtrdiff_t* xDiffIn;
//    goPtrdiff_t* yDiffIn;
    goPtrdiff_t* xDiffOut;
    goPtrdiff_t* yDiffOut;
    
    goDouble     cumulationBuffer = 0.0f;
    goSize_t sz = myMask.getSizeZ();
    goSize_t sy = myMask.getSizeY();
    goSize_t sx = myMask.getSizeX();
    goUInt8* maskPtrZ;
    goUInt8* maskPtrY;
    goUInt8* maskPtrX;
    T_IN* xPtr;
    T_IN* yPtr;
    T_IN* zPtr;
    goPtrdiff_t* inCoeffDx;
    goPtrdiff_t* inCoeffDy;
    goPtrdiff_t* inCoeffDz;
    goPtrdiff_t* maskDx;
    goPtrdiff_t* maskDy;
    goPtrdiff_t* maskDz;
    goPtrdiff_t* maskDxSave = myMask.getXDiff();
    goPtrdiff_t* maskDySave = myMask.getYDiff();
    goPtrdiff_t* maskDzSave = myMask.getZDiff();
    for (z = 0; z < outSignal.getSizeZ(); ++z)
    {
        yPtrOut  = outSignal.getPtr   (0, 0, z);
        yDiffOut = outSignal.getYDiff ();

        for (y = 0; y < outSignal.getSizeY(); ++y)
        {
            xPtrOut  = yPtrOut;
            xDiffOut = outSignal.getXDiff ();

            for (x = 0; x < outSignal.getSizeX(); ++x)
            {
                inCoeff.setPosition (x - myMaskCenterX, y - myMaskCenterY, z - myMaskCenterZ);
                cumulationBuffer = 0.0f;
                goSize_t __i, __j, __k;					
                maskPtrZ  = (goUInt8*)myMask.getPtr(0,0,0);
                zPtr      = inCoeff.getPtr(0,0,0);
                inCoeffDz = inCoeff.getZDiff();
                maskDz    = maskDzSave;
                for (__i = 0; __i < sz; ++__i)		
                {
                    maskPtrY  = maskPtrZ;
                    yPtr      = zPtr;
                    inCoeffDy = inCoeff.getYDiff();
                    maskDy    = maskDySave;
                    for (__j = 0; __j < sy; ++__j)     	
                    {
                        maskPtrX  = maskPtrY;
                        xPtr      = yPtr;
                        inCoeffDx = inCoeff.getXDiff();
                        maskDx    = maskDxSave;
                        for (__k = 0; __k < sx; ++__k)
                        {
                            cumulationBuffer += *xPtr * *(goFloat*)maskPtrX;
                            xPtr += *maskDx;
                            xPtr += *inCoeffDx;
                            ++maskDx;
                            ++inCoeffDx;
                        }
                        maskPtrY += *maskDy;
                        yPtr     += *inCoeffDy;
                        ++maskDy;
                        ++inCoeffDy;
                    }
                    maskPtrZ += *maskDz;
                    zPtr     += *inCoeffDz;
                    ++maskDz;
                    ++inCoeffDz;
                }
                // GO_SIGNAL3D_EACHELEMENT_2 (cumulationBuffer += *__ptr * *__ptr_target,
                //                           myMask, inCoeff, filter3d_mask_t, T_IN);
                *xPtrOut = (T_OUT)cumulationBuffer;
                xPtrOut += *xDiffOut;
                ++xDiffOut;
            }

            yPtrOut += *yDiffOut;
            ++yDiffOut;
        }
    }
    
    return true;
}
#endif


template class goFilter3D<goFloat, goFloat>;
template class goFilter3D<goFloat, goDouble>;
template class goFilter3D<goDouble, goFloat>;
template class goFilter3D<goDouble, goDouble>;
// template class goFilter3D<void, void>;
