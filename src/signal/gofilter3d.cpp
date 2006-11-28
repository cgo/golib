/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log: gofilter3d.cpp,v $
 * Revision 1.1.1.1  2006/04/19 15:26:29  gosch
 * golib local cvs
 *
 * Revision 1.1  2002/10/26 15:45:32  christian
 * 3d filter class
 *
 */

#include <gofilter3d.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <golog.h>
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
    this->setClassID(GO_FILTER3D);
    myMask.setDataType (GO_FLOAT);
    myMask.make (3, 3, 3);
    GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goFloat*)__ptr = 0.33333333f, myMask);
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
                                  bool normalize, goDouble factor)
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
  
    goDouble normalizationFactor = 1.0;
    
    if (normalize)
    {
        if (factor == 0.0)
        {
            normalizationFactor = 0.0;
            GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (normalizationFactor += (fabs(*(const goFloat*)__ptr)), mask);

            assert (normalizationFactor != 0.0);

            normalizationFactor = 1.0 / normalizationFactor;
        }
        else
        {
            normalizationFactor = factor;
        }
    }
    
   GO_SIGNAL3D_EACHELEMENT_2_GENERIC (*(goFloat*)__ptr = normalizationFactor * *(const goFloat*)__ptr_target, myMask, mask);

   return true;
}

template <class T_IN, class T_OUT>
bool goFilter3D<T_IN,T_OUT>::setMask (const goFloat* mask, goIndex_t sizeX, goIndex_t sizeY, goIndex_t sizeZ, bool normalize, goDouble factor)
{
    if (!mask)
    {
        return false;
    }
    goDouble normalizationFactor = 1.0;
    goIndex_t count = sizeX * sizeY * sizeZ;
    if (normalize)
    {
        if (factor == 0.0)
        {
            goIndex_t i;
            const goFloat* p = mask;
            normalizationFactor = 0.0;
            for (i = 0; i < count; ++i,++p)
            {
                normalizationFactor += *p;
            }
            assert (normalizationFactor != 0.0);
            normalizationFactor = 1.0 / normalizationFactor;
        }
        else
        {
            normalizationFactor = factor;
        }
    }
    myMask.destroy ();
    myMask.make (sizeX, sizeY, sizeZ, sizeX, sizeY, sizeZ, 0, 0, 0);
    const goFloat* p = mask;
    GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goFloat*)__ptr = normalizationFactor * *p; ++p, myMask);
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
static bool filterFunction2 (goSignal3DBase<void>& inSignal, goSignal3DBase<void>& outSignal, const goSignal3DBase<void>& mask, goIndex_t myMaskCenterX, goIndex_t  myMaskCenterY, goIndex_t myMaskCenterZ)
{
    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
    
    goSubSignal3D<void> inCoeff (&inSignal, 
                                 mask.getSizeX(),
                                 mask.getSizeY(),
                                 mask.getSizeZ());

    goSize_t x, y, z;
    x = 0; y = 0; z = 0;

    goUInt8*     xPtrOut;
    goUInt8*     yPtrOut;
    goPtrdiff_t* xDiffOut;
    goPtrdiff_t* yDiffOut;
    
    goDouble     cumulationBuffer = 0.0;
    
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
                GO_SIGNAL3D_EACHELEMENT_2_GENERIC (cumulationBuffer += (goDouble)(*(in_T*)(__ptr)) * *(const mask_T*)__ptr_target, inCoeff, mask);
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

template <class in_T, class mask_T>
static bool filterFunction (goSignal3DBase<void>& inSignal, goSignal3DBase<void>& outSignal, const goSignal3DBase<void>& mask, goIndex_t myMaskCenterX, goIndex_t  myMaskCenterY, goIndex_t myMaskCenterZ)
{
    switch (outSignal.getDataType().getID())
    {
        case GO_INT8:
            {
                return filterFunction2<in_T,goInt8,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_UINT8:
            {
                return filterFunction2<in_T,goUInt8,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_INT16:
            {
                return filterFunction2<in_T,goInt16,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_UINT16:
            {
                return filterFunction2<in_T,goUInt16,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_INT32:
            {
                return filterFunction2<in_T,goInt32,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_UINT32:
            {
                return filterFunction2<in_T,goUInt32,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_FLOAT:
            {
                return filterFunction2<in_T,goFloat,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        case GO_DOUBLE:
            {
                return filterFunction2<in_T,goDouble,mask_T> (inSignal,outSignal,mask,myMaskCenterX,myMaskCenterY,myMaskCenterZ);
            }
            break;
        default:
            {
                goLog::error("goFilter3D<>: filterFunction: Unknown data type for output signal. Not filtering.");
                return false;
            }
    }
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
 *
 * @return True if successful, false otherwise.
 **/
template<> bool
goFilter3D<void, void>::filter (goSignal3DBase<void>& inSignal,
                                goSignal3DBase<void>& outSignal)
{
    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
   
    assert (myMaskCenterX >= 0 && myMaskCenterX < static_cast<goIndex_t>(myMask.getSizeX()));
    assert (myMaskCenterY >= 0 && myMaskCenterY < static_cast<goIndex_t>(myMask.getSizeY()));
    assert (myMaskCenterZ >= 0 && myMaskCenterZ < static_cast<goIndex_t>(myMask.getSizeZ()));
    
    switch (inSignal.getDataType().getID())
    {
        case GO_INT8:   return filterFunction<goInt8,goFloat>   (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_UINT8:  return filterFunction<goUInt8,goFloat>  (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_INT16:  return filterFunction<goInt16,goFloat>  (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_UINT16: return filterFunction<goUInt16,goFloat> (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_INT32:  return filterFunction<goInt32,goFloat>  (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_UINT32: return filterFunction<goUInt32,goFloat> (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_FLOAT:  return filterFunction<goFloat,goFloat>  (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        case GO_DOUBLE: return filterFunction<goDouble,goFloat> (inSignal, outSignal, myMask, myMaskCenterX, myMaskCenterY, myMaskCenterZ); break;
        default: goLog::error("filter(): unknown data type for input signal. Not filtering.",this); return false; break;
    }
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
template class goFilter3D<void, void>;
