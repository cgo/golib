/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log$
 */

#include <gofilter3d.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>

#include <assert.h>

/*!
 * @class goFilter3D
 * Provides convolution of a filter mask with a given goSignal3DBase.
 * 
 * @author Christian Gosch
 */


template <class T_IN, class T_OUT>
goFilter3D<T_IN, T_OUT>::goFilter3D ()
    : goObjectBase (),
      myMask       (NULL),
      maskCenterX  (0),
      maskCenterY  (0),
      maskCenterZ  (0)
{
    setClassName ("goFilter3D");
    myMask = new goSignal3D<mask_t> (3, 3, 3);
    GO_SIGNAL3D_EACHELEMENT (*__ptr = 0.33333333f, (*myMask), mask_t);
}

template <class T_IN, class T_OUT>
goFilter3D<T_IN, T_OUT>::~goFilter3D ()
{
    if (myMask)
    {
        delete myMask;
        myMask = NULL;
    }
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
 * @param mask A goSignal3DBase containing the filter mask.
 * @param normalize If true, the filter coefficients are normalized
 *        so that their sum equals one (default)
 * @return True on success, false otherwise
 */
template <class T_IN, class T_OUT>
bool
goFilter3D<T_IN, T_OUT>::setMask (const goSignal3DBase<mask_t>& mask,
                                  bool normalize)
{
    if (myMask)
    {
        delete myMask;
    }

    myMask = new goSignal3D<mask_t> (mask.getSizeX      (),
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
        GO_SIGNAL3D_EACHELEMENT (normalizationFactor += (goDouble)*__ptr, mask, const mask_t);

        normalizationFactor = 1.0f / normalizationFactor;
    }
    
    
   GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = normalizationFactor * *__ptr_target, (*myMask), mask, mask_t, const mask_t);

   return true;
}

template <class T_IN, class T_OUT>
void
goFilter3D<T_IN, T_OUT>::setMaskCenter (goIndex_t x, goIndex_t y, goIndex_t z)
{
   maskCenterX = x; 
   maskCenterY = y; 
   maskCenterZ = z; 
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterX () const
{
    return maskCenterX;
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterY () const
{
    return maskCenterY;
}

template <class T_IN, class T_OUT>
goIndex_t
goFilter3D<T_IN, T_OUT>::getMaskCenterZ () const
{
    return maskCenterZ;
}

template <class T_IN, class T_OUT>
const goSignal3D<goFilter3D<T_IN, T_OUT>::mask_t>&
goFilter3D<T_IN, T_OUT>::getMask () const
{
    return *myMask;
}

template <class T_IN, class T_OUT>
bool
goFilter3D<T_IN, T_OUT>::filter (goSignal3DBase<T_IN>&  inSignal,
                                 goSignal3DBase<T_OUT>& outSignal)
{
    assert (myMask != NULL);

    assert (inSignal.getSizeX() == outSignal.getSizeX() &&
            inSignal.getSizeY() == outSignal.getSizeY() &&
            inSignal.getSizeZ() == outSignal.getSizeZ());
    
    goSubSignal3D<T_IN> inCoeff (&inSignal, 
                                 myMask->getSizeX(),
                                 myMask->getSizeY(),
                                 myMask->getSizeZ());

    goIndex_t x, y, z;
    x = 0; y = 0; z = 0;

    T_IN*        xPtrIn;
    T_IN*        yPtrIn;
    T_IN*        zPtrIn;
    T_OUT*       xPtrOut;
    T_OUT*       yPtrOut;
    T_OUT*       zPtrOut;
    goPtrdiff_t* xDiffIn;
    goPtrdiff_t* yDiffIn;
    goPtrdiff_t* xDiffOut;
    goPtrdiff_t* yDiffOut;
    
    goDouble     cumulationBuffer = 0.0f;

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
                inCoeff.setPosition (x - maskCenterX, y - maskCenterY, z - maskCenterZ);
                cumulationBuffer = 0.0f;
                GO_SIGNAL3D_EACHELEMENT_2 (cumulationBuffer += *__ptr * *__ptr_target,
                                           (*myMask), inCoeff, mask_t, T_IN);
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

template class goFilter3D<goFloat, goFloat>;
template class goFilter3D<goFloat, goDouble>;
template class goFilter3D<goDouble, goFloat>;
template class goFilter3D<goDouble, goDouble>;
