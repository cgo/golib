/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gofilter1d.cpp,v 1.1.1.1 2006/04/19 15:26:29 gosch Exp $
 */

#include <gofilter1d.h>
#include <gosignal3dgenericiterator.h>
#include <gosignal3d.h>
#include <gosignal3dref.h>
#include <goarray.h>
#include <golog.h>

class goFilter1DPrivate
{
    public:
        goFilter1DPrivate ();
        ~goFilter1DPrivate ();

        goArray<goFloat> mask;
        goIndex_t        center;
};

goFilter1DPrivate::goFilter1DPrivate ()
    : mask (), center (0)
{
}

goFilter1DPrivate::~goFilter1DPrivate ()
{
}

goFilter1D::goFilter1D ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goFilter1DPrivate;
}

/** 
 * @brief Constructor, setting the mask and center.
 * 
 * @param mask       Array with mask values.
 * @param center     Center of the mask.
 * @param normalize  If true, the mask is normalized to one.
 */
goFilter1D::goFilter1D (const goArray<goFloat>& mask, goIndex_t center, bool normalize)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goFilter1DPrivate;
    assert(myPrivate);
    this->setMask(mask);
    this->setCenter(center);
    if (normalize)
        this->normalize();
}

goFilter1D::goFilter1D (const goMath::Vector<goFloat>& mask, goIndex_t center, bool normalize)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goFilter1DPrivate;
    assert(myPrivate);
    this->setMask(mask);
    this->setCenter(center);
    if (normalize)
        this->normalize();
}

/** 
 * @brief Constructor, setting the mask and center.
 * 
 * @param mask       Pointer to mask values.
 * @param length     Length of the mask.
 * @param center     Center of the mask.
 * @param normalize  If true, the mask is normalized to one.
 */
goFilter1D::goFilter1D (const goFloat* mask, goIndex_t length, goIndex_t center, bool normalize)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goFilter1DPrivate;
    assert(myPrivate);
    this->setMask(mask, length);
    this->setCenter(center);
    if (normalize)
        this->normalize();
}

goFilter1D::~goFilter1D ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/**
 * @brief Set the filter mask.
 *
 * @note If the mask is not normalized, you can do that by calling 
 * this->normalize().
 * 
 * @param m  Float array containing the mask.
 *
 * @return True if successful, false otherwise.
 **/
bool goFilter1D::setMask (const goArray<goFloat>& m)
{
    myPrivate->mask = m;
    return true;
}

bool goFilter1D::setMask (const goMath::Vector<goFloat>& m)
{
    goSize_t sz = m.getSize();
    myPrivate->mask.resize (sz);
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->mask[i] = m[i];
    }
    return true;
}

/** 
 * @brief Set filter mask.
 * 
 * Sets the filter mask. Note that you need to set the filter centre with setCenter().
 * 
 * @param mask    Pointer to the mask data. Must not be null.
 * @param length  Length of the mask. Must be larger than zero.
 * 
 * @return  True if successful, false otherwise.
 * */
bool goFilter1D::setMask (const goFloat* mask, goIndex_t length)
{
    if (!mask || length <= 0)
        return false;
    assert (length > 0);
    assert (mask);
    myPrivate->mask.resize(length);
    memcpy(myPrivate->mask.getPtr(), mask, length * sizeof(goFloat));
    return true;
}

/**
 * @brief Set the center of the filter mask.
 *
 * @param c  Center index.
 *
 * @return True if successful, false otherwise.
 **/
bool goFilter1D::setCenter (goIndex_t c)
{
    myPrivate->center = c;
    return true;
}

template <class T>
static inline void _filter (goSignal3DBase<void>& sig, goArray<goFloat>& mask, goIndex_t center)
{
    goSignal3DGenericIterator it (&sig);
    goIndex_t maskSize = mask.getSize();
    goArray<T> lineBuffer (sig.getSizeX() + 2*maskSize);
    T* lineBufferP  = 0;
    T* lineBufferP2 = 0;
    // goIndex_t sx = sig.getSizeX();
    goIndex_t i;
    goIndex_t y;
    goIndex_t z = 0;
    goByte* lineP = 0;
    goPtrdiff_t* lineDiff = 0;
    goDouble accum = 0.0;
    goFloat* maskP;
    while(!it.endZ())
    {
        it.resetY();
        y = 0;
        while (!it.endY())
        {
            lineBufferP = &lineBuffer[0];
            lineP = reinterpret_cast<goByte*>(sig.getPtr(-maskSize, y, z));
            lineDiff = sig.getXDiff() - maskSize;
            for (i = 0; i < lineBuffer.getSize(); ++i)
            {
                *lineBufferP = *(T*)lineP;
                ++lineBufferP;
                lineP += *lineDiff;
                ++lineDiff;
            }
            lineBufferP = &lineBuffer[maskSize];
            it.resetX();
            while(!it.endX())
            {
                accum = 0;
                maskP = mask.getPtr();
                lineBufferP2 = lineBufferP - center;
                for (i = 0; i < maskSize; ++i)
                {
                    accum += *lineBufferP2 * *maskP;
                    ++maskP;
                    ++lineBufferP2;
                }
                *(T*)*it = (T)accum;
                it.incrementX();
                ++lineBufferP;
            }
            ++y;
            it.incrementY();
        }
        ++z;
        it.incrementZ();
    }
}

/**
 * @brief Apply the filter in-place.
 *
 * @param sig  Signal to apply the filter to.
 *
 * @return True if successful, false otherwise.
 **/
bool goFilter1D::filter (goSignal3DBase<void>& sig)
{
    if (sig.getBorderX() < myPrivate->mask.getSize())
    {
        goString msg = "filter(): signal ";
        msg += sig.getObjectName();
        msg += " has insufficient border size in X direction. Not filtering.";
        msg += " Border size: ";
        msg += (int) (sig.getBorderX());
        
        goLog::warning(msg,this);
        return false;
    }

    goSize_t channelCount = sig.getChannelCount ();
    goSize_t orgChan = sig.getChannel ();
    for (goSize_t i = 0; i < channelCount; ++i)
    {
        sig.setChannel (i);
        switch (sig.getDataType().getID())
        {
            case GO_INT8:   _filter<goInt8>   (sig, myPrivate->mask, myPrivate->center); break;
            case GO_UINT8:  _filter<goUInt8>  (sig, myPrivate->mask, myPrivate->center); break;
            case GO_INT16:  _filter<goInt16>  (sig, myPrivate->mask, myPrivate->center); break;
            case GO_UINT16: _filter<goUInt16> (sig, myPrivate->mask, myPrivate->center); break;
            case GO_INT32:  _filter<goInt32>  (sig, myPrivate->mask, myPrivate->center); break;
            case GO_UINT32: _filter<goUInt32> (sig, myPrivate->mask, myPrivate->center); break;
            case GO_FLOAT:  _filter<goFloat>  (sig, myPrivate->mask, myPrivate->center); break;
            case GO_DOUBLE: _filter<goDouble> (sig, myPrivate->mask, myPrivate->center); break;
            default: goLog::warning("filter(): Unknown data type.",this); return false; break;
        }
    }
    sig.setChannel (orgChan);
    return true;
}

template <class T, int type_enum>
static inline bool _filterMatrix (goMath::Matrix<T>& m, goArray<goFloat>& mask, goIndex_t center, int direction)
{
    goSize3D sz (m.getColumns(), m.getRows(), 1);
    //= "Simulate" the stride (not implemented in matrices yet) with number of channels -- 
    //= then filter only the first channel.
    goSignal3DRef refSig (m.getPtr(), type_enum, sz, sz, goSize3D(mask.getSize(), mask.getSize(), 0), 1); // m.getStride());
    if (direction == 1)
        refSig.swapXY();

    _filter<T> (refSig, mask, center);
    return true;
}

bool goFilter1D::filter (goMath::Matrixf& m, int direction)
{
    return _filterMatrix<goFloat,GO_FLOAT> (m, myPrivate->mask, myPrivate->center, direction);
}

bool goFilter1D::filter (goMath::Matrixd& m, int direction)
{
    return _filterMatrix<goDouble,GO_DOUBLE> (m, myPrivate->mask, myPrivate->center, direction);
}

/**
 * @brief Normalize the filter mask.
 *
 * @param constant  Constant to normalize to. Default is 1.0.
 *
 * @return True if successful, false otherwise.
 **/
bool goFilter1D::normalize (goDouble constant)
{
    goDouble sum = 0.0;
    goIndex_t i;
    for (i = 0; i < myPrivate->mask.getSize(); ++i)
    {
        sum += fabs(myPrivate->mask[i]);
    }
    if (sum > 0.0)
    {
        goDouble f = constant * 1 / sum;
        for (i = 0; i < myPrivate->mask.getSize(); ++i)
        {
            myPrivate->mask[i] *= f;
        }
        return true;
    }
    else 
    {
        return false;
    }
}
