/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <gofilter1d.h>
#include <gosignal3dgenericiterator.h>
#include <gosignal3d.h>
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

goFilter1D::~goFilter1D ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goFilter1D::setMask (const goArray<goFloat>& m)
{
    myPrivate->mask = m;
    return true;
}

bool goFilter1D::setCenter (goIndex_t c)
{
    myPrivate->center = c;
    return true;
}

template <class T>
static void _filter (goSignal3DBase<void>& sig, goArray<goFloat>& mask, goIndex_t center)
{
    goSignal3DGenericIterator it (&sig);
    goIndex_t maskSize = mask.getSize();
    goArray<T> lineBuffer (sig.getSizeX() + 2*maskSize);
    T* lineBufferP  = 0;
    T* lineBufferP2 = 0;
    goIndex_t sx = sig.getSizeX();
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

bool goFilter1D::filter (goSignal3DBase<void>& sig)
{
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
    return true;
}

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
