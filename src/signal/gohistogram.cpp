/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gohistogram.cpp,v 1.1.1.1 2006/04/19 15:26:29 gosch Exp $
 */

#include <gohistogram.h>
#include <golog.h>
#include <gotypes.h>
#include <gosignalmacros.h>
#include <gosignal3dgenericiterator.h>
#include <gotype.h>
#include <assert.h>

template <class level_type>
class goHistogramPrivate
{
    public:
        goHistogramPrivate ();
        ~goHistogramPrivate ();
        
        bool              userSetLevels;
        goArray<goDouble> bins;
        goDouble          minValue;             // Minimum data value (set in calculate())
        goDouble          _binStep;             // 1 / binStep (set in calculate())
        goTypeEnum        inputSignalType;
        goArray<level_type> levels;
};

template <class T>
goHistogramPrivate<T>::goHistogramPrivate ()
    : userSetLevels (false),
      bins          (),
      minValue      (0.0),
      _binStep      (0.0),
      inputSignalType (GO_UINT8),
      levels ()
{
}

template <class T>
goHistogramPrivate<T>::~goHistogramPrivate ()
{
}

// ------------------------------------------------

template <class level_type>
goHistogram<level_type>::goHistogram ()
    : goObjectBase (),
      myPrivate    (NULL)
{
    this->setClassID(GO_HISTOGRAM);
    myPrivate = new goHistogramPrivate<level_type>;
    assert (myPrivate);
}

template <class level_type>
goHistogram<level_type>::~goHistogram ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

template <class level_type>
goHistogram<level_type>::goHistogram (const goHistogram<level_type>& other)
{
    myPrivate = new goHistogramPrivate<level_type>;
    *myPrivate = *other.myPrivate;
}

template <class level_type>
const goHistogram<level_type>& goHistogram<level_type>::operator= (const goHistogram<level_type>& other)
{
    *myPrivate = *other.myPrivate;
    return *this;
}

template <class level_type>
goDouble goHistogram<level_type>::getMinValue () const
{
    return myPrivate->minValue;
}

template <class level_type>
goDouble goHistogram<level_type>::getBinStep () const
{
    return myPrivate->_binStep;
}

template <class level_type, class signal_type>
static bool calculateHistogram (const goSignal3DBase<void>& sig, 
                                const goArray<level_type>&  levels, 
                                goArray<goDouble>&          bins,
                                goDouble                    _binStep,       // 1 / binStep
                                goDouble                    minValue)
{
    bins.fill (0.0);
    // Fixed binStep:
    if (_binStep > 0.0)
    {
        GO_SIGNAL3D_EACHELEMENT_GENERIC_CONST (
                bins[(goIndex_t)((*(const signal_type*)__ptr - minValue) * _binStep)] += 1.0;, sig);
    }
    return true;
}

template <class level_type>
bool goHistogram<level_type>::calculateCore (const goSignal3DBase<void>& sig)
{
    switch (sig.getDataType().getID())
    {
        case GO_UINT8: calculateHistogram <level_type,goUInt8>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_INT8: calculateHistogram <level_type,goInt8>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_UINT16: calculateHistogram <level_type,goUInt16>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_INT16: calculateHistogram <level_type,goInt16>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_UINT32: calculateHistogram <level_type,goUInt32>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_INT32: calculateHistogram <level_type,goInt32>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_FLOAT: calculateHistogram <level_type,goFloat>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        case GO_DOUBLE: calculateHistogram <level_type,goDouble>(sig, this->getLevels(), this->getHistogram(), this->getBinStep(), this->getMinValue()); break;
        default:
            {
                goString msg;
                msg = "Type ";
                msg += sig.getDataType().getString().toCharPtr();
                msg += " not supported.";
                goLog::warning (msg, this);
                return false;
            }
            break;
    }

    return true;
}

template <class level_type>
bool 
goHistogram<level_type>::calculate (const goSignal3DBase<void>& sig, bool normalize)
{
    goDouble _binStep = -1.0;
    goDouble minValue = 0.0;
    if (!myPrivate->userSetLevels)
    {
        myPrivate->levels.resize(myPrivate->bins.getSize());
        goIndex_t i;
        minValue = sig.getMinimum();
        goDouble maxValue = sig.getMaximum();
        goDouble binStep = (maxValue - minValue) / (float)(myPrivate->levels.getSize()-1);
        if (binStep > 0.0)
        {
            _binStep = 1.0 / binStep;
        }
        else
        {
            binStep = 0.0;
        }
        for (i = 0; i < myPrivate->levels.getSize(); ++i)
        {
            // myPrivate->levels[i] = minValue + binStep * (i+1);
            myPrivate->levels[i] = minValue + binStep * (float)(i);
        }
    }
    else
    {
        return false;
    }
    myPrivate->minValue = minValue;
    myPrivate->_binStep = _binStep;
    myPrivate->inputSignalType = sig.getDataType().getID();

    this->calculateCore (sig);
    
    if (normalize)
    {
        goDouble f = 1.0 / (sig.getSizeX() * sig.getSizeY() * sig.getSizeZ());
        goIndex_t i;
        for (i = 0; i < myPrivate->bins.getSize(); ++i)
        {
            myPrivate->bins[i] *= f;
        }
    }
    return true;
}

template <class level_type>
bool 
goHistogram<level_type>::setLevels (const goArray<level_type>& levelArray)
{
    myPrivate->userSetLevels = true;
    myPrivate->levels = levelArray;
    return true;
}

template <class level_type>
bool
goHistogram<level_type>::setBins (goSize_t n)
{
    myPrivate->userSetLevels = false;
    myPrivate->bins.resize(n);
    myPrivate->levels.resize(n);
    return true;
}

template <class level_type>
goIndex_t
goHistogram<level_type>::getBins () const
{
    return myPrivate->bins.getSize();
}

template <class level_type>
goDouble
goHistogram<level_type>::lookup (const void* valueP)
{
    if (myPrivate->userSetLevels)
    {
        // FIXME!
        return 0;
    }
    level_type value = level_type(0);
    switch (myPrivate->inputSignalType)
    {
        case GO_UINT8: value  = *(goUInt8*)valueP; break;
        case GO_INT8: value   = *(goInt8*)valueP; break;
        case GO_UINT16: value = *(goUInt16*)valueP; break;
        case GO_INT16: value  = *(goInt16*)valueP; break;
        case GO_UINT32: value = *(goUInt32*)valueP; break;
        case GO_INT32: value  = *(goInt32*)valueP; break;
        case GO_FLOAT: value  = *(goFloat*)valueP; break;
        case GO_DOUBLE: value = *(goDouble*)valueP; break;
        default: goLog::warning ("lookup: input signal type unknown.", this); break;
    }
    goIndex_t index = (goIndex_t)((value - myPrivate->minValue) * myPrivate->_binStep);
    if (index < myPrivate->bins.getSize() && index >= 0)
    {
        return myPrivate->bins[index];
    }
    goString msg = "Index ";
    msg += (int)(index);
    msg += " out of range -- max. is ";
    msg += (int)(myPrivate->bins.getSize() - 1);
    msg += ", binStep = ";
    msg += (float)myPrivate->_binStep;
    msg += ", minValue = ";
    msg += (float)myPrivate->minValue;
    goLog::warning (msg, this);
    return 0;
}

template <class level_type>
goArray<goDouble>& 
goHistogram<level_type>::getHistogram ()
{
    return myPrivate->bins;
}

template <class level_type>
goArray<level_type>& 
goHistogram<level_type>::getLevels ()
{
    return myPrivate->levels;
}


class goCDFPrivate
{
    public:
        goCDFPrivate ();
        ~goCDFPrivate ();
        
};

goCDFPrivate::goCDFPrivate ()
{
}

goCDFPrivate::~goCDFPrivate ()
{
}

// --------------------------------- //

template <class level_type>
goCDF<level_type>::goCDF ()
    : goHistogram<level_type> (),
      myPrivate (NULL)
{
    myPrivate = new goCDFPrivate;
    assert (myPrivate);
}

template <class level_type>
goCDF<level_type>::~goCDF ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

template <class level_type>
bool
goCDF<level_type>::calculate (const goSignal3DBase<void>& sig, bool)
{
    if (!goHistogram<level_type>::calculate (sig,true))
    {
        return false;
    }
    goArray<goDouble>& histo = this->getHistogram();
    goIndex_t i;
    goDouble accum = 0.0;
    for (i = 0; i < histo.getSize(); ++i)
    {
        accum   += histo[i];
        histo[i] = accum;
    }
    return true;
}

// Helper to create index functions
template <goIndex_t startIndex, goIndex_t endIndex>
static goIndex_t goCDFDoubleIndexFunction (void* valuePtr)
{
    static goDouble minValue = 0.0;
    static goDouble maxValue = 1.0;
    static goDouble _stepValue = (endIndex - startIndex) / (maxValue - minValue);
    return startIndex + (goIndex_t)(*(goDouble*)valuePtr * _stepValue);
}

template <class level_type>
bool
goCDF<level_type>::makeInverseLUT (goArray<goFloat>& inverseLUTRet, goIndexFunction& indexFunctionRet)
{
    goType typ (GO_DOUBLE);
    inverseLUTRet.resize (typ.getMaxIndex() - typ.getMinIndex() + 1);
    indexFunctionRet = typ.getIndexFunction();
    goIndex_t i;
    goDouble step = 1.0 / (typ.getMaxIndex() - typ.getMinIndex());
    goDouble value = 0.0;
    goIndex_t cdfIndex = 0;
    goArray<goDouble>& histo = this->getHistogram();
    for (i = 0; i < inverseLUTRet.getSize(); ++i)
    {
        if (value > histo[cdfIndex])
        {
            ++cdfIndex;
        }
        if (cdfIndex == 0)
        {
            inverseLUTRet[i] = cdfIndex;
        }
        else
        {
            goDouble diff = histo[cdfIndex] - histo[cdfIndex - 1];
            if (diff == 0.0)
            {
                inverseLUTRet[i] = cdfIndex;
            }
            else
            {
                inverseLUTRet[i] = cdfIndex - 1 + fabs((value - histo[cdfIndex-1]) / diff);
            }
        }
        value += step;
    }
    return true;
}

template <class cdfT, class T>
static bool _equalizeHistogram (goSignal3DBase<void>* sig, goCDF<cdfT>& targetCDF)
{
    if (!sig)
    {
        return false;
    }
    goCDF<cdfT> fromCDF;
    fromCDF.setBins (targetCDF.getBins());
    fromCDF.calculate (*sig);

    goArray<goFloat> inverseLUT;
    goIndexFunction inverseIndexFunction;
    targetCDF.makeInverseLUT (inverseLUT, inverseIndexFunction);
    goArray<cdfT>& targetLevels = targetCDF.getLevels();

    goSignal3DGenericIterator it (sig);
    goDouble temp;
    cdfT     level0;
    cdfT     level1;
    goFloat  indexf;
    goFloat  r;
    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                temp = fromCDF.lookup(*it);
                indexf = inverseLUT[inverseIndexFunction(&temp)];
                level0 = targetLevels[(goIndex_t)indexf];
                level1 = targetLevels[(goIndex_t)ceil(indexf)];
                r      = indexf - (goIndex_t)indexf;
                if ((goIndex_t)(indexf) <= 0 || (goIndex_t)(indexf) >= targetLevels.getSize() - 2)
                {
                    // printf ("indexf == %f, level0 == %f, level1 == %f, r == %f\n", indexf, level0, level1, r);
                }
                *(T*)*it = (T)(level0 + (level1-level0)*r);
                it.incrementX();
            }
            it.incrementY();
        }
        it.incrementZ();
    }
    return true;
}

/** 
 * @brief Equalise histogram of a signal to a given CDF.
 *
 * @note This does not work quite right now. 
 * Check.
 * @bug Histogram gets distorted. See example output.
 * 
 * @param sig 
 * @param targetCDF 
 * 
 * @return 
 */
template <class T>
bool goEqualizeHistogram (goSignal3DBase<void>* sig, goCDF<T>& targetCDF)
{
    
    switch (sig->getDataType().getID())
    {
        case GO_INT8: return _equalizeHistogram<T,goInt8> (sig, targetCDF); break;
        case GO_UINT8: return _equalizeHistogram<T,goUInt8> (sig, targetCDF); break;
        case GO_INT16: return _equalizeHistogram<T,goInt16> (sig, targetCDF); break;
        case GO_UINT16: return _equalizeHistogram<T,goUInt16> (sig, targetCDF); break;
        case GO_INT32: return _equalizeHistogram<T,goInt32> (sig, targetCDF); break;
        case GO_UINT32: return _equalizeHistogram<T,goUInt32> (sig, targetCDF); break;
        case GO_FLOAT: return _equalizeHistogram<T,goFloat> (sig, targetCDF); break;
        case GO_DOUBLE: return _equalizeHistogram<T,goDouble> (sig, targetCDF); break;
        default: goLog::warning("goEqualizeHistogram(): unknown data type."); break;
    }
    return false;
}

bool goMatchHistograms (const goSignal3DBase<void>* fromSignal, const goSignal3DBase<void>* toSignal, goSignal3DBase<void>* retSignal)
{
    if (fromSignal->getSizeX() != retSignal->getSizeX() ||
        fromSignal->getSizeY() != retSignal->getSizeY() ||
        fromSignal->getSizeZ() != retSignal->getSizeZ())
    {
        goString msg;
        msg = "goMatchHistograms(): Size mismatch between fromSignal and retSignal.";
        goLog::warning (msg);
        return false;
    }
    if (retSignal->getDataType().getID() != GO_FLOAT)
    {
        goString msg;
        msg = "goMatchHistograms(): Currently, only goFloat retSignal allowed, but retSignal is of type ";
        msg += retSignal->getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    
    goCDF<goFloat> fromCDF;
    fromCDF.setBins (256);
    fromCDF.calculate (*fromSignal);
    goCDF<goFloat> toCDF;
    toCDF.setBins (256);
    toCDF.calculate (*toSignal);
    
    goArray<goFloat> inverseLUT;
    goIndexFunction inverseIndexFunction;
    toCDF.makeInverseLUT (inverseLUT, inverseIndexFunction);
    goArray<goFloat>& toLevels = toCDF.getLevels();

    const goPtrdiff_t* fromDx;
    goPtrdiff_t* retDx;

    const goByte* fromP;
    goByte*       retP;

    goIndex_t i,j,k;
    goIndex_t sx = (goIndex_t)fromSignal->getSizeX();
    goIndex_t sy = (goIndex_t)fromSignal->getSizeY();
    goIndex_t sz = (goIndex_t)fromSignal->getSizeZ();
    for (k = 0; k < sz; ++k)
    {
        for (j = 0; j < sy; ++j)
        {
            fromDx  = fromSignal->getXDiff();
            retDx = retSignal->getXDiff();
            fromP   = (const goByte*)fromSignal->getPtr (0,j,k);
            retP  = (goByte*)retSignal->getPtr (0,j,k);
            goDouble temp;
            for (i = 0; i < sx; ++i)
            {
                temp = fromCDF.lookup(fromP);
                *(goFloat*)retP = toLevels[(goIndex_t)inverseLUT[inverseIndexFunction(&temp)]];
                retP += *retDx;
                fromP += *fromDx;
                ++retDx;
                ++fromDx;
            }
        }
    }
    return true;
}

template class goHistogram<goFloat>;
template class goHistogram<goDouble>;
template class goCDF<goFloat>;
template class goCDF<goDouble>;
template bool  goEqualizeHistogram<goFloat>(goSignal3DBase<void>* sig, goCDF<goFloat>& targetCDF);
template bool  goEqualizeHistogram<goDouble>(goSignal3DBase<void>* sig, goCDF<goDouble>& targetCDF);

