// Declared in gosignalhelper.h
#include <gosignalhelper.h>
#include <gosignal3d.h>
#include <gosignaloperation.h>

template <class T>
static inline bool _minMaxCoord (const goSignal3DBase<void>& sig, goDouble thresh, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet)
{
    goSignal3DGenericConstIterator it (&sig);
   
    maxRet.fill (0);
    minRet[0] = sig.getSizeX() - 1;
    minRet[1] = sig.getSizeY() - 1;
    minRet[2] = sig.getSizeZ() - 1;
    goSize_t z = 0;
    T v = T(0);
    while (!it.endZ())
    {
        it.resetY ();
        goSize_t y = 0;
        while (!it.endY())
        {
            it.resetX ();
            goSize_t x = 0;
            while (!it.endX())
            {
                v = *(T*)*it;
                if (v >= thresh)
                {
                    minRet[0] = goMath::min<goSize_t> (minRet[0], x);
                    minRet[1] = goMath::min<goSize_t> (minRet[1], y);
                    minRet[2] = goMath::min<goSize_t> (minRet[2], z);
                    maxRet[0] = goMath::max<goSize_t> (maxRet[0], x);
                    maxRet[1] = goMath::max<goSize_t> (maxRet[1], y);
                    maxRet[2] = goMath::max<goSize_t> (maxRet[2], z);
                }
                it.incrementX ();
                ++x;
            }
            it.incrementY ();
            ++y;
        }
        it.incrementZ ();
        ++z;
    }
    return true;
}

template <class T>
static inline bool _minMaxCoordGeneric (const goSignal3DBase<void>& sig, goFunctorBase1<bool, goDouble>& f, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet)
{
    goSignal3DGenericConstIterator it (&sig);
   
    maxRet.fill (0);
    minRet[0] = sig.getSizeX() - 1;
    minRet[1] = sig.getSizeY() - 1;
    minRet[2] = sig.getSizeZ() - 1;
    goSize_t z = 0;
    goDouble v = T(0);
    while (!it.endZ())
    {
        it.resetY ();
        goSize_t y = 0;
        while (!it.endY())
        {
            it.resetX ();
            goSize_t x = 0;
            while (!it.endX())
            {
                v = (goDouble) *(T*)*it;
                if (f (v))
                {
                    minRet[0] = goMath::min<goSize_t> (minRet[0], x);
                    minRet[1] = goMath::min<goSize_t> (minRet[1], y);
                    minRet[2] = goMath::min<goSize_t> (minRet[2], z);
                    maxRet[0] = goMath::max<goSize_t> (maxRet[0], x);
                    maxRet[1] = goMath::max<goSize_t> (maxRet[1], y);
                    maxRet[2] = goMath::max<goSize_t> (maxRet[2], z);
                }
                it.incrementX ();
                ++x;
            }
            it.incrementY ();
            ++y;
        }
        it.incrementZ ();
        ++z;
    }
    return true;
}

/** 
 * @brief Find min and max coordinates in \c sig where the value is below \c 
 * thresh.
 *
 * This gets e.g. used in goSignalCrop().
 * It finds the rectangular region of interest which contains all values larger than
 * \c thresh. Good for example to crop dark borders from images or simply "empty space".
 *
 * @param sig    Signal.
 * @param thresh Threshold.
 * @param minRet On success, contains the minimum coordinates of the rectangular region of interest with values larger than \c thresh.
 * @param maxRet On success, contains the maximum coordinates of the rectangular region of interest with values larger than \c thresh.
 * 
 * @return True if successful, false otherwise.
 */
bool goSignalMinMaxCoord (const goSignal3DBase<void>& sig, goDouble thresh, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet)
{
    if (minRet.getSize() < 3)
        minRet.resize (3);

    if (maxRet.getSize() < 3)
        maxRet.resize (3);
    
    switch (sig.getDataType().getID())
    {
        case GO_UINT8:  return _minMaxCoord<goUInt8> (sig, thresh, minRet, maxRet); break;
        case GO_INT8:   return _minMaxCoord<goInt8> (sig, thresh, minRet, maxRet); break;
        case GO_UINT16: return _minMaxCoord<goUInt16> (sig, thresh, minRet, maxRet); break;
        case GO_INT16:  return _minMaxCoord<goUInt16> (sig, thresh, minRet, maxRet); break;
        case GO_UINT32: return _minMaxCoord<goUInt32> (sig, thresh, minRet, maxRet); break;
        case GO_INT32:  return _minMaxCoord<goUInt32> (sig, thresh, minRet, maxRet); break;
        case GO_FLOAT:  return _minMaxCoord<goFloat> (sig, thresh, minRet, maxRet); break;
        case GO_DOUBLE: return _minMaxCoord<goDouble> (sig, thresh, minRet, maxRet); break;
        default: goLog::error ("goSignalMinMaxCoord(): unknown data type."); break;
    }

    return false;
}

/** 
* @brief Generic ROI finding.
* Find min and max coordinates in \c sig where \c f(\c value) is \c true.
*
* Given a function \c f, works like the other goSignalCrop() function but the border
* is sought using \c f. Instead of testing for \c value >= \c thresh,
* \c f(\c value) == \c true is tested.
* 
* @see goSignalMinMaxCoord()
*
* @param sig    Signal.
* @param f      Pointer to a functor returning bool, taking one goDouble as argument.
* @param minRet On success, contains the minimum coordinates of the rectangular region of interest with values for which f() is true.
* @param maxRet On success, contains the maximum coordinates of the rectangular region of interest with values for which f() is true.
* 
* @return True if success, false otherwise.
*/
bool goSignalMinMaxCoord (const goSignal3DBase<void>& sig, goAutoPtr<goFunctorBase1<bool, goDouble> > f, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet)
{
    if (minRet.getSize() < 3)
        minRet.resize (3);

    if (maxRet.getSize() < 3)
        maxRet.resize (3);
    
    switch (sig.getDataType().getID())
    {
        case GO_UINT8:  return _minMaxCoordGeneric<goUInt8> (sig, *f, minRet, maxRet); break;
        case GO_INT8:   return _minMaxCoordGeneric<goInt8> (sig, *f, minRet, maxRet); break;
        case GO_UINT16: return _minMaxCoordGeneric<goUInt16> (sig, *f, minRet, maxRet); break;
        case GO_INT16:  return _minMaxCoordGeneric<goUInt16> (sig, *f, minRet, maxRet); break;
        case GO_UINT32: return _minMaxCoordGeneric<goUInt32> (sig, *f, minRet, maxRet); break;
        case GO_INT32:  return _minMaxCoordGeneric<goUInt32> (sig, *f, minRet, maxRet); break;
        case GO_FLOAT:  return _minMaxCoordGeneric<goFloat> (sig, *f, minRet, maxRet); break;
        case GO_DOUBLE: return _minMaxCoordGeneric<goDouble> (sig, *f, minRet, maxRet); break;
        default: goLog::error ("goSignalMinMaxCoord(): unknown data type."); break;
    }

    return false;
}

/** 
* @brief Crop dark or empty borders from \c sig.
* 
* Uses goSignalMinMaxCoord() to find a region which contains all values >= \c thresh.
*
* @param sig     Signal to crop.
* @param target  On success, is a subsignal of \c sig representing the region of interest.
* @param thresh  Threshold.
* 
* @return True if successful, false otherwise.
*/
bool goSignalCrop (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goDouble thresh)
{
    goSize_t _min[3];
    goSize_t _max[3];
    goMath::Vector<goSize_t> min (_min, 3, 1);
    goMath::Vector<goSize_t> max (_max, 3, 1);

    if (!goSignalMinMaxCoord (sig, thresh, min, max))
    {
        return false;
    }

    if (min[0] <= max[0] &&
        min[1] <= max[1] &&
        min[2] <= max[2])
    {
        goSize3D tsize (max[0] - min[0] + 1, max[1] - min[1] + 1, max[2] - min[2] + 1);
        target.setParent (&sig);
        target.setSize (tsize);
        target.setPosition (min[0], min[1], min[2]);

        return true;
    }
    else
    {
        return false;
    }
}

class _lower_or_equal
{
    public:
        _lower_or_equal (goDouble thresh)
            : myThresh (thresh)
            {
            };
        
        ~_lower_or_equal ()
        {
        };
        
        bool eval (goDouble d)
        {
            return d <= myThresh;
        };

        goDouble myThresh;
};

/** 
* @brief Like goSignalCrop(), but crops light border.
* 
* Crops the borders where value >= thresh.
*
* @param sig    Signal to crop.
* @param target On success, is a sub signal to \c sig (the cropped region).
* @param thresh Threshold.
* 
* @return True if successful, false otherwise.
*/
bool goSignalCropHigher (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goDouble thresh)
{
    goSize_t _min[3];
    goSize_t _max[3];
    goMath::Vector<goSize_t> min (_min, 3, 1);
    goMath::Vector<goSize_t> max (_max, 3, 1);

    _lower_or_equal leq (thresh);
    return goSignalCrop (sig, target, goMemberFunction<bool, _lower_or_equal, goDouble> (&leq, &_lower_or_equal::eval));
}

/** 
* @brief Generic image cropping.
*
* Given a function \c f, works like the other goSignalCrop() function but the border
* is sought using \c f. Instead of testing for \c value >= \c thresh,
* \c f(\c value) == \c true is tested.
* 
* @param sig Signal to crop.
* @param target On success, is a sub signal of \c sig.
* @param f Functor taking a goDouble and returning a \c bool.
* 
* @return True if success, false otherwise.
*/
bool goSignalCrop (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goAutoPtr<goFunctorBase1<bool, goDouble> > f)
{
    goSize_t _min[3];
    goSize_t _max[3];
    goMath::Vector<goSize_t> min (_min, 3, 1);
    goMath::Vector<goSize_t> max (_max, 3, 1);

    if (!goSignalMinMaxCoord (sig, f, min, max))
    {
        return false;
    }

    if (min[0] <= max[0] &&
        min[1] <= max[1] &&
        min[2] <= max[2])
    {
        goSize3D tsize (max[0] - min[0] + 1, max[1] - min[1] + 1, max[2] - min[2] + 1);
        target.setParent (&sig);
        target.setSize (tsize);
        target.setPosition (min[0], min[1], min[2]);

        return true;
    }
    else
    {
        return false;
    }
}

/** 
* @brief Like goSignalMinMaxCoord(), but finds a light border.
* 
* Finds the borders when they have values higher than \c thresh.
* 
* @see goSignalMinMaxCoord()
*
* @param sig    Signal.
* @param thresh Threshold.
* @param minRet On success, contains the min. coordinates of the region of interest without the light border.
* @param maxRet On success, contains the max. coordinates of the region of interest without the light border.
* 
* @return True if successful, false otherwise.
*/
bool goSignalMinMaxCoordHigher (const goSignal3DBase<void>& sig, goDouble thresh, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet)
{
    _lower_or_equal leq (thresh);
    return goSignalMinMaxCoord (sig, goMemberFunction<bool, _lower_or_equal, goDouble> (&leq, &_lower_or_equal::eval), minRet, maxRet);
}
