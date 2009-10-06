#include <gosignal.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>

template <class S, class T>
static bool _convert2 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, const int* source_chan, const int* target_chan, int channelCount)
{
    int s_chan = source.getChannel ();

    const_cast<goSignal3DBase<void>*> (&source)->setChannel (0);
    target.setChannel (0);

    goSignal3DGenericConstIterator s (&source);
    goSignal3DGenericIterator t (&target);
    int i = 0;

    while (!s.endZ ())
    {
        s.resetY ();
        t.resetY ();
        while (!s.endY ())
        {
            s.resetX ();
            t.resetX ();
            while (!s.endX ())
            {
                const int *sc = source_chan;
                const int *tc = target_chan;
                for (i = 0; i < channelCount; ++i, ++sc, ++tc)
                {
                    *((T*)*t + *tc) = *((const S*)*s + *sc);
                }

                s.incrementX ();
                t.incrementX ();
            }
            s.incrementY ();
            t.incrementY ();
        }
        s.incrementZ ();
        t.incrementZ ();
    }

    const_cast<goSignal3DBase<void>*> (&source)->setChannel (s_chan);

    return true;
}


template <class S>
static bool _convert1 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, const int* source_chan, const int* target_chan, int channelCount)
{
    switch (target.getDataType().getID())
    {
        case GO_UINT8: return _convert2<S,goUInt8> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT8: return _convert2<S,goInt8> (source, target, source_chan, target_chan, channelCount); break;
        case GO_UINT16: return _convert2<S,goUInt16> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT16: return _convert2<S,goInt16> (source, target, source_chan, target_chan, channelCount); break;
        case GO_UINT32: return _convert2<S,goUInt32> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT32: return _convert2<S,goInt32> (source, target, source_chan, target_chan, channelCount); break;
        case GO_FLOAT: return _convert2<S,goFloat> (source, target, source_chan, target_chan, channelCount); break;
        case GO_DOUBLE: return _convert2<S,goDouble> (source, target, source_chan, target_chan, channelCount); break;
        default: return false; break;
    }
}


/** 
 * @addtogroup signal
 * @{
 */
/**
 * @brief Convert a goSignal3DBase to another.
 * 
 * Copies the content of \c source to \c target, converting from the source data type to the target
 * data type. The source and target channels can be mapped to one another freely.
 * 
 * \c Target must be at least of the same size as \c source in all dimensions.
 *
 * @param source Source signal
 * @param target Target signal
 * @param source_chan Array denoting the source channels, e.g. <code> const int s_chan[] = {0, 1, 2} <\code>; must contain \c channelCount elements
 * @param target_chan Array denoting the target channels; must contain \c channelCount elements
 * @param channelCount Number of channels to be copied (e.g. number of elements in source_chan and target_chan)
 * 
 * @return True if successful, false otherwise.
 */
bool goSignal::convert (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, const int* source_chan, const int* target_chan, int channelCount)
{
    if (source.getSizeX() > target.getSizeX() ||
        source.getSizeY() > target.getSizeY() ||
        source.getSizeZ() > target.getSizeZ())
    {
        goLog::warning ("goSignal::convert(): source is larger than target. Aborting.");
        return false;
    }

    switch (source.getDataType().getID())
    {
        case GO_UINT8: return _convert1<goUInt8> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT8: return _convert1<goInt8> (source, target, source_chan, target_chan, channelCount); break;
        case GO_UINT16: return _convert1<goUInt16> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT16: return _convert1<goInt16> (source, target, source_chan, target_chan, channelCount); break;
        case GO_UINT32: return _convert1<goUInt32> (source, target, source_chan, target_chan, channelCount); break;
        case GO_INT32: return _convert1<goInt32> (source, target, source_chan, target_chan, channelCount); break;
        case GO_FLOAT: return _convert1<goFloat> (source, target, source_chan, target_chan, channelCount); break;
        case GO_DOUBLE: return _convert1<goDouble> (source, target, source_chan, target_chan, channelCount); break;
        default: return false; break;
    }
    return false;
}

/** 
 * @brief Convert RGB to BGRA data
 * 
 * Uses goSignal::convert() to convert from RGB \c source to BGRA \c target.
 *
 * @param source Source signal
 * @param target Target signal
 * 
 * @return True if successful, false otherwise.
 */
bool goSignal::RGB2BGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target)
{
    if (target.getChannelCount() != 4)
        return false;
    if (source.getChannelCount () < 3)
        return false;

    int target_chan[] = {2, 1, 0};
    int source_chan[] = {0, 1, 2};

    bool ok = convert (source, target, source_chan, target_chan, 3);
    if (!ok)
        return false;

    target.setChannel (3);
    goFillSignal (&target, target.getDataType().getMaximum ());
    target.setChannel (0);

    return true;
}

/** 
 * @brief Convenience function, converts a source to a BGRA target.
 *
 * @see goSignal::convert()
 *
 * @param source Source signal. Must be 4 channel (RGBA), 3 channel (RGB) or 1 channel (intensity) data.
 * @param target Target signal. Must be 4 channel, and at least of the size of \c source in each dimension.
 * @param alpha Optional alpha value, to be filled in when the source does not have an alpha channel.
 * If alpha < 0, the maximal value for the target data type will be filled in as alpha value if
 * \c source does not provide an alpha channel. If alpha >= 0, the alpha channel is filled with \c alpha
 * regardless of whether \c source provides an alpha channel.
 * 
 * @return True if successful, false otherwise.
 */
bool goSignal::toBGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target, goFloat alpha)
{
    if (target.getChannelCount() != 4)
    {
        goLog::error ("goSignal::toBGRA(): target must have 4 channel.");
        return false;
    }

    int target_chanBGR[] = {2, 1, 0};
    int target_chanBGRA[] = {2, 1, 0, 3};
    int source_chanRGB[] = {0, 1, 2};
    int source_chanRGBA[] = {0, 1, 2, 3};
    int source_chanI[] = {0, 0, 0};

    int *source_chan = 0;
    int *target_chan = 0;
    int chan_count = 3;
    switch (source.getChannelCount())
    {
        case 4:
            {
                source_chan = source_chanRGBA;
                target_chan = target_chanBGRA;
                chan_count = 4;
            } break;
        case 3:
            {
                source_chan = source_chanRGB;
                target_chan = target_chanBGR;
                chan_count = 3;
            } break;
        case 1:
            {
                source_chan = source_chanI;
                target_chan = target_chanBGR;
                chan_count = 3;
            } break;
        default:
            goLog::error ("goSignal::toBGRA(): unknown source channel count");
            break;
    }

    bool ok = convert (source, target, source_chan, target_chan, chan_count);
    if (!ok)
        return false;

    if (chan_count < 4 || alpha >= 0.0)
    {
        goFloat temp = alpha;
        if (temp == -1.0)
        {
            temp = target.getDataType().getMaximum ();
        }

        target.setChannel (3);
        goFillSignal (&target, temp);
        target.setChannel (0);
    }

    return true;
}

/** 
 * @brief Convenience function, converts a source to RGBA.
 * 
 * @see goSignal::toBGRA()
 *
 * This function works like the other toBGRA() function, except that it returns a goAutoPtr to
 * a newly allocated goSignal3D<void>.
 *
 * @param source Source signal
 * @param alpha Optional alpha value
 * 
 * @return goAutoPtr to a new goSignal3D<void> of type goUInt8.
 * If something goes wrong, this is a Null pointer.
 */
goAutoPtr<goSignal3D<void> > goSignal::toBGRA (goSignal3DBase<void>& source, goFloat alpha)
{
    goAutoPtr<goSignal3D<void> > ret = new goSignal3D<void>;
    ret->setDataType (GO_UINT8);
    ret->make (source.getSize(), source.getBlockSize(), source.getBorderSize(), 4);

    bool ok = goSignal::toBGRA (source, *ret, alpha);
    if (!ok)
    {
        ret.reset ();
    }

    return ret;
}
/** @} */
