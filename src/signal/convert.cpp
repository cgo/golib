#include <gosignal.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>

template <class S, class T>
static bool _convert2 (goSignal3DBase<void>& source, goSignal3DBase<void>& target, ptrdiff_t* source_chan, ptrdiff_t* target_chan, int channelCount)
{
    source.setChannel (0);
    target.setChannel (0);

    goSignal3DGenericIterator s (&source), t (&target);
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
                ptrdiff_t *sc = source_chan;
                ptrdiff_t *tc = target_chan;
                for (i = 0; i < channelCount; ++i, ++sc, ++tc)
                {
                    *((T*)*t + *tc) = *((S*)*s + *sc);
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

    return true;
}


template <class S>
static bool _convert1 (goSignal3DBase<void>& source, goSignal3DBase<void>& target, ptrdiff_t* source_chan, ptrdiff_t* target_chan, int channelCount)
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


bool goSignal::convert (goSignal3DBase<void>& source, goSignal3DBase<void>& target, ptrdiff_t* source_chan, ptrdiff_t* target_chan, int channelCount)
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

bool goSignal::RGB2BGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target)
{
    if (target.getChannelCount() != 4)
        return false;
    if (source.getChannelCount () < 3)
        return false;

    ptrdiff_t target_chan[] = {2, 1, 0};
    ptrdiff_t source_chan[] = {0, 1, 2};

    bool ok = convert (source, target, source_chan, target_chan, 3);
    if (!ok)
        return false;

    target.setChannel (3);
    goFillSignal (&target, target.getDataType().getMaximum ());
    target.setChannel (0);

    return true;
}

bool goSignal::toBGRA (goSignal3DBase<void>& source, goSignal3DBase<void>& target, goFloat alpha)
{
    if (target.getChannelCount() != 4)
    {
        goLog::error ("goSignal::toBGRA(): target must have 4 channel.");
        return false;
    }

    ptrdiff_t target_chanBGR[] = {2, 1, 0};
    ptrdiff_t target_chanBGRA[] = {2, 1, 0, 3};
    ptrdiff_t source_chanRGB[] = {0, 1, 2};
    ptrdiff_t source_chanRGBA[] = {0, 1, 2, 3};
    ptrdiff_t source_chanI[] = {0, 0, 0};

    ptrdiff_t *source_chan = 0;
    ptrdiff_t *target_chan = 0;
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

    goFloat temp = alpha;
    if (temp == -1.0)
    {
        temp = target.getDataType().getMaximum ();
    }

    target.setChannel (3);
    goFillSignal (&target, temp);
    target.setChannel (0);

    return true;
}

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
