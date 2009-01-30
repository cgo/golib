#include <gosignal.h>
#include <gosignalhelper.h>
#include <golog.h>

namespace goSignal
{
    template <class T>
    static int _cannyRoundAngle (T angle)
    {
        //= Erzwinge Winkel zw. 90 u. -90 Grad
        if (angle < T(-90))
        {
            angle += T(90);
        }
        else if (angle > T(90))
        {
            angle -= T(90);
        }

        if (angle < T(67.5) || angle < T(-67.5))
            return 90;

        if (angle > T(67.5) && angle < T(22.5))
            return 45;

        if (angle < T(22.5) && angle > T(-22.5))
            return 0;

        return -45;

        //if (angle < T(-22.5) && angle > T(-67.5))
        //    return -45;
    }

    //= currently unused.
    template <class imageT, class retT, class sobelT>
    static void _canny2 (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goSignal3DBase<void>& sobel)
    {
        goSignal3DGenericConstIterator i (&image);
        goSignal3DGenericIterator r (&ret);
        goSignal3DGenericIterator s (&sobel);

        while (!i.endY())
        {
            i.resetX ();
            r.resetX ();
            s.resetX ();
            while (!io.endX())
            {
                sobelT x, y, a;
                x = *(imageT*)i.rightUp() - *(imageT*)i.leftUp() +
                    imageT(2) * (*(imageT*)i.right() - *(imageT*)i.left()) +
                    *(imageT*)i.rightDown() - *(imageT*)i.leftDown();
                y = *(imageT*)i.rightDown() - *(imageT*)i.RightUp() +
                    imageT(2) * (*(imageT*)i.down() - *(imageT*)i.up()) +
                    *(imageT*)i.leftDown() - *(imageT*)i.leftUp();
                a = ::sqrt (x*x + y*y);
                *(sobelT*)*s = x;
                *((sobelT*)*s + 1) = y;
                *((sobelT*)*s + 2) = a;
                i.incrementX ();
                r.incrementX ();
                s.incrementX ();
            }
            i.incrementY ();
            r.incrementY ();
            s.incrementY ();
        }

        s.setPosition (0, 0, 0);
        r.setPosition (0, 0, 0);
        while (!s.endY ())
        {
            s.resetX ();
            r.resetX ();
            while (!s.endX ())
            {
                int angle = _cannyRoundAngle (::atan2 (*((sobelT*)*s + 1), *(sobelT*)*s));
                sobelT a1 = sobelT(0), a2 = sobelT(0);
                switch (angle)
                {
                    case 90: a1 = *((sobelT*)s.left() + 2); a2 = *((sobelT*)s.right() + 2); break;
                    case 45: a1 = *((sobelT*)s.leftDown() + 2); a2 = *((sobelT*)s.rightUp() + 2); break;
                    case 0: a1 = *((sobelT*)s.down() + 2); a2 = *((sobelT*)s.up() + 2); break;
                    case -45: a1 = *((sobelT*)s.leftUp() + 2); a2 = *((sobelT*)s.rightDown() + 2); break;
                    default: goLog::error ("goSignal::canny (): angle error."); return;
                }

                sobelT a = *(sobelT*)*s;
                if (a < a1 || a <= a2)
                    *(retT*)*r = retT(0);
                else
                    *(retT*)*r = retT(1);

                s.incrementX ();
                r.incrementX ();
            }
            s.incrementY ();
            r.incrementY ();
        }
    }

    template <class imageT, class sobelT>
    static void _canny1 (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goSignal3DBase<void>& sobel)
    {
        switch (ret.getDataType().getID ())
        {
            case GO_INT8:   _canny2 <imageT, goInt8, sobelT> (image, ret, sobel); break;
            case GO_UINT8:  _canny2 <imageT, goUInt8, sobelT> (image, ret, sobel); break;
            case GO_INT16:  _canny2 <imageT, goInt16, sobelT> (image, ret, sobel); break;
            case GO_UINT16: _canny2 <imageT, goUInt16, sobelT> (image, ret, sobel); break;
            case GO_INT32:  _canny2 <imageT, goInt32, sobelT> (image, ret, sobel); break;
            case GO_UINT32: _canny2 <imageT, goUInt32, sobelT> (image, ret, sobel); break;
            case GO_FLOAT:  _canny2 <imageT, goFloat, sobelT> (image, ret, sobel); break;
            case GO_DOUBLE: _canny2 <imageT, goDouble, sobelT> (image, ret, sobel); break;
            default: goLog::error ("goSignal::_canny1 (): wrong data type of ret.");
        }
    }
};

bool goSignal::canny (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret)
{
    if (ret.getSize() != image.getSize())
    {
        goSignal3D<void>* ret_p = reinterpret_cast<goSignal3D<void>*> (ret);
        if (!ret_p)
        {
            goLog::error ("goSignal::canny (): ret of wrong size, but it is not a goSignal3D");
            return false;
        }

        ret_p->setDataType (GO_UINT8);
        ret_p->resize (image.getSize(), goSignal::defaultBlockSize2D(), goSize3D (8, 8, 1), 1);
    }

    const goSignal3DBase<void>*  image_p = 0;
    goAutoPtr<goSignal3D<void> > image2 = 0;
    if (image.getChannelCount () > 1)
    {
        goLog::warning ("goSignal::canny (): converting image to scalar.");
        image2 = new goSignal3D<void>;
        image2->setDataType (image.getDataType().getID ());
        goRGBAtoScalar (&image, image2);
        image_p = static_cast<const goSignal3DBase<void>*> (image2);
    }
    else
    {
        image_p = &image;
    }

    goAutoPtr<goSignal3D<void> > sobel = new goSignal3D<void>;
    sobel->setDataType (GO_FLOAT);

    switch (image_p->getDataType().getID())
    {
        case GO_INT8: _canny1 <goInt8, goFloat> (image, ret, *sobel); break;
        case GO_UINT8: _canny1 <goUInt8, goFloat> (image, ret, *sobel); break;
        case GO_INT16: _canny1 <goInt16, goFloat> (image, ret, *sobel); break;
        case GO_UINT16: _canny1 <goUInt16, goFloat> (image, ret, *sobel); break;
        case GO_INT32: _canny1 <goInt32, goFloat> (image, ret, *sobel); break;
        case GO_UINT32: _canny1 <goUInt32, goFloat> (image, ret, *sobel); break;
        case GO_FLOAT: _canny1 <goFloat, goFloat> (image, ret, *sobel); break;
        case GO_DOUBLE: _canny1 <goDouble, goFloat> (image, ret, *sobel); break;
        default: return false; break;
    }

#if 0
    goAutoPtr<goSignal3D<void> > sobel = new goSignal3D<void>;
    sobel->setDataType (GO_FLOAT);

    if (!goSignal::sobel2D (*image_p, *sobel))
    {
        goLog::error ("goSignal::canny (): sobel2D failed.");
        return false;
    }
#endif
    
}
