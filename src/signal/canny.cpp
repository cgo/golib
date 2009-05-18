#include <gosignal.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>

#include <gosubsignal3d.h>
#include <goplot.h>

namespace goSignal
{
    template <class T>
    static int _cannyRoundAngle (T angle)
    {
        static const float range_1 = M_PI * 0.5 + M_PI * 0.125;
        static const float range_2 = M_PI * 0.5 - M_PI * 0.125;
        static const float range_3 = M_PI * 0.25 - M_PI * 0.125;
        static const float range_4 = -M_PI * 0.125;
        static const float range_5 = -M_PI * 0.25 - M_PI * 0.125;
        // static const float range_6 = -M_PI * 0.5 - M_PI * 0.125;

        //= Erzwinge Winkel zw. 90 u. -90 Grad
        if (angle > T(range_1))
        {
            angle -= T(M_PI);
        }
        else if (angle <= T(range_5))
        {
            angle += T(M_PI);
        }

        if (angle <= range_1 && angle > range_2)
            return 90;

        if (angle <= range_2 && angle > range_3)
            return 45;

        if (angle <= range_3 && angle > range_4)
            return 0;

        if (angle <= range_4 && angle > range_5)
            return -45;

        //= This should not happen.
        assert (false == true);
        return 90;

//        if (angle < T(67.5) || angle < T(-67.5))
//            return 90;
//
//        if (angle > T(67.5) && angle < T(22.5))
//            return 45;
//
//        if (angle < T(22.5) && angle > T(-22.5))
//            return 0;
//
//        return -45;

        //if (angle < T(-22.5) && angle > T(-67.5))
        //    return -45;
    }

#if 0
    template <class T>
    static int _cannyRoundAngle (T angle)
    {
        //= Erzwinge Winkel zw. 90 u. -90 Grad
        if (angle < T(-90))
        {
            angle += T(180);
        }
        else if (angle > T(90))
        {
            angle -= T(180);
        }

        if (angle > T(67.5) || angle < T(-67.5))
            //return -1;
            return 45;

        if (angle < T(67.5) && angle > T(22.5))
            //return -1;
            return -45;

        if (angle < T(22.5) && angle > T(-22.5))
            //return -1;
            return 90;

        // return -1;
        return 0;

        //if (angle < T(-22.5) && angle > T(-67.5))
        //    return -45;
    }
#endif

    //= currently unused.
    template <class imageT, class retT, class sobelT>
    static void _canny2 (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goSignal3DBase<void>& sobel)
    {
        {
            goSignal3DGenericConstIterator i (&image);
            goSignal3DGenericIterator s (&sobel);

            while (!i.endY())
            {
                i.resetX ();
                s.resetX ();
                while (!i.endX())
                {
                    sobelT x, y, a;
                    x = *(imageT*)i.rightUp() - *(imageT*)i.leftUp() +
                        imageT(2) * (*(imageT*)i.rightX() - *(imageT*)i.leftX()) +
                        *(imageT*)i.rightDown() - *(imageT*)i.leftDown();
                    y = *(imageT*)i.rightDown() - *(imageT*)i.rightUp() +
                        imageT(2) * (*(imageT*)i.rightY() - *(imageT*)i.leftY()) +
                        *(imageT*)i.leftDown() - *(imageT*)i.leftUp();
                    a = ::sqrt (x*x + y*y);
                    *(sobelT*)*s = x;
                    *((sobelT*)*s + 1) = y;
                    *((sobelT*)*s + 2) = a;
                    i.incrementX ();
                    s.incrementX ();
                }
                i.incrementY ();
                s.incrementY ();
            }
        }

//        {
//            goSignal3D<void> temp;
//            temp.setDataType (GO_FLOAT);
//            temp.make (sobel.getSize(), sobel.getBlockSize(), sobel.getBorderSize(), 1);
//            sobel.setChannel (2);
//            goCopySignalChannel (&sobel, &temp);
//            goPlot::Plot p;
//            p.plotImage (temp, "");
//            p.plotPause ();
//        }

        //= Non-maximum suppression
        goSignal3DGenericIterator r (&ret);
        goSignal3DGenericIterator s (&sobel);
        // goFloat f = 180.0f / M_PI;
        while (!s.endY ())
        {
            s.resetX ();
            r.resetX ();
            while (!s.endX ())
            {
                int angle = _cannyRoundAngle (::atan2 (*((sobelT*)*s + 1), *(sobelT*)*s));
                sobelT a1 = sobelT(0), a2 = sobelT(0);
                sobelT a = *((sobelT*)*s + 2);
                switch (angle)
                {
                    case 0: a1 = *((sobelT*)s.leftX() + 2); a2 = *((sobelT*)s.rightX() + 2); break;
                    case -45: a1 = *((sobelT*)s.leftDown() + 2); a2 = *((sobelT*)s.rightUp() + 2); break;
                    case 90: a1 = *((sobelT*)s.rightY() + 2); a2 = *((sobelT*)s.leftY() + 2); break;
                    case 45: a1 = *((sobelT*)s.leftUp() + 2); a2 = *((sobelT*)s.rightDown() + 2); break;
                    default: goLog::error ("goSignal::canny (): angle error."); a1 = 0; a2 = 0; a = 0; break;
                }

                //if (angle == 45)
                {
                    if (a > a1 && a > a2)
                        *(retT*)*r = retT(1);
                    else
                        *(retT*)*r = retT(0);
                }
                //else
                //{
                //    *(retT*)*r = retT(0);
                //}
                s.incrementX ();
                r.incrementX ();
            }
            s.incrementY ();
            r.incrementY ();
        }

        //= FIXME: Hysteresis
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
        goSignal3D<void>* ret_p = reinterpret_cast<goSignal3D<void>*> (&ret);
        if (!ret_p)
        {
            goLog::error ("goSignal::canny (): ret of wrong size, but it is not a goSignal3D");
            return false;
        }

        ret_p->setDataType (GO_UINT8);
        ret_p->make (image.getSize(), image.getSize(), goSize3D (8, 8, 1), 1);
                // goSignal::defaultBlockSize2D(), goSize3D (8, 8, 1), 1);
    }

    const goSignal3DBase<void>*  image_p = 0;
    goAutoPtr<goSignal3D<void> > image2 = 0;
    if (image.getChannelCount () > 1)
    {
        goLog::warning ("goSignal::canny (): converting image to scalar.");
        image2 = new goSignal3D<void>;
        // image2->setDataType (image.getDataType().getID ());
        image2->setDataType (GO_FLOAT);
        image2->make (image.getSize(), image.getBlockSize(), image.getBorderSize(), 1);
        goRGBAtoScalar (&image, image2);
        image_p = static_cast<const goSignal3DBase<void>*> (image2.get ());
    }
    else
    {
        image_p = &image;
    }

    goAutoPtr<goSignal3D<void> > sobel = new goSignal3D<void>;
    sobel->setDataType (GO_FLOAT);
    sobel->make (image.getSize(), image.getSize(), goSize3D (8, 8, 1), 3);
            // goSignal::defaultBlockSize2D(), goSize3D (8, 8, 1), 3);

    switch (image_p->getDataType().getID())
    {
        case GO_INT8: _canny1 <goInt8, goFloat> (*image_p, ret, *sobel); break;
        case GO_UINT8: _canny1 <goUInt8, goFloat> (*image_p, ret, *sobel); break;
        case GO_INT16: _canny1 <goInt16, goFloat> (*image_p, ret, *sobel); break;
        case GO_UINT16: _canny1 <goUInt16, goFloat> (*image_p, ret, *sobel); break;
        case GO_INT32: _canny1 <goInt32, goFloat> (*image_p, ret, *sobel); break;
        case GO_UINT32: _canny1 <goUInt32, goFloat> (*image_p, ret, *sobel); break;
        case GO_FLOAT: _canny1 <goFloat, goFloat> (*image_p, ret, *sobel); break;
        case GO_DOUBLE: _canny1 <goDouble, goFloat> (*image_p, ret, *sobel); break;
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
    return true;    
}
