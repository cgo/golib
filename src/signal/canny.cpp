/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>

#include <gosubsignal3d.h>
#include <goplot/gnuplot.h>

#include <queue>

namespace goSignal
{
    template <class T>
    static int _cannyRoundAngle (T angle)
    {
        static const float range_1 = -M_PI * 0.125;
        static const float range_2 = M_PI * 0.125;
        static const float range_3 = M_PI * 0.25 + M_PI * 0.125;
        static const float range_4 = M_PI * 0.5 + M_PI * 0.125;
        static const float range_5 = M_PI * 0.75 + M_PI * 0.125;

        //= Erzwinge Winkel zw. 90 u. -90 Grad
        if (angle < T(range_1))
        {
            angle += T(M_PI);
        }
        else if (angle >= T(range_5))
        {
            angle -= T(M_PI);
        }

        if (angle >= range_1 && angle < range_2)
            return 0;

        if (angle >= range_2 && angle < range_3)
            return 45;

        if (angle >= range_3 && angle < range_4)
            return 90;

        if (angle >= range_4 && angle < range_5)
            return -45;

        //= This should not happen.
        assert (false == true);
        return 90;

    }

    //= 
    template <class imageT, class retT, class sobelT>
    static void _canny2 (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goSignal3DBase<void>& sobel, goDouble thresh1, goDouble thresh2)
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
                    x = float(*(imageT*)i.rightUp()) - float(*(imageT*)i.leftUp()) +
                        2.0 * (float(*(imageT*)i.rightX()) - float(*(imageT*)i.leftX())) +
                        float(*(imageT*)i.rightDown()) - float(*(imageT*)i.leftDown());
                    y = float(*(imageT*)i.rightDown()) - float(*(imageT*)i.rightUp()) +
                        2.0 * (float(*(imageT*)i.rightY()) - float(*(imageT*)i.leftY())) +
                        float(*(imageT*)i.leftDown()) - float(*(imageT*)i.leftUp());
                    //x = -x;
                    //y = -y;
                    a = ::sqrt(x*x + y*y);
                    *(sobelT*)*s = x;
                    *((sobelT*)*s + 1) = y;
                    *((sobelT*)*s + 2) = a; // a;
                    i.incrementX ();
                    s.incrementX ();
                }
                i.incrementY ();
                s.incrementY ();
            }

//= Debug: Return sobel filter output.
#if 0
            sobel.setChannel (2);
            printf ("sobel min, max: %f, %f\n", sobel.getMinimum (), sobel.getMaximum ());
            sobel *= 255.0 / sobel.getMaximum();
            sobel.setChannel (0);

            int source_chan[ret.getChannelCount()];
            int target_chan[ret.getChannelCount()];
            for (goSize_t i = 0; i < ret.getChannelCount(); ++i)
            {
                source_chan[i] = 2;
                target_chan[i] = i;
            }
            goSignal::convert (sobel, ret, source_chan, target_chan, ret.getChannelCount());
            return;
#endif
        }


        //= Non-maximum suppression
        goSignal3DGenericIterator r (&ret);
        goSignal3DGenericIterator s (&sobel);

        goIndex_t index = 0;
        std::queue<goIndex_t> Q;
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
                    //= 0 -45 90 45
                    case 0: a1 = *((sobelT*)s.leftX() + 2); a2 = *((sobelT*)s.rightX() + 2); break;
                    case -45: a1 = *((sobelT*)s.leftDown() + 2); a2 = *((sobelT*)s.rightUp() + 2); break;
                    case 90: a1 = *((sobelT*)s.rightY() + 2); a2 = *((sobelT*)s.leftY() + 2); break;
                    case 45: a1 = *((sobelT*)s.leftUp() + 2); a2 = *((sobelT*)s.rightDown() + 2); break;
                    default: goLog::error ("goSignal::canny (): angle error."); a1 = 0; a2 = 0; a = 0; break;
                }

                {
                    if (a > a1 && a > a2)
                    {
                        if (a >= thresh1)
                        {
                            Q.push (index);
                            *((sobelT*)*s) = a;      //= Misuse the first channel as storage for non-maximum suppression result.
                            *(retT*)*r = retT (255);
                        }
                        else
                        {
                            *(retT*)*r = retT (0);
                            *((sobelT*)*s) = a;
                        }
                    }
                    else
                    {
                        *((sobelT*)*s) = 0;
                        *(retT*)*r = retT (0);
                    }
                }
                s.incrementX ();
                r.incrementX ();
                ++index;
            }
            s.incrementY ();
            r.incrementY ();
        }


        //= FIXME: Hysteresis. May be done faster; this is for good readability.
        goIndex_t Nx = static_cast<goIndex_t> (sobel.getSizeX ());
        goIndex_t Ny = static_cast<goIndex_t> (sobel.getSizeY ());
        while (!Q.empty ())
        {
            goIndex_t i = Q.front ();
            Q.pop ();

            goIndex_t y = i / Nx;
            goIndex_t x = i - y * Nx;
            for (goIndex_t k = -1; k < 2; ++k)
            {
                for (goIndex_t l = -1; l < 2; ++l)
                {
                    if (l == 0 && k == 0)
                        continue;

                    if (x + k < 0 || y + l < 0 || x + k >= Nx || y + l >= Ny)
                        continue;

                    if (ret.getValue (x + k, y + l) <= 0 && sobel.getValue (x + k, y + l, 0, 0) >= thresh2)
                    {
                        Q.push (x + k + (y + l) * Nx);
                        ret.setValue (255.0, x + k, y + l);
                    }
                }
            }
        }
    }

    template <class imageT, class sobelT>
    static void _canny1 (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goSignal3DBase<void>& sobel, goDouble thresh1, goDouble thresh2)
    {
        switch (ret.getDataType().getID ())
        {
            case GO_INT8:   _canny2 <imageT, goInt8, sobelT> (image, ret, sobel,   thresh1,  thresh2); break;
            case GO_UINT8:  _canny2 <imageT, goUInt8, sobelT> (image, ret, sobel,  thresh1,  thresh2); break;
            case GO_INT16:  _canny2 <imageT, goInt16, sobelT> (image, ret, sobel,  thresh1,  thresh2); break;
            case GO_UINT16: _canny2 <imageT, goUInt16, sobelT> (image, ret, sobel, thresh1,  thresh2); break;
            case GO_INT32:  _canny2 <imageT, goInt32, sobelT> (image, ret, sobel,  thresh1,  thresh2); break;
            case GO_UINT32: _canny2 <imageT, goUInt32, sobelT> (image, ret, sobel, thresh1,  thresh2); break;
            case GO_FLOAT:  _canny2 <imageT, goFloat, sobelT> (image, ret, sobel,  thresh1,  thresh2); break;
            case GO_DOUBLE: _canny2 <imageT, goDouble, sobelT> (image, ret, sobel, thresh1,  thresh2); break;
            default: goLog::error ("goSignal::_canny1 (): wrong data type of ret.");
        }
    }

/** 
 * @brief Canny edge detector for 2D images.
 *
 * Compute the edge map after Canny.
 * The detector consists of 3 steps:
 * First, a Sobel operator is run on the image to find
 * gradient directions and magnitudes. If the image is a colour image (in RGB(A)), it will be
 * converted to a scalar gray value image first.
 * Second, non-maximum suppression is used to thin out the result of the Sobel operator.
 * Third, a hysteresis thresholding is used to reduce the edge map to prominent edgels.
 * Here, \c thresh1 is the high threshold, and \c thresh2 is the low threshold.
 * That means that first, only the responses higher than \c thresh1 are marked as edgels, and then
 * neighbouring responses higher than \c thresh2 are added successively.
 *
 * @param image Image
 * @param ret Return value, contains the edge map on successful return. Must be properly sized
 * or must be a goSignal3D<void>. If in the latter case it is not of matching size, it will be
 * resized and the type will be set to GO_UINT8.
 * @param thresh1 High threshold for hysteresis (default 80.0)
 * @param thresh2 Low threshold for hysteresis (default 40.0)
 * 
 * @return True is successful, false otherwise.
 */
bool canny (const goSignal3DBase<void>& image, goSignal3DBase<void>& ret, goDouble thresh1, goDouble thresh2)
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
        ret_p->make (image.getSize(), image.getSize(), goSize3D (8, 8, 0), 1);
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
    sobel->setBorderFlags (GO_X | GO_Y, GO_CONSTANT_BORDER);
    sobel->make (image.getSize(), image.getSize(), goSize3D (8, 8, 0), 3);
            // goSignal::defaultBlockSize2D(), goSize3D (8, 8, 1), 3);

    switch (image_p->getDataType().getID())
    {
        case GO_INT8: _canny1 <goInt8, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_UINT8: _canny1 <goUInt8, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_INT16: _canny1 <goInt16, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_UINT16: _canny1 <goUInt16, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_INT32: _canny1 <goInt32, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_UINT32: _canny1 <goUInt32, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_FLOAT: _canny1 <goFloat, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        case GO_DOUBLE: _canny1 <goDouble, goFloat> (*image_p, ret, *sobel, thresh1, thresh2); break;
        default: return false; break;
    }

    return true;    
}

};
