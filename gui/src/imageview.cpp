/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gtkmm.h>
#include <gogui/imageview.h>
#include <gogui/cairoplot.h>
#include <goplot/goplot.h>
#include <golist.h>
#include <goautoptr.h>
#include <gosignalhelper.h>
#include <gosignal.h>
#include <gosignal3dref.h>

#include <list>
#include <vector>
#include <algorithm>

namespace goGUI
{
#if 0
    class ImageViewCurveAttr
    {
        public:
            ImageViewCurveAttr ()
                : thickness (1),
                linestyle (Gdk::LINE_SOLID),
                capstyle (Gdk::CAP_ROUND),
                joinstyle (Gdk::JOIN_ROUND),
                colour (3)
        {
            colour[0] = 1.0f;
            colour[1] = 0.0f;
            colour[2] = 0.0f;
        }

            ~ImageViewCurveAttr () { }

            int thickness;
            Gdk::LineStyle linestyle;
            Gdk::CapStyle capstyle;
            Gdk::JoinStyle joinstyle;
            goVectorf colour;
    };
#endif


    class ImageViewPrivate
    {
        public:
            typedef std::vector<goAutoPtr<goPlot::Object2DImage> > ImageObjectList;
            typedef std::vector<goAutoPtr<goSignal3DRef> >         ImageList;

        public:
            ImageViewPrivate() 
                : imageObjects (),
                  images (),
                  graph (new goPlot::Graph),
                  currentImageIndex (0),
                  currentImage (0),
                  changedCaller ()
        {
            graph->flipY ();
        }

            ~ImageViewPrivate () {}

            /*
             * @brief Create a fresh goPlot::Graph to draw things in.
             * @deprecate
             */
            void makeGraph ()
            {
            }

            // goList<goMatrixd>          curves;
            // goList<ImageViewCurveAttr> curveAttributes;  //= These are not used yet. Add them.
            // goAutoPtr<goSignal3DBase<void> >     image;
            ImageObjectList imageObjects;
            ImageList       images;
            goAutoPtr<goPlot::Graph>                     graph;
            goIndex_t                                    currentImageIndex;
            goAutoPtr<goSignal3DBase<void> >             currentImage;

            goCaller1<void, int>                         changedCaller; //= Called whenever something changes.
    };

    goGUI::ImageView::ImageView ()
        : Gtk::DrawingArea (), myPrivate(0)
    {
        myPrivate = new ImageViewPrivate;
        this->add_events (Gdk::EXPOSURE_MASK | Gdk::POINTER_MOTION_MASK | Gdk::BUTTON_PRESS_MASK | Gdk::BUTTON_RELEASE_MASK);
    }

    goGUI::ImageView::~ImageView ()
    {
        if (myPrivate)
        {
            delete myPrivate;
            myPrivate = 0;
        }
    }

    void goGUI::ImageView::draw ()
    {
        if (myPrivate->graph.isNull())
            return;

        int x, y, w, h;
        this->get_window()->get_geometry (x, y, w, h);
        // printf ("ImageView::draw(): size %d %d\n", w, h);
        goPlot::CairoPlot plot (this->get_window ()->create_cairo_context ()->cobj (), w, h, *myPrivate->graph);
    }

#if 0
    /** 
     * @brief Draws the curve given by point configuration matrix \c curve.
     * 
     * @param curve N x 2 matrix containing points in its rows.
     */
    void goGUI::ImageView::drawCurve (const goMatrixd& curve)
    {
        goGUI::Draw draw (this->get_window());
        ImageViewCurveAttr attr;
        Gdk::Color c;
        c.set_rgb_p (attr.colour[0], attr.colour[1], attr.colour[2]);
        if (!(bool)(draw.getGC()->get_colormap()))
        {
            printf ("ImageView::drawCurve(): Do not have a colormap.");
        }
        //draw.getGC()->set_foreground (c);
        draw.getGC()->set_rgb_fg_color (c);
        draw.getGC()->set_line_attributes (attr.thickness, attr.linestyle, attr.capstyle, attr.joinstyle);
        draw.curve (curve);
    }

    /** 
     * @brief Draws the curves stored in this object.
     */
    void goGUI::ImageView::drawCurves ()
    {
        goList<goMatrixd>::Element* el = myPrivate->curves.getFrontElement();
        while (el)
        {
            this->drawCurve (el->elem);
            el = el->next;
        }
    }

    /** 
     * @brief Draws the image stored in this object.
     */
    void goGUI::ImageView::drawImage ()
    {
        goGUI::Draw draw (this->get_window());
        draw.image (*myPrivate->image);
    }
#endif



    /** 
     * @brief Sets the image dimensions and format. A cairo image is created.
     *
     * The formats can be found in goPlot::Object2DImage.
     * getImage() can be used to get a reference goSignal3DBase object which references
     * the cairo image data directly.
     *
     * @param w Width
     * @param h Height
     * @param format Format, such as goPlot::Object2DImage::RGB24.
     * @param index Index of the image (default -1). If index < 0, a new image is made.
     */
    void goGUI::ImageView::setImage  (int w, int h, int format, goIndex_t index)
    {
//        if (!myPrivate->imageObjects.empty ())
//            myPrivate->graph->remove (myPrivate->imageObjects.front());
//        myPrivate->imageObjects.clear ();
        
        goAutoPtr<goPlot::Object2DImage> o = 0;
        goAutoPtr<goSignal3DRef>        im = 0;
        
        ImageViewPrivate::ImageObjectList::iterator it = myPrivate->imageObjects.begin ();
        ImageViewPrivate::ImageList::iterator itim = myPrivate->images.begin ();

//        if (index < 0)
//        {
//            index = myPrivate->currentImageIndex;
//        }

        if (index >= 0 && index < goIndex_t(myPrivate->imageObjects.size ()))
        {
            o = myPrivate->imageObjects[index];
            im = myPrivate->images[index];
            it = myPrivate->imageObjects.begin() + index;
            itim = myPrivate->images.begin() + index;
        }

        if (o.isNull() || im.isNull () || w != o->width () || h != o->height () || format != o->format ())
        {
            myPrivate->graph->remove (o);  // Remove the current image representation before making a new one
            o = myPrivate->graph->makeImage (w, h, format);
            im = this->imageRef (o);
        }

        if (myPrivate->imageObjects.empty () || index < 0)
        {
            //= Currently no image: push back the object
            myPrivate->imageObjects.push_back (o);
            myPrivate->images.push_back (im);
        }
        else
        {
            *it = o;
            *itim = im;
        }

        if (index == myPrivate->currentImageIndex)
        {
            o->setVisible (true);
        }
        else
        {
            o->setVisible (false);
        }

        assert (!o.isNull());
        assert (!im.isNull());

        goPlot::Trafo2D t = o->transform ();
        // t *= goPlot::Trafo2D (1.0 / float (w), 0.0, 0.0, 1.0 / float (h), 0.0, 0.0);
        t = goPlot::Trafo2D (1.0 / float (w), 0.0, 0.0, 1.0 / float (h), 0.0, 0.0);
        o->setTransform (t);

        myPrivate->changedCaller (IMAGE_SET);

        // myPrivate->graph->setDimensions (0, w, 0, h);
        //= FIXME: This tends to crash in a pango function.
        // myPrivate->graph->setDimensions (0.0, 1.0, 0.0, 1.0);
    }

    /** 
     * @brief Sets the image to draw.
     *
     * The image gets deep copied into a cairo image object.
     * getImage() can be used to get a reference goSignal3DBase object which references
     * the cairo image data directly. getImageObject() can be used to retrieve the respective goPlot::Object2DImage,
     * giving the same data.
     * 
     * @param image Image to draw. The image gets copied, but gets copied into a uint8 type array.
     * That means if the data range is out of the 8 bit range, it will get clamped.
     */
    void goGUI::ImageView::setImage (const goSignal3DBase<void>& image, goIndex_t index)
    {
//        if (!myPrivate->imageObjects.empty ())
//            myPrivate->graph->remove (myPrivate->imageObjects.front());

        if (myPrivate->graph.isNull())
        {
            goLog::error ("ImageView::setImage(): graph is null.");
            return;
        }

        goAutoPtr<goPlot::Object2DImage> o = 0;
        goAutoPtr<goSignal3DRef>        im = 0;
        
        ImageViewPrivate::ImageObjectList::iterator it = myPrivate->imageObjects.begin ();
        ImageViewPrivate::ImageList::iterator itim = myPrivate->images.begin ();

//        if (index < 0)
//        {
//            index = myPrivate->currentImageIndex;
//        }

        if (index >= 0 && index < goIndex_t(myPrivate->imageObjects.size ()))
        {
            o = myPrivate->imageObjects[index];
            im = myPrivate->images[index];
            it = myPrivate->imageObjects.begin() + index;
            itim = myPrivate->images.begin() + index;
        }

        myPrivate->graph->remove (o);  // Remove the current image representation before making a new one
        o = myPrivate->graph->addImage (image);
        im = this->imageRef (o);

        if (myPrivate->imageObjects.empty () || index < 0)
        {
            //= Currently no image: push back the object
            myPrivate->imageObjects.push_back (o);
            myPrivate->images.push_back (im);
        }
        else
        {
            *it = o;
            *itim = im;
        }

        if (index == myPrivate->currentImageIndex)
        {
            o->setVisible (true);
        }
        else
        {
            o->setVisible (false);
        }

        assert (!o.isNull());
        assert (!im.isNull());

        //= FIXME: This tends to crash in a pango function.
        // myPrivate->graph->setDimensions (0.0, 1.0, 0.0, 1.0);

        // myPrivate->imageObjects.clear ();
        // goAutoPtr<goPlot::Object2DImage> o = myPrivate->graph->addImage (image);
        // myPrivate->imageObjects.push_back (o);
        goPlot::Trafo2D t = o->transform ();
        //t *= goPlot::Trafo2D (1.0 / float (image.getSizeX()), 0.0, 0.0, 1.0 / float (image.getSizeY()), 0.0, 0.0);
        t = goPlot::Trafo2D (1.0 / float (image.getSizeX()), 0.0, 0.0, 1.0 / float (image.getSizeY()), 0.0, 0.0);
        o->setTransform (t);

        myPrivate->changedCaller (IMAGE_SET);

        {
            //= This call fixes the minimum size of the image widget to its original size
            // this->set_size_request (myPrivate->image->getSizeX(), myPrivate->image->getSizeY());
            goString info;
            goSignalInfoText (image, info);
            printf ("ImageView::setImage: Infotext\n%s\n", info.toCharPtr());
        }
    }

    /** 
     * @brief Get the image from this object.
     * 
     * @see getImage()
     *
     * @return Reference to the stored image object.
     */
    goAutoPtr<goPlot::Object2DImage> goGUI::ImageView::getImageObject (goIndex_t index)
    {
        if (myPrivate->imageObjects.empty())
            return 0;

        if (index < 0)
        {
            index = myPrivate->currentImageIndex;
        }

        ImageViewPrivate::ImageObjectList::iterator it = myPrivate->imageObjects.begin ();

        if (index >= 0 && index < goIndex_t(myPrivate->imageObjects.size ()))
        {
            goIndex_t i = 0;

            while (i < index && it != myPrivate->imageObjects.end ())
            {
                ++i;
                ++it;
            }

            if (it != myPrivate->imageObjects.end () && i == index)
            {
                return *it;
            }
        }

        return 0;
    }

    /** 
     * @brief Get the image from this object.
     * 
     * @see getImageObject()
     *
     * @return Reference to the stored image object.
     */
    goAutoPtr<goSignal3DBase<void> > goGUI::ImageView::getImage (goIndex_t index)
    {
        if (myPrivate->images.empty())
            return 0;

        if (index < 0)
        {
            index = myPrivate->currentImageIndex;
        }

        ImageViewPrivate::ImageList::iterator it = myPrivate->images.begin ();

        if (index >= 0 && index < goIndex_t(myPrivate->images.size ()))
        {
            goIndex_t i = 0;

            while (i < index && it != myPrivate->images.end ())
            {
                ++i;
                ++it;
            }

            if (it != myPrivate->images.end () && i == index)
            {
                return *it;
            }
        }

        return 0;
    }

    void goGUI::ImageView::removeImage (goIndex_t index)
    {
        if (myPrivate->images.empty())
            return;

        if (index < 0)
        {
            index = myPrivate->currentImageIndex;
        }

        if (index >= 0 && index < goIndex_t(myPrivate->images.size ()))
        {
            myPrivate->graph->remove (myPrivate->imageObjects [index]);
            ImageViewPrivate::ImageList::iterator im_it = myPrivate->images.begin ();
            ImageViewPrivate::ImageObjectList::iterator io_it = myPrivate->imageObjects.begin ();

            for (goIndex_t i = 0; i < index; ++i)
            {
                assert (im_it != myPrivate->images.end());
                assert (io_it != myPrivate->imageObjects.end());
                ++im_it;
                ++io_it;
            }

            assert (im_it != myPrivate->images.end());
            assert (io_it != myPrivate->imageObjects.end());

            myPrivate->imageObjects.erase (io_it);
            myPrivate->images.erase (im_it);
            myPrivate->changedCaller (IMAGE_REMOVED);
        }
    }

    void goGUI::ImageView::reorderImages (const std::vector<int>& order)
    {
        if (order.size () != myPrivate->images.size ())
        {
            return;
        }

        const size_t N = order.size ();
        ImageViewPrivate::ImageList newImages (N);
        ImageViewPrivate::ImageObjectList newImageObjects (N);
        
        ImageViewPrivate::ImageList& images = myPrivate->images;
        ImageViewPrivate::ImageObjectList& imageObjects = myPrivate->imageObjects;

        for (size_t i = 0; i < N; ++i)
        {
        	assert(order[i] >= 0 && order[i] < static_cast<int>(images.size()));
            newImages [i] = images [order[i]];
            newImageObjects [i] = imageObjects [order[i]];
        }

        images = newImages;
        imageObjects = newImageObjects;
    }

    goIndex_t goGUI::ImageView::getImageCount () const
    {
        return static_cast<goIndex_t> (myPrivate->images.size ());
    }

    /**
     * @brief Creates an object that references the data in the Object2DImage directly.
     *
     * The data in the object is organised linearly, rows first.
     * If the internal image format is ARGB32 or RGB24, the number of channels is 4.
     * Otherwise, the number of channels is 1. The data type is always GO_UINT8.
     *
     * The returned object should be a goSignal3DRef and has border size (8,8,0) set.
     *
     * @return Pointer to the reference.
     */
    goAutoPtr<goSignal3DBase<void> > goGUI::ImageView::imageRef (goAutoPtr<goPlot::Object2DImage> img)
    {
//        if (myPrivate->imageObjects.empty())
//            return 0;
//
//        goAutoPtr<goPlot::Object2DImage> img = myPrivate->imageObjects.front ();
        int chan = 0;
        switch (img->format())
        {
            case goPlot::Object2DImage::ARGB32:
            case goPlot::Object2DImage::RGB24: chan = 4; break;
            default: chan = 1; break;
        }
        goSize3D sz (img->stride() / chan, img->height(), 1);

        return new goSignal3DRef (img->data(), GO_UINT8, sz, sz, goSize3D (8, 8, 0), chan); 
    }

    /** 
     * @brief Get the Graph object used to draw the image.
     * 
     * @return goAutoPtr to the goPlot::Graph object.
     */
    goAutoPtr<goPlot::Graph> goGUI::ImageView::graph ()
    {
        return myPrivate->graph;
    }

    void goGUI::ImageView::setCurrentImage (goIndex_t i)
    {
        myPrivate->currentImageIndex = i;
        ImageViewPrivate::ImageObjectList::iterator it = myPrivate->imageObjects.begin ();
        goIndex_t in = 0;
        for (; it != myPrivate->imageObjects.end (); ++it, ++in)
        {
            if (in == i)
                (*it)->setVisible (true);
            else
                (*it)->setVisible (false);
        }

        myPrivate->changedCaller (CURRENT_IMAGE_CHANGED);
    }

    goIndex_t goGUI::ImageView::currentImageIndex () const
    {
        return myPrivate->currentImageIndex;
    }

    goCaller1<void, int>& goGUI::ImageView::changedCaller ()
    {
        return myPrivate->changedCaller;
    }

#if 0
    /** 
     * @brief Adds a curve to the list of curves in this object.
     * 
     * @param M Point configuration matrix, N x 2, one point per row.
     */
    void goGUI::ImageView::addCurve (const goMatrixd& M)
    {
        myPrivate->curves.append(M);
    }

    /** 
     * @brief Get list of curve point configuration matrices.
     * 
     * @return List of matrices.
     */
    const goList<goMatrixd>& goGUI::ImageView::getCurves () const
    {
        return myPrivate->curves;
    }

    /** 
     * @brief Remove curve \c i.
     * 
     * @param i Index of curve to remove.
     */
    void goGUI::ImageView::removeCurve (goIndex_t i)
    {
        myPrivate->curves.remove (myPrivate->curves (i));
    }

    /** 
     * @brief Set the curve points of curve \c i.
     * 
     * @param i Index of curve.
     * @param M N x 2 Matrix of curve points.
     */
    void goGUI::ImageView::setCurve (goIndex_t i, const goMatrixd& M)
    {
        goList<goMatrixd>::Element* el = myPrivate->curves (i);
        if (!el)
            return;
        el->elem = M;
    }
#endif

    /** 
     * @brief Draws everything.
     * 
     * @param event The event.
     * 
     * @return True if successful, false otherwise.
     */
    bool goGUI::ImageView::on_draw (Cairo::RefPtr<Cairo::Context> const& context)
    {
        //    this->drawImage ();
        //    this->drawCurves ();
        this->draw ();
        return Gtk::Widget::on_draw (context);
    }

};
