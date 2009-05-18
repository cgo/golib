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
            ImageViewPrivate() 
                : imageObjects (),
                graph (0)
        {
            //goSignal3D<void> *img = new goSignal3D<void>;
            //image.set (img);
            //img->setDataType (GO_UINT8);
            //img->make (128,128,1,128,128,1,0,0,0,3);
            //img->fill (255.0f);

            graph.set (new goPlot::Graph);
        }

            ~ImageViewPrivate () {}

            /*
             * @brief Create a fresh goPlot::Graph to draw things in.
             */
            void makeGraph ()
            {
                graph.set (0);
                graph.set (new goPlot::Graph);

                this->graph->axis(0)->setVisible (false);
                this->graph->axis(1)->setVisible (false);

                // this->makeImages ();
            }

            /* 
             * @brief Make a cairo image of given proportions.
             * 
             * If an image already exists and fits the specifications, that
             * image (front of imageObjects) will be returned.
             *
             * @param width Width
             * @param height  Height
             * @param format Format as in goPlot::Object2DImage
             * 
             * @return Pointer to the new object
             */
            goAutoPtr<goPlot::Object2DImage> makeImage (int width, int height, int format = goPlot::Object2DImage::RGB24)
            {
                //if (this->image.isNull() || this->graph.isNull())
                //{
                //    return;
                //}

                //if (this->image->getBlockSize() != this->image->getSize())
                //{
                //    goLog::warning ("ImageView: currently no tiled images are supported.");
                //    return;
                //}

                //if (this->image->getDataType().getID() != GO_UINT8)
                //{
                //    goLog::warning ("ImageView: currently only uint8 image data is allowed.");
                //    return;
                //}

                //int format = 0;
                int channels = 0;
                switch (format)
                {
                    case goPlot::Object2DImage::ARGB32:
                        channels = 4; printf ("ARGB32\n"); break;
                            //= RGB24 is stored in 32 bits too, but the first 8 bits are ignored.
                    case goPlot::Object2DImage::RGB24:
                        channels = 4; printf ("RGB24\n"); break;
                    case goPlot::Object2DImage::A8:
                        channels = 1; printf ("A8\n"); break;
                    default: goLog::warning ("ImageView: image format not supported."); return 0; break;
                }

                goAutoPtr<goPlot::Object2DImage> img = 0;

                int w = width;
                int h = height;

                if (imageObjects.empty())
                {
                    img = new goPlot::Object2DImage;
                    img->createImage (format, w, h);
                    imageObjects.push_back (img);
                }
                else
                {
                    goAutoPtr<goPlot::Object2DImage> i = imageObjects.front();
                    if (i->format() != format || i->width() != w || i->height() != h)
                    {
                        imageObjects.clear ();
                        img = new goPlot::Object2DImage;
                        img->createImage (format, w, h);
                        imageObjects.push_back (img);
                    }
                    else
                    {
                        img = i;
                    }
                }
                //= Set the transform so that y-axes is flipped (Graph coordinate system starts at lower left
                //=  corner by default).
                // img->setTransform (goPlot::Trafo2D<goPlot::real> (1.0, 0.0, 0.0, -1.0, 0.0, img->height ()));
                this->graph->setTransform (goPlot::Trafo2D<goPlot::real> (1.0, 0.0, 0.0, -1.0, 0.0, 1.0));

                //= Cairo's format is different --- copy the data into a new buffer.

                // goCopySignalArray (this->image, img->data() + ptrOffset, strides);
                // img->setImage (static_cast<unsigned char*>(this->image->getPtr()), format, this->image->getSizeX(), this->image->getSizeY(), this->image->getSizeX() * this->image->getChannelCount());

                this->graph->add (img);
                return img;
            }

            /*
             * @brief Creates a new cairo image and copies the content of image to it.
             * 
             * @param image The image.
             */
            bool setImage (const goSignal3DBase<void>& image)
            {
                int format = 0;
                switch (image.getChannelCount())
                {
                    case 4: format = goPlot::Object2DImage::ARGB32; break;
                    case 3: format = goPlot::Object2DImage::RGB24; break;
                    case 1: format = goPlot::Object2DImage::A8; break;
                    default:
                        goLog::warning ("ImageView::setImage: channel count not supported."); return false; break;
                }

                int w = image.getSizeX();
                int h = image.getSizeY();

                goAutoPtr<goPlot::Object2DImage> img = this->makeImage (w, h, format);

                switch (image.getChannelCount())
                {
                    case 4:
                    case 3:
                        {
                            goSize3D sz (w, h, 1);
                            goSignal3DRef imgref (img->data(), GO_UINT8, sz, sz, goSize3D (0, 0, 0), 4);
                            goSignal::RGB2BGRA (*const_cast<goSignal3DBase<void>*> (&image), imgref);
                        }
                        break;
                    case 1:
                        {
                            goCopySignalArray (&image, img->data());
                        }
                        break;
                    default:
                        goLog::warning ("ImageView::makeImages: channel count not supported."); return false; break;
                }

                return true;
            }

            // goList<goMatrixd>          curves;
            // goList<ImageViewCurveAttr> curveAttributes;  //= These are not used yet. Add them.
            // goAutoPtr<goSignal3DBase<void> >     image;
            std::list<goAutoPtr<goPlot::Object2DImage> > imageObjects;
            goAutoPtr<goPlot::Graph>             graph;
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

        int w, h;
        this->get_window()->get_size (w, h);
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
     */
    void goGUI::ImageView::setImage  (int w, int h, int format)
    {
        myPrivate->graph = 0;
        myPrivate->makeGraph ();
        myPrivate->makeImage (w, h, format);
        myPrivate->graph->setDimensions (0, w, 0, h);
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
    void goGUI::ImageView::setImage (const goSignal3DBase<void>& image)
    {
        myPrivate->graph = 0;
        // myPrivate->image = image;

        myPrivate->makeGraph ();
        if (!myPrivate->setImage (image))
        {
            goLog::warning ("ImageView::setImage(): Could not set image to myPrivate object");
            return;
        }

        if (myPrivate->graph.isNull())
        {
            goLog::error ("ImageView::setImage(): graph is null.");
            return;
        }

        myPrivate->graph->setDimensions (0, image.getSizeX(), 0, image.getSizeY());
        // myPrivate->graph->setDimensions (0, 1.0, 0, 1.0);

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
    goAutoPtr<goPlot::Object2DImage> goGUI::ImageView::getImageObject ()
    {
        if (myPrivate->imageObjects.empty())
            return 0;
        else
            return myPrivate->imageObjects.front ();
    }

    /*
     * @brief Creates an object that references the data in the Object2DImage directly.
     *
     * The data in the object is organised linearly, rows first.
     * If the internal image format is ARGB32 or RGB24, the number of channels is 4.
     * Otherwise, the number of channels is 1. The data type is always GO_UINT8.
     *
     * @return Pointer to the reference.
     */
    goAutoPtr<goSignal3DBase<void> > goGUI::ImageView::getImage ()
    {
        if (myPrivate->imageObjects.empty())
            return 0;

        goAutoPtr<goPlot::Object2DImage> img = myPrivate->imageObjects.front ();
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
    bool goGUI::ImageView::on_expose_event (GdkEventExpose* event)
    {
        //    this->drawImage ();
        //    this->drawCurves ();
        this->draw ();
        return Gtk::Widget::on_expose_event (event);
    }

};
