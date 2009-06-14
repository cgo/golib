#include <goplot/graph.h>

#include <goautoptr.h>

#include <cairo/cairo.h>

#include <goplot/plot.h>
#include <goplot/graphaxis.h>
#include <goplot/autoptr.h>
#include <goplot/object2dimage.h>

#include <exception>
#include <list>
#include <vector>
#include <algorithm>

#include <assert.h>

#ifndef GOSIGNAL_H
# include <gosignal.h>
#endif
#ifndef GOSIGNAL3DREF_H
# include <gosignal3dref.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goPlot
{
    /** 
     * @brief Construct a graph with axes.
     * 
     * @param dim Dimension -- currently, only 2 is supported.
     */
    Graph::Graph (int dim)
        : Object2D  (), 
        myAxes    (), 
        myObjects (), 
        myDim     (dim), 
        myTitle   ("")
    {
        if (dim < 1)
            return;

        myAxes.resize (0);
        int axes = 0;
        if (dim == 2)
        {
            axes = 4;
        }
        else
        {
            throw std::exception ();
        }

        myAxes.reserve (axes);

        for (int i = 0; i < axes; ++i)
        {
            myAxes.push_back (goAutoPtr<GraphAxis> (new GraphAxis));
            myAxes[i]->setIndex (i);
            //if (axes <= 4)
            //    myAxes[i]->configure (configs[i]);
        }

        this->axis (0)->setVisible (true);
        this->axis (1)->setVisible (true);
        this->axis (2)->setVisible (false);
        this->axis (3)->setVisible (false);
        //this->axis (0)->configure (this->axis(0)->configuration() | GraphAxis::MIDDLE);
        //this->axis (1)->configure (this->axis(1)->configuration() | GraphAxis::MIDDLE);

        this->axis (2)->enableTics (true);
        this->axis (2)->enableTicsText (true);
        this->axis (3)->enableTics (true);
        this->axis (3)->enableTicsText (true);
    }

    Graph::Graph (const Graph& other)
    {
        *this = other;
    }

    Graph& Graph::operator= (const Graph& other)
    {
        int dim = other.dim ();
        myDim = dim;
        myAxes.resize (0);
        myAxes.reserve (other.myAxes.size ());
        for ( size_t i = 0; i < other.myAxes.size (); ++i)
        {
            myAxes.push_back (goAutoPtr<GraphAxis> (new GraphAxis (*other.axis(i))));
        }

        //= FIXME === Pointers are copied, not objects. Is that wanted?
        myObjects = other.myObjects;

        //= Also use the same transformation
        this->setTransform (other.transform ());

        return *this;
    }

    Graph::~Graph ()
    {
    }

    /** 
     * @brief Disables all axes.
     *
     * This can e.g. be used if you want to use this graph as an image display.
     */
    void Graph::disableAxes ()
    {
        const goSize_t N = this->myAxes.size();
        for (goSize_t i = 0; i < N; ++i)
        {
            this->axis(i)->setVisible (false);
        }
    }

    /** 
     * @brief Multiply the current transformation so that
     * the Y axis is mirrored.
     *
     * Useful if displaying an image which starts in the upper left corner.
     */
    void Graph::flipY ()
    {
        Trafo2D t = this->transform();
        t *= Trafo2D (1, 0, 0, -1, 0, 1);
        this->setTransform (t);
    }

    /** 
     * @brief Removes all objects.
     */
    void Graph::clear ()
    {
        this->objects2D ().clear ();
    }

    //================================================================================
    //=
    //= Convenience functions for adding new objects.
    //=

    /** 
     * @brief Add a new curve to the graph.
     * 
     * @param c Curve points as configuration matrix (one point per row)
     * 
     * @return Pointer to the new Object2DPoints.
     */
    goAutoPtr<Object2DPoints> Graph::addCurve (const goMatrixd& c)
    {
        goAutoPtr<goPlot::Object2DPoints> points = new goPlot::Object2DPoints;
        Points2DMatrix<goDouble> *M = new Points2DMatrix<goDouble> (c);
        points->setPoints (M);

        this->add (points);

        return points;
    }

    /** 
     * @brief Makes a new Object2DImage with the given parameters.
     * 
     * @param width Width in pixels
     * @param height Height in pixels
     * @param format Format, one of Object2DImage::ARGB32, Object2DImage::RGB24, Object2DImage::A8
     * 
     * @return Pointer to the new Object2DImage
     */
    goAutoPtr<Object2DImage> Graph::makeImage (int width, int height, int format)
    {
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
            default: goLog::warning ("Graph: image format not supported."); return 0; break;
        }

        goAutoPtr<goPlot::Object2DImage> img = 0;
        img = new goPlot::Object2DImage;
        img->createImage (format, width, height);

        //= Set the transform so that y-axes is flipped (Graph coordinate system starts at lower left
        //=  corner by default).
        // img->setTransform (goPlot::Trafo2DT<goPlot::real> (1.0, 0.0, 0.0, -1.0, 0.0, img->height ()));
        // this->graph->setTransform (goPlot::Trafo2DT<goPlot::real> (1.0, 0.0, 0.0, -1.0, 0.0, 1.0));

        this->add (img);
        return img;
    }

    /** 
     * @brief Adds an image to the graph.
     *
     * You may want to adjust the transformation so that the image fits in the graph.
     * 
     * @param image The image to draw.
     * 
     * @return Pointer to the new Object2DImage object.
     */
    goAutoPtr<Object2DImage> Graph::addImage (const goSignal3DBase<void>& image)
    {
        int format = 0;
        switch (image.getChannelCount())
        {
            case 4: format = goPlot::Object2DImage::ARGB32; break;
            case 3: format = goPlot::Object2DImage::RGB24; break;
            case 1: format = goPlot::Object2DImage::RGB24; break;
            default:
                    goLog::warning ("Graph::addImage: channel count not supported."); return false; break;
        }

        int w = image.getSizeX();
        int h = image.getSizeY();

        goAutoPtr<goPlot::Object2DImage> img = this->makeImage (w, h, format);

        if (img.isNull())
        {
            goLog::error ("Graph::addImage(): img is null.");
            return 0;
        }

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
                    goSize3D sz (w, h, 1);
                    goSignal3DRef imgref (img->data(), GO_UINT8, sz, sz, goSize3D (0, 0, 0), 4);
                    int source_i [] = {0, 0, 0};
                    int target_i [] = {0, 1, 2};
                    goSignal::convert (*const_cast<goSignal3DBase<void>*>(&image), imgref, source_i, target_i, 3);
                }
                break;
            default:
                goLog::warning ("Graph::addImage: channel count not supported."); 
                this->objects2D().pop_back (); // remove the object added by makeImage()
                return 0; 
                break;
        }

        return img;
    }

    //=
    //= End convenience functions.
    //================================================================================

    /** 
     * @brief Set the dimensions of this graph's axes.
     * 
     * Convenience functions calling setLower and setUpper of all axes.
     * Also sets the tics to 10 tics on each axis. 
     *
     * The default value range for the axes is [0,1].
     * Note that these dimensions define the "size" of the graph, that means
     * the actual value range is in the given range.
     * So if, for example, you want an object to fill the graph, you should scale
     * it via its own transformation to the range given by the parameters to setDimensions.
     *
     * @todo Setting the tics may not be wanted (rethink this).
     *
     * @param xmin Min x value
     * @param xmax Max x value
     * @param ymin Min y value
     * @param ymax Max y value
     */
    void Graph::setDimensions (real xmin, real xmax, real ymin, real ymax)
    {
        this->axis(0)->setLower (xmin);
        this->axis(0)->setUpper (xmax);
        this->axis(1)->setLower (ymin);
        this->axis(1)->setUpper (ymax);
        this->axis(2)->setLower (xmin);
        this->axis(2)->setUpper (xmax);
        this->axis(3)->setLower (ymin);
        this->axis(3)->setUpper (ymax);
        //graph.axis(0)->lineTraits().setWidth (0.01);
        //graph.axis(1)->lineTraits().setWidth (0.01);
        this->axis(0)->setTics (10);
        this->axis(1)->setTics (10);
        this->axis(2)->setTics (15);
        this->axis(3)->setTics (15);
    }


    /** 
     * @brief Set the font for the tics text.
     *
     * Convenience function that sets the font of all axes to \c f.
     * @see GraphAxis::setTicsFont().
     *
     * @param f Font description text.
     */
    void Graph::setTicsFont (const char* f)
    {
        for (size_t i = 0; i < this->myAxes.size (); ++i)
        {
            this->axis (i)->setTicsFont (f);
        }
    }

    /** 
     * @brief Number of dimensions (always 2)
     * 
     * @return 2.
     */
    int              Graph::dim () const { return myDim; }


    /** 
     * @brief Get a pointer to a specific axis.
     * 
     * Valid numbers are {0,1,2,3}. 0,1 are the x,y axes, 2 is the upper x axis, 3 is the right y axis.
     *
     * @param i Number of the axis.
     * 
     * @return Pointer to the GraphAxis object.
     */
    goAutoPtr<GraphAxis>  Graph::axis (int i) 
    { 
        if (i >= 0 && i < int (this->myAxes.size ()))
            return myAxes[i];

        throw std::exception (); // ("Graph::axis(): Index out of range");
    }

    /** 
     * @brief Get a pointer to a specific axis.
     * 
     * Valid numbers are {0,1,2,3}. 0,1 are the x,y axes, 2 is the upper x axis, 3 is the right y axis.
     *
     * @param i Number of the axis.
     * 
     * @return Pointer to the GraphAxis object.
     */
    const goAutoPtr<GraphAxis> Graph::axis (int i) const 
    { 
        if (i >= 0 && i < this->dim ())
            return myAxes[i];

        throw std::exception (); // ("Graph::axis(): Index out of range");
    }

    /** 
     * @brief Adds a Object2D to this graph.
     * 
     * @param o The object pointer (managed by a goAutoPtr)
     */
    void Graph::add (goAutoPtr<Object2D> o)
    {
        if (!o.isNull ())
            o->setContext (this->context ());

        myObjects.push_back (o);
    }

    /** 
     * @brief Removes all occurrences of \c o in this graph.
     * 
     * @param o Object pointer to remove.
     */
    void Graph::remove (goAutoPtr<Object2D> o)
    {
        std::list<goAutoPtr<Object2D> >::iterator first = myObjects.begin ();
        std::list<goAutoPtr<Object2D> >::iterator last  = myObjects.end ();
        std::list<goAutoPtr<Object2D> >::iterator it;
        while ((it = std::find (first, last, o)) != last)
        {
            myObjects.erase (it);
            first = myObjects.begin();
            last = myObjects.end();
        }
    }

    /** 
     * @brief Sets the title of the graph.
     *
     * The title is not drawn by the Graph::draw() method. 
     * It is only stored here and should be drawn by the class arranging and plotting the graphs.
     * 
     * @param t Title
     */
    void Graph::setTitle (const char* t)
    {
        myTitle = t;
    }

    /** 
     * @brief Get the title text.
     * @see setTitle()
     * 
     * @return The title text
     */
    const char* Graph::title () const { return myTitle.c_str (); }

    /** 
     * @brief Adds a text label.
     *
     * Convenience function that creates a Object2DText and
     * calls add().
     *
     * \c rel_x and \c rel_y can be used to align the label (see Object2DText::setPosition()).
     *
     * @param x X position
     * @param y Y position
     * @param label Label text
     * @param rel_x Relative x movement in units of text width
     * @param rel_y Relative y movement in units of text height
     *
     * @return Reference to the new Object2DText
     */
    Object2DText& Graph::addLabel (double x, double y, const char* label, double rel_x, double rel_y)
    {
        Object2DText* txt = new Object2DText (label);

        if (!txt)
            throw std::exception ();

        txt->setPosition (x, y, rel_x, rel_y);
        this->add (goAutoPtr<Object2D> (txt));
        return *txt;
    }

    /** 
     * @brief Sets the context and hands it on to all axes and drawable objects.
     * 
     * @param c The context
     */
    void Graph::setContext (cairo_t* c)
    {
        Object2D::setContext (c);

        //= Draw axes
        for (int i = 0; i < int (myAxes.size ()); ++i)
        {
            assert (!myAxes[i].isNull ());
            myAxes[i]->setContext (c);
        }
        for (std::list<goAutoPtr<Object2D> >::iterator it = myObjects.begin (); it != myObjects.end (); ++it)
        {
            (*it)->setContext (c);
        }
    }

    /** 
     * @brief Applies the transform given by the current axes to the given context.
     * 
     * This is called by draw() and probably does not need to be called by the user.
     *
     * @param c The context to apply the transformation to.
     */
    void Graph::applyAxesTransform (cairo_t* c)
    {
        cairo_matrix_t M;
        cairo_matrix_init (&M, 1.0 / myAxes[0]->length (), 0, 0, 1.0 / myAxes[1]->length (), -myAxes[0]->lower() / myAxes[0]->length(), -myAxes[1]->lower() / myAxes[1]->length());
        cairo_transform (c, &M);
    }

    /** 
     * @brief Draws axes (according to their visibilities) and all objects.
     */
    void Graph::draw ()
    {
        cairo_t* cr = this->context ();
        if (!cr)
            return;

        cairo_save (cr);

        this->applyTransform (cr);

        //= Draw axes
        for (int i = 0; i < int (myAxes.size ()); ++i)
        {
            assert (!myAxes[i].isNull ());
            myAxes[i]->draw ();
        }

        this->applyAxesTransform (cr);

        //cairo_matrix_t Mo;
        //cairo_get_matrix (cr, &Mo);
        //cairo_matrix_multiply (&Mo, &Mo, &M);
        //cairo_set_matrix (cr, &Mo);
        //= Draw other objects
        for (std::list<goAutoPtr<Object2D> >::iterator it = myObjects.begin (); it != myObjects.end (); ++it)
        {
            if ((*it)->visible ())
                (*it)->draw ();
        }
        cairo_restore (cr);
    }

    void Graph::getBorderHint (double w, double h, double& x1, double& y1, double& x2, double& y2)
    {
        cairo_t* cr = this->context ();
        if (!cr)
            return;

        cairo_save (cr);

        cairo_new_path (cr);

        this->applyTransform (cr);

        x1 = 0.0;
        x2 = 0.0;
        y1 = 0.0;
        y2 = 0.0;
        //= Draw axes
        int sz = int (myAxes.size ());
        // int sz = 2;
        for (int i = 0; i < sz; ++i)
        {
            double xx1, xx2, yy1, yy2;

            assert (!myAxes[i].isNull ());
            myAxes[i]->getBorderHint (w, h, xx1, yy1, xx2, yy2);
            x1 = std::max (xx1, x1);
            x2 = std::max (xx2, x2);
            y1 = std::max (yy1, y1);
            y2 = std::max (yy2, y2);
        }

        cairo_new_path (cr);

        cairo_restore (cr);
    }

};
