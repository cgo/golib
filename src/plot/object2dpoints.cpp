#include <goplot/object2dpoints.h>

#include <goplot/plot.h>
#include <cairo/cairo.h>

#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goPlot
{
    Object2DPoints::Object2DPoints (size_t N) 
        : Object2D (),
        myPoints (new Points2DMatrix<goFloat> (N)), 
        myLineTraits () 
    {
        myLineTraits.setWidth (1.0);
    }

    Object2DPoints::Object2DPoints (goAutoPtr<Points2D> p)
        : Object2D (),
        myPoints (p),
        myLineTraits ()
    {
    }

    void Object2DPoints::setPoints (goAutoPtr<Points2D> p)
    {
        myPoints = p;
    }

    Object2DPoints::~Object2DPoints () { }

    Points2D& Object2DPoints::points () { return *myPoints; }
    const Points2D& Object2DPoints::points () const { return *myPoints; }

    LineTraits& Object2DPoints::lineTraits () { return myLineTraits; }
    const LineTraits& Object2DPoints::lineTraits () const { return myLineTraits; }

    void Object2DPoints::draw ()
    {
        if (myPoints.isNull ())
            return;

        if (myPoints->size () < 1)
        {
            return;
        }

        cairo_t* cr = this->context ();
        if (!cr)
            return;

        cairo_save (cr);

        myLineTraits.apply (cr);
        this->applyTransform (cr);

        cairo_move_to (cr, myPoints->x (0), myPoints->y (0));
        for (size_t i = 1; i < myPoints->size (); ++i)
        {
            cairo_line_to (cr, myPoints->x (i), myPoints->y (i));
        }
        //= Set to identity matrix before stroke, so that line width is in device coordinates (e.g. pixels)
        cairo_identity_matrix (cr);
        cairo_stroke (cr);

        //= Halbtransparent fuellen ...
        //cairo_stroke_preserve (cr);
        //RGBA c = lineTraits().colour ();
        //c.a = 0.2;
        //lineTraits().setColour (c);
        //lineTraits().apply (cr);
        //cairo_fill (cr);

        cairo_restore (cr);
    }

};
