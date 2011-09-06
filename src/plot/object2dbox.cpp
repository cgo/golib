/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/object2dbox.h>
#include <cairo/cairo.h>

goPlot::Object2DBox::Object2DBox (goDouble x1, goDouble y1, goDouble x2, goDouble y2)
    : Object2D (),
      myCorners (2, 2),
      myLineTraits ()
{
    this->setCorners (x1, y1, x2, y2);
}

goPlot::Object2DBox::~Object2DBox ()
{
}

goPlot::LineTraits& goPlot::Object2DBox::lineTraits ()
{
    return myLineTraits;
}

const goPlot::LineTraits& goPlot::Object2DBox::lineTraits () const
{
    return myLineTraits;
}

void goPlot::Object2DBox::draw ()
{
    cairo_t* cr = this->context ();
    if (!cr)
        return;

    cairo_save (cr);

    myLineTraits.apply (cr);
    this->applyTransform (cr);

    cairo_rectangle (cr, myCorners (0, 0), myCorners (0, 1),
            myCorners (1, 0) - myCorners (0, 0), myCorners (0, 1) - myCorners (1, 1));

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

void goPlot::Object2DBox::setCorners (goDouble x1, goDouble y1, goDouble x2, goDouble y2)
{
    myCorners (0,0) = x1; myCorners (0,1) = y1;
    myCorners (1,0) = x2; myCorners (1,1) = y2;
}

void goPlot::Object2DBox::setCorners (const goMatrixd& c)
{
    myCorners = c;
}

const goMatrixd& goPlot::Object2DBox::getCorners () const
{
    return myCorners;
}
