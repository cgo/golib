/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/object2dtext.h>

#include <goplot/plot.h>
#include <goplot/texttraits.h>
#include <string>

#include <pango/pangocairo.h>

namespace goPlot
{
    Object2DText::Object2DText () 
        : Object2D (), 
        myString (), 
        myTraits (), 
        myX (0.0), myY (0.0), 
        myRelDx (0.0), myRelDy (0.0) { }
    Object2DText::Object2DText (const char* c) 
        : Object2D (), 
        myString (c), 
        myTraits (), 
        myX (0.0), myY (0.0), 
        myRelDx (0.0), myRelDy (0.0) { }

    Object2DText::Object2DText (const Object2DText& other)
        : Object2D (), 
        myString (), 
        myTraits (), 
        myX (0.0), myY (0.0), 
        myRelDx (0.0), myRelDy (0.0) 
    {
        *this = other;
    }

    Object2DText::~Object2DText () 
    {
    }

    Object2DText::operator const char* () const { return myString.c_str (); }
    Object2DText& Object2DText::operator= (const char* t) { myString = t; return *this; }
    Object2DText& Object2DText::operator= (const Object2DText& other) 
    { 
        myString = other.string ();
        myTraits = other.traits ();
        myX = other.x ();
        myY = other.y ();
        myRelDx = other.relDx ();
        myRelDy = other.relDy ();
        this->setContext (const_cast<cairo_t*> (other.context ()));

        return *this;
    }

    std::string& Object2DText::string () { return myString; }
    const std::string& Object2DText::string () const { return myString; }

    TextTraits& Object2DText::traits () { return myTraits; }
    const TextTraits& Object2DText::traits () const { return myTraits; }

    real Object2DText::x () const { return myX; }
    real Object2DText::y () const { return myY; }
    real Object2DText::relDx () const { return myRelDx; }
    real Object2DText::relDy () const { return myRelDy; }

    /** 
     * @brief Sets position of text.
     *
     * Sets the text position at (x,y) and moves the text in (w * rel_dx, h * rel_dy) direction,
     * where w and h are the text's width and height, respectively. You can use this
     * e.g. for alignment. Note that the motion is done before transformations, so it
     * gets transformed.
     *
     * @param x x position.
     * @param y y position. 
     * @param rel_dx relative movement in x direction
     * @param rel_dy relative movement in y direction
     */
    void Object2DText::setPosition (real x, real y, real rel_dx, real rel_dy) 
    { 
        myX = x; 
        myY = y; 
        myRelDx = rel_dx;
        myRelDy = rel_dy;
    }

    void Object2DText::setContext (cairo_t *context)
    {
        Object2D::setContext (context);

        myTraits.setContext (context);
    }

    void Object2DText::draw ()
    {
        cairo_t* c_context = this->context ();
        if (!c_context)
            return;

        cairo_save (c_context);

        PangoLayout* layout = myTraits.layout ();

        // pango_layout_set_text (layout, myString.c_str (), -1);
        //= Set text with pango's simple markup
        pango_layout_set_markup (layout, myString.c_str (), -1);
        myTraits.apply ();
        this->applyTransform (c_context);
        pango_cairo_update_layout (c_context, layout);

        int w,h;
        pango_layout_get_size (layout, &w, &h);
        //printf ("Pango layout size: %d, %d\n", w, h);
        //printf ("PANGO_SCALE: %d\n", PANGO_SCALE);

        // double x = 1.0, y = 1.0;
        double x = float(w) / float(PANGO_SCALE), y = float(h) / float(PANGO_SCALE);
        cairo_device_to_user_distance (c_context, &x, &y);
        // printf ("User coords: %lf, %lf\n", x, y);

        cairo_move_to (c_context, myX + myRelDx * x, myY + myRelDy * y);

        double Sx = 1.0;
        //double Sy = x * h / (y * w) * Sx; 
        double Sy = 1.0;

        cairo_identity_matrix (c_context);
        // cairo_translate (c_context, -w / float(PANGO_SCALE), 0.0);
        pango_cairo_update_layout (c_context, layout);
        cairo_scale (c_context, Sx, Sy);
        //cairo_scale (c_context, Sx / float(PANGO_SCALE), Sy / float(PANGO_SCALE));

        //{
        //    cairo_save (c_context);
        //    double x1, x2, y1, y2;
        //    pango_cairo_layout_path (c_context, layout);
        //    cairo_path_extents (c_context, &x1, &y1, &x2, &y2);
        //    printf ("Extents: %.4lf, %.4lf, %.4lf, %.4lf\n", x1, y1, x2, y2);
        //    cairo_restore (c_context);
        //}
        pango_cairo_show_layout (c_context, layout);


        cairo_restore (c_context);
    }

    /**
     * Used to find out borders with getBorderHint ()
     */
    void Object2DText::addToPath ()
    {
        cairo_t* c_context = this->context ();
        if (!c_context)
            return;

        PangoLayout* layout = myTraits.layout ();

        // pango_layout_set_text (layout, myString.c_str (), -1);
        pango_layout_set_markup (layout, myString.c_str (), -1);
        myTraits.apply ();
        this->applyTransform (c_context);
        pango_cairo_update_layout (c_context, layout);

        int w,h;
        pango_layout_get_size (layout, &w, &h);

        double x = float(w) / float(PANGO_SCALE), y = float(h) / float(PANGO_SCALE);
        cairo_device_to_user_distance (c_context, &x, &y);

        cairo_move_to (c_context, myX + myRelDx * x, myY + myRelDy * y);

        cairo_identity_matrix (c_context);
        pango_cairo_update_layout (c_context, layout);

        // double x1, x2, y1, y2;
        pango_cairo_layout_path (c_context, layout);
        // cairo_path_extents (c_context, &x1, &y1, &x2, &y2);
        // printf ("Extents: %.4lf, %.4lf, %.4lf, %.4lf\n", x1, y1, x2, y2);
    }
};
