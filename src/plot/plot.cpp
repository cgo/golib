/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/plot.h>

#include <math.h>
#include <cairo.h>
#include <pango/pangocairo.h>

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

namespace goPlot
{
    //= Matrix:
    //=   xx xy
    //=   yx yy
    //= Plus a translation (x0, y0)^T
    template <class T>
    Trafo2DT<T>::Trafo2DT (T xx, T yx, T xy, T yy, T x0, T y0)
        : xx (xx), xy (xy), yx (yx), yy (yy), x0 (x0), y0 (y0)
    {
    }

    template <class T>
    Trafo2DT<T>::Trafo2DT (const Trafo2DT<T>& t)
    {
        *this = t;
    }
    template <class T>
    Trafo2DT<T>& Trafo2DT<T>::operator= (const Trafo2DT<T>& t)
    {
        xx = t.xx;
        xy = t.xy;
        yx = t.yx;
        yy = t.yy;
        x0 = t.x0;
        y0 = t.y0;

        return *this;
    }

    template <class T>
    Trafo2DT<T>::~Trafo2DT () { }

    template <class T>
    void Trafo2DT<T>::print () const
    {
        printf ("%.4f %.4f | %.4f\n%.4f %.4f | %.4f\n", xx, xy, x0, yx, yy, y0);
    }

    template <class T>
    void Trafo2DT<T>::apply (cairo_t* cr)
    {
        cairo_matrix_t M;
        cairo_matrix_init (&M, xx, yx, xy, yy, x0, y0);
        cairo_transform (cr, &M);
    }

    /** 
     * @brief this = this * M2;
     * 
     * @param M2 
     */
    template <class T>
    void Trafo2DT<T>::operator*= (const Trafo2DT<T>& M2)
    {
        const Trafo2DT<T>& M1 = *this;

        Trafo2DT<T> temp;
        temp.xx = M1.xy*M2.yx+M1.xx*M2.xx;
        temp.xy = M1.xy*M2.yy+M1.xx*M2.xy;
        temp.yx = M1.yy*M2.yx+M1.yx*M2.xx;
        temp.yy = M1.yy*M2.yy+M1.yx*M2.xy;
        temp.x0 = M1.xy*M2.y0+M1.xx*M2.x0+M1.x0;
        temp.y0 = M1.yy*M2.y0+M1.yx*M2.x0+M1.y0;

        *this = temp;
    }

    template class Trafo2DT<real>;


    Object2D::~Object2D () { }

    void Object2D::setContext (cairo_t* c) { myContext = c; }

    const Trafo2DT<real>& Object2D::transform () const { return myTransform; };
    void Object2D::setTransform (const Trafo2DT<real>& T) { myTransform = T; };

    cairo_t* Object2D::context () { return myContext; }
    const cairo_t* Object2D::context () const { return myContext; }

    void Object2D::applyTransform (cairo_t* cr) 
    {
        this->myTransform.apply (cr);
        //cairo_matrix_t M;
        //cairo_matrix_init (&M, myTransform.xx, myTransform.yx, myTransform.xy, myTransform.yy, myTransform.x0, myTransform.y0);
        //cairo_transform (cr, &M);

        //cairo_matrix_t Mo;
        //cairo_get_matrix (cr, &Mo);
        //cairo_matrix_multiply (&Mo, &Mo, &M);
        //cairo_set_matrix (cr, &Mo);
    }

    Object2D::Object2D () 
        : myTransform (), myContext (0), myVisible (true)
    { }

    bool Object2D::visible () const
    {
        return myVisible;
    }

    void Object2D::setVisible (bool t)
    {
        myVisible = t;
    }

    LineTraits::LineTraits () 
        : myWidth (1.0), 
        myColour (0.0, 0.0, 0.0, 1.0) 
    { }

    void LineTraits::apply (cairo_t* cr) const
    {
        cairo_set_source_rgba (cr, myColour.r, myColour.g, myColour.b, myColour.a);
        cairo_set_line_width (cr, myWidth);
    }

    real        LineTraits::width () const { return myWidth; }
    void        LineTraits::setWidth (real w) { myWidth = w; }
    const RGBA& LineTraits::colour () const { return myColour; }
    void        LineTraits::setColour (const RGBA& c) { myColour = c; }


    FontList::FontList ()
    {
        PangoFontMap* fm = pango_cairo_font_map_get_default ();
        int n_fam = 0;
        PangoFontFamily **families = 0;
        pango_font_map_list_families (fm, &families, &n_fam);

        printf ("n_fam = %d\n", n_fam);

        for (int i = 0; i < n_fam; ++i)
        {
            printf ("%s\n", pango_font_family_get_name (families[i]));
        }

        g_free (families); // FIXME: Stimmt das so?
    }
};
