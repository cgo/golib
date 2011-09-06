/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_DRAW_H
#define GOGUI_DRAW_H

#include <gomatrix.h>
#include <gosignal3d.h>
#include <gtkmm.h>

namespace goGUI
{
/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Interface for drawing on a drawable widget.
     *
     * This currently uses Gdk::Drawable, and will in the future use
     * Cairo (in newer versions of gtkmm).
     *
     * In the constructor, give e.g. the result from \c Widget::get_window().
     *
     */
    class Draw
    {
        public:
            Draw (Glib::RefPtr<Gdk::Drawable> drawable); // Widget::get_window()
            virtual ~Draw ();

            void line (goDouble x0, goDouble y0, goDouble x1, goDouble y1);
            void point (goDouble x0, goDouble y0);
            void curve (const goMatrixd& points);
            void image (const goSignal3D<void>& image);

            Glib::RefPtr<Gdk::GC> getGC ();

        private:
            Glib::RefPtr<Gdk::Drawable> myDrawable;
            Glib::RefPtr<Gdk::GC>       myGC;
    };
/** 
 * @}
 */
};

#endif
