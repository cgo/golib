#ifndef GOGUI_DRAW_H
#define GOGUI_DRAW_H

#include <gomatrix.h>
#include <gosignal3d.h>

namespace goGUI
{
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
};

#endif
