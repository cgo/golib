#ifndef GOGUI_INTERACTIVEDRAW_H
#define GOGUI_INTERACTIVEDRAW_H

#include <gtkmm.h>

#include <goautoptr.h>
#include <goplot/goplot.h>

namespace goGUI
{
    class InteractiveDrawPrivate;

    class InteractiveDraw
    {
        public:
            //= Modes
            enum
            {
                NONE,
                MOVE,
                EDIT
            };

        public:
            InteractiveDraw ();

            virtual ~InteractiveDraw ();

            void setDrawWidget (Gtk::Widget* widget);
            void setGraph (goAutoPtr<goPlot::Graph> g);

        private:
            InteractiveDrawPrivate* myPrivate;
    };
};

#endif
