#ifndef GOGUI_INTERACTIVEDRAW_H
#define GOGUI_INTERACTIVEDRAW_H

#include <gtkmm.h>

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

        private:
            InteractiveDrawPrivate* myPrivate;
    };
};

#endif
