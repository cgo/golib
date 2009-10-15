#ifndef GOGUI_TEXTOUTPUT_H
#define GOGUI_TEXTOUTPUT_H

#include <gtkmm.h>

namespace goGUI
{
    class TextOutputPrivate;

    class TextOutput : public Gtk::TextView
    {
        public:
            TextOutput (int filedescriptor = -1);

            virtual ~TextOutput ();

            void setFile (int filedescriptor);

            bool ioHandler (Glib::IOCondition cond);

        private:
            TextOutputPrivate* myPrivate;
    };
};

#endif
