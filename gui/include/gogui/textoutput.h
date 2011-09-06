/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
