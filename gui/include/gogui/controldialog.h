/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_CONTROLDIALOG_H
#define GOGUI_CONTROLDIALOG_H

#include <gogui/control.h>
#include <gtkmm.h>

namespace goGUI
{
    class ControlDialog : public Gtk::Dialog
    {
        public:
            ControlDialog (Control& ctrl);
            virtual ~ControlDialog ();

//            bool run ();
//            void cancel ();
//            void close ();

        private:
//            Gtk::Button myCancelButton;
//            Gtk::Button myCloseButton;
//            bool myCancelled;
    };
};

#endif
