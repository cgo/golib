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
