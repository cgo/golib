#ifndef GOGUI_OFFVIEW_CONTROL_H
#define GOGUI_OFFVIEW_CONTROL_H

#include <gogui/control.h>
#include <gogui/offview.h>
#include <gtkmm.h>

namespace goGUI
{
    class OFFViewControlPrivate;
    class OFFViewControl : public goGUI::Control
    {
        public:
            OFFViewControl ();
            virtual ~OFFViewControl ();

            void setOFFView (goGUI::OFFView* view);
            void addWidget  (Gtk::Widget& w);

            void angleChanged ();
            void loadOFF ();

        protected:
            OFFViewControlPrivate* myPrivate;
    };
}

#endif
