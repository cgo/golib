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

            goGUI::OFFView* getOFFView ();
            void            addWidget  (Gtk::Widget& w);

            void lightDialog ();
            void lightChangedSlot ();

            void OFFViewRotated ();
            // void angleChanged ();
            void radiusChanged ();
            void loadOFF ();
            void align ();

            void onShow ();
            void onHide ();

            void setRotation (const goVectorf& v);
            void getRotation (goVectorf& v) const;

            goFloat getRadius () const;
            void    setRadius (goFloat r);
            sigc::signal<void, goVectorf>& angleChangedSignal ();

        protected:
            OFFViewControlPrivate* myPrivate;
    };
}

#endif
