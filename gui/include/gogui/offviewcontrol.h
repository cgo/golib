/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_OFFVIEW_CONTROL_H
#define GOGUI_OFFVIEW_CONTROL_H

#include <gogui/control.h>
#include <gogui/offview.h>
#include <gtkmm.h>
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

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
            int lightChangedSlot ();

            int OFFViewRotated ();
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

            goCaller1<int, goVectorf>& angleChangedCaller ();

        private:
            OFFViewControl (const OFFViewControl&);
            OFFViewControl& operator= (const OFFViewControl&);

        protected:
            OFFViewControlPrivate* myPrivate;
    };
}

#endif
