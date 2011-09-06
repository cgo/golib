/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/offviewcontrol.h>
#include <gogui/helper.h>
#include <gogui/gllightinput.h>

namespace goGUI
{
    class OFFViewControlPrivate
    {
        public:
            OFFViewControlPrivate ()
                : loadButton ("Load OFF"),
                  alignButton ("Align"),
                  lightButton ("Light"),
                  lightDialog ("Light"),
                  lightInput  (),
                  phi ("0.00"),
                  theta ("0.00"),
                  radius (),
                  vbox (),
                  tips (),
                  view (),
                  viewWindow (),
                  angle_changed (),
                  angle_changed_caller (),
                  viewConnection ()
            {
                this->radius.set_range (0.0, 10000.0);
                this->radius.set_digits (1);
                this->radius.set_increments (1.0, 10.0);
                this->radius.set_value (5.0);
            };
            ~OFFViewControlPrivate ()
            {
            };

            Gtk::Button loadButton;
            Gtk::Button alignButton;
            Gtk::Button lightButton;
            Gtk::Dialog lightDialog;
            goGUI::GLLightInput lightInput;
            Gtk::Label phi;
            Gtk::Label theta;
            Gtk::SpinButton radius;
            Gtk::VBox   vbox;
            Gtk::Tooltips tips;

            goGUI::OFFView view;
            Gtk::Window viewWindow;
            sigc::signal<void, goVectorf> angle_changed;
            goCaller1<int, goVectorf> angle_changed_caller;

            sigc::connection viewConnection;
    };
}

goGUI::OFFViewControl::OFFViewControl ()
    : goGUI::Control ("OFFView"), myPrivate (0)
{
    myPrivate = new goGUI::OFFViewControlPrivate;
   
    {
        myPrivate->viewWindow.set_title ("OFF View");
        myPrivate->viewWindow.add (myPrivate->view);
        myPrivate->viewWindow.show_all ();

        this->signal_hide().connect (sigc::mem_fun (*this, &OFFViewControl::onHide));
        this->signal_show().connect (sigc::mem_fun (*this, &OFFViewControl::onShow));
        //myPrivate->viewConnection = myPrivate->view.signalRotated().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::OFFViewRotated));
        myPrivate->view.callerRotated().connect (goMemberFunction<int, OFFViewControl> (this, &goGUI::OFFViewControl::OFFViewRotated));
    }

    myPrivate->tips.enable ();
    {
        Gtk::HBox* labelBox = Gtk::manage (new Gtk::HBox);
        labelBox->set_spacing (10);
        labelBox->pack_start (myPrivate->phi, Gtk::PACK_SHRINK);
        labelBox->pack_start (myPrivate->theta, Gtk::PACK_SHRINK);
        labelBox->pack_start (myPrivate->radius, Gtk::PACK_SHRINK);
        myPrivate->vbox.pack_start (*labelBox, Gtk::PACK_SHRINK);
    }
    myPrivate->tips.set_tip (myPrivate->phi, "phi angle");
    myPrivate->tips.set_tip (myPrivate->theta, "theta angle");
    myPrivate->tips.set_tip (myPrivate->radius, "view sphere radius");
    
    Gtk::HBox* buttonBox = Gtk::manage (new Gtk::HBox);
    buttonBox->pack_start (myPrivate->loadButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->loadButton, "Load OFF file");
    buttonBox->pack_start (myPrivate->alignButton, Gtk::PACK_SHRINK);
    buttonBox->pack_start (myPrivate->lightButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->alignButton, "Align object with principal axes");
    myPrivate->vbox.pack_start (*buttonBox, Gtk::PACK_SHRINK);
    this->add (myPrivate->vbox);

    myPrivate->lightDialog.get_vbox()->pack_start (myPrivate->lightInput, Gtk::PACK_SHRINK);
    myPrivate->lightDialog.hide ();

    // myPrivate->lightInput.signalChanged().connect (sigc::mem_fun (*this, &OFFViewControl::lightChangedSlot));
    myPrivate->lightInput.callerChanged().connect (goMemberFunction<int, OFFViewControl> (this, &OFFViewControl::lightChangedSlot));

    //myPrivate->phi.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    //myPrivate->theta.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->radius.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::radiusChanged));
    myPrivate->loadButton.signal_clicked().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::loadOFF));
    myPrivate->alignButton.signal_clicked().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::align));
    myPrivate->lightButton.signal_clicked().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::lightDialog));
}

goGUI::OFFViewControl::~OFFViewControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

//void goGUI::OFFViewControl::setOFFView (goGUI::OFFView* view)
//{
//    myPrivate->view = view;
//    myPrivate->viewConnection.disconnect();
//    if (view)
//    {
//        myPrivate->viewConnection = view->signalRotated().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::OFFViewRotated));
//    }
//}

int goGUI::OFFViewControl::OFFViewRotated ()
{
    printf ("OFFViewControl::OFFViewRotated()\n");
    goVectorf rotation = myPrivate->view.getSphericalPosition ();
    goString s; 
    s.resize (1024);
    sprintf (s.getPtr(), "%.2f", rotation[0]);
    myPrivate->phi.set_label (s.toCharPtr());
    sprintf (s.getPtr(), "%.2f", rotation[1]);
    myPrivate->theta.set_label (s.toCharPtr());

    //        myPrivate->phi.set_value (rotation[0]);
    //        myPrivate->theta.set_value (rotation[1]);
    myPrivate->radius.set_value (rotation[2]);
    // myPrivate->view->setRotation (rotation);
//    myPrivate->view.GLWidgetBegin();
//    myPrivate->view.glDraw();
//    myPrivate->view.GLWidgetEnd();
    myPrivate->angle_changed_caller (rotation);
}

void goGUI::OFFViewControl::radiusChanged ()
{
    goFloat r = myPrivate->radius.get_value();
    myPrivate->view.setRadius (r);
    myPrivate->view.queue_draw ();
    //myPrivate->view.GLWidgetBegin ();
    //myPrivate->view.glDraw ();
    //myPrivate->view.GLWidgetEnd ();
}

//void goGUI::OFFViewControl::angleChanged ()
//{
//    goVectorf angles (3);
//    angles[0] = myPrivate->phi.get_value();
//    angles[1] = myPrivate->theta.get_value();
//    angles[2] = myPrivate->radius.get_value();
//    if (myPrivate->view)
//    {
//        myPrivate->view->setRotation (angles);
//        myPrivate->view->GLWidgetBegin ();
//        myPrivate->view->glDraw ();
//        myPrivate->view->GLWidgetEnd ();
//    }
//    myPrivate->angle_changed (angles);
//}

void goGUI::OFFViewControl::addWidget (Gtk::Widget& w)
{
    myPrivate->vbox.pack_start (w, Gtk::PACK_SHRINK);
}

void goGUI::OFFViewControl::loadOFF ()
{
    goString filename;
    static goString lastFilename;
    if (!goGUI::getFilenameOpen (filename, lastFilename, "Open"))
    {
        return;
    }
    filename.getPathName (lastFilename);
    myPrivate->radius.set_value (1.0f);
    goVectorf r (3);
    r[0] = 0.0; r[1] = 0.0; r[2] = 1.0;
    myPrivate->view.setSphericalPosition (r);
    myPrivate->view.load (filename.toCharPtr());
    myPrivate->view.queue_draw ();
    //myPrivate->view.GLWidgetBegin ();
    //myPrivate->view.glDraw ();
    //myPrivate->view.GLWidgetEnd ();
}

void goGUI::OFFViewControl::align ()
{
    myPrivate->view.align ();
}

goGUI::OFFView* goGUI::OFFViewControl::getOFFView ()
{
    return &myPrivate->view;
}

void goGUI::OFFViewControl::setRotation (const goVectorf& r)
{
    if (r.getSize() < 3)
    {
        return;
    }
    //myPrivate->phi.set_value (r[0]);
    //myPrivate->theta.set_value (r[1]);
    myPrivate->radius.set_value (r[2]);
}

void goGUI::OFFViewControl::getRotation (goVectorf& r) const
{
    if (r.getSize() < 3)
    {
        r.resize (3);
    }
    r = myPrivate->view.getSphericalPosition();
//    r[0] = myPrivate->phi.get_value();
//    r[1] = myPrivate->theta.get_value();
//    r[2] = myPrivate->radius.get_value();
}

goFloat goGUI::OFFViewControl::getRadius () const
{
    return myPrivate->radius.get_value();
}

void goGUI::OFFViewControl::setRadius (goFloat r)
{
    myPrivate->radius.set_value (r);
}

sigc::signal<void, goVectorf>&
goGUI::OFFViewControl::angleChangedSignal ()
{
    return myPrivate->angle_changed;
}

goCaller1<int, goVectorf>&
goGUI::OFFViewControl::angleChangedCaller ()
{
    return myPrivate->angle_changed_caller;
}

void goGUI::OFFViewControl::lightDialog ()
{
    myPrivate->lightInput.set (myPrivate->view.getLight());
    myPrivate->lightDialog.show_all ();
}

int goGUI::OFFViewControl::lightChangedSlot ()
{
    goGL::Light light;
    myPrivate->lightInput.get (light);
    myPrivate->view.setLight (light);
    return 0;
}

void goGUI::OFFViewControl::onHide ()
{
    myPrivate->viewWindow.hide ();
}

void goGUI::OFFViewControl::onShow ()
{
    myPrivate->viewWindow.show ();
}
