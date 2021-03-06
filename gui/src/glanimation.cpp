/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/glanimation.h>
#include <gogui/helper.h>
#include <gofunctor.h>

namespace goGUI
{
    class GLAnimationPrivate
    {
        public:
            GLAnimationPrivate ()
                : animation (0),
                  waypoint (0),
                  loadButton ("Load Animation"),
                  saveButton ("Save Animation"),
                  waypointsButton (),
                  tScale (0.0, 1.0, 0.01),
                  tAdjustment (Gtk::Adjustment::create(0.0, 0.0, 1.0, 0.01, 0.1)),
                  //signalPositionChanged (),
                  //signalWaypointSelected (),
                  waypointSelectedCaller (),
                  positionChangedCaller ()
            {
                tScale.set_adjustment (tAdjustment);
                waypointsButton.set_digits (0);
                waypointsButton.set_range (0,0);
                waypointsButton.set_numeric (true);
                waypointsButton.set_increments (1, 10);
                waypointsButton.set_value (0);
                waypoint.set (new goGL::Waypoint);
            };
            ~GLAnimationPrivate () {};

            void updateWaypoints ()
            {
                if (animation.isNull())
                    return;

                if (animation->getWaypoints().getSize() > 0)
                    waypointsButton.set_range (0, animation->getWaypoints().getSize() - 1);
                else
                    waypointsButton.set_range (0, 0);
            };

            goAutoPtr<goGL::Animation> animation;
            goAutoPtr<goGL::Waypoint> waypoint;

            Gtk::Button         loadButton;
            Gtk::Button         saveButton;
            Gtk::SpinButton     waypointsButton;
            Gtk::HScale         tScale;
            Glib::RefPtr<Gtk::Adjustment>     tAdjustment;

            // sigc::signal<void>  signalPositionChanged;
            // sigc::signal<void>  signalWaypointSelected;
            
            goCaller0<int>      waypointSelectedCaller;  // workaround for bug at uni -- signalWaypointSelected() crashes when called (?????).
            goCaller0<int>      positionChangedCaller;
    };
};

goGUI::GLAnimation::GLAnimation ()
    : Gtk::Frame (),
      myPrivate (0)
{
    myPrivate = new GLAnimationPrivate;
    
    this->set_label ("Animation");

    Gtk::VBox* vbox = Gtk::manage (new Gtk::VBox);
    {
        Gtk::HBox* hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->waypointsButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->saveButton, Gtk::PACK_SHRINK);
        hbox->pack_start (myPrivate->loadButton, Gtk::PACK_SHRINK);
        vbox->pack_start (*hbox, Gtk::PACK_SHRINK);
    }
    {
        vbox->pack_start (myPrivate->tScale, Gtk::PACK_SHRINK);
    }
    {
        myPrivate->waypointsButton.signal_value_changed().connect (sigc::mem_fun (*this, &GLAnimation::waypointSelected));

        myPrivate->loadButton.signal_clicked().connect (sigc::mem_fun (*this, &GLAnimation::loadAnimation));
        myPrivate->saveButton.signal_clicked().connect (sigc::mem_fun (*this, &GLAnimation::saveAnimation));
        myPrivate->tScale.signal_value_changed().connect (sigc::mem_fun (*this, &GLAnimation::tValueChanged));
    }

    this->add (*vbox);
    this->show_all ();
}

goGUI::GLAnimation::~GLAnimation ()
{
    printf ("~GLAnimation\n");
    fflush (stdout);
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::GLAnimation::setAnimation (goAutoPtr<goGL::Animation> a)
{
    myPrivate->animation = a;
}

goAutoPtr<goGL::Animation> goGUI::GLAnimation::getAnimation ()
{
    return myPrivate->animation;
}

void goGUI::GLAnimation::addWaypoint (const goGL::Waypoint& wp)
{
    if (!myPrivate->animation.isNull())
    {
        myPrivate->animation->addWaypoint (wp);
        myPrivate->updateWaypoints ();
    }
}

void goGUI::GLAnimation::removeWaypoint (int index)
{
    if (!myPrivate->animation.isNull())
    {
        myPrivate->animation->removeWaypoint (index);
        myPrivate->updateWaypoints ();
    }
}

void goGUI::GLAnimation::prependWaypoint (const goGL::Waypoint& wp, int index)
{
    if (!myPrivate->animation.isNull())
    {
        myPrivate->animation->insertWaypoint (index, wp);
        myPrivate->updateWaypoints ();
    }
}

void goGUI::GLAnimation::appendWaypoint (const goGL::Waypoint& wp, int index)
{
    if (!myPrivate->animation.isNull())
    {
        myPrivate->animation->insertWaypoint (index + 1, wp);
        myPrivate->updateWaypoints ();
    }
}

void goGUI::GLAnimation::loadAnimation ()
{
    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameOpen (fname, path, "Load Animation"))
    {
        return;
    }

    fname.getPathName (path);
    
    if (myPrivate->animation.isNull())
        myPrivate->animation.set (new goGL::Animation);

    if (!myPrivate->animation->readASCII (fname.toCharPtr()))
    {
        Gtk::MessageDialog dlg ("Loading animation failed.");
        dlg.run ();
        return;
    }

    myPrivate->updateWaypoints ();
}

void goGUI::GLAnimation::saveAnimation ()
{
    if (myPrivate->animation.isNull())
    {
        Gtk::MessageDialog dlg ("No animation set.");
        dlg.run ();
        return;
    }

    static goString path = "./";
    goString fname;
    if (!goGUI::getFilenameSave (fname, path, "Save Animation"))
    {
        return;
    }

    fname.getPathName (path);
    
    if (!myPrivate->animation->writeASCII (fname.toCharPtr()))
    {
        Gtk::MessageDialog dlg ("Saving animation failed.");
        dlg.run ();
        return;
    }
}

void goGUI::GLAnimation::waypointSelected ()
{
    printf ("%p\n", this);
    printf ("WP %d selected.\n", this->selectedWaypoint());
    // myPrivate->signalWaypointSelected ();
    myPrivate->waypointSelectedCaller ();
}

goCaller0<int>& goGUI::GLAnimation::waypointSelectedCaller ()
{
    return myPrivate->waypointSelectedCaller;
}

goCaller0<int>& goGUI::GLAnimation::positionChangedCaller ()
{
    return myPrivate->positionChangedCaller;
}

void goGUI::GLAnimation::tValueChanged ()
{
    if (myPrivate->animation.isNull() || myPrivate->waypoint.isNull())
        return;
    
    goDouble t = myPrivate->tScale.get_value ();
    myPrivate->animation->interpolate (t, *myPrivate->waypoint);

    myPrivate->waypoint->getTranslation().print ();

    // myPrivate->signalPositionChanged ();
    myPrivate->positionChangedCaller ();
}

//sigc::signal<void>& goGUI::GLAnimation::signalPositionChanged ()
//{
//    return myPrivate->signalPositionChanged;
//}

//sigc::signal<void>& goGUI::GLAnimation::signalWaypointSelected ()
//{
//    return myPrivate->signalWaypointSelected;
//}

goAutoPtr<goGL::Waypoint> goGUI::GLAnimation::getWaypoint ()
{
    return myPrivate->waypoint;
}

int goGUI::GLAnimation::selectedWaypoint ()
{
    return myPrivate->waypointsButton.get_value_as_int ();
}

goAutoPtr<goGL::Waypoint> goGUI::GLAnimation::getSelectedWaypoint ()
{
    int i = myPrivate->waypointsButton.get_value_as_int ();
    goAutoPtr<goGL::Waypoint> ret (0);
    if (myPrivate->animation.isNull ())
        return ret;

    if (i >= 0 && i < myPrivate->animation->getWaypoints().getSize())
        ret.set (new goGL::Waypoint (myPrivate->animation->getWaypoint (i)));

    return ret;
}
