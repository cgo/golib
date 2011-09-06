/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gtkmm.h>
#include <gogui/progressbar.h>

goGUI::ProgressBar:ProgressBar ()
    : Gtk::ProgressBar (),
      myFractionStep (0.1),
      myFraction (0.0)
{
}

goGUI::ProgressBar::~ProgressBar ()
{
}

void goGUI::ProgressBar::setFractionStep (goDouble s)
{
    myFractionStep = s;
}

goDouble goGUI::ProgressBar::getFractionStep () const
{
    return myFractionStep;
}

void goGUI::ProgressBar::incrementFraction ()
{
    myFraction += myFractionStep;
    this->set_fraction (myFraction);
}

void goGUI::ProgressBar::update ()
{
    this->set_fraction (this->get_fraction());
}
