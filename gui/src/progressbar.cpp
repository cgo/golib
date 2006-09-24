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
