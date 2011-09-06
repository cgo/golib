/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/countfilenames.h>

goGUI::CountFilenames::CountFilenames ()
    : Gtk::HBox (),
      myBaseEntry (),
      myCount (),
      mySuffixEntry ()
{
    Gtk::Alignment* a = Gtk::manage (new Gtk::Alignment(Gtk::ALIGN_LEFT, Gtk::ALIGN_TOP, 1.0, 0.0));
    a->add (myBaseEntry);
    this->pack_start (*a, Gtk::PACK_SHRINK);
    a = Gtk::manage (new Gtk::Alignment(Gtk::ALIGN_LEFT, Gtk::ALIGN_TOP, 1.0, 0.0));
    a->add (myCount);
    this->pack_start (*a, Gtk::PACK_SHRINK);
    a = Gtk::manage (new Gtk::Alignment(Gtk::ALIGN_LEFT, Gtk::ALIGN_TOP, 1.0, 0.0));
    a->add (mySuffixEntry);
    this->pack_start (*a, Gtk::PACK_SHRINK);

    myBaseEntry.set_width_chars (16);
    mySuffixEntry.set_width_chars (3);
    mySuffixEntry.set_text ("txt");
    myBaseEntry.set_text ("base");

    myCount.set_digits (0);
    myCount.set_range (0, 1000000);
    myCount.set_increments (1.0, 5.0);

    this->set_border_width (2);
}

goGUI::CountFilenames::~CountFilenames ()
{
}

void goGUI::CountFilenames::setBase (const goString& base)
{
    myBaseEntry.set_text (base.toCharPtr());
}

void goGUI::CountFilenames::setCount (goIndex_t count)
{
    myCount.set_value (static_cast<double>(count));
}

void goGUI::CountFilenames::setSuffix (const goString& suffix)
{
    mySuffixEntry.set_text (suffix.toCharPtr());
}

void goGUI::CountFilenames::increment ()
{
    myCount.set_value (myCount.get_value() + 1.0);
}

void goGUI::CountFilenames::getFilename (goString& fRet) const
{
    fRet = Glib::locale_from_utf8(myBaseEntry.get_text()).data ();

    char num[10];
    sprintf (num, "%.7d", myCount.get_value_as_int());
    fRet += num;
    fRet += ".";
    fRet += Glib::locale_from_utf8(mySuffixEntry.get_text()).data ();
}

void goGUI::CountFilenames::getBase (goString& ret) const
{
    ret = Glib::locale_from_utf8(myBaseEntry.get_text()).data ();
}

void goGUI::CountFilenames::getSuffix (goString& ret) const
{
    ret = Glib::locale_from_utf8(mySuffixEntry.get_text()).data ();
}

goIndex_t goGUI::CountFilenames::getCount () const
{
    return static_cast<goIndex_t>(myCount.get_value_as_int());
}
