/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/mainwindow.h>
#include <gtkmm.h>

int main (int argc, char* argv[])
{
    Gtk::Main kit(argc, argv);
    goGUI::MainWindow window;
    Gtk::Main::run(window);
}
