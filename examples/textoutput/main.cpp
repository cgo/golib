/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/mainwindow.h>
#include <gogui/textoutput.h>
#include <gtkmm.h>
#include <stdio.h>
#include <fcntl.h>

int main (int argc, char* argv[])
{
    Gtk::Main mainloop (argc, argv);

    goGUI::MainWindow mainWindow;
    goGUI::TextOutput to;
    mainWindow.getPaned().add1 (to);

    //= Now open a file / use stdout and redirect it to the TextOutput.
    int fd = ::open ("/var/log/messages", O_RDONLY);
    std::cout << "fd == " << fd << "\n";
    to.setFile (fd);


    std::cout << "About to call run.\n";
    Gtk::Main::run (mainWindow);

    ::close (fd);
}
