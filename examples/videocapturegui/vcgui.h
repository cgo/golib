/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef VCGUI_H
#define VCGUI_H

#include <gogui/mainwindow.h>

class VCGuiPrivate;

class VCGui : public goGUI::MainWindow
{
    public:
        VCGui ();
        virtual ~VCGui ();

        int redrawImage ();

    private:
        VCGuiPrivate* myPrivate;
};

#endif
