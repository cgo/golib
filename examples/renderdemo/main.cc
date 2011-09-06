/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <qapplication.h>
#include <qplatinumstyle.h>
#include <renderapplication.h>
#include <rendermainwindow.h>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setStyle (new QPlatinumStyle);
    // RenderApplication gui;
    RenderMainWindow mainWindow;
    QObject::connect ((const QObject*)&mainWindow, SIGNAL(quit()), (const QObject*)&app, SLOT(quit()));
    mainWindow.show();
    app.setMainWidget (&mainWindow);
    return app.exec();
}
