/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef RENDERMAINWINDOW_H
#define RENDERMAINWINDOW_H

#include <qmainwindow.h>
#include <renderapplication.h>

class
RenderMainWindow : public QMainWindow
{
    Q_OBJECT;
 public:
    RenderMainWindow (QWidget * parent = 0, const char * name = 0, WFlags f = WType_TopLevel);
    virtual ~RenderMainWindow();

    RenderApplication* getRenderApp() { return renderApp; }

 public slots:
     void quitThis();
 signals:
 void quit();

 protected:
    
 private:
    RenderApplication *renderApp;
};

#endif
