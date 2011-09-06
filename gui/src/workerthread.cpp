/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/workerthread.h>
#include <gtkmm.h>

namespace goGUI
{
    class WorkerThreadPrivate
    {
        public:
            WorkerThreadPrivate () 
                : progressBar (0),
                  progressStep (0.1) {};
            ~WorkerThreadPrivate () {};

            goGUI::ProgressBar* progressBar;
            goDouble            progressStep;
            Glib::Dispatcher    progressDispatcher;
    };
};

goGUI::WorkerThread::WorkerThread ()
    : goThreadObject (),
      myPrivate (0)
{
    myPrivate = new goGUI::WorkerThreadPrivate;
}

goGUI::WorkerThread::~WorkerThread ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::WorkerThread::setProgressBar (goGUI::ProgressBar* pb)
{
    myPrivate->progressBar = pb;
#ifdef HAVE_GTK_2
    myPrivate->progressDispatcher.connect (SigC::slot(pb, &goGUI::WorkerThread::incrementProcess));
#elif HAVE_GTK_2_4
    myPrivate->progressDispatcher.connect (sigc::mem_fun(this, &goGUI::WorkerThread::incrementProcess));
#endif
}

void goGUI::WorkerThread::incrementProgress ()
{
}
