/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_WORKERTHREAD_H
#define GOGUI_WORKERTHREAD_H

#ifndef GOTHREADOBJECT_H
# include <gothreadobject.h>
#endif

namespace goGUI
{
    class WorkerThreadPrivate;
    /** 
     * @brief Not yet completed.
     * This is intended to be a base class for worker thread
     * that emit some form of progress.
     */
    class WorkerThread : public goThreadObject
    {
        public:
            WorkerThread ();
            virtual ~WorkerThread ();
            
            void setProgressBar (goGUI::ProgressBar* pb);

            void incrementProgress ();
            void resetProgress ();
            void setProgress (goGUI::WorkerThread* t, double progress);

        private:
            WorkerThreadPrivate* myPrivate;
    };
};

#endif
