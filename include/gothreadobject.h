/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOTHREADOBJECT_H
#define GOTHREADOBJECT_H

#include <gotypes.h>
#include <goobjectbase.h>

class goThread;

/*!
 * \addtogroup thread
 * @{
 */
/*!
 * \brief Base class for objects that implement threads.
 *
 * Reimplement method threadMethod() in derived classes.
 * threadMethod() will be run in an extra thread when run() is called.
 *
 * @date 7.1.2002
 * @author Christian Gosch
 */
class goThreadObject : public goObjectBase
{
 public:
    goThreadObject();
    virtual ~goThreadObject();
    
    /*!
     * Creates a new thread of execution. The thread function is threadMethod().
     * @param nt Number of threads to spawn. 1 by default. If this differs
     * from 1, then threadMethod() is executed in nt threads at the same
     * time.
     */
    virtual void run(int nt = 1);
    /*!
     * This method must be reimplemented by a subclass. This method
     * contains the code of the new thread.
     */
    virtual void threadMethod();
    /*!
     * @return A reference to the goThread object used to run the thread(s).
     */
    goThread& getThread() { return thisThread; }

    /*!
     * @brief Check if this thread is the current one.
     * @return True if this thread is the currently running thread, false otherwise.
     */
    bool isCurrentThread () const { return this->thisThread.isCurrentThread(0); }
 private:
    goThread thisThread;
    
};
/*! @} */
#endif
