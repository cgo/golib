/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSTATUSOBJECT_H
#define GOSTATUSOBJECT_H

#include <gotypes.h>
#include <gothread.h>

/*!
 * \addtogroup misc
 * @{
 */
/*!
 * \brief Provides status information for an object INCOMPLETE.
 *
 * \todo Fix this class when there is a use for it.
 *       It is not complete and tested.
 *
 * @author Christian Gosch
 * @date 21.9.2001
 */ 
class
goStatusObject
{
 public:
    goStatusObject();
    virtual ~goStatusObject();

    /*!
     * @return True if this object is busy, false otherwise.
     */ 
    bool isBusy () { return busy; }
    /*!
     * @return A value representing the progress in some work being done by the object.
     * This will usually be something between 0 and 1, but can generally be whatever value is allowed by goFloat.
     */ 
    goFloat	getProgress () { return progress; }
    goFloat	getMaxProgress () { return maxProgress; }
    goFloat	getMinProgress () { return maxProgress; }
    /*!
     * Uses a semaphore to suspend the calling thread until the mutex is released.
     * @return The current progress.
     */ 
    goFloat	waitForProgress () { pSem.dec(); return progress; }
    
    void forwardProgress (goStatusObject* s);
    void forwardProgressThread ();
    void forwardProgressEnd ();
 protected:
    /*!
     * Sets the busy flag.
     * @param b Value of the busy flag
     */ 
    void setBusy (bool b) { busy = b; }
    /*!
     * Sets the progress value.
     * @param p Progress value, should usually be between 0 and 1, but there is no general restriction.
     */
    void setProgress (goFloat p) { progress = p; }
    void signalProgress () { pSem.inc(); }
    /*!
     * Locks the mutex associated with the progress to the thread calling waitForProgress() wait.
     */
    // void	lockProgress () { waitMutex.lock(); }
    /*!
     * Unlocks the mutex associated with the progress to make the thread calling waitForProgress() resume.
     */
    // void	releaseProgress () { waitMutex.unlock(); }

 private:
    goFloat maxProgress;
    goFloat minProgress;

    bool busy;
    goFloat progress;
    goSemaphore pSem;

    goThread fwThread;
    goMutex fwMutex;
    goStatusObject *fwProgressPtr;
};
/*!
 * @}
 */

#endif
