#include <gostatusobject.h>
#include <goconfig.h>
#include <iostream>

goStatusObject::goStatusObject ()
{
    setBusy(false);
    setProgress (0.0f);
    fwProgressPtr = 0;
    minProgress = 0.0f;
    maxProgress = 1.0f;
    // lockProgress();
}

goStatusObject::~goStatusObject ()
{
}

void*
fw_progress_thread (void* p)
{
    goStatusObject *arg = (goStatusObject*)p;
    arg->forwardProgressThread ();
    return 0;
}

void
goStatusObject::forwardProgressThread ()
{
    goThread::setCancelType(0);		// set to asynchronous cancellation
    while (true)
	{
	    // this->lockProgress();
	    this->setProgress(fwProgressPtr->waitForProgress());
	    signalProgress();
	    // this->releaseProgress();
	}
}

void
goStatusObject::forwardProgress (goStatusObject* s)
{
#ifndef MULTIPLE_THREADS	
    return;
#endif	
    forwardProgressEnd();					// End any existing forward
    fwMutex.lock();						// lock the forward mutex
    fwProgressPtr = s;						// Set the forwarding object
    fwThread.create(fw_progress_thread, (void*)this, 1);	// Start the waiting/forwarding thread
}

void
goStatusObject::forwardProgressEnd ()
{
    std::cout << "goStatusObject: Killing fwThread" << std::endl;
    fwThread.cancel();
    fwMutex.unlock();
    fwProgressPtr = 0;
}
