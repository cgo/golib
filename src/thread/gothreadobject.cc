/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gothread.h>
#include <godefs.h>
#include <gothreadobject.h>


goThreadObject::goThreadObject() 
    : goObjectBase(), thisThread()
{
    this->setClassID(GO_THREADOBJECT);
}

/** 
* @TODO Think of a way to clean up threads nicely when object is destroyed.
*/
goThreadObject::~goThreadObject()
{
    // thisThread.join(); //= This causes seg fault!
}

#if defined WIN32
static void goThreadObject_threadFunction(void* p)
{
    ((goThreadObject*)p)->threadMethod();
}
#else
static void* goThreadObject_threadFunction(void* p)
{
    ((goThreadObject*)p)->threadMethod();
    return NULL;
}
#endif
void
goThreadObject::run(int nt)
{
        // Danger in Windows!
#if not defined WIN32   
    thisThread.cancel();  
#endif
    thisThread.create(goThreadObject_threadFunction, (void*)this, nt);
}

void
goThreadObject::threadMethod()
{
    // Do nothing by default
}

