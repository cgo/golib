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

static void* goThreadObject_threadFunction(void* p)
{
    ((goThreadObject*)p)->threadMethod();
    return NULL;
}

void
goThreadObject::run(int nt)
{
    thisThread.cancel();
    thisThread.create(goThreadObject_threadFunction, (void*)this, nt);
}

void
goThreadObject::threadMethod()
{
    // Do nothing by default
}

