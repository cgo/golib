#include <gothread.h>
#include <gothreadobject.h>


goThreadObject::goThreadObject() 
    : goObjectBase()
{
    setClassName("goThreadObject");
}

goThreadObject::~goThreadObject()
{
    thisThread.cancel();
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

