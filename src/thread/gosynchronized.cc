#include <gosynchronized.h>

goSynchronized::goSynchronized()
{
}

goSynchronized::~goSynchronized ()
{
    listeners.resize(0);
}

void
goSynchronized::addSynchronizedListener (goSynchronized* s)
{
    listeners += (void*)s;
}

void
goSynchronized::signalSynchronize ()
{
    cout << "Signalling condition" << endl;
    goIndex_t i;
    for (i = 0; i < listeners.getSize(); i++)
	{
	    ((goSynchronized*)listeners[i])->getSynchronizeCondition().broadcast();
	}
}

void
goSynchronized::waitSynchronize ()
{
    cout << "waiting for condition" << endl;
    condition.wait();
}
