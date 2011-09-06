/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
    std::cout << "Signalling condition" << std::endl;
    goIndex_t i;
    for (i = 0; i < listeners.getSize(); i++)
	{
	    ((goSynchronized*)listeners[i])->getSynchronizeCondition().broadcast();
	}
}

void
goSynchronized::waitSynchronize ()
{
    std::cout << "waiting for condition" << std::endl;
    condition.wait();
}
