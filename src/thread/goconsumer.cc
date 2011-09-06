/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goconsumer.h>
#include <golog.h>

goConsumer::goConsumer()
{
    semaphore = 0;
}

goConsumer::~goConsumer()
{
}

void
goConsumer::waitProduction ()
{
    if (semaphore)
	{
	    semaphore->dec();
	}
    else
	{
        goLog::message("goConsumer::waitProduction(): No semaphore set/no producer set");
	}
}
