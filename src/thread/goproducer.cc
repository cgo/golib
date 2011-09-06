/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goproducer.h>

goProducer::goProducer ()
{
}

goProducer::~goProducer ()
{
}

void
goProducer::signalProduction ()
{
    semaphore.inc();
}


