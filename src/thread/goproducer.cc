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


