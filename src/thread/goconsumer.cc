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
