#include <goconsumer.h>
#include <goerror.h>

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
	    goError::print("goConsumer::waitProduction()","No semaphore set/no producer set");
	}
}
