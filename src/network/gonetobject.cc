#include <gonetobject.h>

goNet::goNetObject::goNetObject(int listenPort)
	: goThreadObject(), server(listenPort)
{
	// Start the internal server thread, accepting connections
	// and putting them into a connection list.
	server.runServer();
	
}

goNet::goNetObject::~goNetObject()
{
	server.stopServer();
}

void
goNet::goNetObject::threadMethod()
{
}
