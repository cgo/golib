/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gonetobject.h>
#include <godefs.h>

goNet::goNetObject::goNetObject(int listenPort)
	: goThreadObject(), server(listenPort)
{
    this->setClassID(GO_NETOBJECT);
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
