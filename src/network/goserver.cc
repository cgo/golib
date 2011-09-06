/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goconfig.h>
#include <goserver.h>
#include <gonetwork.h>
#include <gostring.h>
#include <gotypes.h>
#include <godefs.h>

#include <unistd.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>

namespace goNet {

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

class handlerInfo
{
public:
  handlerInfo ();
  virtual ~handlerInfo ();
  goServer *server;
  goServerConnection *connection;
};

handlerInfo::handlerInfo ()
{
  server = 0;
  connection = 0;
}
handlerInfo::~handlerInfo () {}

#ifdef WIN32
void
#else
void*
#endif
requestHandler (void* p)
{
  handlerInfo *info = (handlerInfo*)p;
  char *data;
  goSize_t size;

  std::cout << "Handler for socket " << info->connection->socket << " up and running." << std::endl;
  while (true)
  { 
      if(!goNetwork::waitForData (info->connection->socket))
	  {
		  std::cout << "Client on socket " << info->connection->socket << " closed connection\n";
#ifndef WIN32
		  return NULL;
#else
		  return
#endif
	  }
      data = (char*)goNetwork::receiveData (&size, info->connection->socket);
      if (size != 0)
		{
	  		std::cout << "Received " << size << " bytes" << std::endl;
	  		std::cout << "String output: " << data << std::endl;
		}
  }
#ifndef WIN32
  return NULL;
#endif
}

goServer::goServer (int p) 
    : goObjectBase () 
{
  this->setClassID(GO_SERVER);
  port = p;

  socklen_t namelen;
  goString msg;
  char nr[256];

  serverMessage ("Initializing goServer");

  hostname = (char*)malloc (sizeof (char) * MAXHOSTNAMELEN);
  if ( gethostname (hostname, MAXHOSTNAMELEN) != 0 ) {
    serverError ("Server: Could not obtain hostname");
  }
  
  msg = "Hello, I am ";
  msg += hostname;
  serverMessage(msg.toCharPtr());
  sprintf(nr, "%d", port);
  msg = "Starting on port ";
  msg += nr;
  serverMessage(msg.toCharPtr());
  
  host = gethostbyname (hostname);
  /* convert host name to IP address and copy to sock_addr */
  bcopy( ((struct hostent*)host)->h_addr, &sock_addr.sin_addr, sizeof (host->h_addr));
  /* initialise protocol prot number */
  sock_addr.sin_port = htons(port);
  sock_addr.sin_family = AF_INET;
  /* create a socket */
  if ( (this->socket = ::socket (AF_INET, SOCK_STREAM, 0)) == -1 ) {
    perror ("");
    serverError ("Server: can't create socket");
  }
  /* connect the socket to the server */
  namelen = sizeof (sock_addr);

  if ( bind(this->socket, (struct sockaddr*)&sock_addr, namelen) != 0 ) {
      serverError ("Server: can't bind socket !");
  }
  stopAccepting = false;
}

goServer::~goServer () {
  connections.resetToFront ();
  while (!connections.isEmpty()) {
    delete (goServerConnection*)connections.getNext();
  }
  connections.erase ();
}

goServerConnection* 
goServer::serverAcceptThread () {
  goServerConnection *con;
  
  con = new goServerConnection;
  if ( listen (this->socket, 1) == -1 ) {
    perror ("goServer");
    serverError ("Server: can't do listen()");
  }
  
  if ( (con->socket = accept (this->socket, (struct sockaddr*)&con->sock_addr, &con->addrlen)) == -1 ) {
    delete con;
    con = NULL;
    serverError ("Server: can't accept client");
  } else {
    conMutex.lock();
    // append does not change the list position, so it is safe
    connections.append ((void*&)con);
    /*
     * Call a thread for every connecting client here!
     */
    goThread hThread;
    // FIXME This is only experimental. The object DOES NOT GET DELETED!
    // handlerInfo *inf = new handlerInfo;
    // inf->server = this;
    // inf->connection = con;
    // hThread.create (goNet::requestHandler, (void*)inf, 1);

    std::cout << "List size is now " << connections.getSize() << std::endl;
    std::cout << "Member sockets are ";
    connections.resetToFront();
    int i;
    for (i = 0; i < connections.getSize(); i++)
	{
	    std::cout << ((goServerConnection*)connections.getCurrent())->socket << " ";
	    connections.getNext();
	} 
    std::cout << std::endl;
    conMutex.unlock();
    std::cout << "Socket " << con->socket << std::endl;
    serverMessage ("Connect");
  }
  return con;
}

bool
goServer::disconnect (goServerConnection* con) {
  serverMessage("Disconnecting");
  close (con->socket);
  connections.resetToFront();
  while (!connections.isTail() && 
	 ( (goServerConnection*)connections.getCurrent() != con ) ) {
    connections.getNext();
  }
  if ( (goServerConnection*)connections.getCurrent() == con ) {
    connections.remove();
    delete con; 
    return true;
  }

  // Something weird happened ...
  return false;
}

bool
goServer::disconnect () {
  if (connections.getSize() < 1)
    {
      return false;
    }
  goServerConnection *con;
  con = ((goServerConnection*)connections.getCurrent());
  serverMessage("Disconnecting");
  close (con->socket);
  connections.remove();
  delete con; 
  return true;
}

bool 
goServer::sendFile (char *path, goServerConnection *con) {
  return goNetwork::sendFile (path, con->socket);
}

static
void* runThread (void* p) {
  while (true) {
    ((goServer*)p)->serverAcceptThread();
  }
  return NULL;
}

bool
goServer::runServer() {
  stopAccepting = false;
  acceptThread.create (goNet::runThread, this, 1);
  return true;
}

bool 
goServer::stopServer() {
  stopAccepting = true;
  acceptThread.kill();
  return true;
}

bool
goServer::requestHandler () {
  return false;
}

void
goServer::serverMessage (const char* t) {
  std::cout << "goServer: " << t << std::endl;
}

void
goServer::serverMessage (goString& s) {
  serverMessage ((const char*)s.toCharPtr());
}

void
goServer::serverError (const char* s) {
  goString tmp;
  tmp = "Error: ";
  tmp += s;
  serverMessage ((const char*)tmp.toCharPtr());
}

};











