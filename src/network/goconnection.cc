#include <goarray.h>
#include <goconnection.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>

namespace goNet {

goConnection::goConnection () 
    : goObjectBase () 
{
  this->setClassName ("goConnection");
  maxConnections 	= 0;
  currentConnections 	= 0;
  lastFailed 		= false;
  remoteSockAddr 	= (struct sockaddr_in*) malloc (sizeof(struct sockaddr_in));
  mySockAddr 		= (struct sockaddr_in*) malloc (sizeof(struct sockaddr_in));
  remoteSockets.resize (0);
}

goConnection::~goConnection () {
  free (remoteSockAddr);
  free (mySockAddr);
}

void
goConnection::setRemoteName (goString& name) {
  lastFailed = false;
  remoteName = name;
}

void
goConnection::setRemoteName (const goString& name) {
  lastFailed = false;
  remoteName = name;
}

void
goConnection::setRemoteAddress (struct in_addr addr) {
  lastFailed = false;
  remoteSockAddr->sin_addr.s_addr = addr.s_addr;
}

void
goConnection::setRemotePort (unsigned short portnr) {
  lastFailed = false;
  remoteSockAddr->sin_port = htons(portnr);
}

void
goConnection::setMyAddress (struct in_addr addr) {
  lastFailed = false;
  mySockAddr->sin_addr.s_addr = addr.s_addr;
}

void
goConnection::setMyPort (unsigned short portnr) {
  lastFailed = false;
  mySockAddr->sin_port = portnr;
}

bool
goConnection::link (int maxconn) {
  maxConnections = maxconn;
  socketDescriptor = socket (AF_INET, SOCK_STREAM, 0); 
  
  switch (maxconn) {
  case 0: 
    remoteHostEnt = gethostbyname((const char*)remoteName.toCharPtr());
    if (!remoteHostEnt) {
      lastFailed = true;
      std::cout << "gethostbyname failed \n";
      perror("");
      return false;
    }
    remoteSockAddr->sin_family = AF_INET;
    memcpy ((char*)&remoteSockAddr->sin_addr,(char*)remoteHostEnt->h_addr,remoteHostEnt->h_length);
    if ( connect (socketDescriptor, (struct sockaddr*)remoteSockAddr, sizeof (*remoteSockAddr)) != 0 ) {
      lastFailed = true;
      std::cout << "goConnection::link(): connect failed \n";
      std::cout << "Address: " << std::hex << ((struct sockaddr_in*)remoteSockAddr)->sin_addr.s_addr << std::dec << std::endl;
      std::cout << "Port: " << ntohs(((struct sockaddr_in*)remoteSockAddr)->sin_port) << std::endl;
      perror("");
      return false;
    }
    currentConnections = 1;
    break;
  default:
    if ( bind (socketDescriptor, (struct sockaddr*)mySockAddr, sizeof (*mySockAddr)) != 0 ) {
      lastFailed = true;
      std::cout << "bind failed \n";
      perror("");
      return false;
    }
    if ( listen (socketDescriptor, maxconn) != 0) {
      lastFailed = true;
      std::cout << "listen failed \n";
      perror("");
      return false;
    }
    lastFailed = false;
    int i = 0;
    socklen_t socklen = 0;
    for (i = 0; i < maxconn; i++) {
      remoteSockets.resize (remoteSockets.getSize() + 1);
      socklen = sizeof (remoteSockAddr[i]);
      if ( (remoteSockets[i] = accept (socketDescriptor, (struct sockaddr*)&remoteSockAddr[i], (socklen_t*)&socklen)) == -1 )  {
          std::cout << "client " << i << ": " << " accept() failed\n";
	perror ("");
	lastFailed = true;
	return false;
      }
      remoteSockAddr = (sockaddr_in*) realloc (remoteSockAddr, (++currentConnections + 1) * sizeof (struct sockaddr_in));
    }
  }  
  lastFailed = false;
  return true;
}

bool
goConnection::close () {
  if ( ::close ((int)socketDescriptor) == -1 ) {
    return false;
  }
  currentConnections--;
  return true;
}

bool
goConnection::send (const void* msg, int length, int remote, unsigned int flags) {
  lastFailed = false;
  switch (maxConnections) {
  case 0:
    if (::send (socketDescriptor, msg, length, flags) == -1) {
      lastFailed = true;
      perror("Error while send");
      return false;
    }
    break;
  default:
    if (::send (remoteSockets[remote], msg, length, flags) == -1) {
      lastFailed = true;
      perror("Error while send");
      return false;
    }
    break;
  }
  return true;
}

bool
goConnection::recv (void* buffer, int length, int remote, unsigned int flags) {
  lastFailed = false;
  switch (maxConnections) {
  case 0:
    if (::recv (socketDescriptor, buffer, length, flags) == -1 ) {
      lastFailed = true;
      return false;
    }
    break;
  default:
    if (::recv (remoteSockets[remote], buffer, length, flags) == -1 ) {
      lastFailed = true;
      return false;
    }
    break;
  }
  return true;
}

};

