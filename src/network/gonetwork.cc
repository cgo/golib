#include <gonetwork.h>
#include <goerror.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <iostream.h>

#ifdef HAVE_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

bool
goNetwork::sendData (const void* data, size_t size, int sock) {
  int s = 0;
  goNetwork::fail (0);
  if ( (s = ::write (sock, data, size)) == -1 ) {
    goError::print("goNetwork::sendData()","Cannot send data to socket");
    goNetwork::fail (1);
    return false;
  }
  goError::note("goNetwork::sendData()","");
  cout << "\tSent data of size " << s << " to socket " << sock << endl;
  return true;
}

void*
goNetwork::receiveData (goSize_t *size, int sock) {
  int s = 0;
  char* data;
  
  goNetwork::fail (0);
  data  = (char*)malloc (1024 * sizeof(char));
  *size = 0;
  do {
    s = ::read ( sock, (void*)(&((char*)data)[*size]), 1024 );
    if ( s == -1 ) {
      goNetwork::fail (1);
      return NULL;
    }
    *size += (unsigned int)s;
    // printf("Received %d bytes\n",s);
    data = (char*)realloc ( (void*)data, (size_t)((1024 + *size) * sizeof(char)) );
  } while (s > 0);
  
  return (void*)data;
}

void*
goNetwork::receiveDataMax (unsigned int recvsize, int sock) {
  int s = 0;
  int size = 0;
  char* data;

  goNetwork::fail (0);
  goError::note("goNetwork::receiveDataMax()","");
  cout << "\trecvsize is " << recvsize << endl;
  data  = (char*)malloc (recvsize);
  do {
    s = ::read ( sock, (void*)(&((char*)data)[size]), recvsize );
    if ( s == -1 ) {
      goNetwork::fail (1);
      return NULL;
    }
    size += (unsigned int)s;
    goError::note("goNetwork::receiveDataMax()","");
    cout << "Received " << s << " bytes" << endl;
    if ((unsigned int)size >= recvsize) {
      s = 0;
      break;
    }
    data = (char*)realloc ( (void*)data, (size_t)((1024 + size) * sizeof(char)) );
  } while (s > 0);

  if (size == 0) {
    goNetwork::fail (1);
  }
  return (void*)data;
}

bool
goNetwork::sendFile (char *path, int sock) {
  FILE *f;
  char buffer[10240];
  size_t bytes = 0;

  goNetwork::fail (0);
  f = fopen (path, "r");
  
  if (f == NULL) {
    goNetwork::fail (1);
    return false;
  }
  while (!feof(f)) {
    bytes = fread ( (void*)&buffer[0], sizeof(char), 10240, f);
    if (ferror(f)) {
      goNetwork::fail (1);
      break;
    }
    goNetwork::sendData ((void*)&buffer[0], bytes, sock);
  }
  fclose(f);
  return true;
}

bool
goNetwork::fail (int internal) {
  static bool last_failed = false;

  switch (internal) {
  case 0: last_failed = false;
    break;
  case 1: last_failed = true;
    break;
  default:
    return last_failed;
    break;
  }
  return last_failed;
}

bool
goNetwork::waitForData (int descriptor)
{
  fd_set fdset;
  int retval;
  FD_ZERO (&fdset);
  FD_SET (descriptor, &fdset);
  retval = select (descriptor + 1, &fdset, NULL, NULL, NULL);
  if (retval) return true;
  return false;
}






