/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gonetwork.h>
#include <golog.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <iostream>

#ifdef HAVE_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace goNet {

bool
goNetwork::sendData (const void* data, size_t size, int sock) {
  int s = 0;
  goNetwork::fail (0);
  if ( (s = ::write (sock, data, size)) == -1 ) {
      goLog::warning("goNetwork::sendData(): Cannot send data to socket");
    goNetwork::fail (1);
    return false;
  }
  goLog::message("goNetwork::sendData()");
  std::cout << "\tSent data of size " << s << " to socket " << sock << std::endl;
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
  goLog::message("goNetwork::receiveDataMax()");
  std::cout << "\trecvsize is " << recvsize << std::endl;
  data  = (char*)malloc (recvsize);
  do {
    s = ::read ( sock, (void*)(&((char*)data)[size]), recvsize );
    if ( s == -1 ) {
      goNetwork::fail (1);
      return NULL;
    }
    size += (unsigned int)s;
    goLog::message("goNetwork::receiveDataMax()");
    std::cout << "Received " << s << " bytes" << std::endl;
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
  if (retval != -1) 
	  return true;
  return false;
}

};
