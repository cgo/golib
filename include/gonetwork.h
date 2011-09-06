/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GONETWORK_H
#define GONETWORK_H

#include <sys/types.h>
#include <sys/socket.h>

#include <gotypes.h>

namespace goNet {

/*! 
 * \addtogroup net
 * @{
 */
/**
 * @brief Basic network functions.
 *
 * @return 
**/
class goNetwork {
 public:
  static bool sendData (const void* data, size_t size, int sock);
  /*! 
   * Returns newly allocated data block containing received data. 
   * The data block is allocated using
   * C malloc.
   */
  static void* receiveData (goSize_t *size, int sock);
  /// Receives exactly recvsize bytes and returns. 
  static void* receiveDataMax (unsigned int recvsize, int sock);
  static bool  sendFile (char *path, int sock);
  static bool  waitForData (int descriptor);

  static bool fail (int internal = 3);
};
/*! @} */
};
#endif
