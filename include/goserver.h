#ifndef __GOSERVER_H
#define __GOSERVER_H

#include <gothread.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <golist.h>
#include <gostring.h>

namespace goNet {

class goServerConnection {
 public:
  struct    sockaddr_in sock_addr;
  socklen_t addrlen;
  int	    socket;
};


/*!
 * \addtogroup net
 * @{
 */
/*!
 * Accepts connections. When you create a server on a specified port, it starts accepting connections
 * on that port when run() is called. The actual request handler has to be implemented by subclasses.
 * The request handler is not automagically invoked. The calling program has to do that itself.
 * @todo serverAcceptThread(): Call a thread for every connection? That might be too special and should be implemented in a specialisation class.
 */
class goServer {
 public:
  goServer (int port);
  virtual ~goServer ();

  /*!
   * Accepts a connection for server server.
   */
  goServerConnection* serverAcceptThread ();

  /*!
   * Disconnects <CODE>con</CODE>.
   * Searches the connections list front to back each time it is called,
   * so use only with small lists or when speed doesn't matter.
   * <strong>Only the address of <code>con</code> is compared.</strong>
   */  
  bool disconnect (goServerConnection* con);

  /*!
   * Disconnects the connection the connections list currently points
   * at. This should be preferred over 
   * <code>bool disconnect (goServerConnection* con) </code> 
   * whenever the list is 
   * at the correct position anyway. <br>
   * Not tested yet.
   */
  bool disconnect ();
  
  bool sendFile (char *path, goServerConnection *con);
  
  bool serverClose ();

  /*! 
   * Accepts connections. 
   * This method starts a new thread that accepts connections and adds them
   * to the connections list.
   * It returns immediately after the thread is created with true on success,
   * false on failure. Call <code>stop()</code> to stop the accepting thread.
   * @return true on success, false on failure
   */
  bool runServer();
  /*!
   * Stops the accepting thread.
   * @return true on success, false on failure
   */
  bool stopServer();
  
  /*! To be implemented by subclasses to handle requests coming in from the connections in the connection 
   *  list. 
   */
  virtual bool requestHandler ();

  void serverMessage (const char*);
  void serverMessage (goString&);
  void serverError (const char*);


 protected:
  /* This server's details */
  int	 socket;
  int	 port;
  char	 *hostname;
  struct sockaddr_in sock_addr;
  struct hostent *host;

  bool	stopAccepting;
  goThread acceptThread;

  // Mutex to protect connections list.
  goMutex	conMutex;
  goList<void*> connections;
};
/*! @} */
};
#endif
