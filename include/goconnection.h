#ifndef __GOCONNECTION_H
#define __GOCONNECTION_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <gostring.h>

namespace goNet {

/*!
 * \addtogroup net
 * @{
 */
/*!
 * Provides a TCP/IP connection.
 * Protocol is TCP, the mode is SOCK_STREAM, so this class should result in a reliable connection.
 * This can be used to act as a client (link(0)) or as a server (link(n>0)).
 * When acting as a server (i.e. waiting for requesting connections) you have to use {\ttsetMyPort()} to
 * set the port this computer accepts connections on. The "clients" have to set their remote port numbers
 * using {\ttsetRemotePort()} to the same value (who would have thought ...).
 * Currently, \underline{all actions are blocking}.
 * Thread safety has to be implemented if needed by the user.
 * @author Christian Gosch
 */
class goConnection {
 public:
  ///
  goConnection ();
  ///
  ~goConnection ();

  /// Sets remote computer name
  void		setRemoteName 	(goString& name);
  /// Sets remote computer name
  void		setRemoteName 	(const goString& name);

  /** Sets remote adress.
   * The current status is that {\ttlink()} always looks for the name set by {\ttsetRemoteName()}
   * and never uses the raw address (who needs that anyway..).
   * @see setRemoteName()
   */
  void		setRemoteAddress    	(struct in_addr addr);
  /// Sets remote adress NOT YET IMPLEMENTED
  void		setRemoteAddress	(goString& addr);
  /// Sets remote adress NOT YET IMPLEMENTED
  void		setRemoteAddress	(const char* addr);
  /// Sets remote port number
  void		setRemotePort		(unsigned short pn);

  /// Sets my address
  void		setMyAddress    	(struct in_addr addr);
  /// Sets my address
  void		setMyAddress		(goString& addr);
  /// Sets my address
  void		setMyAddress		(const char* addr);
  /// Sets my portnumber
  void		setMyPort		(unsigned short pn);
  
  /// Returns this connections socket.
  int		getSocket		() { return socketDescriptor; }

  /// gets the current number of connections
  int		getCurrentConnections	() { return currentConnections; }

  /**
   * Tries to esablish a link. 
   * If maxconn = 0 it assumes to be connecting to a waiting computer.
   * If maxconn > 0 it waits for maxconn computers to connect to it.
   */
  bool		link (int maxconn = 0);
  
  // Closes the current connection.
  bool		close ();

  /// True if the last operation failed.
  bool		fail () { return lastFailed; }

  /** 
   * Sends a bulk of data in {\tt msg} of length {\tt length} to the remote host number
   * {\tt remote}.
   * If {\tt maxconn} was larger than zero in {\tt link()}, you have to 
   * supply a remote host number you want to send to.
   */
  bool		send (const void* msg, int length, int remote = 0, unsigned int flags = 0);

  /**
   * Receives a bulk of data in {\tt buffer} of length {\tt length} from the remote host number
   * {\tt remote}.
   * If {\tt maxconn} was larger than zero in {\tt link()}, you have to 
   * supply a remote host number you want to receive from.
   */
  bool		recv (void* buffer, int length, int remote = 0,unsigned int flags = 0);

 protected:
  ///
  int			maxConnections;
  ///
  int			currentConnections;
  ///
  bool			lastFailed;
  ///
  goString		remoteName;
  ///
  int			socketDescriptor;
  ///
  struct sockaddr_in	*remoteSockAddr;
  /// needed only if acting as the connecting computer.
  struct hostent	*remoteHostEnt;
  ///
  goArray<int>		remoteSockets;
  ///
  struct sockaddr_in	*mySockAddr;
};
/*! @} */
};

#endif /* __GOCONNECTION_H */
