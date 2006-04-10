#ifndef GOIMAGE_BUFFER_H
#define GOIMAGE_BUFFER_H

#include <gotypes.h>

/*
 * Used to monitor the status of a goImageBuffer
 */
struct goImageBufferStatus {
  bool created;
};

/*
 * DEPRECATED
 * Used to store image information.
 * Here, a display interface still has to be implemented. 
 * I intend to create a display class and a displayinterface 
 * class to access the display (to encapsulate the interface so 
 * I can enable more than one display type, e.g. Qt and X11 displays).
 */
class goImageBuffer {
 public:
  ///
  goImageBuffer (int x, int y, GO_TYPE t = GO_INT16);
  ///
  goImageBuffer ();
  ///
  ~goImageBuffer ();

  /// Creates the buffer of type t
  void		create (int x, int y, GO_TYPE t);
  /**
   * @returns void* to the image line x
   */
  inline void*	operator[] (goIndex_t x) { return buffer[x]; }
  
  inline void**	getBufferPtr () { return buffer; }
 protected:
  ///
  void**		buffer;
  ///
  int			sizeX;
  ///
  int			sizeY;
  ///
  GO_TYPE		type;
  ///
  struct goImageBufferStatus status;
};

#endif /* GOIMAGE_BUFFER_H */
