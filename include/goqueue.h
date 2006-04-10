#ifndef __GOQUEUE_H
#define __GOQUEUE_H

#include <golist.h>
#include <gotypes.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief Queue class.
 * @todo Document.
 */
template <class T>
class goQueue {
 public:
  goQueue ();
  virtual ~goQueue ();

  /// Returns the element at the head, the queue remains unchanged.
  T getHead ();
  ///
  bool isEmpty ();
  /// Returns the element at the tail, the queue remains unchanged.
  T getTail ();
  /// Removes one item from the head.
  bool remove ();
  /// Removes one item from the tail.
  bool removeTail ();
  /// Adds item to the tail.
  bool add (T& item);

  ///
  unsigned int getSize () { return (unsigned int)list.getSize(); }
 protected:
  goList<T> list;
};
/** @} */
#endif
