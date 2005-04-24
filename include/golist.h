#ifndef GOLIST_H
#define GOLIST_H

#include <gotypes.h>

template <class T>
class goListElement {
 public:
  T			elem;
  goListElement*	next;
  goListElement*	prev;
};

/**
 * Simple doubly linked list.
 * This list is not yet completely tested but should do so far.
 * @author Christian Gosch
 */
template <class T>
class goList {
 public:
  ///
  goList ();
  /*!
   * @todo Rework the list destructor, it does not destruct anything now.
   * Also have a look at goHashTable.
   */
  ~goList ();

  void      next ();
  void      prev ();
  /// Returns the current item and increments the pointer.
  T			getNext ();
  T*			getNextPtr ();
  /// Returns the current item and decrements the pointer.
  T			getPrev ();
  T*			getPrevPtr ();
  /// Returns the current item without touching the pointer.
  T			getCurrent ();
  T*			getCurrentPtr ();
  /// Returns the front of the list.
  T			getFront ();
  T*			getFrontPtr ();
  /// Returns the tail of the list.
  T			getTail ();
  T*			getTailPtr ();
  ///
  inline bool		isFront () { return position == front; }
  ///
  inline bool		isTail  () { return position == tail; }
  ///
  inline bool		isEmpty	() { return size == 0; }
  ///
  bool			append  (T elem);
  ///
  bool			insert  (T elem);
  /// Removes the current Item.
  bool			remove  ();
  /// Deletes the whole list leaving a zero length list.
  void			erase ();

  /// Resets the pointer to the front of the list.
  inline void		resetToFront () { position = front; }

  ///
  inline goInt32	getSize () { return size; }

  ///
  goList<T>&		operator= (goList<T>& other);

  /// UNTESTED
  bool			operator== (goList<T>& other);
  /// UNTESTED
  bool			operator!= (goList<T>& other);
  bool			operator< (goList<T>& other);
  bool			operator> (goList<T>& other);

 protected:
  ///
  goInt32		size;
  ///
  goListElement<T>*	front;
  ///
  goListElement<T>*	tail;
  ///
  goListElement<T>*	position;

  ///
  goListElement<T>      dummy;
};

#endif
