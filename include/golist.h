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

// #include <golist.h>
// #include <gostring.h>
// #include <gohashtable.h>

template <class T>
goList<T>::goList () {
  front		= 0;
  tail		= 0;
  position	= 0;
  size		= 0;
}

template <class T>
goList<T>::~goList () {
  position = front;
  this->erase();
}

template <class T>
T
goList<T>::getNext () {
  T tmp = 0;
  if (position) {
    tmp = (position->elem);
    if (position != tail) {
      position = position->next;
    }
  }
  return tmp;
}

template <class T>
T*
goList<T>::getNextPtr () {
  T* tmp = 0;
  if (position) {
    tmp = &(position->elem);
    if (position != tail) {
      position = position->next;
    }
  }
  return tmp;
}

template <class T>
T
goList<T>::getPrev () {
  T tmp = 0;
  if (position) {
    tmp = (position->elem);
    if (position != front) {
      position = position->prev;
    }
  }
  return tmp;
}

template <class T>
T*
goList<T>::getPrevPtr () {
  T* tmp = 0;
  if (position) {
    tmp = &(position->elem);
    if (position != front) {
      position = position->prev;
    }
  }
  return tmp;
}

template <class T>
T
goList<T>::getCurrent () {
  if (position) {
      return (position->elem);
  }
  return 0;
}

template <class T>
T*
goList<T>::getCurrentPtr () {
  if (position) {
      return &(position->elem);
  }
  return (T*)0;
}

template <class T>
T
goList<T>::getFront () {
    return front->elem;
}

template <class T>
T*
goList<T>::getFrontPtr () {
  return &front->elem;
}

template <class T>
T
goList<T>::getTail () {
  return tail->elem;
}

template <class T>
T*
goList<T>::getTailPtr () {
  return &tail->elem;
}

// template <class T>
// bool
// goList<T>::isFront () {
//   return (position == front);
// }

// template <class T>
// bool
// goList<T>::isTail () {
//     return (position == tail);
// }

// template <class T>
// bool
// goList<T>::isEmpty () {
//     return (size == 0);
// }

template <class T>
bool
goList<T>::append (T elem) {
  goListElement<T> *e = new goListElement<T>;

  e->elem = elem;
  if (!front) {
    front = e;
    tail = e;
    e->next = 0;
    e->prev = 0;
  } else {
    tail->next = e;
    e->next = 0;
    e->prev = tail;
    tail = e;
  }
  if (!position) {
    position = front;
  }
  size++;
  return true;
}

template <class T>
bool
goList<T>::insert (T elem) {
  goListElement<T> *e = new goListElement<T>;
  
  e->elem = elem;

  if (position) {
    goListElement<T>* tmp = position->next;
    position->next = e;
    e->prev = position;
    e->next = tmp;
    tmp->prev = e;
    size++;
  } else {
    append (elem);
  }
  return true;
}

template <class T>
bool
goList<T>::remove () {
  if (position) {
    goListElement<T>* tmp = position->next;
    goListElement<T>* tmp2 = position->prev;
    if (tmp) {
      tmp->prev = tmp2;
    } else {
      tail = tmp2;
    }
    if (tmp2) {
      tmp2->next = tmp;
    } else {
      front = tmp;
    }
    delete position;
    position = tmp;
    size--;
  } else {
    return false;
  }
  return true;
}

template <class T>
void
goList<T>::erase () {
  resetToFront();
  if (!isEmpty()) {
    while (!isTail() && !isEmpty()) {
      remove();
    }
    remove();
    size = 0;
  }
}

template <class T>
bool
goList<T>::operator> (goList<T>& other) {
  if (other.getSize() < this->getSize()) {
    return true;
  }
  return false;
}

template <class T>
bool
goList<T>::operator< (goList<T>& other) {
  return !this->operator> (other);
}

template <class T>
goList<T>&
goList<T>::operator= (goList<T>& other) {
  erase ();
  if (!other.isEmpty()) {
    other.resetToFront ();
    while (!other.isTail()) {
      append (other.getNext());
    }
    append (other.getCurrent());
  }
  return *this;
}

template <class T>
bool
goList<T>::operator== (goList<T>& other) {
  if (!other.isEmpty() && !isEmpty()) {
    other.resetToFront ();
    resetToFront();
    while (!other.isTail()) {
      if (other.getNext() != getNext()) return false;
    }
    if (other.getCurrent() != getCurrent()) return false;
  }
  return true;
}

template <class T>
bool
goList<T>::operator!= (goList<T>& other) {
  return !( (*this) == other );
}


#endif












