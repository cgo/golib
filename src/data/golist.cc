#include <golist.h>
#include <gostring.h>
#include <gohashtable.h>

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

template class goListElement<goInt8>;
template class goListElement<goUInt8>;
template class goListElement<goInt16>;
template class goListElement<goUInt16>;
template class goListElement<goInt32>;
template class goListElement<goUInt32>;
template class goListElement<goFloat>;
template class goListElement<goDouble>;
template class goListElement<void*>;

template class goList<goInt8>;
template class goList<goUInt8>;
template class goList<goInt16>;
template class goList<goUInt16>;
template class goList<goInt32>;
template class goList<goUInt32>;
template class goList<goFloat>;
template class goList<goDouble>;
template class goList<void*>;

// template class goList<goString>;

//template class goList<goHashEntry<goUInt64, void*> >;
//template class goList<goHashEntry<goUInt32, void*> >;










