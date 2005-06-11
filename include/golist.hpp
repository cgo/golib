// #include <golist.h>
// #include <gostring.h>
// #include <gohashtable.h>

template<class T>
goListElement<T>::goListElement ()
    : elem (),
      next (0),
      prev (0)
{
}

template<class T>
goListElement<T>::~goListElement ()
{
}

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
void goList<T>::next ()
{
    if (position) 
    {
        if (position != tail) 
        {
            position = position->next;
        }
    }
}

template <class T>
void goList<T>::prev ()
{
    if (position) 
    {
        if (position != front) 
        {
            position = position->prev;
        }
    }
}

template <class T>
T&
goList<T>::getNext () {
  T* tmp = 0;
  if (position) {
    tmp = &(position->elem);
    if (position != tail) {
      position = position->next;
    }
  }
  return *tmp;
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
T&
goList<T>::getPrev () {
  T* tmp = 0;
  if (position) {
    tmp = &(position->elem);
    if (position != front) {
      position = position->prev;
    }
  }
  return *tmp;
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
T&
goList<T>::getCurrent () {
  if (position) {
      return (position->elem);
  }
  return dummy.elem;
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
T&
goList<T>::getFront () {
    if (!front)
    {
        return dummy.elem;
    }
    return front->elem;
}

template <class T>
T*
goList<T>::getFrontPtr () {
    if (!front)
    {
        return &dummy.elem;
    }
  return &front->elem;
}

template <class T>
T&
goList<T>::getTail () {
  if (!tail)
  {
      return dummy.elem;
  }
  return tail->elem;
}

template <class T>
T*
goList<T>::getTailPtr () {
  if (!tail)
  {
      return &dummy.elem;
  }
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
goList<T>::append (const T& elem) {
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
goList<T>::insert (const T& elem) {
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
    if (position) 
    {
        goListElement<T>* tmp = position->next;
        goListElement<T>* tmp2 = position->prev;
        if (tmp) 
        {
            tmp->prev = tmp2;
        } else 
        {
            tail = tmp2;
        }
        if (tmp2) 
        {
            tmp2->next = tmp;
        } else 
        {
            front = tmp;
        }
        delete position;
        if (tmp)
        {
            position = tmp;
        }
        else
        {
            if (tmp2)
            {
                position = tmp2;
            }
            else
            {
                position = 0;
            }
        }
        size--;
    } 
    else 
    {
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
