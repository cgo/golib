
template <class T>
goQueue<T>::goQueue () {
}

template <class T>
goQueue<T>::~goQueue () {
  list.erase ();
}

template <class T>
T
goQueue<T>::getHead () {
  if (!isEmpty()) {
    return *list.getFrontPtr();
  } else {
    T dummy = 0;
    return dummy;
  }
}

template <class T>
bool
goQueue<T>::isEmpty () {
  return list.isEmpty();
}

template <class T>
T
goQueue<T>::getTail () {
  if (!list.isEmpty()) {
    return *list.getTailPtr();
  } else {
    T dummy = 0;
    return dummy;
  }
}

template <class T>
bool
goQueue<T>::remove () {
  if (!list.isEmpty()) {
    list.remove();
    return true;
  }
  return false;
}

template <class T>
bool
goQueue<T>::removeTail () {
  if (!list.isEmpty()) {
    while (!list.isTail()) {
      list.getNextPtr();
    }
    list.remove();
    list.resetToFront();
    return true;
  }
  return false;
}

template <class T>
bool
goQueue<T>::add (T& item) {
  bool retval;
  retval = list.append (item);
  list.resetToFront ();
  return retval;
}
