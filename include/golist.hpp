// #include <golist.h>
// #include <gostring.h>
// #include <gohashtable.h>
#include <assert.h>

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
goList<T>::goList (const goList<T>& other) 
{
  front		= 0;
  tail		= 0;
  position	= 0;
  size		= 0;
  *this = other;
}

template <class T>
goList<T>::~goList () {
  position = front;
  this->erase();
}

template <class T>
typename goList<T>::Element* goList<T>::getFrontElement ()
{
    return this->front;
}

template <class T>
typename goList<T>::Element* goList<T>::getTailElement ()
{
    return this->tail;
}

template <class T>
typename goList<T>::Element* goList<T>::find (const T& e)
{
    Element* el = this->getFrontElement();
    goSize_t sz = this->getSize();
    goSize_t i;
    for (i = 0; i < sz; ++i)
    {
        if (el->elem == e)
        {
            return el;
        }
        el = el->next;
    }
    return 0;
}

template <class T>
typename goList<T>::ConstElement* goList<T>::find (const T& e) const
{
    ConstElement* el = this->getFrontElement();
    goSize_t sz = this->getSize();
    goSize_t i;
    for (i = 0; i < sz; ++i)
    {
        if (el->elem == e)
        {
            return el;
        }
        el = el->next;
    }
    return 0;
}

template <class T>
typename goList<T>::ConstElement* goList<T>::getFrontElement () const
{
    return this->front;
}

template <class T>
typename goList<T>::ConstElement* goList<T>::getTailElement () const
{
    return this->tail;
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
goList<T>::prepend (T& elem)
{
  goListElement<T> *e = new goListElement<T>;

  e->elem = elem;
  if (!front) 
  {
    front = e;
    tail = e;
    e->next = 0;
    e->prev = 0;
  } else 
  {
    front->prev = e;
    e->prev = 0;
    e->next = front;
    front = e;
  }
  if (!position) 
  {
    position = front;
  }
  size++;
  return true;
}

template <class T>
bool
goList<T>::insert (T& elem) {
  goListElement<T> *e = new goListElement<T>;
  
  e->elem = elem;

  if (position) {
    goListElement<T>* tmp = position->next;
    position->next = e;
    e->prev = position;
    e->next = tmp;
    if (tmp)
        tmp->prev = e;
    size++;
  } else {
    append (elem);
  }
  return true;
}

/** 
 * @brief Append element after another element.
 * 
 * @param el Element to append 
 * @param here Point in list where to append el.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goList<T>::append (goListElement<T>* el, goListElement<T>* here)
{
    if (here->next)
    {
        here->next->prev = el;
        el->prev = here;
        el->next = here->next;
        here->next = el;
        ++this->size;
    }
    else
    {
        assert (here == this->tail);
        here->next = el;
        el->prev = here;
        el->next = 0;
        ++this->size;
    }
    //= In closed lists, front and tail must be adjacent. Therefore, change tail.
    if (here == this->tail)
    {
        this->tail = el;
    }
    return true;
}

/** 
 * @brief Prepend element before another element.
 * 
 * @param el Element to prepend
 * @param here Point in list where to prepend el.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goList<T>::prepend (goListElement<T>* el, goListElement<T>* here)
{
    if (here->prev)
    {
        here->prev->next = el;
        el->next = here;
        el->prev = here->prev;
        here->prev = el;
        ++this->size;
    }
    else
    {
        assert (here == this->front);
        here->prev = el;
        el->next = here;
        el->prev = 0;
        ++this->size;
    }
    //= In closed lists, front and tail must be adjacent. Therefore, change front.
    if (here == this->front)
    {
        this->front = el;
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

/**
 * @brief Erase an element from the list.
 *
 * @param el  The element to remove.
 *
 * @return  The next element in the list (el->next).
 **/
template <class T>
goListElement<T>* goList<T>::remove (goListElement<T>* el)
{
    goListElement<T>* n = this->unhook(el);
    if (el)
    {
        delete el;
    }
    el = 0;
    return n;
#if 0
    if (!el)
        return 0;
    if (el->prev)
    {
        el->prev->next = el->next;
    }
    if (el->next)
    {
        el->next->prev = el->prev;
    }
    if (el == front)
    {
        front = el->next;
    }
    if (el == tail)
    {
        tail = el->prev;
    }
    if (position == el)
    {
        if (el->next)
            position = el->next;
        else
            position = el->prev;
    }
    goListElement<T>* n = el->next;
    --this->size;
    delete el;
    el = 0;
    return n;
#endif
}

/** 
 * @brief Unhook an element.
 * 
 * The element is not deleted, as opposed to remove(). It can be re-inserted with the prepend and append methods
 * or put into another list.
 * However, if it is not re-inserted into a list, the user must make sure
 * it gets deleted using the standard C++ delete operator.
 *
 * @param el Element to unhook.
 * 
 * @return The next element in the list after el, or 0.
 */
template <class T>
goListElement<T>* goList<T>::unhook (goListElement<T>* el)
{
    if (!el)
        return 0;
    if (el->prev)
    {
        el->prev->next = el->next;
    }
    if (el->next)
    {
        el->next->prev = el->prev;
    }
    if (el == front)
    {
        front = el->next;
    }
    if (el == tail)
    {
        tail = el->prev;
    }
    if (position == el)
    {
        if (el->next)
            position = el->next;
        else
            position = el->prev;
    }
    goListElement<T>* n = el->next;
    --this->size;
    return n;
}

template <class T>
void
goList<T>::erase () {
    typename goList<T>::Element* el = this->getFrontElement();
    if (!el)
    {
        return;
    }
    if (this->isClosed())
    {
        this->open (this->getFrontElement());
    }
    while (el)
    {
        el = this->remove(el);
    }
    this->size = 0;
}

template <class T>
void
goList<T>::reverse () 
{
    typename goList<T>::Element* el = this->getFrontElement();
    goIndex_t sz = this->getSize();
    goIndex_t i = 0;
    while (el && i < sz)
    {
        typename goList<T>::Element* temp = el->next;
        el->next = el->prev;
        el->prev = temp;
        el = temp;
        ++i;
    }
    goListElement<T>* temp = this->front;
    this->front = this->tail;
    this->tail = temp;
}

template <class T>
void goList<T>::close ()
{
    if (this->front && this->tail)
    {
        this->front->prev = this->tail;
        this->tail->next = this->front;
    }
}

template <class T>
void goList<T>::open (goList<T>::Element* newFront)
{
    if (!this->isClosed() || !newFront)
        return;
    typename goList<T>::Element* p = newFront->prev;
    newFront->prev = 0;
    if (p)
    {
        p->next = 0;
    }
    this->front = newFront;
    this->tail = p;
}

template <class T>
bool goList<T>::isClosed () const
{
    if (this->front && this->tail)
    {
        if (this->front->prev == this->tail && this->tail->next == this->front)
        {
            return true;
        }
    }
    return false;
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

/** --------------------------------------------------------------------------
 * @brief Deep-copy this list.
 * 
 * Makes a deep copy of this list, that is copied each element in other
 * and appends it to this.
 * 
 * @param other list to be copied.
 * @return Reference to *this.
 ----------------------------------------------------------------------------*/
template <class T>
goList<T>&
goList<T>::operator= (const goList<T>& other) {
  this->erase ();
  if (other.isEmpty())
      return *this;
  
  typename goList<T>::ConstElement* el = other.getFrontElement();
  goIndex_t sz = other.getSize();
  goIndex_t i = 0;
  while (i < sz && el)
  {
      this->append(el->elem);
      el = el->next;
      ++i;
  }
  if (other.isClosed())
  {
      this->close();
  }
  else
  {
      this->open (this->getFrontElement());
  }
  return *this;
//  if (!other.isEmpty()) {
//    other.resetToFront ();
//    while (!other.isTail()) {
//      append (other.getNext());
//    }
//    append (other.getCurrent());
//  }
//  return *this;
}

template <class T>
bool
goList<T>::operator== (const goList<T>& other) const {
    if (other.isEmpty() || this->isEmpty())
    {
        return false;
    }
    if (other.getSize() != this->getSize())
    {
        return false;
    }
    typename goList<T>::ConstElement* el = this->getFrontElement();
    typename goList<T>::ConstElement* oEl = other.getFrontElement();
    goSize_t sz = this->getSize();
    goSize_t i = 0;
    for (i = 0; i < sz; ++i, el = el->next, oEl = oEl->next)
    {
        if (el->elem != oEl->elem)
        {
            return false;
        }
    }
    return true;
}

template <class T>
bool
goList<T>::operator!= (const goList<T>& other) const {
  return !( (*this) == other );
}
