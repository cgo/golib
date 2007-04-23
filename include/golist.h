#ifndef GOLIST_H
#define GOLIST_H

#include <gotypes.h>
#include <assert.h>

/** @addtogroup data
 * @{ */
/** 
 * @brief Element of a list of type T.
 */
template <class T>
class goListElement {
 public:
    goListElement () :
     elem(), next(0), prev(0), index(0) {};
     ~goListElement () {};

  T                 elem;
  goListElement*    next;
  goListElement*    prev;
  goIndex_t         index;
};

template <class T>
class goListIterator
{
    public:
        goListIterator () : el (0) {};
        goListIterator (goListElement<T>* e) : el (e) {};
        ~goListIterator () {};

        T& operator* () { return el->elem; };
        const T& operator* () const { return el->elem; };
        
        bool operator== (const goListIterator<T>& other) const { return this->el == other.el; };
        bool operator!= (const goListIterator<T>& other) const { return !(this->operator== (other)); };
        void operator++ () { el = el->next; };
        void operator-- () { el = el->prev; };

        goIndex_t getIndex () const { return el->index; };
        void      setIndex (goIndex_t i) { el->index = i; };

    private:
        goListElement<T>* el;
};

template <class T>
class goListConstIterator  
{
    public:
        goListConstIterator () : el (0) {};
        goListConstIterator (const goListElement<T>* e) : el (e) {};
        ~goListConstIterator () {};

        const T& operator* () const { return el->elem; };
        
        bool operator== (const goListConstIterator<T>& other) const { return this->el == other.el; };
        bool operator!= (const goListConstIterator<T>& other) const { return !this->operator== (other); };
        void operator++ () { el = el->next; };
        void operator-- () { el = el->prev; };

        goIndex_t getIndex () const { return el->index; };

    private:
        const goListElement<T>* el;
};

/**
 * Simple doubly linked list.
 * This list is not yet completely tested but should do so far.
 * @author Christian Gosch
 */
template <class T>
class goList {

 public:
    typedef goListElement<T>        Element;
    typedef const goListElement<T>  ConstElement;
    typedef goListIterator<T>       iterator;
    typedef goListConstIterator<T>  const_iterator;

 public:
  ///
  goList ()
      : size (0), front (0), tail (0), position (0), dummy ()
  {};
  goList (const goList<T>& other) 
      : size (0), front (0), tail (0), position (0), dummy ()
  {
      *this = other;
  };
  ~goList ()
  {
      position = front;
      this->erase ();
  };

  iterator begin () { return iterator (this->getFrontElement()); };
  /** 
   * @note Returns the iterator pointing to NULL if the list is not closed.
   * If it is closed, note that the tail element is returned, so 
   * in order to go through every element do the tail element at the end
   * or use the getSize() method to determine the size and iterate over that
   * number of elements. That should be preferred if you do not know 
   * if the list is closed or not.
   */
  iterator end   () 
  { 
      if (!this->isClosed())
          return iterator (0); 
      else
          return iterator (this->getTailElement());
  };
  const_iterator begin () const { return const_iterator (this->getFrontElement()); };
  const_iterator end   () const
  { 
      if (!this->isClosed())
          return const_iterator (0); 
      else
          return const_iterator (this->getTailElement());
  };

  Element*      getFrontElement ()
  { return this->front; };
  Element*      getTailElement  ()
  { return this->tail; };
  ConstElement* getFrontElement () const
  { return this->front; };
  ConstElement* getTailElement  () const
  { return this->tail; };

  Element*      find            (const T& e)
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
  };

  ConstElement* find            (const T& e) const
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
  };

  /** 
   * @brief Return element number i.
   *
   * @note This is slow, O(this->getSize()). Lists are not meant for
   * indexed access.
   * 
   * @return Element number i.
   */
  Element* operator() (goSize_t i)
  {
      if (i >= (goSize_t)this->getSize())
      {
          return 0;
      }
      Element* el = this->getFrontElement();
      goSize_t c = 0;
      while (c < i)
      {
          el = el->next;
          ++c;
      }
      return el;
  };

  /** 
   * @brief Return element number i.
   *
   * @note This is slow, O(this->getSize()). Lists are not meant for
   * indexed access.
   * 
   * @return Element number i.
   */
  ConstElement* operator() (goSize_t i) const
  {
      if (i >= (goSize_t)this->getSize())
      {
          return 0;
      }
      ConstElement* el = this->getFrontElement();
      goSize_t c = 0;
      while (c < i)
      {
          el = el->next;
          ++c;
      }
      return el;
  };

  /** 
   * @brief Fill the index field of the list elements with indices starting at 0.
   */
  void          index           ()
  {
      Element* el = this->getFrontElement();
      goSize_t sz = this->getSize();
      goSize_t i;
      for (i = 0; i < sz; ++i)
      {
          el->index = i;
          el = el->next;
      }
  };
  
  /** 
   * @brief DEPRECATED
   */
  void      next ()
  {
      if (position) 
      {
          if (position != tail) 
          {
              position = position->next;
          }
      }
  };

  /** 
   * @brief DEPRECATED
   */
  void      prev ()
  {
      if (position) 
      {
          if (position != front) 
          {
              position = position->prev;
          }
      }
  };

  // Returns the current item and increments the pointer.
  /** 
   * @brief DEPRECATED
   */
  T&        getNext ()
  {
      T* tmp = 0;
      if (position) {
          tmp = &(position->elem);
          if (position != tail) {
              position = position->next;
          }
      }
      return *tmp;
  };

  /** 
   * @brief DEPRECATED
   */
  T*        getNextPtr ()
  {
      T* tmp = 0;
      if (position) {
          tmp = &(position->elem);
          if (position != tail) {
              position = position->next;
          }
      }
      return tmp;
  };
  // Returns the current item and decrements the pointer.
  /** 
   * @brief DEPRECATED
   */
  T&        getPrev ()
  {
      T* tmp = 0;
      if (position) {
          tmp = &(position->elem);
          if (position != front) {
              position = position->prev;
          }
      }
      return *tmp;
  };

  /** 
   * @brief DEPRECATED
   */
  T*        getPrevPtr ()
  {
      T* tmp = 0;
      if (position) {
          tmp = &(position->elem);
          if (position != front) {
              position = position->prev;
          }
      }
      return tmp;
  };

  // Returns the current item without touching the pointer.
  /** 
   * @brief DEPRECATED
   */
  T&        getCurrent ()
  {
      if (position) {
          return (position->elem);
      }
      return dummy.elem;
  };
  /** 
   * @brief DEPRECATED
   */
  T*        getCurrentPtr ()
  {
      if (position) {
          return &(position->elem);
      }
      return (T*)0;
  };
  /// Returns the front of the list.
  T&        getFront ()
  {
      if (!front)
      {
          return dummy.elem;
      }
      return front->elem;
  };
  const T&        getFront () const
  {
      if (!front)
      {
          return dummy.elem;
      }
      return front->elem;
  };

  T*        getFrontPtr ()
  {
      if (!front)
      {
          return &dummy.elem;
      }
      return &front->elem;
  };

  /// Returns the tail of the list.
  T&        getTail ()
  {
      if (!tail)
      {
          return dummy.elem;
      }
      return tail->elem;
  };
  const T&        getTail () const
  {
      if (!tail)
      {
          return dummy.elem;
      }
      return tail->elem;
  };

  T*        getTailPtr ()
  {
      if (!tail)
      {
          return &dummy.elem;
      }
      return &tail->elem;
  };

  ///
  inline bool       isFront () const { return position == front; }
  ///
  inline bool       isTail  () const { return position == tail; }
  ///
  inline bool       isEmpty () const { return size == 0; }
  ///
  bool          append  (const T& elem)
  {
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
  };
  ///
  bool prepend  (const T& elem)
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
  };
  ///
  bool          insert  (const T& elem)
  {
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
  };

  /** 
   * @brief Append element after another element.
   * 
   * @param el Element to append 
   * @param here Point in list where to append el.
   * 
   * @return True if successful, false otherwise.
   */
  bool          append  (goListElement<T>* el, goListElement<T>* here)
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
  };

  /** 
   * @brief Prepend element before another element.
   * 
   * @param el Element to prepend
   * @param here Point in list where to prepend el.
   * 
   * @return True if successful, false otherwise.
   */
  bool          prepend (goListElement<T>* el, goListElement<T>* here)
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
  };

  /// Removes the current Item.
  bool          remove  ()
  {
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
          --size;
      } 
      else 
      {
          return false;
      }
      return true;
  };

  /**
   * @brief Erase an element from the list.
   *
   * @param el  The element to remove.
   *
   * @return  The next element in the list (el->next).
   **/
  goListElement<T>* remove (goListElement<T>* el)
  {
      if (this->size == 0)
      {
          return 0;
      }
      goListElement<T>* n = this->unhook(el);
      if (el)
      {
          delete el;
          el = 0;
      }
      return n;
  };

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
  goListElement<T>* unhook (goListElement<T>* el)
  {
      if (!el)
          return 0;
      if (el->prev)
      {
          //= This can happen in closed lists (2 elements)
          if (el->prev->next != el->next)
              el->prev->next = el->next;
          else
              el->prev->next = 0;
      }
      if (el->next)
      {
          //= This can happen in closed lists (2 elements)
          if (el->next->prev != el->prev)
              el->next->prev = el->prev;
          else
              el->next->prev = 0;
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
  };

  /// Deletes the whole list leaving a zero length list.
  void          erase ()
  {
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
  };

  void          reverse ()
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
  };

  /// Makes a cyclic list by connecting front and tail.
  void          close ()
  {
      if (this->getSize() > 1 && this->front && this->tail)
      {
          this->front->prev = this->tail;
          this->tail->next = this->front;
      }
  };

  /// Opens a closed (cyclic) list and makes newFront the new front and the preceding element the new tail.
  void          open  (goListElement<T>* newFront)
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
  };

  bool          isClosed () const
  {
      if (this->front && this->tail)
      {
          if (this->front->prev == this->tail && this->tail->next == this->front)
          {
              return true;
          }
      }
      return false;
  };

  bool cyclicPermutation ()
  {
        if (this->front && this->front->next)
        {
            if (this->isClosed())
            {
                this->open (this->front->next);
                this->close ();
            }
            else
            {
                this->close ();
                this->open (this->front->next);
            }
        }
        return true;
  };

  /// Resets the pointer to the front of the list.
  inline void       resetToFront () { position = front; }

  ///
  inline goInt32    getSize () const { return size; }

  /**
   * @brief Deep-copy this list.
   * 
   * Makes a deep copy of this list, that is copied each element in other
   * and appends it to this.
   * 
   * @param other list to be copied.
   * @return Reference to *this.
   */ 
  goList<T>&        operator= (const goList<T>& other)
  {
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
  };

  /// UNTESTED
  bool          operator== (const goList<T>& other) const
  {
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
  };
  /// UNTESTED
  bool          operator!= (const goList<T>& other) const
  {
      return !( (*this) == other );
  };

  bool          operator< (goList<T>& other)
  {
      return !this->operator> (other);
  };
  
  bool          operator> (goList<T>& other)
  {
      if (other.getSize() < this->getSize()) {
          return true;
      }
      return false;
  };

 protected:
  ///
  goInt32       size;
  ///
  goListElement<T>* front;
  ///
  goListElement<T>* tail;
  ///
  goListElement<T>* position;

  ///
  goListElement<T> dummy;
};

/**
 * @brief Algorithm base class to run through a list.
 *
 * Overload the action() methods and call run().
 * @author Christian Gosch
 */
template <class T>
class goListAlgorithm
{
    public:
        goListAlgorithm () {};
        virtual ~goListAlgorithm () {};
        /**
         * @brief Action to perform on each element. Called for each element by run().
         * Reimplement in subclass.
         */
        virtual bool action (typename goList<T>::Element* el) {return false;};
        /**
         * @brief Action to perform on each element. Called for each element by run().
         * Reimplement in subclass.
         */
        virtual bool action (typename goList<T>::ConstElement* el) const {return false;};
        /**
         * @brief Calls action() for each element from first to last.
         */
        bool run(typename goList<T>::Element* first, typename goList<T>::Element* last) 
        {
            if (!first || !last)
                return false;

            while (true)
            {
                this->action(first);
                if (!first->next || first == last)
                    break;
                first = first->next;
            }
            return (first == last);
        };
        /**
         * @brief Calls action() for each element from first to last.
         */
        bool run(typename goList<T>::ConstElement* first, typename goList<T>::ConstElement* last) const
        {
            if (!first || !last)
                return false;

            while (true)
            {
                this->action(first);
                if (!first->next || first == last)
                    break;
                first = first->next;
            }
            return (first == last);
        };
};
/** @} */
#endif
