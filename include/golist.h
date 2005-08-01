#ifndef GOLIST_H
#define GOLIST_H

#include <gotypes.h>

template <class T>
class goListElement {
 public:
     goListElement ();
     ~goListElement ();

  T                 elem;
  goListElement*    next;
  goListElement*    prev;
};

/**
 * Simple doubly linked list.
 * This list is not yet completely tested but should do so far.
 * @author Christian Gosch
 */
template <class T>
class goList {

 public:
    typedef goListElement<T>       Element;
    typedef const goListElement<T> ConstElement;

 public:
  ///
  goList ();
  ~goList ();

  Element*      getFrontElement ();
  Element*      getTailElement  ();
  ConstElement* getFrontElement () const;
  ConstElement* getTailElement  () const;
  
  void      next ();
  void      prev ();
  /// Returns the current item and increments the pointer.
  T&        getNext ();
  T*        getNextPtr ();
  /// Returns the current item and decrements the pointer.
  T&        getPrev ();
  T*        getPrevPtr ();
  /// Returns the current item without touching the pointer.
  T&        getCurrent ();
  T*        getCurrentPtr ();
  /// Returns the front of the list.
  T&        getFront ();
  T*        getFrontPtr ();
  /// Returns the tail of the list.
  T&        getTail ();
  T*        getTailPtr ();

  ///
  inline bool       isFront () const { return position == front; }
  ///
  inline bool       isTail  () const { return position == tail; }
  ///
  inline bool       isEmpty () const { return size == 0; }
  ///
  bool          append  (const T& elem);
  ///
  bool          prepend  (T& elem);
  ///
  bool          insert  (T& elem);
  /// Removes the current Item.
  bool          remove  ();
  /// Removes element and returns next if any.
  goListElement<T>* remove (goListElement<T>* el);
  /// Deletes the whole list leaving a zero length list.
  void          erase ();

  /// Makes a cyclic list by connecting front and tail.
  void          close ();
  /// Opens a closed (cyclic) list and makes newFront the new front and the preceding element the new tail.
  void          open  (goListElement<T>* newFront);
  bool          isClosed () const;

  /// Resets the pointer to the front of the list.
  inline void       resetToFront () { position = front; }

  ///
  inline goInt32    getSize () const { return size; }

  ///
  goList<T>&        operator= (const goList<T>& other);

  /// UNTESTED
  bool          operator== (goList<T>& other);
  /// UNTESTED
  bool          operator!= (goList<T>& other);
  bool          operator< (goList<T>& other);
  bool          operator> (goList<T>& other);

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
  goListElement<T>      dummy;
};

/**
 * @brief Algorithm base class to run through a list.
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
        virtual bool action (typename goList<T>::Element* el) {};
        /**
         * @brief Action to perform on each element. Called for each element by run().
         * Reimplement in subclass.
         */
        virtual bool action (typename goList<T>::ConstElement* el) const {};
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
        bool run(typename goList<T>::ConstElement* start, typename goList<T>::ConstElement* end) const
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

#endif
