#ifndef GOHASHTABLE_H
#define GOHASHTABLE_H

#include <gotypes.h>
#include <goarray.h>
#include <golist.h>

template <class I, class O>
class goHashEntry {
 public:
    virtual goHashEntry<I,O>& operator = (goHashEntry<I,O>& other);
  I	key;
  O	value;
};

// typedef goList<goHashEntry<goUInt64, void*> >	entryList;

/**
 * Implementation of a simple hash table.
 * You always have to check fail() before doing anything to the result of an operator[] call,
 * if you don't, the code is likely to crash. If you do, it should be safe.
 * The table index is calculated out of the last lb(modValue+1) bits of the key.
 * @author Christian Gosch
 * @note This class is considered as not tested hard enough. If you find bugs,
 * please contact the author.
 * @note I didn't say this is fast, did I?
 * @todo I need testing!!! Class was changed together with goList 21.8.2001.
 * Optimizations are possible.
 */
template <class I, class O>
class goHashTable {
 public:
  ///
  goHashTable ();
  /*!
   * @todo Problem: Hashtable für Zeiger. Daten müssen per Hand gelöscht werden. 
   */
  virtual ~goHashTable ();

  /*!
   * Sets the hash mod value. Only call this once, before you do anything else with the
   * hash table object. If you don't, or if you call it more than once, unexpected behaviour is
   * the result.
   * should always be something like (2^n-1).
   * @attention All previous entries are lost when calling this method. Call only once!
   * @param i Mod value.
   */
  void	setModValue (goUInt32);

  ///
  goUInt32 getModValue () { return modValue; }
  ///
  O&	operator[] (I in);
  ///
  virtual void	add (I key, O value);
  /*!
   * @note Only superficially tested.
   * @return The value for the given key.
   */ 
  virtual O	remove (I key);
  
  ///
  bool	fail ();

 protected:
  void addEntry (I table, void* entry);
  /// Erases all lists and resizes the table to zero.
  void eraseLists();
  ///
  bool		lastFailed;
  ///
  goUInt32	modValue;
  ///
    // goArray<goList<goHashEntry<I,O> > >	*table;
  goArray<void*> theTable;
  ///
  O		dummy;
};


template <class I, class O>
goHashEntry<I,O>&
goHashEntry<I,O>::operator= (goHashEntry<I,O>& other) {
  key = other.key;
  value = other.value;
  return *this;
}

template <class I, class O>
goHashTable<I,O>::goHashTable () {
    // setModValue (511);
    lastFailed = false;
    dummy = (O)0;
    // table = new goArray<goList<goHashEntry<I,O> > >;
    theTable.resize(0);
}

template <class I, class O>
goHashTable<I,O>::~goHashTable () {
    eraseLists();
}

template <class I, class O>
void
goHashTable<I,O>::eraseLists ()
{
  goIndex_t i;
  for ( i = 0; i < theTable.getSize(); i++)
    {
	delete (goList<void*>*)theTable[i];
    }
  theTable.resize (0);
}

template <class I, class O>
void
goHashTable<I,O>::setModValue (goUInt32 i)
{
    eraseLists();
  modValue = i;
  theTable.resize (modValue + 1);
  goIndex_t c;
  goList<void*>* list;

  std::cout << "hash table generating " << theTable.getSize() << " lists" << std::endl;
  for (c = 0; c < theTable.getSize(); c++)
    {
	list = new goList<void*>();
	theTable[c] = (void*)list;
    }
}


template <class I, class O>
O&
goHashTable<I,O>::operator[] (I in) {
  I	tmp = (in & modValue);   // use the last n bits for accessing the list
  goList<void*> *list = 0;
  list = (goList<void*>*)theTable[tmp];
  if (!list->isEmpty()) {
    list->resetToFront();
    bool endFlag = false;
      do {
	  endFlag = list->isTail();
	  if ( ((goHashEntry<I,O>*)list->getCurrent())->key == in) {
	    lastFailed = false;
	    return (O&)( ((goHashEntry<I,O>*)list->getCurrent())->value);
	  } else {
	    list->getNext();
	  }
      } while (!endFlag);
  }
  lastFailed = true;
  return dummy;
}

template <class I, class O>
void
goHashTable<I,O>::add (I key, O value) {
    //    cout << "hashtable adding " << key << "," << value << endl;
  I tmp = (key & modValue);
  (*this)[key] = value;
  if (fail()) {
      // cout << "...adding new entry." << endl;
    goHashEntry<I,O> *entry = new goHashEntry<I,O>();
    entry->key = key;
    entry->value = value;
    addEntry (tmp, (void*)entry);
  }
  lastFailed = false;
}

template <class I, class O>
void
goHashTable<I,O>::addEntry (I table, void* e)
{
    ((goList<void*>*)theTable[table])->append (e);
}

template <class I, class O>
O
goHashTable<I,O>::remove (I key)
{
  I	tmp = (key & modValue);   // use the last n bits for accessing the list
  goList<void*>	*list = 0;

  lastFailed = false;
//   if (tmp > (unsigned)theTable.getSize()) {
//     lastFailed = true;
//   }
  list = (goList<void*>*)theTable[tmp];
  O retval = 0;
  if (list->getFront()) {
    list->resetToFront();
    bool endFlag = false;
    do {
	endFlag = list->isTail();
      if ( ((goHashEntry<I,O>*)list->getCurrent())->key == key) {
	retval = ((goHashEntry<I,O>*)list->getCurrent())->value;
	delete ((goHashEntry<I,O>*)list->getCurrent());
	list->remove();
	return retval;
      }
      list->getNext();
    } while (!endFlag);
  }
  lastFailed = true;
  return retval;
}

template <class I, class O>
bool
goHashTable<I,O>::fail() {
  return lastFailed;
}

/*!
 * \example hashtable.cc
 * Example and test program for the goHashTable.
 * @author Christian Gosch
 * @date 6.8.2001
 */

#endif
