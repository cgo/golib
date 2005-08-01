#ifndef GOHASHCACHE_H
#define GOHASHCACHE_H

#include <gotypes.h>
#include <goarray.h>
#include <golist.h>
#include <gothread.h>


namespace Vol {

template<class T> class goPresenceManager;

template <class I, class O>
class goHashCacheEntry {
 public:
  goHashCacheEntry<I,O>& operator = (goHashCacheEntry<I,O>& other);
  I		key;
  O		value;
  goUInt32	p;    // "Probability"
};

// typedef goList<goHashCacheEntry<goUInt64, void*> >	entryList;

/*!
 * Implementation of a simple hash table which provides 
 * memory protection together with classes in the Vol namespace (if present).
 * You always have to check fail() before doing anything to the result of an operator[] call,
 * if you don't, the code is likely to crash. If you do, it should be safe.
 * The table index is calculated out of the last lb(modValue+1) bits of the key.
 * You HAVE to call setModValue() before you can do ANYTHING with an object of this class.
 * If you do not call setModValue(), your code WILL crash or do other funny things.
 * @attention goList is NOT THREADSAFE. So this class has to take care of
 * concurrent read/write access by using mutexes for each list used.
 * @author Christian Gosch
 * @note This class is considered as tested but if you find bugs,
 * please contact the author.
 * @note I didn't say this is fast, did I?
 */
template <class I, class O, class T>
class goHashCache {
 public:
  ///
  goHashCache ();
  /*!
   * @todo Problem: Hashtable für Zeiger. Daten müssen per Hand gelöscht werden. 
   */
  ~goHashCache ();

  /*!
   * Sets the hash mod value. Only call this once, before you do anything else with the
   * hash table object. If you don't, or if you call it more than once, unexpected behaviour is
   * the result.
   * should always be something like (2^n-1).
   * @attention All previous entries are lost when calling this method. Call only once!
   * Again: CALL ONLY ONCE PER INSTANCE OF THIS CLASS!
   * @param i Mod value.
   */
  void     setModValue (goUInt32);
  goUInt32 getModValue ();
  ///
  inline O&			 operator[] (I in);
  goHashCacheEntry<I,O>* getEntry (I in);

  ///
  void	add (I key, O value);
  
  // void  incrementTime () { time++; cout << "Time now: " << time << endl; }

  // void  setRemovability (I key, goUInt32 p);

  /*!
   * @attention It would be best not to use this from an application, although it is possible.
   * Better use removeSome() or stick to goHashTable if you don't need the added functionality
   * of access tracking.
   * @return The value for the given key.
   */ 
  O	remove (I key);
  /*!
   * Remove some, up to, but not necessarily exactly, a given number of elements.
   * This method uses the "time stamps" on each element to determine which one to delete.
   * Elements with time stamp >= current time will not be deleted.
   * The algorithm iterates ONCE through the table of lists. It deletes all 
   * elements in each list that have the lowest time stamp value
   * (which is stored for each table entry individually). There is one little flaw which is
   * documented in the source code. See if it works ok in everyday use, if not, 
   * implement another loop/algorithm.
   * @param keys Contains the deleted keys after method returns. Needs to be of size zero
   * before the call.
   * @param values Contains the deleted values after method returns. Needs to be of size zero
   * before the call.
   * @param upTo Number of elements up to which should be deleted.
   */
  void  removeSome (goArray<I>& keys, goArray<O>& values, 
  					goUInt8 flag, goPresenceManager<T>& pm);

  ///
  bool	fail ();

  void  print();

  void  lockList (goIndex_t i)
  {
	  ((goMutex*)listMutex[i])->lock();
  }
  void  unlockList (goIndex_t i)
  {
	  ((goMutex*)listMutex[i])->unlock();
  }
 protected:
  void updateRemovability (I listIndex);
  void addEntry (I table, void* entry);
  /// Erases all lists and resizes the table to zero.
  void eraseLists();
  ///
  bool		lastFailed;
  ///
  goUInt32	modValue;
  ///
    // goArray<goList<goHashCacheEntry<I,O,T> > >	*table;
  goArray<void*>	theTable;
  goArray<void*>	listMutex;
  goArray<goUInt32>	removable;
  goUInt32		time;
  goArray<goIndex_t>	indices;
  ///
  O		dummy;
};


template <class I, class O, class T>
inline
O&
goHashCache<I,O,T>::operator[] (I in) {
  I	tmp = (in & modValue);   // use the last n bits for accessing the list
  goList<void*> *list = 0;
  lockList ((goIndex_t)tmp);
  list = (goList<void*>*)theTable[tmp];
  if (!list->isEmpty()) {
    list->resetToFront();
    bool endFlag = false;
    goHashCacheEntry<I,O>* entry;
      do {
	  endFlag = list->isTail();
	  entry = (goHashCacheEntry<I,O>*)list->getCurrent();
	  if ( entry->key == in) {
	    lastFailed = false;
	    entry->p = time;
		unlockList((goIndex_t)tmp);
	    return entry->value;
	  }
	  list->getNext();
      } while (!endFlag);
  }
  lastFailed = true;
  unlockList ((goIndex_t)tmp);
  return dummy;
}

};
/*!
 * \example hashcache.cc
 * Example and test program for the goHashCache.
 * @author Christian Gosch
 * @date 28.10.2001 ( at least somewhere on the way between 27 and 28 ;) )
 */

#endif
