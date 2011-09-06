/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gohashcache.h>
#include <gorandom.h>
#include <gopresencemanager.h>
#include <goconfig.h>

namespace Vol {

template <class I, class O>
goHashCacheEntry<I,O>&
goHashCacheEntry<I,O>::operator= (goHashCacheEntry<I,O>& other) {
  key   = other.key;
  value = other.value;
  p     = other.p;
  return *this;
}

template <class I, class O, class T>
goHashCache<I,O,T>::goHashCache () {
    // setModValue (511);
    lastFailed = false;
    dummy = (O)0;
    // table = new goArray<goList<goHashCacheEntry<I,O> > >;
    theTable.resize(0);
    removable.resize(0);
    // Start at "time" 0
    time = 1;
}

template <class I, class O, class T>
goHashCache<I,O,T>::~goHashCache () {
    eraseLists();
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::eraseLists ()
{
  goIndex_t i;
  for ( i = 0; i < theTable.getSize(); i++)
    {
		delete (goList<void*>*)theTable[i];
    }
  theTable.resize (0);
  removable.resize(0);
  indices.resize (0);
  for (i = 0; i < listMutex.getSize(); i++)
  {
	  delete (goMutex*)listMutex[i];
  }
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::setModValue (goUInt32 i)
{
    eraseLists();
    modValue = i;
    theTable.resize (modValue + 1);
    removable.resize  (modValue + 1);
    indices.resize (modValue + 1);
	
	listMutex.resize (modValue + 1);
	int idx;
	for (idx = 0; idx < listMutex.getSize(); idx++)
	{
		listMutex[idx] = (void*)(new goMutex);
	}
    goIndex_t c;
    goList<void*>* list;
    
    cout << "hash table generating " << theTable.getSize() << " lists" << endl;
    for (c = 0; c < theTable.getSize(); c++)
	{
	    list = new goList<void*>();
	    theTable[c] = (void*)list;
	    removable[c] = 0;		// initialize p_max
	    indices[c] = c;			// initialize index array
	}
}

// template <class I, class O, class T>
// O&
// goHashCache<I,O,T>::operator[] (I in) {
//   I	tmp = (in & modValue);   // use the last n bits for accessing the list
//   goList<void*> *list = 0;
//   list = (goList<void*>*)theTable[tmp];
//   if (!list->isEmpty()) {
//     list->resetToFront();
//     bool endFlag = false;
//     goHashCacheEntry<I,O>* entry;
//       do {
// 	  endFlag = list->isTail();
// 	  entry = (goHashCacheEntry<I,O>*)list->getCurrent();
// 	  if ( entry->key == in) {
// 	    lastFailed = false;
// 	    entry->p = time;
// 	    return entry->value;
// 	  }
// 	  list->getNext();
//       } while (!endFlag);
//   }
//   lastFailed = true;
//   return dummy;
// }

template <class I, class O, class T>
goHashCacheEntry<I,O>*
goHashCache<I,O,T>::getEntry (I in) {
  I	tmp = (in & modValue);   // use the last n bits for accessing the list
  goList<void*> *list = 0;
  lockList ((goIndex_t)tmp);
  list = (goList<void*>*)theTable[tmp];
  if (!list->isEmpty()) {
    list->resetToFront();
    bool endFlag = false;
      do {
	  endFlag = list->isTail();
	  if ( ((goHashCacheEntry<I,O>*)list->getCurrent())->key == in) {
	    lastFailed = false;
  		unlockList ((goIndex_t)tmp);
	    return (goHashCacheEntry<I,O>*)list->getCurrent();
	  } else {
	    list->getNext();
	  }
      } while (!endFlag);
  }
  lastFailed = true;
  unlockList ((goIndex_t)tmp);
  return 0;
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::add (I key, O value) {
    //    cout << "hashtable adding " << key << "," << value << endl;
  I tmp = (key & modValue);
  (*this)[key] = value;
  if (fail()) {
      // cout << "...adding new entry." << endl;
    goHashCacheEntry<I,O> *entry = new goHashCacheEntry<I,O>;
    entry->key   = key;
    entry->value = value;
    entry->p     = time;
    addEntry (tmp, (void*)entry);
  }
  lastFailed = false;
}

// Deprecated
#if 0
template<class I, class O, class T>
void
goHashCache<I,O,T>::setRemovability (I key, goUInt32 p)
{
    I tmp = (key & modValue);
    
    if (p > removable[tmp])
	{
	    
	    removable[tmp] = p;
	}
    goHashCacheEntry<I,O>* entry;
    entry = getEntry (key);
    if (entry)
	{
	    entry->p = p;
	    // Nicht nötig, da removable ggf. schon gesetzt wurde.
	    // updateRemovability (tmp);
	    return;
	} 
    else 
	{
	    lastFailed = true;
	}
}
#endif

template <class I, class O, class T>
void
goHashCache<I,O,T>::addEntry (I table, void* e)
{
	lockList ((goIndex_t)table);
    ((goList<void*>*)theTable[table])->append (e);
	unlockList ((goIndex_t)table);
}


template <class I, class O, class T>
O
goHashCache<I,O,T>::remove (I key)
{
  I	tmp = (key & modValue);   // use the last n bits for accessing the list
  goList<void*>	*list = 0;

  lastFailed = false;
//   if (tmp > (unsigned)theTable.getSize()) {
//     lastFailed = true;
//   }
  lockList ((goIndex_t)tmp);
  list = (goList<void*>*)theTable[tmp];
  O retval = 0;
  if (list->getFront()) {
    list->resetToFront();
    bool endFlag = false;
    goUInt32 p = 0;
    goHashCacheEntry<I,O> *entry;
    do {
	endFlag = list->isTail();
	if ( ((goHashCacheEntry<I,O>*)list->getCurrent())->key == key) {
	    entry  = (goHashCacheEntry<I,O>*)list->getCurrent();
	    retval = entry->value;
	    p      = entry->p;
	    delete ((goHashCacheEntry<I,O>*)list->getCurrent());
	    list->remove();
	    if (p < time)
		{
		    updateRemovability (tmp);
		}
		unlockList ((goIndex_t)tmp);
	    return retval;
	}
      list->getNext();
    } while (!endFlag);
  }
  lastFailed = true;
  unlockList ((goIndex_t)tmp);
  return retval;
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::removeSome (goArray<I>& keys, goArray<O>& values, 
								goUInt8 flag, goPresenceManager<T>& pm)
{
    // Generate a random starting point in the table of lists -- dont use it yet.
    // goFloat f = goRandom (true);
    // goIndex_t startIndex = (goIndex_t)(f * (theTable.getSize() - 1));
    goIndex_t i;
    // Iterate all lists
    for (i = 0; (i < theTable.getSize()); i++)
	{
	    // if there is an entry we may discard (removable[i] < time and > 0, so theres
	    // entries in there and there are some of them older than the current time state
		lockList (i);
	    goList<void*>* list  = (goList<void*>*)theTable[i];
		//cout << "hashcache: removable[" << i << "] = " << P << ", time = " << time << endl;
	    if ( !list->isEmpty() )
		{
		    list->resetToFront();
		    goHashCacheEntry<I,O>* entry = 0;
		    // track the next smallest time tag value to store it in removable[i]
		    bool lastOne;
		    do {
				lastOne = list->isTail();
				entry   = (goHashCacheEntry<I,O>*)list->getCurrent();
				// Check if deletable and do so if yes
				bool R = pm.removalCheck(entry->key);
				// cout << "goHashCache: checking block " << entry->key << ": " << 
				// "removable = " << R << ", flag = " << (int)pm.getFlag(entry->key) << "\n";
				if ( R && 
					 (!pm.isScheduled(entry->key)) && 
					 (pm.getFlag(entry->key) == flag) )
				{
					// delete it
					pm.removalRemoved(entry->key);
					pm.removalRelease();
					keys += entry->key;
					values += entry->value;
					list->remove();
					delete entry;
				} 
				else
				{
					pm.removalRelease();
					list->getNext();
				}
		   	 } while (!lastOne);
		} // this list
		unlockList (i);
	} // for all lists
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::updateRemovability(I listIndex)
{
	lockList ((goIndex_t)listIndex);
    goList<void*>* list = (goList<void*>*)theTable[listIndex];
    goHashCacheEntry<I,O> *entry;
    if (!list->isEmpty())
	{
	    list->resetToFront();
	    bool endFlag = false;
	    goUInt32 p = removable[listIndex];
	    do {
			endFlag = list->isTail();
			entry = (goHashCacheEntry<I,O>*)list->getCurrent();
			if (entry->p < p)
		   	 {
				cout << "updating removable:" << endl;
				cout << "Current time = " << time << endl;
				cout << "\tIndex = " << listIndex << endl;
				cout << "\tOld p = " << p << endl;
				cout << "\tKey   = " << entry->key << endl;
				cout << "\tValue = " << entry->value << endl;
				cout << "\tNew p = " << entry->p << endl;
				removable[listIndex] = entry->p;
		   	 }
			list->getNext();
	    } while (!endFlag);
	}
	unlockList ((goIndex_t)listIndex);
}

template <class I, class O, class T>
void
goHashCache<I,O,T>::print()
{
    goIndex_t i;
    for (i = 0; i < theTable.getSize(); i++)
	{
	    goList<void*>* list = (goList<void*>*)theTable[i];
	    if (!list->isEmpty())
		{
		    cout << "List " << i << ", max. p = " << removable[i] << endl;
		    cout << "--------" << endl;
		    list->resetToFront();
		    bool endFlag = false;
		    do {
			endFlag = list->isTail();
			goHashCacheEntry<I,O> *entry = 
			    (goHashCacheEntry<I,O>*)list->getCurrent();
			cout << "[" << entry->key << " ";
			cout << entry->value << " ";
			cout << entry->p << "]  ";
			list->getNext();
		    } while (!endFlag);
		    cout << endl << endl;
		}
	}
}

template <class I, class O, class T>
bool
goHashCache<I,O,T>::fail() 
{
    return lastFailed;
}

};

/* Instantiation */

#ifdef HAVE_INT64
template class Vol::goHashCacheEntry<goUInt64, void*>;
#endif
template class Vol::goHashCacheEntry<goUInt32, void*>;
#ifdef HAVE_INT64
template class Vol::goHashCache<goUInt64, void*, goInt8>;
template class Vol::goHashCache<goUInt64, void*, goUInt8>;
template class Vol::goHashCache<goUInt64, void*, goInt16>;
template class Vol::goHashCache<goUInt64, void*, goUInt16>;
template class Vol::goHashCache<goUInt64, void*, goInt32>;
template class Vol::goHashCache<goUInt64, void*, goUInt32>;
template class Vol::goHashCache<goUInt64, void*, goFloat>;
template class Vol::goHashCache<goUInt64, void*, goDouble>;
#endif
// template class Vol::goHashCache<goUInt32, void*>;   // Im Wesentlichen das einzige, was gebraucht wird
template class Vol::goHashCache<goUInt32, void*, goInt8>;
template class Vol::goHashCache<goUInt32, void*, goUInt8>;
template class Vol::goHashCache<goUInt32, void*, goInt16>;
template class Vol::goHashCache<goUInt32, void*, goUInt16>;
template class Vol::goHashCache<goUInt32, void*, goInt32>;
template class Vol::goHashCache<goUInt32, void*, goUInt32>;
template class Vol::goHashCache<goUInt32, void*, goFloat>;
template class Vol::goHashCache<goUInt32, void*, goDouble>;

// template class goHashCache<goInt32, goInt16>;
// template class goHashCache<goInt32, goInt32>;

