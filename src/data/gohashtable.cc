#include <gohashtable.h>

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

  cout << "hash table generating " << theTable.getSize() << " lists" << endl;
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

/* Instantiation */

template class goHashEntry<goUInt64, void*>;
template class goHashTable<goUInt64, void*>;
template class goHashEntry<goUInt32, void*>;
template class goHashTable<goUInt32, void*>;


