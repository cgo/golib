#include <gohashtable.h>

template <class I, class O>
goHashEntry<I,O>&
goHashEntry<I,O>::operator= (goHashEntry& other) {
  key = other.key;
  value = other.value;
  return *this;
}

template <class I, class O>
goHashTable<I,O>::goHashTable (goUInt32 modval) {
  setModValue (modval);
  table.resize (modval + 1);
  goSize_t i;
  for (i = 0; i < (modval + 1); i++) {
    table[i] = (void*)(new goArray<void*>);
  }
  lastFailed = false;
  dummy = 0;
}

template <class I, class O>
goHashTable<I,O>::~goHashTable () {
  goIndex_t i,j;
  goArray<void*> *t2;
  for (i = 0; i < table.getSize(); i++) {
    t2 = (goArray<void*>*)table[i];
    for (j = 0; j < t2->getSize(); j++) {
      delete (goHashEntry<I,O>*)(*t2)[j];
    }
    t2->resize(0);
    delete t2;
  }
  table.resize(0);
}

template <class I, class O>
O&
goHashTable<I,O>::operator[] (I in) {
  I	tmp = (in & modValue);   // use the last n bits for accessing the list
  goArray<void*>	*t2;

  lastFailed = false;
  //  if (tmp > (unsigned)table.getSize()) {
  //  lastFailed = true;
  //  return dummy;
  // }
  t2 = (goArray<void*>*) table[tmp];
  goHashEntry<I,O> *entry;
  goIndex_t i;
  if (t2->getSize() == 0) {
    lastFailed = true;
    return dummy;
  }
  entry = (goHashEntry<I,O>*)(*t2)[0];
  if ( entry->key == in ) {
    return entry->value;
  } else {
    for (i = 1; i < t2->getSize(); i++) {
      entry = (goHashEntry<I,O>*)(*t2)[i];
      if ( entry->key == in ) {
	return entry->value;
      }   
    }
  }
  lastFailed = true;
  return dummy;
}

template <class I, class O>
void
goHashTable<I,O>::add (I key, O value) {
  (*this)[key] = value;
  if (fail()) {
    I tmp = (key & modValue);
    goArray<void*> *t2 = (goArray<void*>*) table[tmp];
    goHashEntry<I,O> *entry = new goHashEntry<I,O>;
    if ( tmp >= (I)table.getSize() ) {
      table.resize (tmp + 1);
    }
    entry->key = key;
    entry->value = value;
    t2->resize (t2->getSize() + 1);
    (*t2)[t2->getSize() - 1] = (void*)(entry);
  }
  lastFailed = false;
}

template <class I, class O>
bool
goHashTable<I,O>::fail() {
  return lastFailed;
}

template <class I, class O>
void
goHashTable<I,O>::sort () {
  
}

/* Instantiation */

template class goHashEntry<goInt32, goInt32>;
template class goHashEntry<goInt32, goUInt32>;
template class goHashEntry<goInt32, goFloat>;
template class goHashTable<goInt32, goInt32>;
template class goHashTable<goInt32, goUInt32>;
template class goHashTable<goUInt32, void*>;




