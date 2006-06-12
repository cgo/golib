template <class I, class O>
goHashEntry<I,O>::goHashEntry ()
{
}

template <class I, class O>
goHashEntry<I,O>::~goHashEntry ()
{
}

template <class I, class O>
goHashEntry<I,O>&
goHashEntry<I,O>::operator= (goHashEntry<I,O>& other) {
  key = other.key;
  value = other.value;
  return *this;
}

template <class I, class O>
goHashTable<I,O>::goHashTable (goUInt32 mod_value) 
    :   lastFailed (false),
        modValue   (0),
        theTable   (),
        dummy      ()
{
    setModValue (mod_value);
    lastFailed = false;
//    dummy = (O)0;   // This doesn't work for strings and the like. Don't use.
//    theTable.resize(0);
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
  for (i = 0; i < theTable.getSize(); i++)
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
    this->modValue = i;
    theTable.resize (this->modValue + 1);
    goList<void*>* list;

    // printf("hash table generating %d lists.\n", theTable.getSize());
    goIndex_t c;
    for (c = 0; c < theTable.getSize(); c++)
    {
        list = new goList<void*>();
        theTable[c] = (void*)list;
    }
}

/** --------------------------------------------------------------------------
 * @brief  Set the default return value.
 * 
 * This value is returned whenever operator[] fails, i.e. there is no 
 * output value stored for a given input value.
 * 
 * @param d  Default value.
 ----------------------------------------------------------------------------*/
template <class I, class O>
void
goHashTable<I,O>::setDefault (const O& d)
{
    this->dummy = d;
}

template <class I, class O>
goUInt32
goHashTable<I,O>::getModValue () const
{ 
    return this->modValue; 
}

template <class I, class O>
void goHashTable<I,O>::clear ()
{
    this->setModValue (this->getModValue());
}

template <class I, class O>
O&
goHashTable<I,O>::operator[] (const I& in) 
{
    goUInt32	tmp = (in & modValue);   // use the last n bits for accessing the list
    goList<void*> *list = 0;
    list = (goList<void*>*)theTable[tmp];
    if (!list->isEmpty()) 
    {
        list->resetToFront();
        bool endFlag = false;
        do 
        {
            endFlag = list->isTail();
            if ( ((goHashEntry<I,O>*)list->getCurrent())->key == in) 
            {
                lastFailed = false;
                return (O&)( ((goHashEntry<I,O>*)list->getCurrent())->value);
            } else 
            {
                list->getNext();
            }
        } while (!endFlag);
    }
    lastFailed = true;
    return dummy;
}

//template <class I, class O>
//void
//goHashTable<I,O>::add (I key, O value) {
//    //    cout << "hashtable adding " << key << "," << value << endl;
//  I tmp = (key & modValue);
//  (*this)[key] = value;
//  if (fail()) {
//      // cout << "...adding new entry." << endl;
//    goHashEntry<I,O> *entry = new goHashEntry<I,O>();
//    entry->key = key;
//    entry->value = value;
//    addEntry (tmp, (void*)entry);
//  }
//  lastFailed = false;
//}

template <class I, class O>
void
goHashTable<I,O>::add (const I& key, const O& value) {
    //    cout << "hashtable adding " << key << "," << value << endl;
  goUInt32 tmp = (key & modValue);
  O* old = &(*this)[key];
  if (fail()) {
      // cout << "...adding new entry." << endl;
    goHashEntry<I,O> *entry = new goHashEntry<I,O>();
    entry->key = key;
    entry->value = value;
    addEntry (tmp, (void*)entry);
  }
  else
  {
      *old = value;
  }
  lastFailed = false;
}

template <class I, class O>
void
goHashTable<I,O>::addEntry (goUInt32 table, void* e)
{
    ((goList<void*>*)theTable[table])->append (e);
}

template <class I, class O>
O
goHashTable<I,O>::remove (const I& key)
{
    goUInt32 tmp = (key & modValue);   // use the last n bits for accessing the list
    goList<void*>	*list = 0;

    lastFailed = false;
    //   if (tmp > (unsigned)theTable.getSize()) {
    //     lastFailed = true;
    //   }
    list = (goList<void*>*)theTable[tmp];
    O retval = 0;
    if (!list->isEmpty()) 
    {
        list->resetToFront();
        bool endFlag = false;
        do 
        {
            endFlag = list->isTail();
            if ( ((goHashEntry<I,O>*)list->getCurrent())->key == key) 
            {
                retval = ((goHashEntry<I,O>*)list->getCurrent())->value;
                delete ((goHashEntry<I,O>*)list->getCurrent());
                list->remove();
                return retval;
            }
            list->next();
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

template <class I, class O>
goArray<void*>& goHashTable<I,O>::getLists ()
{
    return theTable;
}

