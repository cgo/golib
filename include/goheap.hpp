
template <class T,class KEY_T>
goHeap<T,KEY_T>::goHeap() {
  this->size = 0;
  dummy = new T;
}

template <class T,class KEY_T>
goHeap<T,KEY_T>::goHeap(int sz) {
  this->size = sz;
  tree.resize(sz);
  dummy = new T;
}

template <class T,class KEY_T>
goHeap<T,KEY_T>::~goHeap() {
  delete dummy;
}

template <class T,class KEY_T>
bool
goHeap<T,KEY_T>::set(int pos, T item, KEY_T key) {
  if (pos < size) {
      valueTree[pos] = item;
      tree[pos]	     = key;
      return true;
  }
  return false;
}

template <class T,class KEY_T>
bool 
goHeap<T,KEY_T>::set(int length,int pos,const T* items, const KEY_T* keys) {
  if ((pos < size) && ((size - pos) <= length)) {
    for (int i=pos; i < (pos+length); i++) {
      /* this->set(i,items[i-pos]); */
      valueTree[i] = items[i-pos];
      tree[i]	   = keys[i-pos];
    }
    return true;
  } 
  return false;
}

template <class T,class KEY_T>
T
goHeap<T,KEY_T>::get(int pos) {
    if (pos < size) {
	return valueTree[pos];
    }
    return *dummy;
}

template <class T,class KEY_T>
KEY_T
goHeap<T,KEY_T>::getKey (int pos) {
    if (pos < size)
	{
	    return tree[pos];
	}
    return 0;
}

template <class T,class KEY_T>
void
goHeap<T,KEY_T>::heapify(void) {
}
  
template <class T,class KEY_T>
bool
goHeap<T,KEY_T>::sift(int pos) {
  int leftson = 2*pos+1;
  int rightson = 2*pos+2;
  KEY_T temp;
  T     tempValue;
  /* don't sift any more: */
  if (pos >= (size >> 1)) {
    return false;
  }
  /* last node with only one left son */
  if (size <= rightson) {
    if (tree[pos] < tree[leftson]) {
      temp = tree[pos];
      tree[pos] = tree[leftson];
      tree[leftson] = temp;

      tempValue = valueTree[pos];
      valueTree[pos] = valueTree[leftson];
      valueTree[leftson] = tempValue;
      return true;
    }
    else return false;
  }

  /* sift down left or right */
  if (tree[leftson] > tree[rightson]) {
    if (tree[pos] < tree[leftson]) {
      temp = tree[pos];
      tree[pos] = tree[leftson];
      tree[leftson] = temp;

      tempValue = valueTree[pos];
      valueTree[pos] = valueTree[leftson];
      valueTree[leftson] = tempValue;

      sift (leftson);
      return true;
    }
    else return false;
  }
  else
    if (tree[pos] < tree[rightson]) {
      temp = tree[pos];
      tree[pos] = tree[rightson];
      tree[rightson] = temp;

      tempValue = valueTree[pos];
      valueTree[pos] = valueTree[rightson];
      valueTree[rightson] = tempValue;

      sift (rightson);
      return true;
    }
    else return false;
}  

template <class T,class KEY_T>
void
goHeap<T,KEY_T>::buildHeap(void) {
  int i;
  bool sifted;
  do {
    sifted = false;
    for (i=0; i < (size >> 1); i++) {
      if (this->sift(i)) { 
	sifted = true;
      }
    }
  }
  while (sifted);
}

template <class T,class KEY_T>
bool
goHeap<T,KEY_T>::resize(int sz) {
  size = sz;
  tree.resize(sz);
  valueTree.resize(sz);
  return true;
}
