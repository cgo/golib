#include <goheappair.h>
#include <gotypes.h>

template <class T, class T2>
goHeapPair<T,T2>::goHeapPair() {
  this->size = 0;
  dummy = new T;
  dummy2 = new T2;
}

template <class T, class T2>
goHeapPair<T,T2>::goHeapPair(int sz) {
  this->size = sz;
  tree.resize(sz);
  tree2.resize (sz);
  dummy = new T;
  dummy2 = new T2;
}

template <class T, class T2>
goHeapPair<T,T2>::~goHeapPair() {
  delete dummy;
  delete dummy2;
}

template <class T, class T2>
bool
goHeapPair<T,T2>::set(int pos,const T& item, const T2& item2) {
  if (pos < size) {
    int realpos = size - pos - 1;
    tree[realpos] = item;
    tree2[realpos] = item2;
    return true;
  }
  return false;
}

template <class T, class T2>
bool 
goHeapPair<T,T2>::set(int length,int pos,const T* items,const T2* items2) {
  if ((pos < size) && ((size - pos) <= length)) {
    for (int i=pos; i < (pos+length); i++) {
      /* this->set(i,items[i-pos]); */
      tree[size - i - 1] = items[i-pos];
      tree2[size - i - 1] = items2[i-pos];
    }
    return true;
  } 
  return false;
}

template <class T, class T2>
const T&
goHeapPair<T,T2>::getFirst(int pos) {
  if (pos < size) {
    return tree[size - pos - 1];
  }
  return *dummy;
}

template <class T, class T2>
const T2&
goHeapPair<T,T2>::getSecond(int pos) {
  if (pos < size) {
    return tree2[size - pos - 1];
  }
  return *dummy2;
}

template <class T, class T2>
void
goHeapPair<T,T2>::heapify(void) {
}
  
template <class T, class T2>
bool
goHeapPair<T,T2>::sift(int pos) {
  int leftson = (pos << 1) + 1;
  int rightson = leftson + 1;
  T temp;
  T2 temp2;

  /* don't sift any more: */
  if (pos >= (size >> 1)) {
    return false;
  }
  int realpos = size - pos - 1;
  leftson = size - leftson - 1;
  rightson = size - rightson - 1;
  /* last node with only one left son */
  if (size <= rightson) {
    if (tree2[realpos] < tree2[leftson]) {
      temp = tree[realpos];
      tree[realpos] = tree[leftson];
      tree[leftson] = temp;
      temp2 = tree2[realpos];
      tree2[realpos] = tree2[leftson];
      tree2[leftson] = temp2;

      return true;
    }
    else return false;
  }

  /* sift down left or right */
  if (tree2[leftson] > tree2[rightson]) {
    if (tree2[realpos] < tree2[leftson]) {
      temp = tree[realpos];
      tree[realpos] = tree[leftson];
      tree[leftson] = temp;
      temp2 = tree2[realpos];
      tree2[realpos] = tree2[leftson];
      tree2[leftson] = temp2;
      sift (leftson);
      return true;
    }
    else return false;
  }
  else
    if (tree2[realpos] < tree2[rightson]) {
      temp = tree[realpos];
      tree[realpos] = tree[rightson];
      tree[rightson] = temp;
      temp2 = tree2[realpos];
      tree2[realpos] = tree2[rightson];
      tree2[rightson] = temp2;
      sift (rightson);
      return true;
    }
    else return false;
}  

template <class T, class T2>
void
goHeapPair<T,T2>::buildHeap(int pos) {
  int i;
  bool sifted;
  do {
    sifted = false;
    cout << "pos = " << pos << ", size >> 1 = " << (size >> 1) << endl;
    for (i=pos; i < (size >> 1); i++) {
      if (this->sift(i)) { 
	sifted = true;
      }
    }
  }
  while (sifted);
}

template <class T, class T2>
bool
goHeapPair<T,T2>::resize(int sz) {
  size = sz;
  cout << "Resizing heap to " << sz << endl;
  tree.resize(sz);
  tree2.resize(sz);
  return true;
}

/* instantiation */
template class goHeapPair<goInt32,goUInt32>;
template class goHeapPair<goInt32,goFloat>;




