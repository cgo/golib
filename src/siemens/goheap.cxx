#include <goheap.h>
#include <gotypes.h>

template <class T>
goHeap<T>::goHeap() {
  this->size = 0;
  dummy = new T;
}

template <class T>
goHeap<T>::goHeap(int sz) {
  this->size = sz;
  tree.resize(sz);
  dummy = new T;
}

template <class T>
goHeap<T>::~goHeap() {
  delete dummy;
}

template <class T>
bool
goHeap<T>::set(int pos,const T& item) {
  if (pos < size) {
    tree[pos] = item;
    return true;
  }
  return false;
}

template <class T>
bool 
goHeap<T>::set(int length,int pos,const T* items) {
  if ((pos < size) && ((size - pos) <= length)) {
    for (int i=pos; i < (pos+length); i++) {
      /* this->set(i,items[i-pos]); */
      tree[i] = items[i-pos];
    }
    return true;
  } 
  return false;
}

template <class T>
const T&
goHeap<T>::get(int pos) {
  if (pos < size) {
    return tree[pos];
  }
  return *dummy;
}

template <class T>
void
goHeap<T>::heapify(void) {
}
  
template <class T>
bool
goHeap<T>::sift(int pos) {
  int leftson = 2*pos+1;
  int rightson = 2*pos+2;
  T temp;

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
      sift (rightson);
      return true;
    }
    else return false;
}  

template <class T>
void
goHeap<T>::buildHeap(void) {
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

template <class T>
bool
goHeap<T>::resize(int sz) {
  size = sz;
  tree.resize(sz);
  return true;
}

/* instantiation */
template class goHeap<goInt32>;
template class goHeap<goFloat>;
template class goHeap<goUInt32>;



