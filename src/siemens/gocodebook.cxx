#include <gocodebook.h>

template< class T >
goCodeBook<T>::goCodeBook () {
  array.resize(0);
}

template< class T >
goCodeBook<T>::~goCodeBook () {
  array.resize(0);
}

template< class T >
T&
goCodeBook<T>::operator[] (goIndex_t idx) {
  return array[idx];
}

template< class T >
void
goCodeBook<T>::add (T word, goIndex_t position) {
  if (position >= array.getSize()) {
    array.resize ((goSize_t)position + 1);
  }
  array[position] = word;
}


template class goCodeBook< goInt32 >;
