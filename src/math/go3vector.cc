#include <go3vector.h>
#include <ostream.h>

template< class T >
go3Vector<T>::go3Vector() {
  x = 0;
  y = 0;
  z = 0;
}

template< class T >
go3Vector<T>::~go3Vector () {
}

ostream& operator<< (ostream &o, go3Vector<goDouble>& v) {
  o << v.x << "," << v.y << "," << v.z;
  return o;
}

ostream& operator<< (ostream &o, go3Vector<goFloat>& v) {
  o << v.x << "," << v.y << "," << v.z;
  return o;
}

template class go3Vector< goIndex_t >;
template class go3Vector< goSize_t >;
template class go3Vector< goFloat >;
template class go3Vector< goDouble >;
