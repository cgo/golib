#include <go3vector.h>
#include <ostream.h>

template< class T >
go3Vector<T>::go3Vector() {
  x = 0;
  y = 0;
  z = 0;
}

template< class T >
go3Vector<T>::go3Vector(const go3Vector<T>& other) {
  x = other.x;
  y = other.y;
  z = other.z;
}

template< class T >
go3Vector<T>::go3Vector(T _x, T _y, T _z) {
  x = _x;
  y = _y;
  z = _z;
}

template< class T >
go3Vector<T>::~go3Vector () {
}

#if 0
ostream& operator<< (ostream &o, go3Vector<goDouble>& v) {
  o << v.x << "," << v.y << "," << v.z;
  return o;
}

ostream& operator<< (ostream &o, go3Vector<goFloat>& v) {
  o << v.x << "," << v.y << "," << v.z;
  return o;
}
#endif

template class go3Vector< goIndex_t >;
template class go3Vector< goSize_t >;
template class go3Vector< goFloat >;
template class go3Vector< goDouble >;
