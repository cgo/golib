#include <go4vector.h>
#include <ostream.h>

template< class T >
go4Vector<T>::go4Vector() {
  x = 0;
  y = 0;
  z = 0;
  t = 0;
}

template< class T >
go4Vector<T>::~go4Vector () {
}

ostream& operator<< (ostream &o, go4Vector<goDouble>& v) {
  o << v.x << "," << v.y << "," << v.z << "," << v.t;
  return o;
}

ostream& operator<< (ostream &o, go4Vector<goFloat>& v) {
  o << v.x << "," << v.y << "," << v.z << "," << v.t;
  return o;
}

template class go4Vector< goIndex_t >;
template class go4Vector< goSize_t >;
template class go4Vector< goFloat >;
template class go4Vector< goDouble >;
