#include <go4vector.h>
#include <ostream>

std::ostream& operator<< (std::ostream &o, go4Vector<goDouble>& v) {
  o << v.x << "," << v.y << "," << v.z << "," << v.t;
  return o;
}

std::ostream& operator<< (std::ostream &o, go4Vector<goFloat>& v) {
  o << v.x << "," << v.y << "," << v.z << "," << v.t;
  return o;
}

template class go4Vector< goIndex_t >;
template class go4Vector< goSize_t >;
template class go4Vector< goFloat >;
template class go4Vector< goDouble >;
