#include <gocomplex.h>

std::ostream& operator<< (std::ostream& o, const goComplex<double>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}

std::ostream& operator<< (std::ostream& o, const goComplex<float>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}
