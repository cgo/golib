#include <gotypes.h>
#include <go44matrix.h>
#include <go44matrix.hpp>
#include <stdlib.h>
#include <iostream.h>


//  template <class T>
//  go4Vector<T>
//  go44Matrix<T>::operator* (go4Vector<T>& v)
//  {
//    go4Vector<T> vr;
//    T* m = matrix;
//    vr.x = *(m) * v.x + *(++m) * v.y + *(++m) * v.z + *(++m) * v.t;
//    vr.y = *(++m) * v.x + *(++m) * v.y + *(++m) * v.z + *(++m) * v.t;
//    vr.z = *(++m) * v.x + *(++m) * v.y + *(++m) * v.z + *(++m) * v.t;
//    vr.t = *(++m) * v.x + *(++m) * v.y + *(++m) * v.z + *(++m) * v.t;
//    return vr;
//  }


ostream& operator<< (ostream& o, go44Matrix<goFloat>& mat) {
  goIndex_t i = 0, j = 0;
  for (i = 0; i < 4; i++) {
    for (j = 0; j < 4; j++) {
      o << mat.elem(i,j) << " ";
    }
    o << "\n";
  }
  o << "\n";
  return o;
}

ostream& operator<< (ostream& o, go44Matrix<goDouble>& mat) {
  goIndex_t i = 0, j = 0;
  for (i = 0; i < 4; i++) {
    for (j = 0; j < 4; j++) {
      o << mat.elem(i,j) << " ";
    }
    o << "\n";
  }
  o << "\n";
  return o;
}

ostream& operator<< (ostream& o, const go44Matrix<goDouble>& mat) {
  goIndex_t i = 0, j = 0;
  for (i = 0; i < 4; i++) {
    for (j = 0; j < 4; j++) {
      o << mat.elem(i,j) << " ";
    }
    o << "\n";
  }
  o << "\n";
  return o;
}

/* Instantiation */
//  template class go44Matrix<goInt8>;
//  template class go44Matrix<goInt16>;
template class go44Matrix<goInt32>;
template class go44Matrix<goFloat>;
template class go44Matrix<goDouble>;









