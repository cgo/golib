#include <gointegral.h>
#include <gotypes.h>

template <class T>
goIntegral<T>::goIntegral () {
  status.value_valid	= false;
  value			= 0;
  type			= GO_INTEGRAL_TRAPEZ;
  function		= 0;
}

template <class T>
goIntegral<T>::~goIntegral () {
  if (function && delete_function) {
    delete function;
  }
}

template <class T>
void
goIntegral<T>::setFunction (goArray<T>& v, T dist) {
  if ((!function) || (!delete_function)) {
    function = new goArray<T>;
    delete_function = true;
  }
  *function = v;
  distance  = dist;
  status.value_valid = false;
}

template <class T>
void
goIntegral<T>::setFunctionPtr (goArray<T>* v) {
  if (function) {
    delete function;
  }
  function = v;
  delete_function = false;
  status.value_valid = false;
}

template <class T>
T
goIntegral<T>::eval () {
  goIndex_t i = 0;
  switch (type) {
  case GO_INTEGRAL_TRAPEZ:
    value = 0;
    for ( i = 1; i < (function->getSize()-1); i++) {
      value+= (*function)[i];
    }
    value+= ((*function)[0] + (*function)[function->getSize()-1]) > 1;
    value*= distance;
    status.value_valid = true;
  }
  return (T) value;
}

/* Instantiation */
template class goIntegral<double>;
template class goIntegral<goFloat>;
template class goIntegral<goInt8>;
template class goIntegral<goInt16>;
template class goIntegral<goInt32>;
