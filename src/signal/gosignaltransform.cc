/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignaltransform.h>
#include <gocomplex.h>
#include <math.h>


template <class T>
goSignalTransform<T>::goSignalTransform () : goSignal<T> () {
  transform = new goSignal<goComplex<goDouble> >;
  info.last_failed	= false;
  info.transformValid	= false;
}

template <class T>
goSignalTransform<T>::~goSignalTransform () {
  if (transform) {
    delete transform;
  }
}

template <class T>
inline
void*
goSignalTransform<T>::getTransform () {
  if (info.transformValid) {
    info.last_failed = false;
  } else { info.last_failed = true; }
  return (void*)transform;
}

template <class T>
bool
goSignalTransform<T>::dft () {
  goIndex_t i = 0, k = 0;
  
  // Define the phase factor
  goDouble alpha  = 2 * M_PI / (float)getSize();
  // goDouble factor = 0;

  if (!info.transformValid) {
    transform->resize (getSize());
    // This works for real signals (NOT goComplex)
    for (i = 0; i < getSize(); i++) {
      (*transform)[i].im() = 0;
      (*transform)[i].re() = 0;
      for (k = 0; k < getSize(); k++) {
	(*transform)[i].re() += (*this)[k] * cos (alpha * i * (k - offset));
	(*transform)[i].im() -= (*this)[k] * sin (alpha * i * (k - offset));
      }
    }
    // Still have to overload methods from parent classes to reset flag
    // info.transformValid = true;
  }
  info.last_failed = false;
  return true;
}


/* Instantiation */
template class goSignalTransform<goDouble>;

















