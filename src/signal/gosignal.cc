/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal.h>
#include <gocomplex.h>

template <class T>
goSignal<T>::goSignal () : goNVector<T> () {
  timeStep = 1;
  last_failed = false;
  info.energyValid = false;
  info.powerValid = false;
  offset = 0;
}

template <class T>
goSignal<T>::~goSignal () {
  
}

template <class T>
goDouble
goSignal<T>::energy () {
  goIndex_t	i = 0;
  goDouble	retval = 0;
  
  if (info.energyValid) {
    return info.energy;
  }
  retval = 0;
  for (i = 0; i < getSize(); i++) {
    retval += (*this)[i] * (*this)[i];
  }
  info.energyValid = true;
  info.energy = retval;
  return retval;
}

goDouble
goSignal<goComplex<goDouble> >::energy () {
  goIndex_t	i = 0;
  goDouble	retval = 0;
  
  if (info.energyValid) {
    return info.energy;
  }
  retval = 0;
  for (i = 0; i < getSize(); i++) {
    retval += ((*this)[i] * (*this)[i]).abs();
  }
  info.energyValid = true;
  info.energy = retval;
  return retval;
}

template <class T>
goDouble
goSignal<T>::power () {
  return 0;
}

template <class T>
bool
goSignal<T>::setTimeStep (goTime_t t) {
  timeStep = t;
  return true;
}

template <class T>
goTime_t
goSignal<T>::getTimeStep () {
  return timeStep;
}

template <class T>
bool
goSignal<T>::zeroPadding (goSize_t s) {
  goIndex_t i = 0;
  resize (getSize() + s);
  for (i = getSize()-s; i < getSize(); i++) {
    (*this)[i] = 0;
  }
  return true;
}

bool
goSignal<goComplex<goDouble> >::zeroPadding (goSize_t s) {
  goIndex_t i = 0;
  resize (getSize() + s);
  for (i = 0; i < getSize(); i++) {
    (*this)[i].re() = 0;
    (*this)[i].im() = 0;
  }
  return true;
}

template <class T>
void
goSignal<T>::setOffset (goInt32 o) {
  offset = o;
}

template <class T>
goInt32
goSignal<T>::getOffset () {
  return offset;
}


/* Instantiation */
template class goSignal<goDouble>;
template class goSignal<goComplex<goDouble> >;















