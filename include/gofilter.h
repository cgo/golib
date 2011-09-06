/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GOFILTER_H
#define __GOFILTER_H

#include <gotypes.h>
#include <gosignal2d.h>

/*!
 * \brief Filter class.
 *
 * \todo This works on the old 2D classes. Rework to signal3d.
 */
template <class T>
class goFilter {
 public:
  goFilter ();
  virtual ~goFilter ();

  /* deprecated */
  void setDataPtr (T* ptr) { data = ptr; }

  virtual void filter ();
  virtual void filter (goSignal2D<T>& s);

 protected:
  T* data;
  goSize_t sizeX;
  goSize_t sizeY;
};


#endif
