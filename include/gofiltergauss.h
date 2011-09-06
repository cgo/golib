/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GOFILTERGAUSS_H
#define __GOFILTERGAUSS_H

#include <gofilter.h>
#include <gotypes.h>

/*
 * DEPRECATED
 * Gauss filter. <br>
 * A simple Gauss filter class. <br>
 * Usage is very simple: make a goSignal2D object and a target object of the same type and size,
 * then call the filter member with the source and target signals as arguments.
 *
 * @note DEPRECATED
 * 
 * @author Christian Gosch
 * @see goSignal2D
 * @see goFilterFIR
 * @see goFilter
 */
template <class T>
class goFilterGauss : public goFilter<T> {
 public:
  goFilterGauss (goSize_t maskLength);
  virtual ~goFilterGauss ();

  /*!
   * Applies a Gauss filter to src.
   * The result is stored in src (!!), the horizontally filtered version is in target.
   */
  void filter (goSignal2D<T> &src, goSignal2D<T> &target);

 protected:
  goArray<goDouble> mask;
  goSize_t	    length;
};


#endif
