/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GOFILTERFIR_H
#define __GOFILTERFIR_H

#include <gotypes.h>
#include <gofilter.h>
#include <gosignal2d.h>
#include <gothread.h>

class
goFilterFIRInfo {
public:
  int type;
  void* filterPtr;
};

/*
 * DEPRECATED
 *
 * FIR filter. <br>
 */
template <class T>
class goFilterFIR : public goFilter<T> {
public:
  goFilterFIR (goSize_t maskWidth = 3, goSize_t maskHeight = 3);
  virtual ~goFilterFIR ();
  
  void setMask (goDouble *maskData);

  void setCenter (goIndex_t x, goIndex_t y);

  /// First implementation, slow, needs optimization !
  // step: only every step^th value is stored in target. Use for downsampling.
  // STEP IS NOT IMPLEMENTED YET !
  void filter (goSignal2D<T>& s, goSignal2D<T>& target, goInt32 step = 1);

  void setFilterInfo (goFilterFIRInfo &info);
  void filterThread (goInt32 step = 1);  

protected:
  goArray<goDouble> mask;
  goSize_t sizeX;
  goSize_t sizeY;

  goIndex_t centerX;
  goIndex_t centerY;

  goThread threads;
  goSignal2D<T> *s_ptr;
  goSignal2D<T> *target_ptr;
};


#endif



