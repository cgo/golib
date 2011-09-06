/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOTRANSFERFUNCTION_H
#define GOTRANSFERFUNCTION_H

#include <gotypes.h>
#include <goarray.h>
#include <iostream>

template <class IN_T, class OUT_T>
class
goTransferFunction {
 public:
  goTransferFunction ();
  virtual ~goTransferFunction ();

  void initialise ();

  inline OUT_T operator[] (IN_T) const;

  /* DO  NOT USE */
  inline OUT_T get (IN_T in);

  void addSegment (IN_T in1, OUT_T out1, IN_T in2, OUT_T out2);

  inline void operator= (goTransferFunction<IN_T,OUT_T> &other);

  goArray<IN_T>& getMin() { return *min; }
  goArray<IN_T>& getMax() { return *max; }
  goArray<OUT_T>& getStart() { return *start; }
  goArray<goDouble>& getSlope() { return *slope; }

 protected:
  goArray<IN_T>* min;
  goArray<IN_T>* max;
  goArray<OUT_T>* start;
  goArray<goDouble>* slope;
};

template <class IN_T,class OUT_T>
inline
OUT_T
goTransferFunction<IN_T,OUT_T>::
operator[] (IN_T inVal) const {
  goSize_t i;
  IN_T *minPtr = min->getPtr();
  IN_T *maxPtr = max->getPtr();
  goSize_t iMax = min->getSize();
  for (i = 0; i < iMax; i++) 
  {
    if ( (inVal >= *minPtr) && (inVal <= *maxPtr) ) 
	{
      return (OUT_T) ( (*start)[i] + (OUT_T)((inVal - *minPtr) * (*slope)[i]) );
      // return value;
    }
    minPtr++;
    maxPtr++;
  }
  return (OUT_T)0;
}

template <class IN_T,class OUT_T>
inline 
OUT_T
goTransferFunction<IN_T,OUT_T>::
get (IN_T in) {
  OUT_T val;
  goIndex_t i;
  for (i = 0; i < min->getSize(); i++) {
    if ( (in >= (*min)[i]) && (in <= (*max)[i]) ) {
      val = (OUT_T) ((*start)[i] + (in - (*min)[i]) * (*slope)[i]);
      return val;
    }
  }
  return 0;
}

template <class IN_T,class OUT_T>
inline
void
goTransferFunction<IN_T,OUT_T>::
operator= (goTransferFunction<IN_T,OUT_T>& other) 
{
    *min = other.getMin();
    *max = other.getMax();
    *start = other.getStart();
    *slope = other.getSlope();
}


#endif
