/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOFILTERBANKSTAGEREVERSE_H
#define GOFILTERBANKSTAGEREVERSE_H

#include <gofilterbankstage.h>
#include <gofilterfir.h>
#include <gotypes.h>

/**
 * @brief 2D filter bank stage (reverse).
 *
 * \todo Make it work with signal3d.
 **/
template <class T>
class
goFilterBankStageReverse {
 public:
  goFilterBankStageReverse (goFilterBankStage<T>* fwStage);
  virtual ~goFilterBankStageReverse ();

  /// This is VERY slow ... specialize the filter class to filter and add simultaneously ...  
  void filter (goSignal2D<T>& LL, goSignal2D<T>& LH,
	       goSignal2D<T>& HH, goSignal2D<T>& HL);
  
  goSignal2D<T> *getSignal() { return signal; }
  
 protected:
  goSignal2D<T>		*signal;
  goFilterFIR<T>	*hiPassH;
  goFilterFIR<T>	*loPassH;
  goFilterFIR<T>	*hiPassV;
  goFilterFIR<T>	*loPassV;

  goFilterBankStage<T>	*forwardStage;
};

#endif
