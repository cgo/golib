/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOFILTERBANKREVERSE_H
#define GOFILTERBANKREVERSE_H

#include <gofilterbank.h>
#include <gofilterbankstagereverse.h>

/*!
 * \todo Make it work with signal3d.
 *
 * 2D reconstruction filterbank using <CODE>goFilterBank</CODE> to reconstruct the original
 * signal.
 */
template <class T>
class
goFilterBankReverse {
 public:
  goFilterBankReverse (goFilterBank<T>* fb);
  virtual ~goFilterBankReverse ();

  bool	filter ();

  goFilterBankStageReverse<T>& getStage (goIndex_t idx) { return (*(goFilterBankStageReverse<T>*)stages[idx]); }
 protected:
  goFilterBank<T>	*forwardBank;
  goSignal2D<T>		*output;

  /// contains pointers to <CODE>goFilterBankStageReverse<T></CODE>
  goArray<void*>	stages;
};


#endif
