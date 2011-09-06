/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gofilterbankreverse.h>
#include <gofilterbankstagereverse.h>
#include <gotypes.h>

template <class T>
goFilterBankReverse<T>::goFilterBankReverse (goFilterBank<T>* fb) {
  forwardBank = fb;
  stages.resize (fb->getStagesPtr()->getSize());
  
  goIndex_t i;
  for (i = 0; i < stages.getSize(); i++) {
    ((goFilterBankStageReverse<T>*)stages[i]) = new goFilterBankStageReverse<T> (&forwardBank->getStage(i));
  }
}

template <class T>
goFilterBankReverse<T>::~goFilterBankReverse () {
  goIndex_t i;
  for (i = 0; i < stages.getSize(); i++) {
    delete ((goFilterBankStageReverse<T>*)stages[i]);
  }
  stages.resize (0);
}

template <class T>
bool
goFilterBankReverse<T>::filter () {
  goIndex_t i;
  goSignal2D<T>		*LL;
  goSignal2D<T>		*LH;
  goSignal2D<T>		*HH;
  goSignal2D<T>		*HL;

  LL = (forwardBank->getStage(stages.getSize() - 1).getLL());
  for (i = (stages.getSize() - 1); i >= 0; i--) {
    HH = (forwardBank->getStage(i).getHH());
    HL = (forwardBank->getStage(i).getHL());
    LH = (forwardBank->getStage(i).getLH());
    ((goFilterBankStageReverse<T>*)stages[i])->filter ((goSignal2D<T>&)*LL, (goSignal2D<T>&)*LH,
						       (goSignal2D<T>&)*HH, (goSignal2D<T>&)*HL);
    LL = ((goFilterBankStageReverse<T>*)stages[i])->getSignal();
    
    /* Is this right ??? */
    // *LL *= (T)(1 << (i - stages.getSize() + 1));
  }

  return true;
}


template class goFilterBankReverse<goInt32>;
template class goFilterBankReverse<goFloat>;
template class goFilterBankReverse<goDouble>;
