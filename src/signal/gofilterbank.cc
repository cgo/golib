/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gofilterbank.h>

template<class T>
goFilterBank<T>::goFilterBank (goSize_t num_stages) {
  stages.resize (num_stages);
  goSize_t i;
  for (i = 0; i < num_stages; i++) {
    stages[i] = (void*) (new goFilterBankStage<T>);
  }
}

template<class T>
goFilterBank<T>::~goFilterBank () {
  goIndex_t i;
  for (i = 0; i < stages.getSize(); i++) {
    delete (goFilterBankStage<T>*)stages[i];
  }
  stages.resize(0);
}

template<class T>
void
goFilterBank<T>::filter (goSignal2D<T>& in) {
  goIndex_t i;
  ((goFilterBankStage<T>*)stages[0])->filter (in);
  for (i = 1; i < stages.getSize(); i++) {
    ((goFilterBankStage<T>*)stages[i])->filter ( *((goFilterBankStage<T>*)stages[i-1])->getLL() );
    //in.resize (in.getSizeX() >> 1,
    //	       in.getSizeY() >> 1);
  }
}

template <class T>
void
goFilterBank<T>::makeWaveletSignal (goSignal2D<T>& target) {
  int startX, startY, endX, endY;
  
  goIndex_t i;
  goIndex_t num_stages = (goIndex_t)stages.getSize();

  endX = ((goFilterBankStage<T>*)stages[0])->getLL()->getSizeX();
  endY = ((goFilterBankStage<T>*)stages[0])->getLL()->getSizeY();
  startX = 0;
  startY = 0;
  goSignal2D<T> *stage;
  for (i = 0; i < num_stages; i++) {
    endX = ((goFilterBankStage<T>*)stages[i])->getLL()->getSizeX();
    endY = ((goFilterBankStage<T>*)stages[i])->getLL()->getSizeY();

    stage = ((goFilterBankStage<T>*)stages[i])->getLH();
    target.put (*stage, (goIndex_t)endX, (goIndex_t)startY);
    stage = ((goFilterBankStage<T>*)stages[i])->getHL();
    target.put (*stage, startX, endY);
    stage = ((goFilterBankStage<T>*)stages[i])->getHH();
    target.put (*stage, endX, endY);
    
  }
  stage = ((goFilterBankStage<T>*)stages[num_stages-1])->getLL();
  target.put (*stage, startX, startY);
  
}

template class goFilterBank<goDouble>;
template class goFilterBank<goFloat>;
template class goFilterBank<goInt32>;
