#include <gofilterbankstagereverse.h>
#include <goresample.h>

template <class T>
goFilterBankStageReverse<T>::goFilterBankStageReverse (goFilterBankStage<T>* fwStage) {
//    hiPassH = fwStage->getHiPassH ();
//    loPassH = fwStage->getLoPassH ();
//    hiPassV = fwStage->getHiPassV ();
//    loPassV = fwStage->getLoPassV ();
  signal  = new goSignal2D<T>( fwStage->getLL()->getSizeX() << 1,
			       fwStage->getLL()->getSizeY() << 1);
  forwardStage = fwStage;

  loPassH = new goFilterFIR<T>(4, 1);
  hiPassH = new goFilterFIR<T>(4, 1);
  loPassV = new goFilterFIR<T>(1, 4);
  hiPassV = new goFilterFIR<T>(1, 4);

  loPassH->setCenter (1,0);
  hiPassH->setCenter (1,0);
  loPassV->setCenter (0,1);
  hiPassV->setCenter (0,1);
  
  /********
   * Set filter coefficients, hiPass = complement(loPass).
   ********/
  
  goDouble loMaskD4[]   = {-0.12940952255126, 0.22414386804201, 0.83651630373781, 0.48296291314453}; // D4
  goDouble hiMaskD4[]   = {0.48296291314453, -0.83651630373781, 0.22414386804201, 0.12940952255126}; // D4
  goDouble loMaskHaar[] = {1,1};
  goDouble hiMaskHaar[] = {-1,1};

  goDouble *loMask = &loMaskD4[0];
  goDouble *hiMask = &hiMaskD4[0];
  //goDouble *loMask = &loMaskHaar[0];
  //goDouble *hiMask = &hiMaskHaar[0];

  goDouble f = 0, f2 = 0;
  goIndex_t i;
  goIndex_t s = 4;
  for (i = 0; i < s; i++) {
    f += fabs(loMask[i]);
    f2 += fabs(hiMask[i]);
  }

  // f = 1 / (4 * sqrt(2));
  //f = (goDouble)(1 / (goDouble)sqrt(2.0f));
  // f = 0.5;
  // f = sqrt(2);
  f = 1;

  for (i = 0; i < s; i++) {
    loMask[i] *= f;
    hiMask[i] *= f;
  }

//    goDouble *loMask = 0;
//    goDouble *hiMask = 0;
//    loMask = makeSincMask (3);
//    hiMask = makeSincMaskComplement (3);

  loPassH->setMask (&loMask[0]);
  hiPassH->setMask (&hiMask[0]);
  loPassV->setMask (&loMask[0]);
  hiPassV->setMask (&hiMask[0]);

}

template <class T>
goFilterBankStageReverse<T>::~goFilterBankStageReverse () {
  if (signal) {
    delete signal;
  }
}

template <class T>
void
goFilterBankStageReverse<T>::filter (goSignal2D<T>& LL, goSignal2D<T>& LH,
				     goSignal2D<T>& HH, goSignal2D<T>& HL) {
  goResample<T> resample(2,2);

  goSignal2D<T> temp1 (signal->getSizeX(),
		       signal->getSizeY());
  goSignal2D<T> temp2 (signal->getSizeX(),
		       signal->getSizeY());
  goSignal2D<T> tempFilt (signal->getSizeX(),
			  signal->getSizeY());

  goSignal2D<T> LLup (signal->getSizeX(),
		      signal->getSizeY());
  goSignal2D<T> LHup (signal->getSizeX(),
		      signal->getSizeY());
  goSignal2D<T> HHup (signal->getSizeX(),
		      signal->getSizeY());


  resample.up (LL, temp1);
  // temp1.interpolateBorders (0,1,-1,-1);
  temp1.interpolateBordersPeriodic ();
  loPassH->filter (temp1, temp2);
  // temp2.interpolateBorders (-1,-1,0,1);
  temp2.interpolateBordersPeriodic ();
  loPassV->filter (temp2, LLup);

  temp1.fill (0);
  resample.up (LH, temp1);
  // temp1.interpolateBorders (0,1,-1,-1);  
  temp1.interpolateBordersPeriodic ();
  loPassH->filter (temp1, temp2);
  // temp2.interpolateBorders (-1,-1,0,1);
  temp2.interpolateBordersPeriodic ();
  hiPassV->filter (temp2, LHup);

  temp1.fill (0);
  resample.up (HH, temp1);
  // temp1.interpolateBorders (0,1,-1,-1);  
  temp1.interpolateBordersPeriodic ();
  hiPassH->filter (temp1, temp2);
  // temp2.interpolateBorders (-1,-1,0,1);
  temp2.interpolateBordersPeriodic ();
  hiPassV->filter (temp2, HHup);

  temp1.fill (0);
  signal->fill (0);
  resample.up (HL, temp1);
  // temp1.interpolateBorders (0,1,-1,-1);  
  temp1.interpolateBordersPeriodic ();
  hiPassH->filter (temp1, temp2);
  // temp2.interpolateBorders (-1,-1,0,1);
  temp2.interpolateBordersPeriodic ();
  loPassV->filter (temp2, *signal);

  *signal += HHup;
  *signal += LHup;
  *signal += LLup;
}


template class goFilterBankStageReverse<goInt32>;
template class goFilterBankStageReverse<goFloat>;
template class goFilterBankStageReverse<goDouble>;
