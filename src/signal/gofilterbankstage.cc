#include <gofilterbankstage.h>
#include <goresample.h>
#include <math.h>

static goDouble Sinc (goDouble d) {
  if (d == 0) return 1;
  return (goDouble)( sin(M_PI / 2.0f * d) / (float)(M_PI / 2.0f * d) );
}

static goDouble* makeSincMask (int n) {
  goIndex_t x,y,i;
  goInt32 s = (goInt32)((2*n+1)*(2*n+1)) + 1;
  goDouble *m = new goDouble[s];
  for (i = 0; i < s; i++) {
    m[i] = 0;
  }
  i = 0;
  for (y = -n; y <= n; y++) {
    for (x = -n; x <= n; x++) {
      m[i] = Sinc (sqrt ((float)(x*x) + (float)(y*y)));
      i++;
    }
  }
  goDouble f = 0;
  for (i = 0; i < s; i++) {
    f += fabs(m[i]);
  }
  for (i = 0; i < s; i++) {
    m[i] /= f;
  }
  return m;
}

static goDouble* makeSincMaskComplement (int n) {
  goIndex_t x,y,i;
  goInt32 s = (goInt32)((2*n+1)*(2*n+1)) + 1;
  goDouble *m = new goDouble[s];
  for (i = 0; i < s; i++) {
    m[i] = 0;
  }
  i = 0;
  goDouble temp;
  for (y = -n; y <= n; y++) {
    for (x = -n; x <= n; x++) {
      temp = sqrt ((float)(x*x) + (float)(y*y));
      m[i] = Sinc (temp);
      if ((int)temp%2 != 0) {
	m[i] *= -1;
      }
      i++;
    }
  }
  goDouble f = 0;
  for (i = 0; i < s; i++) {
    f += fabs(m[i]);
  }
  for (i = 0; i < s; i++) {
    m[i] /= f;
  }
  return m;
}




template <class T>
goFilterBankStage<T>::goFilterBankStage () {
  LL = 0;
  LH = 0;
  HH = 0;
  HL = 0;
  
  loPassH = new goFilterFIR<T>(4, 1);
  hiPassH = new goFilterFIR<T>(4, 1);
  loPassV = new goFilterFIR<T>(1, 4);
  hiPassV = new goFilterFIR<T>(1, 4);

  loPassH->setCenter (2,0);
  hiPassH->setCenter (2,0);
  loPassV->setCenter (0,2);
  hiPassV->setCenter (0,2);

  
  /********
   * Set filter coefficients, hiPass = complement(loPass).
   ********/
  
  goDouble hiMaskD4[]   = {0.12940952255126, 0.22414386804201, -0.83651630373781, 0.48296291314453}; // D4
  goDouble loMaskD4[]   = {0.48296291314453, 0.83651630373781, 0.22414386804201, -0.12940952255126}; // D4
  goDouble loMaskHaar[] = {1,1}; //Haar = unskalierte Daubechies 2
  goDouble hiMaskHaar[] = {1,-1}; //Haar = unskalierte Daubechies 2

  goDouble *loMask = &loMaskD4[0];
  goDouble *hiMask = &hiMaskD4[0];
  //goDouble *loMask = &loMaskHaar[0];
  //goDouble *hiMask = &hiMaskHaar[0];

  goDouble f = 0, f2 = 0;
  goIndex_t i;
  goIndex_t s = 2;
  for (i = 0; i < s; i++) {
    f += fabs(loMask[i]);
    f2 += fabs(hiMask[i]);
  }

  // f = 1 / (4 * sqrt(2));
  //f = (goDouble) (1 / (goDouble)sqrt(2.0f));
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
goFilterBankStage<T>::~goFilterBankStage () {
}

template <class T>
void
goFilterBankStage<T>::filter (goSignal2D<T>& in) {
  if (LL)
    delete LL;
  if (LH)
    delete LH;
  if (HH)
    delete HH;
  if (HL)
    delete HL;
  
  LL = new goSignal2D<T>(in.getSizeX() >> 1, 
			 in.getSizeY() >> 1);
  LH = new goSignal2D<T>(in.getSizeX() >> 1, 
			 in.getSizeY() >> 1);
  HH = new goSignal2D<T>(in.getSizeX() >> 1, 
			 in.getSizeY() >> 1);
  HL = new goSignal2D<T>(in.getSizeX() >> 1, 
			 in.getSizeY() >> 1);


//    loSignal2D = new goSignal2D<T>(in.getSizeX(), 
//  				 in.getSizeY());
//    hiSignal2D = new goSignal2D<T>(in.getSizeX(), 
//  				 in.getSizeY());

  goSignal2D<T> tmp1(in.getSizeX(), in.getSizeY());
  goSignal2D<T> tmp2(in.getSizeX(), in.getSizeY());
  goResample<T> resample;
  resample.setM(2,2);
  /**********
   * Start hi and loPass filter and store results.
   **********/

  tmp1.fill (0);
  tmp2.fill (0);
  LL->fill (0);
  LH->fill (0);
  HH->fill (0);
  HL->fill (0);
  // in.interpolateBorders (false);
  in.interpolateBordersPeriodic ();
  loPassH->filter (in, tmp1);
  // tmp1.interpolateBorders (false);
  tmp1.interpolateBordersPeriodic ();
  loPassV->filter (tmp1, tmp2);
  resample.down (tmp2, *LL);

  tmp2.fill (0);
  hiPassV->filter (tmp1, tmp2);
  resample.down (tmp2, *LH);
  
  tmp1.fill (0);
  tmp2.fill (0);
  hiPassH->filter (in, tmp1);
  tmp1.interpolateBordersPeriodic ();
  hiPassV->filter (tmp1, tmp2);
  resample.down (tmp2, *HH);

  tmp2.fill (0);
  loPassV->filter (tmp1, tmp2);
  resample.down (tmp2, *HL);

}


template class goFilterBankStage<goDouble>;
template class goFilterBankStage<goFloat>;
template class goFilterBankStage<goInt32>;

