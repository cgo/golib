#ifndef GOFILTERBANKSTAGEREVERSE_H
#define GOFILTERBANKSTAGEREVERSE_H

#include <gofilterbankstage.h>
#include <gofilterfir.h>
#include <gotypes.h>

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
