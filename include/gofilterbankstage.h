/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOFILTERBANKSTAGE_H
#define GOFILTERBANKSTAGE_H

#include <gosignal2d.h>
#include <gofilterfir.h>

/*!
 * \todo Make it work with signal3d.
 *
 * Stage for a goFilterBank.<br>
 * Currently works only for goSignal2D. 
 */
template<class T>
class goFilterBankStage {
 public:
  ///
  goFilterBankStage ();
  ///
  virtual ~goFilterBankStage ();

  /*!
   * Runs the filter on <CODE>in</CODE>.
   */
  void filter (goSignal2D<T>& in);

  /// @return Low/Low part
  goSignal2D<T> *getLL() { return LL; }
  /// @return Low/High part
  goSignal2D<T> *getLH() { return LH; }
  /// @return High/High part
  goSignal2D<T> *getHH() { return HH; }
  /// @return High/Low part
  goSignal2D<T> *getHL() { return HL; }

  /// @return Horizontal low pass filter
  goFilterFIR<T>	*getLoPassH() { return loPassH; }
  /// @return Horizontal High pass filter
  goFilterFIR<T>	*getHiPassH() { return hiPassH; }
  /// @return Vertical low pass filter
  goFilterFIR<T>	*getLoPassV() { return loPassV; }
  /// @return Vertical high pass filter
  goFilterFIR<T>	*getHiPassV() { return hiPassV; }

 protected:
  goFilterFIR<T>	*loPassH;
  goFilterFIR<T>	*hiPassH;
  goFilterFIR<T>	*loPassV;
  goFilterFIR<T>	*hiPassV;
  goSignal2D<T>		*LL;
  goSignal2D<T>		*LH;
  goSignal2D<T>		*HH;
  goSignal2D<T>		*HL;
};

#endif
