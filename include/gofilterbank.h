#ifndef GOFILTERBANK_H
#define GOFILTERBANK_H

#include <gofilterbankstage.h>
#include <gotypes.h>
#include <goarray.h>

/*!
 * \brief 2D-Filterbank.
 * 
 * \todo Fix this to work with signal3d.
 * Filterbank, currently only for two dimensional signals. <br>
 * This is handy for playing with wavelet transforms. <br>
 * Uses an array of goFilterBankStage. This should be changed to using only one filter which is used
 * iteratively. It could be cool to implement a parallel version that keeps track of the available data in
 * the previous stage to start calculation as soon as enough data is available.
 * @author Christian Gosch
 * @see goFilterBankStage
 * @see goFilterFIR
 * @see goFilterBankReverse
 */
template <class T>
class goFilterBank {
 public:
  /// Creates a filter bank with num_stages stages.
  goFilterBank (goSize_t num_stages = 3);
  ///
  virtual ~goFilterBank ();

  /*!
   * Runs the filter bank on <CODE>in</CODE>.<br>
   * The result can be retrieved by getting the filtered signals from the stages with getStage().
   * @see goFilterBankStage
   */
  void filter (goSignal2D<T>& in);

  /*!
   * Fills the 2D signal target with the wavelet and scaling function coefficients of the wavelet transform
   * done by the filter bank. 
   */
  void makeWaveletSignal (goSignal2D<T>& target);


  /// @return Stage number idx.
  goFilterBankStage<T>& getStage (goIndex_t idx) { return *((goFilterBankStage<T>*)stages[idx]); }
  /// @return Pointer to the stages array (goArray<void*>).
  goArray<void*>* getStagesPtr () { return &stages; }
 protected:
  
  /// Array of goFilterBankStage<T>* .
  goArray<void*> stages;

};
#endif

