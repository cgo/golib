#ifndef GODWT_H
#define GODWT_H

#include <gotypes.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>

/*!
 * \addtogroup signal
 * @{
 */
/*!
 * Type for transformed data.
 * Using goDouble for this takes double the amount of memory than goFloat.
 * The goDouble Haar transforms are about 18 percent slower than the goFloat operations.
 * There is, however, a loss in precision when using goFloat.
 */
typedef goDouble godwt_t;

/*!
 * DWT class for 3d signals (separable, so it's for all signals dim < 4)
 * @author Christian Gosch 
 * @todo src/signal/godwt.cc aufraeumen
 */
template<class T>
class
goDWT
{
 public:
  goDWT();
  virtual ~goDWT();

  /*!
   * Non-specialised Haar transform. 
   * Uses goDouble values for the transform, 
   * @param signal The signal to be transformed.
   * @param targetSignal The signal where the dwt is to be stored in.
   * targetSignal has to be made using <code>goSignal3D::make()</code> in the 
   * correct size.
   * @see goDWT::unHaar()
   */
  void haar (goSignal3D<T>& signal, goSignal3D<godwt_t>& targetSignal);
  int  haar (goSignal3D<T>& signal, goSignal3D<godwt_t>& targetSignal, int stage);

  /*!
   * In place version of the Haar transform. There are special implementations
   * for <code>goFloat</code>, <code>goDouble</code>, ... <br>
   * Note that this can only be used for types allowing for the
   * type of the transform to be the same as the signal itself.
   * Currently, this is possible for goFloat and goDouble.
   * @param signal Contains the signal to be transformed and after execution,
   * contains the transform of the signal.
   */
  void haar (goSubSignal3D<T>& signal);

  /*!
   * In place version of the Haar transform with <code>stage</code> stages.
   * This is implemented for the types
   *  - goInt8
   *  - goUInt8
   *  - goInt16
   *  - goUInt16
   *  - goInt32
   *  - goUInt32
   *  - goFloat
   *  - goDouble
   * The integer type transform is realised not as a "real" Haar transform, but as a
   * modified S-Transform as described in 
   * B. Aiazzi et al., "Lossless compression of medical images based on an enhanced generalized
   * multidimensional S-Transform", SPIE Vol. 2825, pp. 868--878.
   * \par
   * @param signal Contains the signal to be transformed and after execution,
   * contains the transform of the signal.
   * @param stage Stage up to which the transform shall be executed.
   */
  int haar (goSignal3D<T>& signal, int stage);
  
  /*!
   * Non-specialised Haar reverse transform.
   * Pretty slow due to a copy operation at the end.
   * \todo Remove the copy operation at the end by specialising a macro
   * to the last filter operation. Using godwt_t types for all data is better anyway
   * as long as it is scalar data. Use a different solution for multimodal data.
   * @param haarSignal Signal containing the Haar transform as produced by <code>haar()</code>
   * @param targetSignal Signal where the reverse transform is to be stored in.
   * Again, this has to be created before calling this member.
   * @see goDWT::haar()
   */
  void unHaar (goSignal3D<godwt_t>& haarSignal, goSignal3D<T>& targetSignal);

  /*!
   * In place version of the reverse Haar transform. There are special implementations
   * for <code>goFloat</code>, ...
   */
  void unHaar (goSubSignal3D<T>& haarSignal);

  /*!
   * @see haar(goSignal3D, int);
   */
  int  unHaar (goSignal3D<T>& signal, int stage); 
  
 protected:

};
/*! @} */
/*!
 * \example dwt.cc
 * This is an example and test program for the goDWT class.
 * @author Christian Gosch
 */

#endif
