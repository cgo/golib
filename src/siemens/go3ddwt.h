#ifndef __GO3DDWT_H__
#define __GO3DDWT_H__

#include <iostream.h>
#include "go3dblock.h"
#include "gotypes.h"


/*!
 * DWT implementation, currently containing only a modified S-Transform.
 * For DWT, see the signal classes in the original golib.
 * Possibilities for optimisations are in st, ts and all related functions.
 * @author Christian Gosch
 */
template< class T,class OUT_T >
class 
go3DDWT {
 public:
  go3DDWT ();
  virtual ~go3DDWT ();
  
  void set3DBlock (go3DBlock<T> *b);
  go3DBlock<T> *get3DBlock () { return block; }

  /*!
   * Does nothing and is not needed yet.
   */
  go3DBlock<OUT_T> *dwt ();

  /*!
   * Modified S-Transform as described in 
   * B. Aiazzi et al., "Lossless compression of medical images based on an 
   * enhanced generalized multidimensional S-Transform", SPIE Vol. 2825,
   * pp. 868 ff.
   *
   * The following has changed. The data ordering will be changed
   * back to the mixed form described below.
   * Currently, no re-ordering 
   * of data is done, i.e. the 8 resulting subbands are
   * "mixed" in the data. With an appropriate offset, the subbands
   * can be addressed. If this is suitable for the desired performance
   * is not clear. This code is subject to changes.
   * Optimised: No.
   */
  go3DBlock<OUT_T> *st ();
  /*!
   * Reverse operation of <CODE>st()</CODE>.
   * Reconstructs the DWT signal in sTransform. 
   * The result is written in the block set with set3DBlock() and a
   * pointer to that block is returned.
   * @see go3DDWT::st()
   */ 
  go3DBlock<T> *ts ();
  go3DBlock<OUT_T> *getSTransform () { return sTransform; }
  /*!
   * Sets the pointer to the block of the S Transform.
   * sTransform is set to non-deletable.
   * If <CODE>s = 0</CODE>, sTransform is set to deletable again (default).
   * Always set to 0 if no external sTransform block is used.
   */
  void setSTransform (go3DBlock<OUT_T> *s);
  
 protected:
  go3DBlock<T> *block;
  go3DBlock<OUT_T> *sTransform;
  bool	sTransformDeletable;
};

#endif



