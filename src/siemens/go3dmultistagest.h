#ifndef GO3DMULTISTAGEST_H
#define GO3DMULTISTAGEST_H

#include <gotypes.h>
#include <godefs.h>
#include <go3dblock.h>
#include <go3ddwt.h>

/*!
 * Performs multi stage S transform (ST).
 * Uses <CODE>go3DDWT</CODE> to make several ST's of a <CODE>go3DBlock</CODE>.
 * To perform a transform of a <CODE>go3DBlock</CODE>, do something like this: <br>
 *  <code>
 *   go3DBlock<goInt16> block; <br>
 *   go3DBlock<goInt32> transform; <br>
 *   go3DMultiStageST<goInt16, goInt32> st; <br>
 *   st.setStages (3);  // three stages <br>
 *   st.set3DBlock (&block); <br>
 *   st.setSTransform (&transform); <br>
 *   st.execute(); <br>
 *   ... <br>
 *   block.destroy(); <br>
 *   transform.destroy(); <br>
 *   exit(1); <br>
 *  </code>
 * @author Christian Gosch
 * @see go3DBlock, go3DDWT
 */
template < class T, class OUT_T >
class
go3DMultiStageST {
 public:
  go3DMultiStageST ();
  virtual ~go3DMultiStageST ();

  /*!
   * Initialises the object. This is called by the constructor method.
   */
  void init (void*);
  
  /*!
   * Sets the number of stages the transform will go through.
   * Default is 1.
   */
  void setStages (goSize_t s) { stages = s; }
  /*!
   * Sets the input <CODE>go3DBlock</CODE>.
   */
  void set3DBlock (go3DBlock<T> *b) { block = b; }
  go3DBlock<T> *get3DBlock () { return block; }
  /*!
   * Sets the transform <CODE>go3DBlock</CODE>. You have to set this before
   * performing the transform or reverse transform.
   */
  void setSTransform (go3DBlock<OUT_T> *b) { transform = b; }
  go3DBlock<OUT_T> *getSTransform () { return transform; }
  /*!
   * Transform the block set with <CODE>set3DBlock()</CODE> 
   * using the number of stages set with <CODE>setStages</CODE> using
   * The S Transform from <CODE>go3DDWT</CODE>.
   */
  void execute ();
  /*!
   * Reverse transform. The transform is saved in the block set by
   * <CODE>setSTransform()</CODE>.
   * reconStages gives the number of stages the transform is done starting at the
   * lowest resolution stage.
   */
  void reverse (goSize_t reconStages = 1);

 private:
  go3DBlock<T>		*block;
  go3DBlock<OUT_T>	*transform;
  go3DDWT<T,OUT_T>	dwt1;
  go3DDWT<OUT_T,OUT_T>	dwt2;

  goSize_t	stages;
};


#endif
