#ifndef GOVOLUMERENDEREREXP_H
#define GOVOLUMERENDEREREXP_H

#include <go3dinfo.h>
#include <go3dviewplane.h>
#include <go3dblock.h>
#include <gotransferfunction.h>

#include <go3dcompressed.h>

/*!
 * Experimental volume renderer. 
 * This code is incomplete and subject to development.
 * If it works, it is NOT FREE. DO NOT USE IT.
 * @author Christian Gosch
 */
template< class T, class OUT_T >
class
goVolumeRendererEXP {
 public:
  goVolumeRendererEXP ();
  virtual ~goVolumeRendererEXP ();

  //  void set3DBlock (go3DBlock<T> *b) { volume = b; }
  void set3DCompressed (go3DCompressed<T,goInt32> *c) { compressed = c; }
  
  void setViewPlane (go3DViewPlane& vp) { viewPlane = vp; }
  void set3DBlockInfo (go3DInfo &i);
  void setStages (goSize_t s) { stages = s; }

  /*!
   * \todo Write RENDERBOX macro (see code)
   */
  void render (OUT_T *image, goSize_t width, goSize_t height, goSize_t stage);

 protected:
  /*! Beschraenkung auf integer Werte! */
  go3DCompressed<T, goInt32> *compressed;
  // This is not supported by exp renderer, so when the time comes, fuck it.
  // go3DBlock<T>	*volume;
  go3DInfo	volumeInfo;
  go3DViewPlane	viewPlane;

  goTransferFunction<T,goDouble>	densityToFloat;
  goTransferFunction<goDouble,OUT_T>	floatToGrey;

  goSize_t stages;
};

#endif




