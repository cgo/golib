#ifndef GOVOLUMERENDERER_H
#define GOVOLUMERENDERER_H

#include <go3dinfo.h>
#include <go3dviewplane.h>
#include <go3dblock.h>
#include <gotransferfunction.h>

#include <govolumerendererconfig.h>
#ifdef GO_VOLUME_COMPRESSED
#include <go3dcompressed.h>
#endif

/*!
 * Simple volume renderer.
 * \todo Optimise voxel access if there's time for it
 * (see code for macro <CODE>GET_VOXEL_[DIRECT|COMPRESSED]</CODE>).
 * @author Christian Gosch
 */
template< class T, class OUT_T >
class
goVolumeRenderer {
 public:
  goVolumeRenderer ();
  virtual ~goVolumeRenderer ();

  // #ifndef GO_VOLUME_COMPRESSED
  inline void set3DBlock (go3DBlock<T> *b) { volume = b; }
  // #else
  //  void set3DCompressed (go3DCompressed<T,goInt32> *c) { compressed = c; }
  //  void set3DCompressed (go3DCompressed<T,goInt32> *c) { compressed = c; }
  // #endif
  
  void setViewPlane (go3DViewPlane& vp) { viewPlane = vp; }
  void set3DBlockInfo (go3DInfo &i);
  void setStages (goSize_t s) { stages = s; }

  /*!
   * \todo Calculation of the render boundaries may not work -> rethink!
   */
  void render (OUT_T *image, goSize_t width, goSize_t height, goSize_t stage);

 protected:
  /*! Beschraenkung auf integer Werte! */
#ifdef GO_VOLUME_COMPRESSED
  //  go3DCompressed<T, goInt32> *compressed;
  go3DCompressed<T, goInt32> *compressed;
#else
  go3DBlock<T>	*volume;
#endif
  go3DInfo	volumeInfo;
  go3DViewPlane	viewPlane;

  goTransferFunction<T,goDouble>	densityToFloat;
  goTransferFunction<goDouble,OUT_T>	floatToGrey;

  goSize_t stages;
};

#endif




