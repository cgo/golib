#ifndef GOVOLUMERENDEREREXP_H
#define GOVOLUMERENDEREREXP_H

#include <goviewplane.h>
#include <gosignal3d.h>
#include <gotransferfunction.h>
#include <goblockprovider.h>
#include <go44matrix.h>

namespace Vol {

/*!
 * Experimental volume renderer. 
 * This code is incomplete and subject to development.
 * @author Christian Gosch
 * @date 31.7.2001
 * @note This class is under development. It will enable blockwise
 * rendering with blending of each block's images.
 */
template< class T, class OUT_T >
class
goVolumeRendererEXP {
 public:
  goVolumeRendererEXP ();
  virtual ~goVolumeRendererEXP ();
  
  //  void set3DBlock (go3DBlock<T> *b) { volume = b; }
  void setBlockProvider (goBlockProvider<T> *p) { provider = p; }
  
  void setViewPlane (goViewPlane& vp) { viewPlane = vp; }
  void setPosition (go3Vector<volFloat> &p) { position = p; }
  void setSize     (go3Vector<volFloat> &s) { size = s; }
  
  void renderInit(OUT_T *img, goSize_t w, goSize_t h);
  void render ();
  
 protected:
   // go3DInfo	volumeInfo;
  go3Vector<volFloat> position;
  go3Vector<volFloat> size;
  goViewPlane	viewPlane;

  goBlockProvider<T> *provider;
  
  goTransferFunction<T,volFloat>	densityToFloat;
  goTransferFunction<volFloat,OUT_T>	floatToGrey;
  
  goSize_t stages;

 private:
  volFloat		*tempImage;
  go3Vector<volFloat>	eyePos;
  goArray<goSize_t>	blockIndices;
  goArray<volFloat>	blockDistances;
  goSize_t		width, height;
  OUT_T			*image;
  go3Vector<volFloat>   u3; 
  go3Vector<volFloat>   v3;
  go3Vector<volFloat>   normal;

  goSize_t		xBlocks;
  goSize_t		yBlocks;
  goSize_t		zBlocks;
  volFloat		xStep;
  volFloat		yStep;
  volFloat		zStep;
  go3Vector<volFloat>   curPos;
  go3Vector<volFloat>   curPosStart;
  go44Matrix<volFloat>  Tproj;

};
};

#endif





