#ifndef GOVOLUMERENDERER_H
#define GOVOLUMERENDERER_H

#include <goviewvolume.h>
#include <gosignal3d.h>
#include <gotransferfunction.h>
#include <goblockprovider.h>
#include <go44matrix.h>
#include <gostatusobject.h>

#include <govol.h>

namespace Vol {

/*!
 * Volume renderer base. 
 * This code is incomplete and subject to development.
 * @author Christian Gosch
 * @date 13.8.2001
 */
template< class T >
class
goVolumeRenderer : public goStatusObject 
{
 public:
  goVolumeRenderer ();
  virtual ~goVolumeRenderer ();
  
  //  void set3DBlock (go3DBlock<T> *b) { volume = b; }
  virtual void setBlockProvider (goBlockProvider<T> *p) { provider = p; }
  
  virtual void setViewPlane (goViewVolume& vp) { viewPlane = vp; setSampleDistance (vp.getSampleDistanceX(),
  																					vp.getSampleDistanceY(),
																					vp.getSampleDistanceZ()); }
  virtual void setPosition (go3Vector<volFloat> &p) { position = p; }
  virtual void setSize     (go3Vector<volFloat> &s) { size = s; }
  virtual void setSampleDistance (goFloat dx, goFloat dy, goFloat dz)
      {
	  	sampleDistanceX = dx;
	  	sampleDistanceY = dy;
	  	sampleDistanceZ = dz;
	  	sampleDistanceX_1 = 1 / dx;
	  	sampleDistanceY_1 = 1 / dy;
	  	sampleDistanceZ_1 = 1 / dz;
      }
  
  /*!
   * Sets the distance between two samples in x and y direction on the view plane in world coordinates.
   * This is not necessarily used by all renderers, check for a particular renderer!
   * Default value: 1.0 and 1.0
   * @param w Sample distance in x direction
   * @param h Sample distance in y direction
   */
  void setViewPlaneSampleDistance (volFloat w, volFloat h) { viewPlaneSampleWidth = w; viewPlaneSampleHeight = h; }

  virtual void setImageSize (goSize_t w, goSize_t h) { width = w; height = h; }
  virtual void renderInit();
  virtual void render ();
  inline goSize_t getImageWidth () { return width; }
  inline goSize_t getImageHeight () { return height; }
  inline volFloat getViewPlaneSampleWidth () { return viewPlaneSampleWidth; }
  inline volFloat getViewPlaneSampleHeight () { return viewPlaneSampleHeight; }

 protected:
  inline goViewVolume& getViewPlane () { return viewPlane; }
  inline goBlockProvider<T>* getBlockProvider () { return provider; }
  inline go3Vector<volFloat>& getSize () { return size; }
  inline go3Vector<volFloat>& getPosition () { return position; }

  volFloat sampleDistanceX;
  volFloat sampleDistanceY;
  volFloat sampleDistanceZ;
  // The following are set by setSampleDistance()
  goDouble sampleDistanceX_1;		// 1 / sampleDistanceX
  goDouble sampleDistanceY_1;		// 1 / sampleDistanceY
  goDouble sampleDistanceZ_1;		// 1 / sampleDistanceZ

  goViewVolume	viewPlane;
 private:
  go3Vector<volFloat> position;
  go3Vector<volFloat> size;

  /*
   * Distance between two samples in x and y direction on the view plane in world coordinates.
   * This is not necessarily used by all renderers, check for a particular renderer!
   */
  volFloat	viewPlaneSampleWidth;
  volFloat	viewPlaneSampleHeight;
  
  goBlockProvider<T> *provider;
  
  goSize_t		width, height;

};

};

#endif





