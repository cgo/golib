#ifndef GOBLOCKRENDERER_H
#define GOBLOCKRENDERER_H

#include <goviewplane.h>
#include <gosignal3d.h>
#include <gosignal2d.h>
#include <gotransferfunction.h>
#include <goblockprovider.h>
#include <go44matrix.h>
#include <govolumerenderer.h>
#include <goresolutionmanager.h>
#include <gotransferfunction.h>
#include <gotimerobject.h>
#include <gothread.h>

#include <govol.h>

namespace Vol {

/*!
 * Block volume renderer. 
 * This code is incomplete and subject to development.
 * @author Christian Gosch
 * @todo The findbounds macro seems a bit buggy, which is probably the reason
 * we need those correction terms for the boundaries of a block projection on the viewplane.
 * Fix this as soon as there's time. The good old "line artifact" is still visible btw, but
 * that's not that important for now.
 * @date 13.8.2001
 */
template< class T >
class
  goBlockRenderer : public goVolumeRenderer<T>, public goTimerObject 
{
 public:
  goBlockRenderer ();
  virtual ~goBlockRenderer ();
  
  //  void set3DBlock (go3DBlock<T> *b) { volume = b; }

  /*!
   * Initializes the object. A resolution manager has to be set with setResolutionManager()
   * and the maximum stage should be set with setStages().
   * Not setting these before initialization may lead to unstable behaviour and/or
   * exceptions.
   * init() allocates the temporary image buffer. If it existed before,
   * the image buffer is deleted. init() must be called when the image size
   * was changed using setImageSize().
   * @throw goExceptionString
   */
  virtual void init();
  virtual void render (goArray<goSize_t> *indices);
	  /// Waits for the current rendering process
	  void wait();
  /*!
   * Cancels the current rendering process by setting the REQUEST_THREAD_CANCEL boolean variable.
   * The renderer must test this variable if it wants to react on cancellation.
   */
  virtual void cancel();
  /// returns true if the RENDERER_BUSY flag is true. 
  virtual bool rendererBusy();
  void setStages (goSize_t s)
      {
	  stages = s;
      }
  void setResolutionManager (goResolutionManager *rm)
      {
	  resolutionManager = rm;
      }

  void setAlpha (goTransferFunction<T, volFloat>& tf)
      {
	  alphaFunction = tf;
      }
  
  void setColor (goTransferFunction<T, volFloat>& tf)
      {
	  colorFunction = tf;
      }
  // Must be of size getImageWidth() * getImageHeight()
  void setGrey (goTransferFunction<volFloat,goUInt32>& tf)
      {
	  greyFunction = tf;
      }
  
  inline goSignal2D<volFloat> *getTempImage () { return tempImage; }
  inline goSignal2D<goUInt32> *getFinalImage () { return finalImage; }

 protected:
  /// Must be set
  goTransferFunction<T, volFloat> alphaFunction;
  goTransferFunction<T, volFloat> colorFunction;

  /// Is created by init(), the size has to be set beforehand
  // volFloat		*tempImage;
  goSignal2D<volFloat>	*tempImage;
  /// Is created by init(), the size has to be set beforehand
  // volFloat		*alphaImage;
  goSignal2D<volFloat>	*alphaImage;
  // Is created by init(), the size has to be set beforehand
  goSignal2D<goUInt32>	*finalImage;
  // Final image
  goTransferFunction<volFloat, goUInt32> greyFunction;


  go3Vector<volFloat>	eyePos;
  /// Must be set
  goResolutionManager   *resolutionManager;
  /// Must be set
  goSize_t		stages;

  goSize_t		xBlocks;
  goSize_t		yBlocks;
  goSize_t		zBlocks;
  volFloat		xStep;
  volFloat		yStep;
  volFloat		zStep;
  go3Vector<volFloat>   curPos;
  go3Vector<volFloat>   curPosStart;
  go44Matrix<volFloat>  Tproj;

  // Render thread, if multiple threads are used.
  goThread		rThread;
  // flags for the thread
  bool			REQUEST_THREAD_CANCEL;    // cancel request
  bool			RENDERER_BUSY;			  // indicates renderer is busy	
};

};

#endif





