#ifndef GOVIEWMANAGER_H
#define GOVIEWMANAGER_H

#include <gotypes.h>
#include <gopresencemanager.h>
#include <goresolutionmanager.h>
#include <goblockstore.h>
#include <gosblockrenderer.h>
#include <govglblockrenderer.h>
#include <gotimerobject.h>
#include <goqueue.h>
#include <gostatusobject.h>
#include <gosignal2d.h>
#include <goobjectinfo.h>

namespace Vol {

/*!
 * View manager.
 * @author Christian Gosch
 * @date 10.12.2001
 */
template <class T>
class
    goViewManager : public goTimerObject, public goStatusObject, public goObjectInfo
{
 public:
  goViewManager();
  virtual ~goViewManager();
    
  /*!
   * @throw goExceptionString
   */
  void init();
  /*!
   * Set the name of the transformed volume file.
   * @param f The file name
   */
  void setFileName(const char* f);
  /*!
   * Set the name of the transformed volume file.
   * @param f The file name
   */
  void setFileName(goString& f);

  /*!
   * Updates presence/resolution flags and initiates loading of needed sub volumes.
   * The blocks and their needed resolutions are calculated and loaded by goVolumeFile.
   * @todo Add resolution determination strategies!
   */
  void update();
  
  void TEST(int); // runs a test with the view manager / see source code

  /*!
   * Starts renderer in order to render the current view.
   * Update is not called from within this method, but must be called before render() is called in order
   * to have the next view from the view queue and to initiate a loading thread in goVolumeFile, if needed.
   */
  void render ();

  /*!
   * @return A Pointer to a goSignal2D array containing the rendered temporary
   * image (not grey values!). These values have to be mapped to grey values.
   * The size of the image is stored in the goViewVolume object
   * which must be set with setViewVolume().
   */
  inline goSignal2D<volFloat> *getTempImage() { return renderer.getTempImage(); }

  inline goSignal2D<goUInt32> *getFinalImage() { return renderer.getFinalImage(); }
  
  goSize_t getImageWidth() { return renderer.getImageWidth(); }
  goSize_t getImageHeight() { return renderer.getImageHeight(); }
  /*!
   * @attention This method is exclusively used to connect to other parts 
   * of a navigation/data visualization
   * system. It should not be used without knowledge about how the whole system functions.
   * @return The pointer to the goBlockStore object used to store the actual data.
   */
  goBlockStore<T>* getBlockStore() { return &store; }

  /*!
   * Sets an approximate upper bound of memory to be used by data. 
   * The value is handed over to the goBlockStore.
   * @param max Memory bound in bytes.
   */
  void setMaxMemoryUsage(goSize_t max);

  /*!
   * From goObjectInfo.
   */
  virtual goSize_t memoryUsage() { return store.memoryUsage(); }
  /*!
   * @return The pointer to the goResolutionManager object used to keep track
   * of the resolution information for each block.
   * @attention This method is exclusively used to connect to other parts 
   * of a navigation/data visualization
   * system. It should not be used without knowledge about how the whole system functions.
   */
  goResolutionManager* getResolutionManager() { return &rm; }
  /*!
   * @return The size of the volume in voxels as stored in the file info block.
   * @see goVolumeFileInfo
   * @see goVolumeFile
   */
  inline const goSize3D& getVolumeSize() const { return file.getFileInfo().size; }
  /*!
   * @return The goVolumeFileInfo block as stored in the volume file.
   * @see goVolumeFileInfo
   * @see goVolumeFile
   */
  inline const goVolumeFileInfo& getFileInfo() const { return file.getFileInfo(); }

  /*!
   * @attention Don't make any changes to the renderer. That is not necessary.
   * Only make changes if you know what you are doing.
   * Only used by goVolumeNavigator to set the transfer function(s) directly.
   * @return Reference to the goSBlockRenderer<T> used to render the images.
   */
  inline goSBlockRenderer<T>& getRenderer () { return renderer; }

  /*!
   * Adds a view volume to the internal view queue.
   * @param v The view volume to be added
   */
  void addView (goViewVolume& v);
  /*!
   * Use given view as a prediction and try to trigger pre-loading of associated 
   * blocks.
   * @param v Predicted goViewVolume 
   */
  void predict (goViewVolume& v);


  /*!
   * Calculate needed resolution of all blocks in the renderBlocks array and set the resolution in 
   * goResolutionManager rm.
   * @note Needs the private variable <code>view</code> to be the applicable view and needs the update method
   * of that view called beforehand without any changes being made afterwards. In other words, <code>view</code> needs to be
   * the proper view volume.
   */
  void calculateResolutionAreas();
  
  /*!
   * Calculates an approximation of the blocks in the current view and adds them to the list of blocks to be rendered.
   * @param myView goViewVolume describing the view plane
   * @param blockArray goArray<goSize_t> in which the block indices
   * 		will be stored after this method returns
   * @param resolutionArray goArray<int> in which the block resolutions 
   *		will be stored after this method returns
   * @param prediction If true, myView is interpreted as a predicted, 
   *		not the actual view
   */
  void calculateBlocksInView(goViewVolume& myView, goArray<goSize_t>&
		  blockArray, goArray<int>& resolutionArray, bool prediction=false);

  /*!
   * Sets the sample distance in the volume renderer. Used by goVolumeNavigator.
   * @param x Sample distance in x direction
   * @param y Sample distance in y direction
   * @param z Sample distance in z direction
   * @see goVolumeNavigator
   * @see goVolumeRenderer
   */
  /* void setRendererSampleDistance (goFloat x, goFloat y, goFloat z); */
  void setRendererAlpha		 (goTransferFunction<T, volFloat> &a) { renderer.setAlpha (a); }
  void setRendererColor		 (goTransferFunction<T, volFloat> &c) { renderer.setColor (c); }

  /*!
   * Sets a desired frame rate (or rendering rate) in frames/second.
   * Do not expect this value to come true. The resolution will, however, be adjusted to 
   * try to approach this value.
   * @param rate The desired frame rate in frames/second
   */ 
  void setFrameRate (goFloat rate) { maxRenderTime = 1 / rate; }

  /*!
   * Sets the maximum resolution the calling application wants to get
   * For any view.
   * The addView() routine automatically sets the pixel size in any newly
   * added view to (1 << (stages - r)).
   * So when the resolution is set to r, the resulting image will be
   * (1 << (stages - r)) times smaller in each direction.
   * Default value is 0 when an instance of this class is created,
   * and is set to the maximum possible value when it is initialized.
   * @param r The resolution ranging from 0 (lowest) to the number of
   * stages the given data set was decomposed into.
   * @see goDWT
   * @see goSBlockRenderer
   */
  void setMaxResolution (int r);
 
#if defined _GODEBUG_VOLUMEFILE || defined GO_BENCHMARK
  goVolumeFile<T>* getVolumeFile() { return &file; }
  // goBlockStore<T>* getBlockStore() { return &store; }
#endif

 private:
  goPresenceManager<T>	pm;			// Presence manager
  goResolutionManager	rm;			// Resolution manager
  goBlockStore<T>	store;			// Block provider
  goVolumeFile<T>	file;			// File
  
  goViewVolume		view;			// View to be served next
  int				maxResolution;  // Maximum desired resolution
  goQueue<void*>	viewQ;			// Queue, filled with pointers to goViewVolume objects
  goMutex			viewQMutex;		// Mutex to protect the viewQ against reentrance (queues are based
  goMutex			updateMutex;	// To protect the update() method from reentrance,
  									// since it uses some class-scope variables.
  									// on lists) 
    
  goSBlockRenderer<T>	renderer;		// Choose a renderer
  //goVGLBlockRenderer<T>	renderer;		// Choose a renderer
 
  go3Vector<volFloat>	volumePosition;		// Position of the volume for the renderer
  goArray<goSize_t>	renderBlocks;		// Blocks to be rendered (given as argument to the renderer)
  
  goArray<volFloat>     resolutionAreas;	// Array for the distance dependent resolution information.
						// resolutionAreas[resolution], resolution from 0 to max. stage.
						// [0] contains highest resolution border.

  goDouble		maxRenderTime;		// Maximal rendering time per frame, used for adapting
						// the resolution to the needed frame rate 
						// Set by setFrameRate()
  int			renderTimeAdjust;	// Value to subtract from each resolution to adjust for the desired render time
};



};

#endif
