#ifndef GOVOLUMENAVIGATOR_H
#define GOVOLUMENAVIGATOR_H

#include <gotypes.h>
#include <goviewvolume.h>
#include <gostring.h>
#include <gotimerobject.h>
#include <gostatusobject.h>
#include <goobjectinfo.h>
#include <gosignal2d.h>
#include <gotransferfunction.h>
#include <govol.h>
#include <gonavdevice.h>
// #include <goposdisplay3d.h>

class goMutex;

namespace Vol {

class goVolumeFileInfo;

  /*!
   * Main class for 3D data set navigation.
   * A view manager and a renderer object are created and handled internally.
   * Through the view manager initialization the whole data handling system is initialized and
   * is used by the volume navigator object.
   * @author Christian Gosch
   * @date 21.8.2001
   * @see goViewManager
   * @see goBlockRenderer
   * @see goVolumeRenderer
   * @see goVolumeFile
   * @see goViewVolume
   */
class
goVolumeNavigator : public goTimerObject, public goStatusObject, public goObjectInfo, public goNavSlot
{
 public:
    goVolumeNavigator();
    virtual ~goVolumeNavigator();

	/*!
	 * @return The amount of memory used by the block data in bytes.
	 */
    virtual goSize_t memoryUsage ();
	/*!
	 * Set the approximate upper memory bound to max. 
	 * @param max The upper memory bound in bytes.
	 */
	void setMaxMemoryUsage (goSize_t max);
	
    /*!
     * Sets the data type of the data file.
     * @param t One of the types listed in gotypes.h,
     * - GO_INT8
     * - GO_UINT8
     * - GO_INT16
     * - GO_UINT16
     * - GO_INT32
     * - GO_UINT32
     * - GO_FLOAT
     * - GO_DOUBLE
     */
    void setDataType(int t);
	/*!
	 * Sets the data type according to the footer information 
	 * in the given transformed file.
	 * @param filename Name of the transformed file.
	 */
	void setDataType(const char* filename);
    /*!
     * Sets the file name of the file containing the volume data. This file name is
     * reached through to the internal view manager object when init() is called.
     */
    void setFileName (goString& s);
    void setFileName (const char* s);

    /*!
     * @throw goExceptionString
     */
    void init ();

    /*!
	 * @param render If true, triggers rendering a new image.
     * @todo Think of a different name. Does not update the view manager anymore, but just adds a view to it.
     */
    void updateView(bool render=true);
	/*!
	 * Switch the automatic triggering of the renderer on or off.
	 * If off, updateView() will not actually render an image, but only trigger the loading process.
	 * @param yes_or_no If true, rendering is triggered. If false, rendering is not triggered.
	 */
	void updateViewRenders(bool yes_or_no) { update_view_renders=yes_or_no; }    
	/*!
	 * @return A pointer to a goSignal2D<goUInt32> containing the final image
	 *			(256 grey values)
     */
    goSignal2D<goUInt32> *getFinalImage ();
	void				 optGreyValues();
	
    /*!
     * Saves the current image in a file.
     * @param filename Name of the file. The format currently is PGM.
     * @throw Whatever getFinalImage() throws.
     */
    void saveImage (const char* filename);
    
    /*
     * Navigation commands:
     */
    /*
     * Translation
     */
    /****************************************************************/
    /// Move view to absolute position newPos
    void moveViewTo	(go3Vector<volFloat>& newPos);
    /// Move view relative to current view for diff
    void moveView	(go3Vector<volFloat>& diff);
    /****************************************************************/
    /*
     * Rotation
     */
    void rotateView (volFloat angle, GO_ROTATION_AXIS);
	/*!
	 * Resets the view angle so the view is directed towards the positive
	 * z-axis, up-vector pointing in the direction of the negative y-axis.
	 */
	void resetViewAngle();
    /*
     * View geometry
     */
    void setViewDepth (goSize_t d);
    void setViewEyeDistance (volFloat d);

    /*
     * Volume settings
     */
    void setSampleDistance (goFloat x, goFloat y, goFloat z);

	/*!
	 * Sets the maximal resolution the data should be loaded in.
	 * A smaller resolution will result in smaller image sizes
	 * in that only a part of the image is used. The image center stays the 
	 * same whereas the size of the actually rendered image decreases
	 * with 2^-(stages-r), where stages is the number of stages the
	 * data was decomposed into.
	 * So if stages = 3, maximum resolution you can get is 3, minimum is always
	 * 0.
	 * @param r Desired resolution
	 */
	void setMaxResolution (int r);
    
    /*!
     * Sets the screen image size in pixels. This needs to be called BEFORE initialization.
     * @param w Width of the screen image in pixels
     * @param h Height of the screen image in pixels
     */
    void setScreenSize (int w, int h);

    /*
     * Image settings
     */
    /*!
     * Sets the transfer function to translate from rendered volFloat values to grey values.
     * @param pointsX Pointer to a goArray containing the X coordinates of the points in the transfer function.
     * Values are between 0.0f and 1.0f and have to be translated according to the image statistics.
     * @param pointsY Pointer to a goArray containing the Y coordinates of the points in the transfer function.
     * Values are between 0.0f and 1.0f and have to be translated according to the image statistics.
	 * @param t Type of the transfer function to set the points in. One of
	 *  - GO_TF_GREY
	 *  - GO_TF_DENSITY (means colour)
	 *  - GO_TF_OPACITY
     */
    void setTransferFunctionPoints (goArray<volFloat>* pointsX, goArray<volFloat>* pointsY, GO_TF_TYPE t);

    /*!
     * @return True if this object is initialized.
     */
    inline bool isInitialized () { return initialized; }

	const goVolumeFileInfo& getFileInfo();
	void* getViewManager() { return vm; }

	/// From goNavSlot
	virtual void motion(goNavSlot::motionType t, void* arg);
	/*!
	 * Contains the code that is executed as a separate thread to 
	 * update the information delivered by the connected goNavDevice.
	 */
	void motionUpdater();

#ifdef _GODEBUG_VOLUMEFILE
    void debugVolumeFile();
#endif

	// goPosDisplay3D *getPosDisplay3D() { return &posDisplay; }

	void TEST(int sel=0); // runs benchmarking tests / see source code
 protected:
    goSignal2D<volFloat> *getTempImage ();

    void deleteViewManager();
    void createViewManager();

    void setInitialized (bool i) { initialized = i; }
    
    /// Sets the transfer functions in the renderer. Used by setTransferFunctionPoints()
    void setTransferFunctions();
   
    /// Returns true if the renderer is currently indicating busy state, i.e. the renderer's RENDERER_BUSY flag is set to true. 
	bool rendererBusy();
	void waitForRenderer(); // Waits for the renderer to return 
	void predict(goViewVolume&); // sets given view volume as prediction in the view
								 // manager
 private:
    /*
     * Void pointer represented objects
     */
    // view manager
    void* vm;
    goArray<volFloat> greyTFPointsX;
    goArray<volFloat> greyTFPointsY;
    goArray<volFloat> opacityTFPointsX;
    goArray<volFloat> opacityTFPointsY;
    goArray<volFloat> densityTFPointsX;
    goArray<volFloat> densityTFPointsY;

    goTransferFunction<volFloat,goUInt32> *greyTF;
	    
    volFloat greyTFMin;			// Minimum value of the volFloat rendered values for the transfer function
    volFloat greyTFMax;			// Maximum value of the volFloat rendered values for the transfer function
    volFloat opacityTFMin;		// Min/Max value for opacity (usually the same as for density)
    volFloat opacityTFMax;
    volFloat opacityMapMax;		// Min/Max values for opacity. Defaults to 0.0 --> 1.0
    volFloat opacityMapMin;
    volFloat densityTFMin;		// Min/Max value for opacity (usually the same as for density)
    volFloat densityTFMax;
    volFloat densityMapMin;
    volFloat densityMapMax;

    goSignal2D<goUInt32> *finalImage;
	goFloat				 imageZoomFactor;	// If images should be zoomed (resolution dependent
											// this is the scale factor.
	goMutex				 finalImageMutex;
    /*
     * Variables
     */
    // data type (see type enum in gotypes.h)
    int			dataType;
    
    goViewVolume	viewVolume;
    // file name of the file containing the volume data
    goString		volumeFileName;
    
    int screenWidth;	// set by setScreenSize();
    int screenHeight;   // set by setScreenSize();
    
    bool		initialized;

	// Motion vectors maintained by motion()
	goMutex motionMutex;  // locks the speed vectors
	go3Vector<volFloat> rotationSpeed;
	go3Vector<volFloat> translationSpeed;
	goSemaphore motionSema;  // wird erhoeht wenn Bewegungsaenderung erkannt wird (motion())
	goThread motionUpdaterThread;
	bool	 update_view_renders; // call renderer when updateView is called?

	// Position wisplay window
	// goPosDisplay3D posDisplay;

};
};
#endif
