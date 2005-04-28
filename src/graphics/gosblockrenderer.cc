#include <gosblockrenderer.h>
#include <gotypes.h>
#include <goarray.h>
#include <go3vector.h>
#include <go4vector.h>
#include <go3dalgo.h>
#include <gomath.h>
#include <govolumerenderermacros.h>
#include <gotransferfunction.h>
#include <gohashtable.h>

#include <govol.h>

#include <goconfig.h>

#include <gopresencemanager.h>

#include <goviewvolume.h>

#include <goerror.h>
#include <goexception.h>

#include <math.h>

#include <stdio.h>			// sprintf


#ifdef _GODEBUG_SBLOCKRENDERER_SEGFAULT
#include <signal.h>
#endif

#include <gopowfunction.h>

namespace Vol {

powFuncTable::powFuncTable()
{
}

powFuncTable::~powFuncTable()
{
	goSize_t i;
	for (i = 0; i < (goSize_t)powfuncs.getSize(); i++)
	{
		delete (goPowFunction*)powfuncs[i];
	}
	powfuncs.resize(0);
}

void powFuncTable::init(int maxres)
{
	goSize_t i;
	for (i = 0; i < (goSize_t)powfuncs.getSize(); i++)
	{
		delete (goPowFunction*)powfuncs[i];
	}
	powfuncs.resize(maxres + 1);
	for (i = 0; i < (goSize_t)powfuncs.getSize(); i++)
	{
		goPowFunction *p;
		p = new goPowFunction;
		p->init(i,9); //Genauigkeit 512 Werte (0,002)
		powfuncs[i] = (void*)p;
	}
}


// Use subimages
#define USE_SUBIMAGES 1
//#undef USE_SUBIMAGES

// .. but don't actually blend them together as no old subimages are reused as yet.
#define NO_SUBIMAGE_BLENDING 1   


#define GO_SBR_RENDERTHREAD(__TYPE) { \
    /* goThread::setCancelType (0); */ \
    Vol::goSBlockRenderer<goInt8>::render_thread_arg *arg = (Vol::goSBlockRenderer<goInt8>::render_thread_arg*)p; \
    Vol::goSBlockRenderer<goInt8>* renderer = (Vol::goSBlockRenderer<goInt8>*)arg->renderer; \
    renderer->renderThread(&arg->indices); \
    arg->indices.resize(0); \
    return 0; \
}

void*
render_thread_8 (void* p)
{
	GO_SBR_RENDERTHREAD(goInt8);
}
void*
render_thread_u8 (void* p)
{
	GO_SBR_RENDERTHREAD(goUInt8);
}
void*
render_thread_16 (void* p)
{
	GO_SBR_RENDERTHREAD(goInt16);
}
void*
render_thread_u16 (void* p)
{
	GO_SBR_RENDERTHREAD(goUInt16);
}
void*
render_thread_32 (void* p)
{
	GO_SBR_RENDERTHREAD(goInt32);
}
void*
render_thread_u32 (void* p)
{
	GO_SBR_RENDERTHREAD(goUInt32);
}
void*
render_thread_f (void* p)
{
	GO_SBR_RENDERTHREAD(goFloat);
}
void*
render_thread_d (void* p)
{
	GO_SBR_RENDERTHREAD(goDouble);
}

static inline void blendSubImage (goSubImage *subImage, goSignal2D<volFloat> *image, 
				  goSignal2D<volFloat> *alphaImage, 
				  goSize_t w, goSize_t h, 
				  volFloat sampleWidth, volFloat sampleHeight);

static inline 
volFloat bilinear (volFloat A, volFloat B, volFloat C, volFloat D, volFloat px, volFloat py);
static inline 
volFloat bilinearSample	(goSubImage *subImage, volFloat subX, volFloat subY);
static inline 
void	 bilinearSample2 (goSubImage *subImage, volFloat subX, volFloat subY, volFloat& Color, volFloat& Alpha);
static inline 
volFloat bilinearSampleAlpha (goSubImage *subImage, volFloat subX, volFloat subY);
static inline 
void	 copyImageAlpha (goSignal2D<volFloat>* alphaImage, goSubImage *subImage);
static inline 
void 	 copyAlphaImage	(goSubImage *subImage, goSignal2D<volFloat>* alphaImage);


// Define a segmentation fault handler if we are looking for that nasty segfault...
#ifdef _GODEBUG_SBLOCKRENDERER_SEGFAULT
volFloat SFxSample, SFySample, SFzSample;
int SFx, SFy, SFz;
int SFxAlpha, SFyAlpha, SFzAlpha;
int SFblock;


void SEGFAULTHANDLER(int i)
{
	cout << "Renderer segfault handler\n";
	cout << "-------------------------\n";
	cout << "SFxSample = " << SFxSample << "\n";
	cout << "SFySample = " << SFySample << "\n";
	cout << "SFzSample = " << SFzSample << "\n";
	cout << "SFx	   = " << SFx << "\n";
	cout << "SFy	   = " << SFy << "\n";
	cout << "SFz	   = " << SFz << "\n";
	cout << "SFxAlpha  = " << SFxAlpha << "\n";
	cout << "SFyAlpha  = " << SFyAlpha << "\n";
	cout << "SFzAlpha  = " << SFzAlpha << "\n";
	cout << "SFblock   = " << SFblock << endl;
	exit(2);
}
#endif

template <class T>
goSBlockRenderer<T>::goSBlockRenderer() 
    : goBlockRenderer<T>(), goConsumer()
{
    setClassName ("goSBlockRenderer");
    subImages.setModValue (1023);
    pm = 0;
    renderTime = 0.0f;
    MRImages.resize(0);
    MRViews.resize(0);
#ifdef _GODEBUG_SBLOCKRENDERER_SEGFAULT
	signal (SIGSEGV, SEGFAULTHANDLER);
#endif
	powFuncs.init(16);
}
    
template <class T>
goSBlockRenderer<T>::~goSBlockRenderer()
{
}

template <class T>
void
goSBlockRenderer<T>::init ()
{
    goBlockRenderer<T>::init();
    if (!pm)
	{
	    goString s;
	    s = getClassName();
	    s += " does not have a presence manager set";
	    throw goExceptionString(s);
	    return;
	}
    xStep *= sampleDistanceX;
    yStep *= sampleDistanceY;
    zStep *= sampleDistanceZ;

    goInt32 imageWidth  = getImageWidth();
    goInt32 imageHeight = getImageHeight();
    
    goInt32 res = resolutionManager->getMinResolution();
    goInt32 n;
    for (n = 0; n < MRViews.getSize(); n++)
    {
	// goSignal2D<volFloat>* rmSig;
	// rmSig = (goSignal2D<volFloat>*)MRImages[n]; 
		goViewVolume *rmView;
		rmView = (goViewVolume*)MRViews[n];
// 		cout << "Before deletion: " << endl;
// 		cout << "MRImages[" << n << "] = " << hex << MRImages[n] << dec << endl;
// 		cout << "MRViews[" << n << "] = " << hex << MRViews[n] << dec << endl;

		// delete rmSig;
		delete rmView;
	
    }
    MRViews.resize (res + 1);
    // MRImages.resize (res + 1);
    for (n = res; n >= 0; n--)
    {
		//cout << "Making MRImage " << n << ", size " << imageWidth << " x " << imageHeight << endl;
		// MRImages[n] = (void*) new goSignal2D<volFloat>(imageWidth, imageHeight, 32, 32);
		// cout << "MRImages[" << n << "] = " << hex << MRImages[n] << dec << endl;
		// ((goSignal2D<volFloat>*)MRImages[n])->fill (0.0f);
		//cout << "Making MRView " << n << endl;
		MRViews[n] = (void*) new goViewVolume;
		//cout << "MRViews[" << n << "] = " << hex << MRViews[n] << dec << endl;
		imageWidth  >>= 1;
		imageHeight >>= 1;
    }
}



template<class T>
void
goSBlockRenderer<T>::initMR()
{
    goViewVolume *view;
    goInt32 res = resolutionManager->getMinResolution();
    goInt32 n;
    volFloat scalefactor = 1.0f;
    for (n = res; n >= 0; n--)
    {
 		view = (goViewVolume*)MRViews[n];
 		*view = getViewPlane();
 		view->scale (scalefactor);
 		view->update();
 		scalefactor *= 0.5f;
    }
}

template<class T>
void
goSBlockRenderer<T>::renderThread (goArray<goSize_t> *indices)
{
    // goThread::setCancelType(0);
    RENDERER_BUSY = true; 
    /*
     * MR variable initialization
     */
    goString s;
#if _GODEBUG >= 1
    s = getClassName();
    s += "::render()";
    goError::note(s.toCharPtr(), " initMR()");
#endif
    initMR();
        
    /*
     * Initialize sub images
     */
#if _GODEBUG >= 1
    s = getClassName();
    s += "::render()";
    goError::note(s.toCharPtr(), " initSubImages()");
#endif

    initSubImages(indices);

    /*
     * Render sub images
     */
#if _GODEBUG >= 1
    goError::note(s.toCharPtr(), " renderSubImages()");
#endif
	startTimer();
    renderSubImages(indices);
    stopTimer();
    renderTime = getTimerSeconds();
	goString s2;
	s2 = " Time in render loop: ";
	char tm[255];
	sprintf(&tm[0],"%.2f",getTimerSeconds());
	s2 += &tm[0];
	s2 += "s";
	goError::note (s.toCharPtr(), s2.toCharPtr());

    /*
     * Composite sub images to the final image
     */
#if _GODEBUG >= 1
    goError::note(s.toCharPtr(), " compositeSubImages()");
    //goError::note(s.toCharPtr(), " sampleSubImages()");
#endif
	// Only composite if the thread wasn't cancelled (which would mean the image is 
	// not completed, or usually black.
	if (!REQUEST_THREAD_CANCEL)
	{
#ifdef USE_SUBIMAGES
#ifndef NO_SUBIMAGE_BLENDING		
     compositeSubImages(indices);
#endif	 
     //sampleSubImages(indices);
#else
     // compositeMRImages();
#endif
	}
	RENDERER_BUSY = false;
    // Make sure progress is ending correctly at the max value.
    setProgress (getMaxProgress());
    signalProgress();
}


template <class T>
void
goSBlockRenderer<T>::render (goArray<goSize_t> *indices)
{
	goError::print("goSBlockRenderer::render()","No specialization for this data type!");
	return;
    // rThread.cancel();   
	// rThread.join();
	// 'tention ... only one render thread at a time may exist, since we have only one
	// thread arg and if another thread overwrites the old indices, the first thread
	// gets confused. Don't mix this up.
//    threadArg.renderer = (void*)this;
//    threadArg.indices         = *indices;
#ifdef MULTIPLE_THREADS	
	//rThread.create (render_thread, (void*)&threadArg, 1);
//    rThread.create (render_thread<T>, (void*)&threadArg, 1);
#else	
    // render_thread<T> ((void*)&threadArg);
//	render_thread<T> ((void*)&threadArg);
#endif	
}


void
goSBlockRenderer<goInt8>::render (goArray<goSize_t> *indices)
{
    // rThread.cancel();   
	// rThread.join();
	this->cancel();  // sets REQUEST_THREAD_CANCEL flag and waits for the render thread to return.
	// 'tention ... only one render thread at a time may exist, since we have only one
	// thread arg and if another thread overwrites the old indices, the first thread
	// gets confused. Don't mix this up.
    threadArg.renderer = (void*)this;
    threadArg.indices         = *indices;
#ifdef MULTIPLE_THREADS	
    rThread.create (render_thread_8, (void*)&threadArg, 1);
#else	
	render_thread_8 ((void*)&threadArg);
#endif	
}

void
goSBlockRenderer<goUInt8>::render (goArray<goSize_t> *indices)
{
	// rThread.join();
	this->cancel();  // sets REQUEST_THREAD_CANCEL flag and waits for the render thread to return.
    threadArg.renderer = (void*)this;
    threadArg.indices         = *indices;
#ifdef MULTIPLE_THREADS	
    rThread.create (render_thread_u8, (void*)&threadArg, 1);
#else	
	render_thread_u8 ((void*)&threadArg);
#endif	
}

void
goSBlockRenderer<goInt16>::render (goArray<goSize_t> *indices)
{
	// rThread.join();
	this->cancel();  // sets REQUEST_THREAD_CANCEL flag and waits for the render thread to return.
    threadArg.renderer = (void*)this;
    threadArg.indices         = *indices;
#ifdef MULTIPLE_THREADS	
    rThread.create (render_thread_16, (void*)&threadArg, 1);
#else	
	render_thread_16 ((void*)&threadArg);
#endif	
}

void
goSBlockRenderer<goUInt16>::render (goArray<goSize_t> *indices)
{
	// rThread.join();
	this->cancel();  // sets REQUEST_THREAD_CANCEL flag and waits for the render thread to return.
    threadArg.renderer = (void*)this;
    threadArg.indices         = *indices;
#ifdef MULTIPLE_THREADS	
    rThread.create (render_thread_u16, (void*)&threadArg, 1);
#else	
	render_thread_u16 ((void*)&threadArg);
#endif	
}

template <class T>
void
goSBlockRenderer<T>::renderSubImages (goArray<goSize_t> *indices)
{

//#ifndef USE_SUBIMAGES    
#ifdef NO_SUBIMAGE_BLENDING
    tempImage->fill (0.0f);    // braucht nicht gemacht zu werden, da ich mit subimages arbeite
								  // Ausserdem gibts schwarze Bilder, wenn der Renderer abgebrochen wird.
								  // SUCKS.
#endif								  
    alphaImage->fill (1.0f);
//#endif    

    goSignal3D<T> *block;
    
    goSize_t i;
    goIndex_t x,y,z;

	goIndex_t invalidLines = 0;
	goIndex_t totalLines   = 0;

#if _GODEBUG >= 1
    goError::note("goSBlockRenderer::render()","Entering render loop");
    cout << "Number of indices: " << indices->getSize() << endl;
#endif
    setProgress (0.0f);
    signalProgress();
    // goFloat progressStep = 1.0 / ((float)indices->getSize() + 1);

    goViewVolume 	 *view;
    // view = (goViewVolume*)MRViews[resolution];
    view = &getViewPlane();
    // u vector in world coordinates
    go3Vector<volFloat> u = view->getU();
    // v vector in world coordinates
    go3Vector<volFloat> v = view->getV();
    u *= view->getPixelWidth();
    v *= view->getPixelHeight();
    volFloat rayDepth = view->getDepth();

    
    goSignal2D<volFloat> rayStepX(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> rayStepY(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> rayStepZ(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> clipStartX(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> clipStartY(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> clipStartZ(tempImage->getSizeX(), tempImage->getSizeY());
    
    goSignal2D<volFloat> planePointX(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> planePointY(tempImage->getSizeX(), tempImage->getSizeY());
    goSignal2D<volFloat> planePointZ(tempImage->getSizeX(), tempImage->getSizeY());

    go3Vector<volFloat> p, p2; // Startpunkt auf der Proj.ebene
    p2.x = view->getPosition().x - u.x * (view->getScreenSize().x * 0.5)
	- v.x * (view->getScreenSize().y * 0.5);
    p2.y = view->getPosition().y - u.y * (view->getScreenSize().x * 0.5)
	- v.y * (view->getScreenSize().y * 0.5);
    p2.z = view->getPosition().z - u.z * (view->getScreenSize().x * 0.5)
	- v.z * (view->getScreenSize().y * 0.5);
    go3Vector<volFloat> r;
    for (y = 0; y < (goIndex_t)rayStepX.getSizeY(); y++)
	{
	    p = p2;
	    for (x = 0; x < (goIndex_t)rayStepX.getSizeX(); x++)
		{
		    r = p;
		    r -= view->getEyePos();
		    r *= 1 / r.abs();
		    *planePointX.getPtr (x,y) = p.x;
		    *planePointY.getPtr (x,y) = p.y;
		    *planePointZ.getPtr (x,y) = p.z;
		    *rayStepX.getPtr (x,y) = r.x;
		    *rayStepY.getPtr (x,y) = r.y;
		    *rayStepZ.getPtr (x,y) = r.z;
		    *clipStartX.getPtr (x,y) = p.x;
		    *clipStartY.getPtr (x,y) = p.y;
		    *clipStartZ.getPtr (x,y) = p.z;
		    p.x += u.x;
		    p.y += u.y;
		    p.z += u.z;
		}
	    p2 += v;
	}

    
    for (i = 0; i < (goSize_t)indices->getSize(); i++)
	{
		goSize_t blockIndex = (*indices)[i];
	    while ((!pm->isPresent(blockIndex)) || pm->isScheduled(blockIndex))
		{
#ifdef MULTIPLE_THREADS
			// cancellation point according to pthreads standard -- don't cancel!
		    waitProduction();
#else
		    cout << getClassName() << ": Block not yet present" << endl;
			break;
#endif
		}
	   
	   /*
	    * Thread flags (goBlockRenderer) bearbeiten
		*/
	   if (REQUEST_THREAD_CANCEL)
	   {
		   // Kontrolliert abbrechen, kein Block ist gelockt
		   return;
	   }
	    
	    /*
	     * Get this block
	     */ 
	    block = getBlockProvider()->getBlock(blockIndex);		// Get block pointer
	    // DEBUGGING ONLY!
	    if (block == 0)
		{
		    goString s;
		    char num[255];
		    s = "Block ";
		    sprintf(&num[0],"%d",(*indices)[i]);
		    s += &num[0];
		    s += " seems not to be in the block store!";
		    goError::print("goSBlockRenderer::renderSubImages()",s);
			getBlockProvider()->releaseBlock (blockIndex);
		    break;
		}
		
	    //setProgress (getProgress() + progressStep);
	    //signalProgress();
	    
	    /*
	     * Get subimage object, resolution, scaled viewplane and tempimage
	     */
	    goSubImage   *subImage 	  = (goSubImage*)subImages[blockIndex];
	    if (subImages.fail())
		{
		    goError::print("goSBlockRenderer::renderSubImages()","subImage hash table access failed!!!!!");
		    return;
		}
	    // goInt32 resolution	= resolutionManager->getResolution (blockIndex);
	    
	    // goSignal2D<volFloat> *MRImage;
	    // MRImage = (goSignal2D<volFloat>*)MRImages[resolution];
	    int resDiff 	= subImage->resDiff;
	    goDouble resDiffD_1 = 1.0f / (float)(1 << resDiff);
			    
	    /*
	     * Calculate the start point on the viewplane, in world coordinates.
	     */

	    go3Vector<volFloat> planePointSave;
	    go3Vector<volFloat> tempV;
	    planePointSave	= view->getPosition();
		
	    tempV		= u;		// take the original U and V values here
	    tempV		*= subImage->firstXHi;
	    planePointSave	+= tempV;

	    tempV		= v;
	    tempV		*= subImage->firstYHi;
	    planePointSave	+= tempV;
	    // cout << "plane point (start): " << planePointSave << endl;

		

	    // Debugging output --- usually commented out
#if 0
	    if ((*indices)[i] == 0)
		{
			cout << "size block 0: " << block->getSizeX() << "," << block->getSizeY() << "," << block->getSizeZ() << endl;
			int r,k;
			for (r = -1; r <= (int)block->getSizeY(); r++)
			{
				for (k = -1; k <= (int)block->getSizeX(); k++)
				{
					cout << *block->getPtr(k,r,-1) << " ";
				}
				cout << endl;
			}
			cout << endl;
		}
#endif		
		
	    
	    go4Vector<volFloat> blockStartPos = subImage->blockStartPos;	// Previously calculated start position in scaled world coords.
		
	    volFloat tempAlphaVal;
	    volFloat tempImageVal;
	    // goPtrdiff_t blockDiff;
	    go3Vector<volFloat> planePoint;
	    go3Vector<volFloat> start,end;
	    go3Vector<volFloat> rayStep;
	    bool lineIsValid;
	    
#ifdef USE_SUBIMAGES
	    goIndex_t xMax = subImage->width;
	    goIndex_t yMax = subImage->height;
	    goIndex_t yMin = 0;
	    goIndex_t xMin = 0;
#else
	    goIndex_t xMin = subImage->firstX + (goInt32)(view->getSize().x * 0.5);
	    goIndex_t yMin = subImage->firstY + (goInt32)(view->getSize().y * 0.5);
//	    goIndex_t xMin = 0;
//	    goIndex_t yMin = 0;
	    goIndex_t xMax = xMin + subImage->width;
	    goIndex_t yMax = yMin + subImage->height;
	    
	    goIndex_t subX = 0;
	    goIndex_t subY = 0;
#endif
	    go3Vector<goInt32> blockSize;
	    //volFloat xStepScaled = xStep * resDiffD_1;
	    //volFloat yStepScaled = yStep * resDiffD_1;
	    //volFloat zStepScaled = zStep * resDiffD_1;

	    goDouble rayStartCorrectionX = sampleDistanceX_1 * resDiffD_1;
	    goDouble rayStartCorrectionY = sampleDistanceY_1 * resDiffD_1;
	    goDouble rayStartCorrectionZ = sampleDistanceZ_1 * resDiffD_1;

	    // Copy big alpha data area to subimage alpha data
	    // copyImageAlpha (alphaImage, subImage);
	    goIndex_t alphaX;
		goIndex_t alphaXMax = tempImage->getSizeX();
	    goIndex_t alphaY;
		goIndex_t alphaYMax = tempImage->getSizeY();

		// cout << getClassName() << ": trying to render block " << blockIndex << " Ptr " << block << endl;
		// ((goBlockStore<T>*)getBlockProvider())->_dumpBlockInfo(blockIndex);
	    
// 	    volFloat rayDepth_1 = 1.0f / rayDepth;

	    // Predefine some constants
	    volFloat eyeDistance_1 = 1 / (volFloat)view->getEyeDistance();
	    volFloat rayDepth_1    = 1 / (volFloat)view->getDepth();

	    GO_3D_CLIPLIANGBARSKY_START()
		// FOR (The bounding box on the view plane) DO
#ifdef USE_SUBIMAGES
	    for (y = yMin, alphaY = subImage->topImage; (y < yMax) && (alphaY < alphaYMax) && 
													(alphaY >= 0); y++, alphaY++)
#else
	    for (y = yMin, subY = 0; y < yMax; y++, subY++)
#endif						    
		    {
			planePoint = planePointSave;
#ifdef USE_SUBIMAGES
			for (x = xMin, alphaX = subImage->leftImage; (x < xMax) && (alphaX < alphaXMax) &&
														 (alphaX >= 0); x++, alphaX++)
#else			    
		        for (x = xMin, subX = 0; (x < xMax); x++, subX++)
#endif			
			    {
#ifdef USE_SUBIMAGES
				//tempAlphaVal = 1.0f;
				// tempAlphaVal = *subImage->alphaData->getPtr(x,y);
				tempAlphaVal = *alphaImage->getPtr (alphaX,alphaY);
#ifdef NO_SUBIMAGE_BLENDING
				tempImageVal = *tempImage->getPtr (alphaX,alphaY);
#else								
				tempImageVal = 0.0f;
#endif				
#else
				tempImageVal = *MRImage->getPtr(x,y);
				tempAlphaVal = *subImage->alphaData->getPtr(subX,subY);
#endif
				if (tempAlphaVal > 0.0f)
				{
				    // rayStep.x = *rayStepX.getPtr (alphaX, alphaY);
				    // rayStep.y = *rayStepY.getPtr (alphaX, alphaY);
				    // rayStep.z = *rayStepZ.getPtr (alphaX, alphaY);

				    //rayStep = planePoint;
				    //rayStep -= view->getEyePos();
				    // rayStep *= 1 / rayStep.abs();
				    // rayStep *= rayDepth / view->getEyeDistance();
				    
				    // rayStep *= 1 / rayStep.abs();
				    go3Vector<volFloat> clipStart;
				    //clipStart = rayStep;
				    //clipStart *= view->getFrontClip();
				    //clipStart += view->getEyePos();
				    //clipStart = planePoint;
				    //clipStart.x = *clipStartX.getPtr (alphaX, alphaY);
				    //clipStart.y = *clipStartY.getPtr (alphaX, alphaY);
				    //clipStart.z = *clipStartZ.getPtr (alphaX, alphaY);
					rayStep = planePoint;
					rayStep -= view->getEyePos();
					clipStart = rayStep;
					clipStart *= view->getFrontClip() * eyeDistance_1;
					clipStart += view->getEyePos();
					rayStep *= 1 / rayStep.abs();
					rayStep *= rayDepth;
				    // rayStep *= rayDepth / view->getEyeDistance(); // (view->getEyeDistance() - view->getFrontClip() + rayDepth);
				    lineIsValid = false;
				    GO_3D_CLIPLIANGBARSKY((blockStartPos.x - 0.5), (blockStartPos.x - 0.5 +
									       xStep ),
							  (blockStartPos.y - 0.5), (blockStartPos.y - 0.5 +
									       yStep ),
							  (blockStartPos.z - 0.5), (blockStartPos.z - 0.5 +
									       zStep ),
							  rayStep, clipStart,
							  start, end, lineIsValid);
				    totalLines++;
				    if (!lineIsValid)
					{
					    invalidLines++;
					    //cout << "Invalid line: " << endl;
					    //cout << "rayStep = " << rayStep << endl;
					    //cout << "Planepoint = " << planePoint << endl;
					    //char c;
					    //cin >> c;
					}
				    else
					{
					    // rayStep *= 1 / (rayStep.abs());
					    rayStep *= rayDepth_1;
					    end.x -= start.x;
					    end.y -= start.y;
					    end.z -= start.z;
					    goDouble dz = end.abs() * resDiffD_1;
					    z = (goIndex_t)(dz);
					    volFloat l = 1; // (1 << resDiff);
					    volFloat lLast = (dz - z) * l;     // Last sample length
					    
					    // Calculate the relative start in a block and store it in start:
					    start += clipStart; // planePoint;
					    start.x -= blockStartPos.x;
					    start.y -= blockStartPos.y;
					    start.z -= blockStartPos.z;
					    
					    start.x *= rayStartCorrectionX;
					    start.y *= rayStartCorrectionY;
					    start.z *= rayStartCorrectionZ;
					    
					    rayStep.x *= sampleDistanceX_1;
					    rayStep.y *= sampleDistanceY_1;
					    rayStep.z *= sampleDistanceZ_1;
					    
					    volFloat alpha;
					    volFloat fm;
					    volFloat gm;
					    T val;
					    
					    //cout << "dz = " << dz << endl;
					    //cout << "z = " << z << endl;
					    //cout << "lLast = " << lLast << endl;
					    
					    for ( ; (z > 0); z--)
						{
						    // Akkumulieren
						    
						    // Sampling
							/*
						    // val = (T)block->sample (start);
						    val = (T)block->sample (start);
						    // val = 1;
						    alpha = l * alphaFunction[val];
						    //fm = 1 - (pow (1 - alpha, resDiff + 1));
						    //gm = alpha != 0 ? fm / alpha : 1.0f;
								tempImageVal += colorFunction[val] * alpha * tempAlphaVal; //  * gm;
								// tempImageVal = 1;
								tempAlphaVal *= (1 - alpha); // pow((1 - alpha), resDiff + 1);
								*/
#ifdef _GODEBUG_SBLOCKRENDERER_SEGFAULT
								SFxSample = start.x;
								SFySample = start.y;
								SFzSample = start.z;
								SFx = x;
								SFy = y;
								SFz = z;
								SFxAlpha = alphaX;
								SFyAlpha = alphaY;
								SFblock  = blockIndex;
#endif
#if 1
								val = (T)block->sample (start);
								alpha = alphaFunction[val];
								// This is highly inefficient.
								// Unfortunately, it is necessary
								// to implement alpha correction
								// and I don't have enough time
								// left to optimize it.
								// This is only needed if
								// different resolution blocks
								// are rendered into the same image.
								// volFloat p =  pow(1 - alpha, (float)(1 << resDiff)); 
								volFloat p = powFuncs.get(resDiff,1-alpha);
						    	fm = 1 - p;
						    	gm = alpha != 0 ? fm / alpha : 1.0f;
								/* ------ Experiment alpha correction */
								//p = 1 - alpha; fm = 1; gm = 1;
								/* ------ */
								tempImageVal += colorFunction[val] * alpha * gm * tempAlphaVal;
								tempAlphaVal = tempAlphaVal * p;
#else
								alpha = block->sample(start,alphaFunction);
								volFloat p = powFuncs.get(resDiff,1-alpha);
							    fm = 1 - p;
							    gm = alpha != 0 ? fm / alpha : 1.0f;
								tempImageVal += block->sample(start,colorFunction) * alpha * gm * tempAlphaVal;
								tempAlphaVal = tempAlphaVal * p;
								
#endif
								  // Funzt
								// alpha = l * alphaFunction[val];
								// tempImageVal += l * colorFunction[val] * (1 - tempAlphaVal); //  * gm;
								// tempAlphaVal += alpha * (1 - tempAlphaVal); // pow((1 - alpha), resDiff + 1);
								// MIP
								//if (tempImageVal < l * colorFunction[val])
								//    tempImageVal = l * colorFunction[val];
								start.x += rayStep.x;
								start.y += rayStep.y;
								start.z += rayStep.z;
							}
							// Last sample
							/*
							// val   = (T)block->sample (start);
							val = (T)block->sample (start);
							alpha = lLast * alphaFunction[val]; // * (1 << resDiff);
							//fm = 1 - (pow (1 - alpha, resDiff + 1));
							//gm = alpha != 0 ? fm / alpha : 1.0f;
							tempImageVal += colorFunction[val] * alpha * tempAlphaVal;
							// tempImageVal += lLast * colorFunction[val];
							tempAlphaVal *= (1 - alpha); // pow ((1 - alpha), resDiff + 1);
							*/
#ifdef _GODEBUG_SBLOCKRENDERER_SEGFAULT
							SFxSample = start.x;
							SFySample = start.y;
							SFzSample = start.z;
							SFx = x;
							SFy = y;
							SFz = z;
							SFxAlpha = alphaX;
							SFyAlpha = alphaY;
							SFblock  = blockIndex;
#endif
							// Choose between interpolation and then using TFs and
							// using TFs while sampling (slower, Gasparakis)
#if 1
						    val = (T)block->sample (start);
							alpha = alphaFunction[val];
							//volFloat p =  pow (1 - lLast*alpha, (float)(1 << resDiff)); 
							volFloat p = powFuncs.get(resDiff,1-lLast*alpha);
						    fm = 1 - p;
						    gm = alpha != 0 ? fm / (lLast*alpha) : 1.0f;
							/* ------ Experiment alpha correction */
							//p = 1 - lLast * alpha; fm = 1; gm = 1;
							/* ------ */
							tempImageVal += lLast * colorFunction[val] * alpha * gm * tempAlphaVal;
							tempAlphaVal = tempAlphaVal * p;
#else
							alpha = block->sample(start,alphaFunction);
							volFloat p = powFuncs.get(resDiff,1-lLast*alpha);
						    fm = 1 - p;
						    gm = alpha != 0 ? fm / (lLast*alpha) : 1.0f;
							tempImageVal += lLast * block->sample(start,colorFunction) * alpha * gm * tempAlphaVal;
							tempAlphaVal = tempAlphaVal * p;
#endif
							  // Funzt
							// alpha = lLast * alphaFunction[val]; // * (1 << resDiff);
							// tempImageVal += lLast * colorFunction[val] * (1 - tempAlphaVal);
							// tempAlphaVal += alpha * (1 - tempAlphaVal); // pow ((1 - alpha), resDiff + 1);
							
							
#ifdef USE_SUBIMAGES
#ifdef NO_SUBIMAGE_BLENDING
							*tempImage->getPtr(alphaX,alphaY) = tempImageVal;
#else														
							*subImage->imageData->getPtr(x,y) = tempImageVal;
#endif							
							// FALSCH, NUR TEST
							// *alphaImage->getPtr (xFinal, yFinal) = tempAlphaVal;
#else
							if ( (x >= 0) && (y >= 0) &&
							 (x < MRImage->getSizeX()) && (y < MRImage->getSizeY()) )
							{
								*MRImage->getPtr (x,y) = tempImageVal;
							}
#endif
						} // lineIsValid (hopefully...)
					} // alphaValue > x
#ifdef USE_SUBIMAGES
					// *subImage->alphaData->getPtr(x,y) = tempAlphaVal;
					*alphaImage->getPtr (alphaX, alphaY) = tempAlphaVal;
#else 					
					*subImage->alphaData->getPtr (subX, subY) = tempAlphaVal;
					
#endif					
					planePoint += u;
				}
			planePointSave += v;
			}
		// Copy the new alpha values into the alpha image, scaled as needed
#ifdef USE_SUBIMAGES		    
		// copyAlphaImage (subImage, alphaImage);
#else
		copyAlphaImage (subImage, alphaImage);
		//char c;
		//cin >> c;
#endif		    
		GO_3D_CLIPLIANGBARSKY_END();
		// Does not have a meaning yet, remove

		getBlockProvider()->releaseBlock(blockIndex);
	} // for all indices
	cout << "Total lines: " << totalLines << "\tInvalid lines: " << invalidLines << endl;
}



	// Copy big alpha image to subimage alpha data
static inline void copyImageAlpha (goSignal2D<volFloat>* alphaImage, goSubImage *subImage)
{
	volFloat px = (subImage->lastXHi - subImage->firstXHi) / (float)(subImage->width - 1);
	volFloat py = (subImage->lastYHi - subImage->firstYHi) / (float)(subImage->height - 1);
	volFloat subX, subY;
	goIndex_t x,y;
	subY = subImage->topImage;

	for (y = 0; (y < (goIndex_t)subImage->height); y++)
	{
		subX = subImage->leftImage;
		for (x = 0; (x < (goIndex_t)subImage->width); x++)
		{
			volFloat C;
			alphaImage->sample (subX, subY, C);
			*subImage->alphaData->getPtr (x,y) = C;
			subX += px;
		}
		subY += py;
	}
}

	// Copy sub image alpha data to big image
static inline void copyAlphaImage (goSubImage *subImage, goSignal2D<volFloat>* alphaImage)
{
	volFloat px = (subImage->width - 1) / (float)(subImage->lastXHi - subImage->firstXHi);
	volFloat py = (subImage->height - 1) / (float)(subImage->lastYHi - subImage->firstYHi);
	volFloat subX, subY;
	goIndex_t x,y;
	subY = 0;

	for (y = subImage->topImage; (y <= subImage->bottomImage) && (y < (goIndex_t)alphaImage->getSizeY()); y++)
	{
	subX = 0;
	for (x = subImage->leftImage; (x <= (subImage->rightImage)) && (x < (goIndex_t)alphaImage->getSizeX()); x++)
	{
		volFloat C;
		subImage->alphaData->sample (subX, subY, C);
		*alphaImage->getPtr (x,y) = C;
		subX += px;
	}
	subY += py;
	}
}

static inline volFloat bilinear (volFloat A, volFloat B, volFloat C, volFloat D, volFloat px, volFloat py)
{
	volFloat p1 = A + ((B - A)*px);
	volFloat p2 = C + ((D - C)*px);
	return (p1 + ((p2 - p1)*py));
}

static inline volFloat bilinearSample (goSubImage *subImage, volFloat subX, volFloat subY)
{
	goSize_t x = (goSize_t)subX;
	goSize_t y = (goSize_t)subY;
	volFloat px = subX - x;
	volFloat py = subY - y;
	volFloat A,B,C,D;

	A = *subImage->imageData->getPtr (x,y);
	B = *subImage->imageData->getPtr (x+1,y);
	C = *subImage->imageData->getPtr (x, y+1);
	D = *subImage->imageData->getPtr (x + 1, y + 1);
	return bilinear (A,B,C,D,px,py);
}

static inline void bilinearSample2 (goSubImage *subImage, volFloat subX, volFloat subY, volFloat& Color, volFloat& Alpha)
{
	int x = (int)subX;
	int y = (int)subY;
	volFloat px = subX - x;
	volFloat py = subY - y;
	volFloat A,B,C,D;

	A = *subImage->imageData->getPtr (x,y);
	B = *subImage->imageData->getPtr (x+1,y);
	C = *subImage->imageData->getPtr (x, y+1);
	D = *subImage->imageData->getPtr (x + 1, y + 1);
	Color = bilinear (A,B,C,D,px,py);
	A = *subImage->alphaData->getPtr (x,y);
	B = *subImage->alphaData->getPtr (x+1,y);
	C = *subImage->alphaData->getPtr (x, y+1);
	D = *subImage->alphaData->getPtr (x + 1, y + 1);
	Alpha = bilinear (A,B,C,D,px,py);
}

static inline volFloat bilinearSampleAlpha (goSubImage *subImage, volFloat subX, volFloat subY)
{
	goSize_t x = (goSize_t)subX;
	goSize_t y = (goSize_t)subY;
	volFloat px = subX - x;
	volFloat py = subY - y;
	volFloat A,B,C,D;

	A = *subImage->alphaData->getPtr (x,y);
	B = *subImage->alphaData->getPtr (x+1,y);
	C = *subImage->alphaData->getPtr (x, y+1);
	D = *subImage->alphaData->getPtr (x + 1, y + 1);
	return bilinear (A,B,C,D,px,py);
}

/*
 * Smoothly blend sub image subImage into the final image.
 * @param subImage     Pointer to the goSubImage to blend
 * @param image        Pointer to the goSignal2D<volFloat> representing the intermediate image to blend subImage into
 * @param alphaImage   Pointer to the goSignal2D<volFloat> representing the alpha image to blend subImage into
 * @param w	       Width of the intermediate image
 * @param h	       Height of the intermediate image
 * @param sampleWidth  Sample distance in x direction on the target image
 * @param sampleHeight Sample distance in y direction on the target image
 * @todo Replace slow getPtr() calls to goSignal2D objects
 */
static inline void blendSubImage (goSubImage *subImage, goSignal2D<volFloat> *image, 
				  goSignal2D<volFloat> *alphaImage, 
				  goSize_t w, goSize_t h, 
				  volFloat sampleWidth, volFloat sampleHeight)
{

		/*
		 * Steps in x and y directions on the sub image.
		 */
		volFloat dx = (subImage->width - 1) / (double)(subImage->lastXHi - subImage->firstXHi);
		volFloat dy = (subImage->height - 1) / (double)(subImage->lastYHi - subImage->firstYHi);
		/*
		 * Position on the whole image
		 */ 
		goIndex_t x,y;
		volFloat C;
		/*
		 * Position on the sub image
		 * Hier muss eventuell eine Korrektur addiert werden!
		 */
		volFloat subX	    = 0;
		volFloat subXStart  = 0; // (subImage->left - ceil(subImage->left));
		volFloat subY	    = 0;
		volFloat subYStart  = 0; // -0.5 * subImage->resDiff; // (subImage->top - ceil(subImage->top));
		
		
		goInt32 xFinalStart;
		goInt32 yFinalStart;
		goInt32 xFinalEnd;
		goInt32 yFinalEnd;
		xFinalStart = subImage->leftImage;
		xFinalEnd   = subImage->rightImage + 1;
		yFinalStart = subImage->topImage;
		yFinalEnd   = subImage->bottomImage + 1;
		subY = subYStart;
	for (y = yFinalStart; (y < yFinalEnd) && (y < (goIndex_t)h); y++)
	{
		subX = subXStart;
		for (x = xFinalStart; (x < xFinalEnd) && (x < (goIndex_t)w); x++)
		{
			C = bilinearSample (subImage, subX, subY);
			*image->getPtr(x, y) += C ; // * (1 << subImage->resDiff);
			subX += dx;
		}
		subY += dy;
	}
}

template <class T>
void
goSBlockRenderer<T>::compositeMRImages ()
{
	goInt32 res = resolutionManager->getMinResolution();
	goInt32 x, y;
	volFloat subX, subY;
	// Schrittweiten fuer Interpolation auf MR Bildern
	volFloat dx = 1.0f, dy = 1.0f;                 // Skalierungsfaktor war immer 0.5

	go4Vector<volFloat> commonPoint;   // common point in Hi coordinates
	go44Matrix<volFloat> proj;
	proj = getViewPlane().getViewMatrix();
	commonPoint.x = 0; commonPoint.y = 0; commonPoint.z = 0;   // Noch ändern, abhängig von Blickrichtung
	commonPoint.t = 1.0f;
	
	goInt32 j;
	for (j = res; j >= 0; j--)
	{
		goViewVolume	     *view;
		view = (goViewVolume*)MRViews[j];
		proj    = view->getViewMatrix();
		goSignal2D<volFloat>* MRImage;
		MRImage = (goSignal2D<volFloat>*&)(MRImages[j]);
		cout << "Scale " << j << " size " << MRImage->getSizeX() << " x " << MRImage->getSizeY() << endl;
		subY = -0.5f;
		for (y = 0; y < (goInt32)tempImage->getSizeY(); y++)
		{
			subX = -0.5f;
			for (x = 0; x < (goInt32)tempImage->getSizeX(); x++)
			{
				volFloat temp = 0;
				MRImage->sample (subX, subY, temp);
				*tempImage->getPtr(x,y) += temp;
				subX += dx;
			}
			subY += dy;
		}
		dx *= 0.5f;
		dy *= 0.5f;
	}
}

template <class T>
void
goSBlockRenderer<T>::compositeSubImages (goArray<goSize_t> *indices)
{
		/*
		 * TEST VERSION: just add everything 
		 * The final version needs to be much more sophisticated... think about this problem (alpha!)
		 * Make the renderer as much independent of this problem as possible!
		 */
		goSize_t i;
		goSubImage *subImage;

		tempImage->fill (0.0f);
		clock_t mean_clock = 0;
		for (i = 0; i < (goSize_t)indices->getSize(); i++)
		{
			subImage = (goSubImage*)subImages[(*indices)[i]];
#if 0
			goString s;
			s = getClassName();
			s += "::compositeSubImages()";
			s += " sub image at (h/v) ";
			char c[255];
			sprintf(&c[0], "%f", subImage->left);
			s += &c[0];
			s += "->";
			sprintf(&c[0], "%f", subImage->right);
			s += &c[0];
			s += "/";
			sprintf(&c[0], "%f", subImage->top);
			s += &c[0];
			s += "->";
			sprintf(&c[0], "%f", subImage->bottom);
			s += &c[0];
			cout << s << "\n";
#endif
			
			/*
			 * Ineffizient, aber nur ein Test :-)
			 */
	// 	    cout << "subImage->height = " << subImage->height << "\n";
	// 	    cout << "subImage->width = " << subImage->width << "\n";
	// 	    cout << "xFinalSave = " << xFinalSave << "\n";
	// 	    cout << "yFinal = " << yFinal << "\n";

#if 1
		clock_t t1,t2;	
		t1 = clock();
		blendSubImage (subImage, tempImage, alphaImage, getImageWidth(), getImageHeight(),
				   getViewPlaneSampleWidth(), getViewPlaneSampleHeight());
		t2 = clock();
		mean_clock += (t2 - t1);
#else
			goIndex_t x, y, xFinal, yFinal, xFinalSave;
			volFloat left = subImage->firstX;
			volFloat top = subImage->firstY;
			xFinalSave = (goIndex_t)left;
			yFinal = (goIndex_t)top;
			
			for (y = 0; (y < (goIndex_t)subImage->height) && (yFinal < (goIndex_t)tempImage->getSizeY()) && (yFinal >= 0) && (yFinal <= subImage->lastY); y++)
			{
				xFinal = xFinalSave;
				for (x = 0; (x < (goIndex_t)subImage->width) && (xFinal >= 0) && (xFinal <= subImage->lastX) && (xFinal <
						(goIndex_t)tempImage->getSizeX()); x++)
				{
					//goSize_t imageAddress = getImageWidth() * yFinal + xFinal;
					if (*alphaImage->getPtr(xFinal,yFinal) > 0.0f)
					{
				   	 	*tempImage->getPtr(xFinal,yFinal) += *subImage->imageData.getPtr(x,y);
					}
			    	xFinal++;
				}
		    	yFinal++;
			}
#endif
	}
	
		cout << getClassName() << "::compositeSubImages(): Mean time to blend image:";
		cout << mean_clock / (float)(CLOCKS_PER_SEC * indices->getSize()) << "s\n";
}


// FIXME Diese Routine ist wahrscheinlich zu langsam fuer sehr viele Bilder.
template <class T>
void 
goSBlockRenderer<T>::initSubImages (goArray<goSize_t>* indices)
{

    goIndex_t i;
    volFloat    Z = xBlocks * yBlocks;
    // volFloat _1_Z = 1.0f / Z;
	volFloat    Y = xBlocks;
    // volFloat _1_Y = 1.0f / Y;
    goSize_t x,y,z;
    
    goSubImage* subImage;
    
    go4Vector<volFloat> volumeStartPos;
    volumeStartPos.x = getPosition().x;
    volumeStartPos.y = getPosition().y;
    volumeStartPos.z = getPosition().z;
    volumeStartPos.t = 1.0f;
    go3Vector<volFloat> size = getSize();
    
    volumeStartPos.x -= (size.x) * 0.5f * sampleDistanceX;
    volumeStartPos.y -= (size.y) * 0.5f * sampleDistanceY;
    volumeStartPos.z -= (size.z) * 0.5f * sampleDistanceZ;

    go3Vector<volFloat> blockSize;
    
    int minResolution = resolutionManager->getMinResolution();
    int resolution;
    int resDiff;
    volFloat resDiffD = 0;
    volFloat resDiffD_1 = 0;
    GO_VREXP_FINDBOUNDS_START()
    for (i = 0; i < indices->getSize(); i++)
	{
	    /*
	     * Calculate bounds on the final picture of the current block
	     */
	    /******************************************************************************************/
	    goSize_t 		blockIndex 	= (*indices)[i];
	    resolution = resolutionManager->getResolution (blockIndex);

	    goViewVolume 	*view;
	    view = (goViewVolume*)(MRViews[resolution]);
	    // Vorsicht, bissiger copy constructor. Ich hasse copy Kontruktoren. Ich hasse sie von ganzem Herzen.
	    go44Matrix<volFloat> projMatrix;
	    projMatrix	= view->getViewMatrix(); 	

	    resDiff	= minResolution - resolution;
	    resDiffD	= (volFloat)(1 << resDiff);
	    resDiffD_1	= 1.0f / resDiffD;
	    blockSize.x = (volFloat)getBlockProvider()->getBlockSize().x * resDiffD_1;
	    blockSize.y = (volFloat)getBlockProvider()->getBlockSize().y * resDiffD_1;
	    blockSize.z = (volFloat)getBlockProvider()->getBlockSize().z * resDiffD_1;

	    	    // Bounding box auf der viewplane berechnen
	    go4Vector<volFloat> blockStartPos;
	    go4Vector<volFloat> blockStartPosHi;

	    blockStartPos    = volumeStartPos;
 	    blockStartPosHi  = volumeStartPos;
 	    blockStartPos.x *= resDiffD_1; 
 	    blockStartPos.y *= resDiffD_1;
 	    blockStartPos.z *= resDiffD_1;
	    
	    // Find out block position
		// Slow but precalculated values seem to add some error so it used to crash here sometimes
	    volFloat 		tempD = blockIndex / Z;
	    z 				= (goSize_t)floor(tempD);
	    volFloat tempD2 = blockIndex - z * Z;
	    y 				= (goSize_t)floor((tempD2 / Y));
	    x 				= (goSize_t)(tempD2 - y * Y);

	    volFloat xStepCorr = xStep * resDiffD_1;
	    volFloat yStepCorr = yStep * resDiffD_1;
	    volFloat zStepCorr = zStep * resDiffD_1;
	    // World coordinates of current block's 0th corner
	    // Move the block's corner for -0.5 in each direction (samples in the middle of each block)
	    blockStartPos.x   += x * xStepCorr - 0.5;
	    blockStartPos.y   += y * yStepCorr - 0.5;
	    blockStartPos.z   += z * zStepCorr - 0.5;
	    blockStartPos.t    = 1;
 	    blockStartPosHi.x += x * xStep - 0.5;
 	    blockStartPosHi.y += y * yStep - 0.5;
 	    blockStartPosHi.z += z * zStep - 0.5;
 	    blockStartPosHi.t  = 1;

//	    cout << "Blockstartpos: " << 
//		blockStartPos.x << "," << blockStartPos.y << "," << blockStartPos.z << endl;
    
	    volFloat left, right, top, bottom;
	    // Find the bounding box on the final picture for this block:

//	    cout << "Finding bounds on MR plane" << endl;	   
	    GO_VREXP_FINDBOUNDS_2(blockStartPos, left, right, top, bottom, xStepCorr, yStepCorr, zStepCorr, projMatrix);
  	    blockStartPos.x += 0.5;
  	    blockStartPos.y += 0.5;
  	    blockStartPos.z += 0.5;
		
	    
//	    cout << "Finding bounds on final plane" << endl;
 	    volFloat leftHi, rightHi, topHi, bottomHi;
//		cout << "xStep = " << xStep << endl;
//		cout << "yStep = " << yStep << endl;
//		cout << "zStep = " << zStep << endl;
 	    GO_VREXP_FINDBOUNDS_2(blockStartPosHi, leftHi, rightHi, topHi, bottomHi, xStep, yStep, zStep, Tproj);
   	    blockStartPosHi.x += 0.5;
 	    blockStartPosHi.y += 0.5;
   	    blockStartPosHi.z += 0.5;
	
	    volFloat pixelPerU = 1.0f / view->getPixelWidth();
	    volFloat pixelPerV = 1.0f / view->getPixelHeight();
 	    rightHi  *= pixelPerU;
 	    leftHi   *= pixelPerU;
 	    topHi    *= pixelPerV;
 	    bottomHi *= pixelPerV;
	    
	    // Sollte eigentlich nur gemacht werden, wenn die Projektion auf dem Ebenenursprung liegt.
	    if (ceil(rightHi) == floor (rightHi))
 		{
// 			cout << "Katastrophenfall: rightHi = " << rightHi << endl;
		    if (rightHi == 0)
			rightHi -= 0.1;
 		}
	    if (ceil(bottomHi) == floor (bottomHi))
		{
		    //			cout << "Katastrophenfall: bottomHi = " << bottomHi << endl;
		    if (bottomHi == 0)
			bottomHi -= 0.1;
		}
	    
	    if (ceil(right) == floor (right))
		{
		    //			cout << "Katastrophenfall: right = " << right << endl;
		    if (right == 0)
			right -= 0.1;
		}
	    if (ceil(bottom) == floor (bottom))
		{
		    //			cout << "Katastrophenfall: bottom = " << bottom << endl;
		    if (bottom == 0)
			bottom -= 0.1;
		}
	    
	    //    	    left   = leftHi   * resDiffD_1;	    
	    //   	    right  = rightHi  * resDiffD_1;	    
	    //    	    top    = topHi    * resDiffD_1;	    
	    //    	    bottom = bottomHi * resDiffD_1;	    
	    
	    // Fuer gleichmaessige Abtastung. Ungleichmaessige gibt Artefakte (keine Zeit mehr dafuer)			
	    left   = leftHi;	    
	    right  = rightHi;	    
    	top    = topHi;	    
     	bottom = bottomHi;    
	
		volFloat Xborder = (getViewPlane().getSize().x - 1) * 0.5;	
		volFloat Yborder = (getViewPlane().getSize().y - 1) * 0.5;	
		
		// Zu Sicherheit -- braucht viel Zeit :(
		if (left < -Xborder)
		{
			left = -Xborder;
		} else
		{
			if (left > Xborder)
				left = Xborder;
		}
		if (right < -Xborder)
		{
			right = -Xborder;
		} else
		{
			if (right > Xborder)
				right = Xborder;
		}
		if (top < -Yborder)
		{
			top = -Yborder;
		} else
		{
			if (top > Yborder)
				top = Yborder;
		}
		if (bottom < -Yborder)
		{
			bottom = -Yborder;
		} else
		{
			if (bottom > Yborder)
				bottom = Yborder;
		}
		if (leftHi < -Xborder)
		{
			leftHi = -Xborder;
		} else
		{
			if (leftHi > Xborder)
				leftHi = Xborder;
		}
		if (rightHi < -Xborder)
		{
			rightHi = -Xborder;
		} else
		{
			if (rightHi > Xborder)
				rightHi = Xborder;
		}
		if (topHi < -Yborder)
		{
			topHi = -Yborder;
		} else
		{
			if (topHi > Yborder)
				topHi = Yborder;
		}
		if (bottomHi < -Yborder)
		{
			bottomHi = -Yborder;
		} else
		{
			if (bottomHi > Yborder)
				bottomHi = Yborder;
		}
		
		
	    volFloat scaleFactorX = (rightHi - leftHi) / (right - left);
	    volFloat scaleFactorY = (bottomHi - topHi) / (bottom - top);

	    goIndex_t subImageWidth, subImageHeight;
	    subImageWidth = (goSize_t) ( (floor(right) - ceil(left) + 1) );
	    subImageHeight = (goSize_t) ( (floor(bottom) - ceil(top) + 1) );
	    /******************************************************************************************/
	    /*
	     * Create the sub image as needed
	     */
	    /******************************************************************************************/
	    subImage = (goSubImage*)subImages[(*indices)[i]];
	    if (!subImages.fail())
		{
		    subImage = (goSubImage*)subImages.remove ((*indices)[i]);
		    delete subImage;
		}
		{
		    subImage = new goSubImage (subImageWidth, subImageHeight);
					       
		    subImage->left   = left;
		    subImage->right  = right;
		    subImage->top    = top;
		    subImage->bottom = bottom;

		    /*
		     * Translate bounds so that upper left corner of the viewplane is (0,0)
		     */
		    subImage->firstX	  = (goInt32) ceil(left);
		    subImage->firstY	  = (goInt32) ceil(top);
		    subImage->lastX	  = (goInt32) floor(right);
		    subImage->lastY	  = (goInt32) floor(bottom);
		    
 		    subImage->firstXHi	  = (goInt32) ceil (leftHi);
 		    subImage->lastXHi	  = (goInt32) floor (rightHi);
 		    subImage->firstYHi	  = (goInt32) ceil (topHi);
 		    subImage->lastYHi	  = (goInt32) floor (bottomHi);

		    subImage->leftImage   = subImage->firstXHi + (goInt32)((getViewPlane().getScreenSize().x) * 0.5);
		    subImage->rightImage  = subImage->lastXHi  + (goInt32)((getViewPlane().getScreenSize().x) * 0.5);
		    subImage->topImage    = subImage->firstYHi + (goInt32)((getViewPlane().getScreenSize().y) * 0.5);
		    subImage->bottomImage = subImage->lastYHi  + (goInt32)((getViewPlane().getScreenSize().y) * 0.5);
			

		    subImage->scaleFactorX = scaleFactorX;
		    subImage->scaleFactorY = scaleFactorY;
					    
		    subImage->blockX	  = x;
		    subImage->blockY	  = y;
		    subImage->blockZ	  = z;
		    subImage->resDiff	  = resDiff;
		    subImage->ageStamp	  = 0;
		    subImage->imageData->fill(0.0f);
		    subImage->alphaData->fill(1.0f);
		    subImage->blockStartPos = blockStartPosHi;
		    //		    cout << "Adding subImage " << (*indices)[i] << " to hash table" << endl;
		    subImages.add ((*indices)[i], (void*)subImage);
		} 
#if 0
		else {
		    /*
		     * If image exists and is older than [CHANGE VALUE HERE], delete the old one and make a new one
		     */
		    if (subImage->ageStamp > 0)
			{
			    // cout << "goSBlockRenderer: Image " << (*indices)[i] << " overaged. Removing." << endl;
			    goSubImage *remImage = (goSubImage*)subImages.remove ((*indices)[i]);
			    delete remImage;
			    subImage = new goSubImage (subImageWidth, subImageHeight);
						       
			    subImage->left   = left;
			    subImage->right  = right;
			    subImage->top    = top;
			    subImage->bottom = bottom;
			    /*
			     * Translate bounds so that upper left corner of the viewplane in (0,0)
			     */
			    // 			    subImage->leftImage   = left   + (view->getSize().x - 1) * 0.5;
			    // 			    subImage->rightImage  = right  + (view->getSize().x - 1) * 0.5;
			    // 			    subImage->topImage    = top    + (view->getSize().y - 1) * 0.5;
			    // 			    subImage->bottomImage = bottom + (view->getSize().y - 1) * 0.5;
			    subImage->firstX	  = (goInt32) ceil(left);
			    subImage->firstY	  = (goInt32) ceil(top);
			    subImage->lastX	  = (goInt32) floor(right);
			    subImage->lastY	  = (goInt32) floor(bottom);
			    
			    subImage->firstXHi	  = (goInt32) ceil (leftHi);
			    subImage->lastXHi	  = (goInt32) floor (rightHi);
			    subImage->firstYHi	  = (goInt32) ceil (topHi);
	 		    subImage->lastYHi	  = (goInt32) floor (bottomHi);
			    subImage->leftImage   = subImage->firstXHi + (goInt32)((getViewPlane().getScreenSize().x) * 0.5);
			    subImage->rightImage  = subImage->lastXHi  + (goInt32)((getViewPlane().getScreenSize().x) * 0.5);
			    subImage->topImage    = subImage->firstYHi + (goInt32)((getViewPlane().getScreenSize().y) * 0.5);
			    subImage->bottomImage = subImage->lastYHi  + (goInt32)((getViewPlane().getScreenSize().y) * 0.5);
			    
			    subImage->scaleFactorX = scaleFactorX;
			    subImage->scaleFactorY = scaleFactorY;
		    
			    subImage->blockX	  = x;
			    subImage->blockY	  = y;
			    subImage->blockZ	  = z;
			    subImage->ageStamp    = 0;
			    subImage->resDiff	  = resDiff;
			    subImage->imageData->fill(0.0f);
			    subImage->alphaData->fill(1.0f);
			    subImage->blockStartPos = blockStartPosHi;		    
			    //		    	    cout << "Adding subImage " << (*indices)[i] << " to hash table" << endl;
			    subImages.add ((*indices)[i], (void*)subImage);
			}
		}
#endif
	    /******************************************************************************************/
	    /*
	     * Go on here!
	     */
	}
    GO_VREXP_FINDBOUNDS_END();
    
    /*
     * Sort using block distances as sort keys (so images close to the viewer get in first)
     */ 
    goArray<volFloat> distances;
    distances.resize(0);
    go3Vector<volFloat> tempV3;
    for (i = 0; i < indices->getSize(); i++)
	{
	    // Calculate distance to eye
	    tempV3.x = -((goSubImage*)subImages[(*indices)[i]])->blockStartPos.x;
	    tempV3.y = -((goSubImage*)subImages[(*indices)[i]])->blockStartPos.y;
	    tempV3.z = -((goSubImage*)subImages[(*indices)[i]])->blockStartPos.z;
	    tempV3.x -= xStep * 0.5;
	    tempV3.y -= yStep * 0.5;
	    tempV3.z -= zStep * 0.5;
	    tempV3 += getViewPlane().getEyePos();
	    distances += tempV3.abs();
	}
    // Sort both arrays using the distances as sort keys.
    distances.sort (*indices);

}

}

template class Vol::goSBlockRenderer<goInt8>;
template class Vol::goSBlockRenderer<goUInt8>;
template class Vol::goSBlockRenderer<goInt16>;
template class Vol::goSBlockRenderer<goUInt16>;

template class Vol::goSBlockRenderer<goInt32>;
template class Vol::goSBlockRenderer<goUInt32>;
template class Vol::goSBlockRenderer<goFloat>;
template class Vol::goSBlockRenderer<goDouble>;
