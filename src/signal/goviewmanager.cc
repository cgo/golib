#include <config.h>
#include <goviewmanager.h>
#include <goexception.h>
#include <goerror.h>

#include <gostring.h>
#include <stdio.h>    //sprintf

#include <gotransferfunction.h>  //for the renderer -- think about (automatic?) 
                                 //adaptation of transfer function
#include <gomath.h>	// MIN/MAX macros
#include <gorandom.h>
#include <time.h>	// für TEST()
#include <math.h>
namespace Vol {

template<class T>
goViewManager<T>::goViewManager()
{
    setClassName("goViewManager");
    goString s;
    char i[2];
    s = "Size of data type: ";
    sprintf(&i[0],"%d",sizeof(T));
    s += &i[0];
    s += " bytes.";
    goError::note("goViewManager()",s);
    setFrameRate (2.0f);			// Initialize frame rate to 2 fps desired rate.
    renderTimeAdjust = 0;
	setMaxResolution (0);
}

template<class T>
goViewManager<T>::~goViewManager()
{
}

template<class T>
void
goViewManager<T>::init ()
{
//     if (!view)
// 	{
// 	    goString s;
// 	    s = "goViewManager::init() does not have a goViewVolume set";
// 	    throw goExceptionString(s);
// 	    return;
// 	}
    store.setPresenceManager(&pm);
	// store.setMaxMemoryUsage (1 * 1024 * 1024);
    try 
	{
	    store.init();
	}
    catch (goException &e)
	{
	    e.print();
	    goString s;
	    s = getClassName();
	    s += ": can not initialize block store";
	    throw goExceptionString (s);
	    return;
	}
    pm.setResolutionManager(&rm);
    pm.setBlockStore(&store);
    pm.setVolumeFile (&file);
    try
	{
	    pm.init();
	}
    catch (goException &e)
	{
	    e.print();
	    goString s;
	    s = getClassName();
	    s += ": Can not initialize goPresenceManager";
	    throw goExceptionString(s);
	    return;
	}
	// Set max. resolution to maximum
	setMaxResolution (getFileInfo().stages);
    /*
     * Initialize renderer
     */
    renderer.setBlockProvider (&store);		// Set the block (data) source
    go3Vector<volFloat> vec;
    //vec.x = file.getFileInfo().size.x * 0.5; vec.y = file.getFileInfo().size.y * 0.5; 
	//vec.z = file.getFileInfo().size.z * 0.5;		// Default volume position to (0,0,0)
	//vec.x = sqrt(2); vec.y = sqrt(2); vec.z = sqrt(2);
    vec.x = 0.0; vec.y = 0.0; vec.z = 0.0;
    volumePosition = vec;
    renderer.setPosition (vec);
    vec.x = file.getFileInfo().size.x;		
    vec.y = file.getFileInfo().size.y;
    vec.z = file.getFileInfo().size.z;
    renderer.setSize (vec);			// Set the volume size according to
						// the information in the volume file
    // goBlockRenderer specific settings
    renderer.setStages (file.getFileInfo().stages);
    renderer.setResolutionManager (&rm);
    renderer.setPresenceManager   (&pm);
    // Two transfer functions need to be set before rendering!

    // Connect the synchronized objects of types goVolumeFile and goSBlockRenderer
    // file.addSynchronizedListener (&renderer);
    renderer.setProducer (file);	// set the goProducer object to that produces data for goConsumer renderer.
    forwardProgress (&renderer);	// forward progress information from renderer
}

template<class T>
void
goViewManager<T>::setFileName(const char* f)
{
    file.setTransFileName(f);
}

template<class T>
void
goViewManager<T>::setFileName(goString& f)
{
    file.setTransFileName(f.toCharPtr());
}

template<class T>
void
goViewManager<T>::render()
{
	// Change the cancellation behaviour of the renderer to something more controllable.
	// cancelType(0) is sh.t
	// Deferred cancellation is pthread default. The renderer should only be cancelled when
	// it is waiting for the production of a block (cancellation point).
    renderer.cancel();
	// renderer.wait();
    renderer.setViewPlane (view);
    /* 
     * Do this differently. Create another init member in block renderer (reinit or update or something)
     * That does not do as much as init()
     * SLOW -- OPTIMIZE
     */
    //     goTransferFunction<T,goDouble> alpha;
    //     goTransferFunction<T,goDouble> color;
    // 	goDouble min,max;
    // 	min = file.getFileInfo().minimum;
    // 	max = file.getFileInfo().maximum;
    //     alpha.addSegment ((T)min, 0.0f, (T)max, 0.05f);
    //     color.addSegment ((T)min, min, (T)max, max);
    //     renderer.setAlpha (alpha);
    //     renderer.setColor (color);
    
    try 
	{
	    renderer.init ();
	}
    catch (goException& e)
	{
	    e.print();
	    cout << getClassName() << ": Can not initalize renderer!" << endl;
	    return;
	}
    // Starts a new render thread, killing the old thread if one exists.
#if _GODEBUG >= 2
    cout << "\tCalling renderer..." << endl;
#endif
	
    renderer.render(&renderBlocks);
}

template<class T>
void
goViewManager<T>::setMaxMemoryUsage(goSize_t max)
{
	store.setMaxMemoryUsage(max);
}

template<class T>
void
goViewManager<T>::update()
{
	updateMutex.lock();
	
    if (viewQ.isEmpty())
	{
		updateMutex.unlock();
	    return;
	}

    startTimer();
    goViewVolume* viewPtr;
	// Get the next view and delete it after taking over the values.
	viewQMutex.lock();
    viewPtr = (goViewVolume*)viewQ.getHead();
    viewQ.remove();
	viewQMutex.unlock();
    view = *viewPtr;
    delete viewPtr;

    view.update();

    /*
     * Adapt the renderer to the new view
     */
    /********************************************************************************/
						// Adapt the image size to the
						// size set for the view volume
    renderer.setViewPlaneSampleDistance (1.0, 1.0);
    // !!! TAKE CARE WITH THE FINAL IMAGE SET IN THE RENDERER !!! NEEDS TO BE OF THE SAME SIZE !!!
    renderer.setImageSize ((goSize_t)view.getScreenSize().x * 1, (goSize_t)view.getScreenSize().y * 1);
    renderer.renderInit();			// Does nothing by default in block 
						// renderer (yet)
    /********************************************************************************/

    /*
     * Resolution adaption precalculations
     */
    /***********************************************************/
    // 1. Calculate borders for distance dependent resolution
    calculateResolutionAreas();
    // 2. Check the frame rate
    goDouble renderTime = renderer.getRenderTime();
    if ( (renderTime > maxRenderTime) && (renderTimeAdjust < file.getFileInfo().stages) ) 
	// less than /n/ frames per second pure rendering time (no disc access time)
	// This feature is currently not used
	{
	    renderTimeAdjust++; // lower resolution
	}
    else 
	if ( (renderTime < (maxRenderTime - 0.2)) && (renderTimeAdjust > 0))  // Experiment with the value!
	    {
			renderTimeAdjust--;
	    }
    // 3. Calculate blocks in view with resolutions
    goArray<int> renderResolutions;
	
	renderResolutions.resize (0);
	renderBlocks.resize (0);
	
    calculateBlocksInView(view, renderBlocks, renderResolutions);

#if _GODEBUG >= 1
    cout << "***** Resolution adjustment to rendering speed *****" << endl;
	cout << "maxResolution    = " << maxResolution << endl;
	cout << " (Set by calling class)" << endl;
    cout << "renderTime       = " << renderTime << endl;
    cout << "maxRenderTime    = " << maxRenderTime << endl;
    cout << "renderTimeAdjust = " << renderTimeAdjust << endl;
    cout << "****************************************************" << endl;
#endif
    /***********************************************************/

	
    // Startet einen Ladethread, kehrt sofort zurück.
    cout << "viewManager blocklistsize = " << renderBlocks.getSize() << endl;
//	cout << "\tblocks in detail: " << endl;
//	int tempCount;
//	for (tempCount = 0; tempCount < renderBlocks.getSize(); tempCount++)
//	{
//		cout << renderBlocks[tempCount] << ", ";
//	}
//	cout << endl;
    pm.makePresent (renderBlocks, renderResolutions, true);
    /********************************************************************************/
    
    stopTimer();
//#if _GODEBUG >= 1
    cout << getClassName() << ": Time to update: " << getTimerSeconds() << "s" << endl;
//#endif

	updateMutex.unlock();
}

template <class T>
void
goViewManager<T>::TEST(int sel)
{
    goArray<int> renderResolutions;
	
	renderResolutions.resize (0);
	renderBlocks.resize (0);
	// Random blocks test
	switch(sel)
	{
		case 0:
		{
			cout << getClassName() << ": **** RANDOM BLOCKS SELECTION, SORTED ****\n";
			goRandom(true);
			int i;
			goSize_t B;
			goSize_t b;
			B = getFileInfo().blocks.x * getFileInfo().blocks.y * getFileInfo().blocks.z - 1;
			for (i = 0; i < 1000; i++)
			{	
				b = (goSize_t)(goRandom() * B);
				renderBlocks += b;
				pm.setProtection(Vol::PM_PROTECTED,b);
				renderResolutions += 3;
			}
			renderBlocks.sort();
		}
		break;
	// Linear blocks test
		case 1:
		{
			cout << getClassName() << ": **** LINEAR BLOCKS SELECTION ****\n";
			goRandom(true);
			int i;
			goSize_t B;
			B = getFileInfo().blocks.x * getFileInfo().blocks.y * getFileInfo().blocks.z - 1;
			for (i = 0; i < 1000; i++)
			{	
				// b = (goSize_t)(goRandom() * B);
				renderBlocks += (goSize_t)i;
				pm.setProtection(Vol::PM_PROTECTED,(goSize_t)i);
				renderResolutions += 3;
			}
		}
		break;
	}
	// Uhrzeit messen!
	time_t t1 = time(NULL);
	pm.makePresent(renderBlocks, renderResolutions, true);
	file.waitForProgress();
	time_t t2 = time(NULL);
	cout << getClassName() << "::TEST(): " << t2 - t1 << " seconds elapsed\n";
}

template <class T>
void
goViewManager<T>::addView(goViewVolume& v)
{
    // Deleted in update() after being taken from the viewQ
    goViewVolume *newV = new goViewVolume;
    *newV = v;
	// Adapt the pixel size to the maximal wanted resolution set with setMaxResolution()
	newV->setPixelHeight ((volFloat)(1 << (getFileInfo().stages - maxResolution)));
	newV->setPixelWidth ((volFloat)(1 << (getFileInfo().stages - maxResolution)));
	viewQMutex.lock();
    viewQ.add((void*&)newV);
	viewQMutex.unlock();
}

template <class T>
void
goViewManager<T>::predict(goViewVolume& v)
{
	// resolutionAreas is not made for this view -- 
	// it should be close to it if the predicted view is not very far away (which it
	// shouldn't) so don't bother about it.

	cout << getClassName() << "::predict() called\n";	
	goArray<goSize_t> blocks;
	goArray<int> res;
	blocks.resize(0);
	res.resize(0);
	calculateBlocksInView(v,blocks,res,true);
	pm.makePresent(blocks,res,true);
	cout << getClassName() << "::predict() leaving\n";	
	
}

// FIXME
template <class T>
void
goViewManager<T>::calculateResolutionAreas ()
{
    resolutionAreas.resize(0);
    goSize_t i;
    goFloat l = 1.0f;
    //    goDouble d = 0.0f;
    go3Vector<volFloat>	s1, s2;
    s1 = view.getPosition();
    s1 -= view.getEyePos();		// s1 = eyePos -> plane
    s2 = s1;
    s2 += view.getU();			// s2 = next right to s1
    volFloat s1_s2 = s1 * s2;
    volFloat r = 1 / (2 * sin (0.5 * acos (s1_s2 / (s1.abs() * s2.abs()))));
#if _GODEBUG >= 1
    cout << getClassName() << ": Resolution areas:";
#endif
    for (i = 0; i < (goSize_t)getFileInfo().stages; i++)
	{
	    resolutionAreas += l * r;
#if _GODEBUG >= 1
	    cout << l*r << ",";
#endif
	    l *= 2.0f;
	}
#if _GODEBUG >= 1
    cout << "\n";
#endif
}

// template <class T>
// void
// goViewManager<T>::setRendererSampleDistance (goFloat x, goFloat y, goFloat z)
// {
//     renderer.setSampleDistance (x, y, z);
// }


// FIXME
// This works fine for now, but needs some adjustment if it is to be used in a
// production system.
template <class T>
void
goViewManager<T>::calculateBlocksInView(goViewVolume& myView, goArray<goSize_t>& blockArray, goArray<int>& resolutionArray, bool prediction)
{
    volFloat minx, maxx, miny, maxy, minz, maxz;

    myView.calculateBounds (minx, maxx,		// Calculate bounding box for the 
			    			miny, maxy,		// current view
			    			minz, maxz);

    go3Vector<goSize_t>  blocks;
    go3Vector<goInt32>	 blockSize;

    blockSize.x = file.getFileInfo().blockSize.x;
    blockSize.y = file.getFileInfo().blockSize.y;
    blockSize.z = file.getFileInfo().blockSize.z;

    blocks.x = file.getFileInfo().blocks.x;
    blocks.y = file.getFileInfo().blocks.y;
    blocks.z = file.getFileInfo().blocks.z;

//#if _GODEBUG >= 2
	cout << getClassName() << ": ";
    cout << "\tx bounds: " << minx << " -> " << maxx << endl;
    cout << "\ty bounds: " << miny << " -> " << maxy << endl;
    cout << "\tz bounds: " << minz << " -> " << maxz << endl;
//#endif
    /*
     * calculate bounds in "blocked" coordinates
     */
    /********************************************************************************/
    go3Vector<volFloat> p,min,max;
    // p ist p' in den handschriftlichen Unterlagen
    p.x = volumePosition.x - (file.getFileInfo().size.x * 0.5);
    p.y = volumePosition.y - (file.getFileInfo().size.y * 0.5);
    p.z = volumePosition.z - (file.getFileInfo().size.z * 0.5);
    volFloat tmpx = 1 / (float)blockSize.x;
    volFloat tmpy = 1 / (float)blockSize.y;
    volFloat tmpz = 1 / (float)blockSize.z;
    min.x = ((minx - p.x) * tmpx);
#if _GODEBUG >= 2
    cout << "min.x = " << min.x << endl;
#endif
    if (min.x > 0)
	{
	    min.x = MIN(min.x, (blocks.x));
	} else
	    {
			min.x = 0;
	    }
    min.y = ((miny - p.y) * tmpy);
#if _GODEBUG >= 2
    cout << "min.y = " << min.y << endl;
#endif
    if (min.y > 0)
	{
	    min.y = MIN(min.y, (blocks.y));
	} else
	    {
		min.y = 0;
	    }
    min.z = ((minz - p.z) * tmpz);
#if _GODEBUG >= 2
    cout << "min.z = " << min.z << endl;
#endif
    if (min.z > 0)
	{
	    min.z = MIN(min.z, (blocks.z));
	} else
	    {
		min.z = 0;
	    }

    max.x = ((maxx - p.x) * tmpx);
    if (max.x > 0)
	{
	    max.x = MIN(max.x, (blocks.x - 1));
	} else 
	    {
		max.x = 0;
	    }
    max.y = ((maxy - p.y) * tmpy);
    if (max.y > 0)
	{
	    max.y = MIN(max.y, (blocks.y - 1));
	} else 
	    {
		max.y = 0;
	    }
    max.z = ((maxz - p.z) * tmpz);
    if (max.z > 0)
	{
		
	    max.z = MIN(max.z, (blocks.z - 1));
	} else 
	    {
		max.z = 0;
	    }
    /********************************************************************************/

// #if _GODEBUG >= 1
    cout << getClassName() << ": view bounds in block coordinates:" << endl;
    cout << "\tX: " << min.x << " --> " << max.x << endl;
    cout << "\tY: " << min.y << " --> " << max.y << endl;
    cout << "\tZ: " << min.z << " --> " << max.z << endl;
// #endif
    
//     for (z = (goSize_t)min.z; z <= (goSize_t)max.z; z++)
//       {
// 	  for (y = (goSize_t)min.y; y <= (goSize_t)max.y; y++)
// 	      {
// 		  for (x = (goSize_t)min.x; x <= (goSize_t)max.x; x++)
// 		      {
// 			  renderBlocks += diffz * z + diffy * y + x;
// 		      }
// 	      }
//       }
    /********************************************************************************/

    /*
     * Add the blocks in the view volume to the render array.
     */
    /********************************************************************************/
    go3Vector<volFloat> volumeStartPos;
    blocks.x 		 = file.getFileInfo().blocks.x;
    blocks.y 		 = file.getFileInfo().blocks.y;
    blocks.z 		 = file.getFileInfo().blocks.z;
    blockSize.x 	 = file.getFileInfo().blockSize.x;
    blockSize.y 	 = file.getFileInfo().blockSize.y;
    blockSize.z 	 = file.getFileInfo().blockSize.z;
    volFloat xStep 	 = blockSize.x * myView.getSampleDistanceX();
    volFloat yStep 	 = blockSize.y * myView.getSampleDistanceY();
    volFloat zStep 	 = blockSize.z * myView.getSampleDistanceZ();
    volumeStartPos.x = volumePosition.x - (getFileInfo().size.x * myView.getSampleDistanceX() * 0.5);
    volumeStartPos.y = volumePosition.y - (getFileInfo().size.y * myView.getSampleDistanceY() * 0.5);
    volumeStartPos.z = volumePosition.z - (getFileInfo().size.z * myView.getSampleDistanceZ() * 0.5);
    
    blockArray.resize(0);
    resolutionArray.resize(0);
	
    goSize_t x,y,z;
    goSize_t diffz = blocks.x * blocks.y;
    goSize_t diffy = blocks.x;
    go4Vector<volFloat> blockCenter, blockCenterY, blockCenterZ;
    blockCenter.x = volumeStartPos.x + (xStep * 0.5);
    blockCenter.y = volumeStartPos.y + (yStep * 0.5);
    blockCenter.z = volumeStartPos.z + (zStep * 0.5);
    blockCenter.t = 1;
    go4Vector<volFloat>  proj;
    go44Matrix<volFloat> M;
    M = myView.getViewMatrix();
    blockCenterY = blockCenter;
    blockCenterZ = blockCenter;
    go3Vector<volFloat> L;
    L = myView.getNormal();
    volFloat D   = sqrt(xStep * xStep + yStep * yStep + zStep * zStep);
    // volFloat D   = 2 * xStep;
    // volFloat D_2 = 0.5 * D;
    volFloat L_min = myView.getFrontClip() - D; 
	L_min = MAX(L_min,D);
	// L_min = MAX(L_min,0);
					 // myView.getEyeDistance() - myView.getFrontClip() - D;
    volFloat L_max = myView.getFrontClip() + myView.getDepth() + D; 
					 // myView.getEyeDistance() + myView.getDepth() + D;  // myView.getEyeDistance() + myView.getDepth() + D_2;
#if _GODEBUG >= 2					 
    cout << "L_min = " << L_min << ", L_max = " << L_max << endl;
    cout << "eyePos = " << myView.getEyePos() << endl;
#endif
	goSize_t xMax, yMax, zMax;
	goSize_t xMin, yMin, zMin;

	if (max.x < (blocks.x - 1))
	{
    	xMax = (goSize_t)(max.x + 1); 
	} else
	{
		xMax = (goSize_t)(max.x); 
	}
	if (max.y < (blocks.y - 1))
	{
    	yMax = (goSize_t)(max.y + 1); 
	} else
	{
		yMax = (goSize_t)(max.y); 
	}
	if (max.z < (blocks.z - 1))
	{
    	zMax = (goSize_t)(max.z + 1); 
	} else
	{
		zMax = (goSize_t)(max.z); 
	}
	if (min.x > 1)
	{
		xMin = (goSize_t)(min.x - 1);
	} else
	{
		xMin = (goSize_t)(min.x);
	}
	if (min.y > 1)
	{
		yMin = (goSize_t)(min.y - 1);
	} else
	{
		yMin = (goSize_t)(min.y);
	}
	if (min.z > 1)
	{
		zMin = (goSize_t)(min.z - 1);
	} else
	{
		zMin = (goSize_t)(min.z);
	}

// This would check ALL blocks in the volume.
	
    xMax = blocks.x - 1;
    yMax = blocks.y - 1;
    zMax = blocks.z - 1;
	xMin = 0;
	yMin = 0;
	zMin = 0;


	// goSize_t n_o_Blocks = blocks.x * blocks.y * blocks.z;
	
	// Grenzen für Projektion
	volFloat projMinX, projMaxX, projMinY, projMaxY;	
	// Abhängig vom gewünschten Verhalten von Vol
	if ((Vol::getVolBehaviour() & Vol::GO_BEHAVIOUR_RES_DEPENDENT_VIEW) == 0)
	{
		projMinX = -(myView.getSize().x * 0.5 / myView.getPixelWidth());
		projMaxX = -projMinX;
		projMinY = -(myView.getSize().y * 0.5 / myView.getPixelHeight());
		projMaxY = -projMinY;
	} else
	{
		projMinX = -(myView.getSize().x * 0.5);
		projMaxX = -projMinX;
		projMinY = -(myView.getSize().y * 0.5);
		projMaxY = -projMinY;
	}
	// Hier wird entschieden, welche Blöcke in die Renderliste gesetzt werden
	goArray<int> blocks_res;
	blocks_res.resize(file.getFileInfo().stages + 1);
	blocks_res.fill(0);
    for (z = 0; z <= zMax; z++)
	{
	    blockCenterY = blockCenterZ;
	    for (y = 0; y <= yMax; y++)
		{
		    blockCenter = blockCenterY;
		    for (x = 0; x <= xMax; x++)
			{
			    proj = blockCenter;
			    //cout << "Projecting " << proj << endl;
			    proj *= M;
			    proj.div();
			    //cout << "to " << proj << endl;
			    // Prüfen ob Projektion auf sichtbare Fläche fällt, um Pixelgröße korrigieren
			    if ( (proj.x >= projMinX) &&
				 	 (proj.x < projMaxX) &&
				 	 (proj.y >= projMinY) &&
				 	 (proj.y < projMaxY) )
				{
				    // Prüfen ob Abstand sich in Grenzen bewegt
				    go3Vector<volFloat> l;
				    volFloat delta;
				    l.x = blockCenter.x;
				    l.y = blockCenter.y;
				    l.z = blockCenter.z;
				    l -= myView.getEyePos();
				    delta = l.abs();
				    // delta = l * L;
// 	cout << "Block " << x << "," << y << "," << z << " at delta = " << delta << endl;
// 	cout << "Blockcenter " << blockCenter << endl;
// 	cout << "Proj " << proj << endl;
// 	cout << "|l| " << l.abs() << endl;
				    if ( (delta >= L_min) && (delta < L_max) )
					//if (delta >= L_min)
					{
						goSize_t blockIndex = x + y * diffy + z * diffz;
						// Dieser Block befindet sich im sichtbaren Bereich => setze flags in PM auf PROTECTED
						if (prediction)
							pm.setFlag(Vol::PM_PREDICTED, blockIndex);
						pm.setProtection(Vol::PM_PROTECTED,
										 blockIndex);
#if _GODEBUG >= 3
						if (prediction)
							cout << "Prediction: ";
					    cout << "Adding block " << x << "," << y << "," << z << endl;
#endif
					    /*
					     * Correct the resolution for this block
					     */
					    /****************************************************************************/
					    // Check for resolution areas
					    goInt8 resolution = file.getFileInfo().stages;		// Start with max. resolution
					    goIndex_t i;
					    volFloat distance = l.abs();
					    for (i = 0; i < resolutionAreas.getSize(); i++)
						{
 						    if (distance < resolutionAreas[i])
 							{
 							    break;
 							}
 						}
					   	// Subtract distance based resolution reduction
						resolution -= i;
						blocks_res[resolution]++;										  
						if (resolution >= renderTimeAdjust)
						{
						    // resolution -= renderTimeAdjust;
						}
						// If resolution exceeds maximum wanted resolution, don't exceed
						// that maximum.
	    				if (resolution > maxResolution)
							{
								resolution = maxResolution;
							}
					    // Store the resolution 
					    resolutionArray += resolution;
					    // Store the block index
					    blockArray 		+= x + y * diffy + z * diffz;
					}
					else
					{
						// change protection, block is not in view frustum
					}
				} // if (block is in view) 
				else // drop protection only if not predicting
				{
					if (!prediction)
					{
						goSize_t blockIndex = x + y * diffy + z * diffz;
						pm.setProtection (Vol::PM_LESS_PROTECTED, blockIndex);
					} else
					{
						goSize_t blockIndex = x + y * diffy + z * diffz;
						pm.unSetFlag(Vol::PM_PREDICTED,blockIndex);
					}
				}
			    blockCenter.x += xStep;
			}
		    blockCenterY.y += yStep;
		}
	    blockCenterZ.z += zStep;
	}

	int i;
	for (i = 0; i < blocks_res.getSize(); i++)
	{
		cout << getClassName() << ": Blocks in resolution " << i << ": " << blocks_res[i] << "\n";
	}
}

template <class T>
void
goViewManager<T>::setMaxResolution (int r)
{
	maxResolution = r;
	
}

}		// namespace Vol


template class Vol::goViewManager<goInt8>;
template class Vol::goViewManager<goUInt8>;
template class Vol::goViewManager<goInt16>;
template class Vol::goViewManager<goUInt16>;
template class Vol::goViewManager<goInt32>;
template class Vol::goViewManager<goUInt32>;
template class Vol::goViewManager<goFloat>;
template class Vol::goViewManager<goDouble>;





