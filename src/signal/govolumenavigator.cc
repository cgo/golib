#include <govolumenavigator.h>
#include <gostring.h>
#include <goviewvolume.h>
#include <goviewmanager.h>
#include <goerror.h>
#include <goexception.h>
#include <gomath.h>		// MAX/MIN macros
#include <go4vector.h>
#include <go3vector.h>
#include <gothread.h>	// goMutex
#include <gofileio.h>
#include <config.h>
#include <govol.h>
	
namespace Vol {

static void* govn_updater_thread(void* p);

goVolumeNavigator::goVolumeNavigator ()
    : goTimerObject(), goStatusObject(), goObjectInfo(), goNavSlot()
{
    setClassName ("goVolumeNavigator");
    vm = 0;
    dataType = GO_INT16;			// Initial data type
    go3Vector<volFloat> vec;			
    setScreenSize (0,0);
    /*
     * Initialize a view volume
     */
    vec.x = 0; vec.y = 0; vec.z = -300;		// Position of the eye
    viewVolume.setEyePos (vec);
    viewVolume.setScreenSize (screenWidth,screenHeight);		// Size of the screen the viewplane is looking at view plane
    vec.x = 0; vec.y = 0; vec.z = 1;		// Normal vector
    viewVolume.setNormal (vec);			
    vec.x = 0; vec.y = -1; vec.z = 0;		// Up vector
    viewVolume.setUp (vec);
    viewVolume.setEyeDistance (100.0f);	// Distance eye --> view plane
    viewVolume.setDepth (32);			// Depth of the view from the view plane
    viewVolume.setFrontClip (100.0f);
    viewVolume.setSampleDistanceX (1.0f);
    viewVolume.setSampleDistanceY (1.0f);
    viewVolume.setSampleDistanceZ (1.0f);
    viewVolume.update();			// Update/Initialize

    finalImage = 0;
	imageZoomFactor = 1.0f;
    opacityMapMin = 0.0f;
    opacityMapMax = 1.0f;
    densityMapMin = 0.0f;
    densityMapMax = 1.0f;
	greyTF		  = 0;
    setInitialized (false);
	updateViewRenders(true);
}

goVolumeNavigator::~goVolumeNavigator()
{
    if (vm)
    {
		deleteViewManager();
    }
}

void
goVolumeNavigator::resetViewAngle()
{
    go3Vector<volFloat> vec;			
    vec.x = 0; vec.y = 0; vec.z = 1;		// Normal vector
    viewVolume.setNormal (vec);			
    vec.x = 0; vec.y = -1; vec.z = 0;		// Up vector
    viewVolume.setUp (vec);
}

goSize_t
goVolumeNavigator::memoryUsage()
{
	if (isInitialized())
	{
		switch(dataType)
		{
			case GO_INT8:
				return ((goViewManager<goInt8>*)vm)->memoryUsage();
				break;
			case GO_UINT8:
				return ((goViewManager<goUInt8>*)vm)->memoryUsage();
					break;
			case GO_INT16:
				return ((goViewManager<goInt16>*)vm)->memoryUsage();
				break;
			case GO_UINT16:
				return ((goViewManager<goUInt16>*)vm)->memoryUsage();
				break;
				
			case GO_INT32:
				return ((goViewManager<goInt32>*)vm)->memoryUsage();
				break;
			case GO_UINT32:
				return ((goViewManager<goUInt32>*)vm)->memoryUsage();
				break;
			case GO_FLOAT:
				return ((goViewManager<goFloat>*)vm)->memoryUsage();
				break;
			case GO_DOUBLE:
				return ((goViewManager<goDouble>*)vm)->memoryUsage();
				break;
	
			default: return 0; break;
		}
  	  return 0;
	}
}

void
goVolumeNavigator::setMaxMemoryUsage(goSize_t max)
{
	if (isInitialized())
	{
		switch(dataType)
		{
			case GO_INT8:
				 ((goViewManager<goInt8>*)vm)->setMaxMemoryUsage(max);
				break;
			case GO_UINT8:
				 ((goViewManager<goUInt8>*)vm)->setMaxMemoryUsage(max);
				break;
				case GO_INT16:
				 ((goViewManager<goInt16>*)vm)->setMaxMemoryUsage(max);
				break;
			case GO_UINT16:
				 ((goViewManager<goUInt16>*)vm)->setMaxMemoryUsage(max);
				break;
				
			case GO_INT32:
				 ((goViewManager<goInt32>*)vm)->setMaxMemoryUsage(max);
				break;
			case GO_UINT32:
				 ((goViewManager<goUInt32>*)vm)->setMaxMemoryUsage(max);
				break;
			case GO_FLOAT:
				 ((goViewManager<goFloat>*)vm)->setMaxMemoryUsage(max);
				break;
			case GO_DOUBLE:
				 ((goViewManager<goDouble>*)vm)->setMaxMemoryUsage(max);
				break;
	
			default: break;
		}
	}
}

void
goVolumeNavigator::deleteViewManager()
{
    switch (dataType)
    {
	case GO_INT8: delete (goViewManager<goInt8>*)vm; break;
	case GO_UINT8: delete (goViewManager<goUInt8>*)vm; break;
	case GO_INT16: delete (goViewManager<goInt16>*)vm; break;
	case GO_UINT16: delete (goViewManager<goUInt16>*)vm; break;
	case GO_INT32: delete (goViewManager<goInt32>*)vm; break;
	case GO_UINT32: delete (goViewManager<goUInt32>*)vm; break;
	case GO_FLOAT: delete (goViewManager<goFloat>*)vm; break;
	case GO_DOUBLE: delete (goViewManager<goDouble>*)vm; break; 
	default: goError::print("goVolumeNavigator::deleteViewManager()","Unknown data type for view manager!");
    }
}

void
goVolumeNavigator::createViewManager()
{
    goString s1,s2;
    s1 = "goVolumeNavigator::createViewManager()";
    s2 = "Creating view manager for type ";
    switch (dataType)
    {
        case GO_INT8: 
	{
	    s2 += "goInt8";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goInt8> *temp_vm = new goViewManager<goInt8>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_UINT8: 
	{
	    s2 += "goUInt8";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goUInt8> *temp_vm = new goViewManager<goUInt8>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_INT16: 
	{
	    s2 += "goInt16";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goInt16> *temp_vm = new goViewManager<goInt16>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_UINT16: 
	{
	    s2 += "goUInt16";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goUInt16> *temp_vm = new goViewManager<goUInt16>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
#if 1
	case GO_INT32: 
	{
	    s2 += "goInt32";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goInt32> *temp_vm = new goViewManager<goInt32>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_UINT32: 
	{
	    s2 += "goUInt32";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goUInt32> *temp_vm = new goViewManager<goUInt32>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_FLOAT: 
	{
	    s2 += "goFloat";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goFloat> *temp_vm = new goViewManager<goFloat>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
	case GO_DOUBLE: 
	{
	    s2 += "goDouble";
	    goError::note(s1.toCharPtr(),s2);
	    goViewManager<goDouble> *temp_vm = new goViewManager<goDouble>; 
	    // temp_vm->setViewVolume (&viewVolume);
	    temp_vm->setFileName (volumeFileName);	    
	    try 
	    {
		goError::note(s1.toCharPtr(),"Initializing view manager.");
		temp_vm->init();
	    } 
	    catch (goException& e)
	    {
		e.print();
		throw;
		break;
	    }
	    goError::note(s1.toCharPtr(),"Updating view manager.");
	    temp_vm->update();
	    forwardProgress(temp_vm);
	    vm = (void*)temp_vm;
	    break;
	}
#endif
	default: s2 = "Unknown data type.";
	    goError::print(s1.toCharPtr(),s2);
	    break;
    }
}



void
goVolumeNavigator::setDataType (int t)
{
    dataType = t;
}

void
goVolumeNavigator::setDataType (const char* filename)
{
	switch(Vol::DWTVGetDataType(filename))
	{
		case 0: dataType = GO_INT8; break;
		case 1: dataType = GO_UINT8; break;
		case 2: dataType = GO_INT16; break;
		case 3: dataType = GO_UINT16; break;
		
		case 4: dataType = GO_INT32; break;
		case 5: dataType = GO_UINT32; break;
		case 6: dataType = GO_FLOAT; break;
		case 7: dataType = GO_DOUBLE; break;
		
		default: goError::print("goVolumeNavigator::setDataType()","Unknown data type."); break;
	}
}

void
goVolumeNavigator::setFileName(goString& s)
{
    volumeFileName = s;
}

void
goVolumeNavigator::setFileName(const char* s)
{
    volumeFileName = s;
}


void
goVolumeNavigator::init()
{
    if ( (screenWidth == 0) || (screenHeight == 0) )
	{
	    goString s;
	    s = getClassName();
	    s += ": Screen of either zero height or zero width was set. Navigator not initialized.";
	    throw goExceptionString(s);
	    return;
	}
    
    if (finalImage)
	{
	    delete finalImage;
	}
    finalImage = new goSignal2D<goUInt32> (screenWidth, screenHeight);
    viewVolume.setScreenSize (screenWidth, screenHeight);

    if (vm)
    {
	deleteViewManager();
	vm = 0;
    }
    try 
	{
	    createViewManager();
	}
    catch (goException &e)
	{
	    e.print();
	    goString s;
	    s = getClassName();
	    s += ": Can not create goViewManager";
	    throw goExceptionString(s);
	    return;
	}
    greyTFPointsX.resize(2);
    greyTFPointsY.resize(2);
    greyTFPointsX[0] = 0.0f;
    greyTFPointsX[1] = 1.0f;
    greyTFPointsY[0] = 0.0f;
    greyTFPointsY[1] = 1.0f;
    greyTFMin = 0.0f;
    greyTFMax = 255.0f;		// Wird in getFinalImage() auf maximalen Bereich angepaßt

    opacityTFPointsX.resize(2);
    opacityTFPointsY.resize(2);
    opacityTFPointsX[0] = 0.0f;
    opacityTFPointsX[1] = 1.0f;
    opacityTFPointsY[0] = 0.0f;
    opacityTFPointsY[1] = 1.0f;
    opacityTFMin = -1000.0f;
    opacityTFMax = 3095.0f;		// Wird in setTransferFunctions() gesetzt

    densityTFPointsX.resize(2);
    densityTFPointsY.resize(2);
    densityTFPointsX[0] = 0.0f;
    densityTFPointsX[1] = 1.0f;
    densityTFPointsY[0] = 0.0f;
    densityTFPointsY[1] = 1.0f;
    densityTFMin = 0.0f;
    densityTFMax = 1.0f;		// Wird in setTransferFunctions() gesetzt
    
    setTransferFunctions();		// Initialize transfer functions for the first time

	rotationSpeed.x = 0;
	rotationSpeed.y = 0;
	rotationSpeed.z = 0;
	translationSpeed.x = 0;
	translationSpeed.y = 0;
	translationSpeed.z = 0;

	motionUpdaterThread.cancel();
	motionUpdaterThread.create(govn_updater_thread,(void*)this,1);

	// Initialize position display window
	//int argc = 1;
	//char *argv[1];
	//char argv0[] = "volumenavigator";
	//argv[0] = &argv0[0];
	goSize3D vsize;
	vsize = getFileInfo().size;
	// posDisplay.setVolumeSize(vsize);
	// posDisplay.init();
	
	
    setInitialized (true);
}

static void* govn_updater_thread(void* p)
{
	goVolumeNavigator *vn;
	vn = (goVolumeNavigator*)p;
	vn->motionUpdater();
	return 0;
}

/*
 * Change this when the view manager has the final interface.
 * takePicture currently only renders an image to double.
 */
#define GO_VN_UPDATEVIEW(__TYPE) {					\
    goViewManager<__TYPE>* temp_vm = (goViewManager<__TYPE>*)vm;	\
    temp_vm->addView(viewVolume);     					\
    temp_vm->update();							\
	if (render)\
       temp_vm->render();							\
}

void
goVolumeNavigator::updateView(bool render)
{
    switch (dataType)
	{
	case GO_INT8: 
	    {
		GO_VN_UPDATEVIEW(goInt8);
		break;
	    }
	case GO_UINT8: 
	    {
		GO_VN_UPDATEVIEW(goUInt8);
		break;
	    }
	case GO_INT16: 
	    {
		GO_VN_UPDATEVIEW(goInt16);
		break;
	    }
	case GO_UINT16: 
	    {
		GO_VN_UPDATEVIEW(goUInt16);
		break;
	    }
#if 1
	case GO_INT32: 
	    {
		GO_VN_UPDATEVIEW(goInt32);
		break;
	    }
	case GO_UINT32: 
	    {
		GO_VN_UPDATEVIEW(goUInt32);
		break;
	    }
	case GO_FLOAT: 
	    {
		GO_VN_UPDATEVIEW(goFloat);
		break;
	    }
	case GO_DOUBLE: 
	    {
		GO_VN_UPDATEVIEW(goDouble);
		break;
	    }
#endif
	default: 
	    goError::print("goVolumeNavigator::updateView()","Unknown data type!");
	    break;
	}
	// posDisplay.setViewVolume(viewVolume);
}


#define GO_VN_GETIMAGE(__TYPE) {					\
    goViewManager<__TYPE> *temp_vm = (goViewManager<__TYPE>*)vm;	\
    tempImage = temp_vm->getTempImage();	       			\
    tempImageWidth  = temp_vm->getImageWidth();				\
    tempImageHeight = temp_vm->getImageHeight();       			\
}

void
goVolumeNavigator::predict(goViewVolume& p)
{
	if ((Vol::getVolBehaviour() & Vol::GO_BEHAVIOUR_MOTION_PREDICTION) != 0)
	{
		switch(dataType)
		{
			case GO_INT8:
				((goViewManager<goInt8>*)vm)->predict(p);
				break;
			case GO_UINT8:
				((goViewManager<goUInt8>*)vm)->predict(p);
			break;
				case GO_INT16:
				((goViewManager<goInt16>*)vm)->predict(p);
			break;
			case GO_UINT16:
				((goViewManager<goUInt16>*)vm)->predict(p);
				break;
					
			case GO_INT32:
				((goViewManager<goInt32>*)vm)->predict(p);
				break;
			case GO_UINT32:
				((goViewManager<goUInt32>*)vm)->predict(p);
				break;
			case GO_FLOAT:
				((goViewManager<goFloat>*)vm)->predict(p);
				break;
			case GO_DOUBLE:
				((goViewManager<goDouble>*)vm)->predict(p);
				break;
				
			default: 
		    	goError::print("goVolumeNavigator::predict()","Unknown data type!");
		    	break;
		}
	}
}

goSignal2D<volFloat>*
goVolumeNavigator::getTempImage ()
{
    if (!isInitialized())
	{
	    throw goExceptionString("goVolumeNavigator is not initialized");
	    return 0;
	}

    goSignal2D<volFloat> *tempImage = 0;
    goSize_t tempImageHeight = 0;
    goSize_t tempImageWidth = 0;
    switch (dataType)
	{
	case GO_INT8: 
	    {
		GO_VN_GETIMAGE(goInt8);
		break;
	    }
	case GO_UINT8: 
	    {
		GO_VN_GETIMAGE(goUInt8);
		break;
	    }
	case GO_INT16: 
	    {
		GO_VN_GETIMAGE(goInt16);
		break;
	    }
	case GO_UINT16: 
	    {
		GO_VN_GETIMAGE(goUInt16);
		break;
	    }
#if 1
	case GO_INT32: 
	    {
		GO_VN_GETIMAGE(goInt32);
		break;
	    }
	case GO_UINT32: 
	    {
		GO_VN_GETIMAGE(goUInt32);
		break;
	    }
	case GO_FLOAT: 
	    {
		GO_VN_GETIMAGE(goFloat);
		break;
	    }
	case GO_DOUBLE: 
	    {
		GO_VN_GETIMAGE(goDouble);
		break;
	    }
#endif
	default: 
	    goError::print("goVolumeNavigator::getImage()","Unknown data type!");
	    break;
	}
	return tempImage;
}

goSignal2D<goUInt32>*
goVolumeNavigator::getFinalImage ()
{
	finalImageMutex.lock();
	goSignal2D<volFloat> *tempImage = 0;
	tempImage = getTempImage();

	//if (!greyTF)
	{
		optGreyValues();
	}
	
	if (!tempImage)
	{
		goError::print("goVolumeNavigator::getFinalImage()","temp image is null pointer");
		finalImageMutex.unlock();
		return 0;
	}
	
	goSize_t tempImageWidth  = tempImage->getSizeX();
	goSize_t tempImageHeight = tempImage->getSizeY();
	if ( (finalImage->getSizeX() != tempImageWidth) || (finalImage->getSizeY() != tempImageHeight) )
	{
	    delete finalImage;
	    finalImage = new goSignal2D<goUInt32> (tempImageWidth, tempImageHeight);
	}
    
    // volFloat *xPtr, *yPtr = tempImage->getPtrStart();
    // goPtrOffset_t dx = tempImage->getOffsetX();
    // goPtrOffset_t dy = tempImage->getOffsetY();
    goSize_t 	  x,y;
    goUInt32 	  *imagePtr, *imagePtrY = finalImage->getPtrStart();
    goPtrOffset_t imagedx = finalImage->getOffsetX();
    goPtrOffset_t imagedy = finalImage->getOffsetY();
	goUInt32 	  val = 0;
	goFloat 	  tempDX, tempDY, tempX, tempY, tempXSave;
	goFloat		  sampleVal;
	// Nimm an, daß das Bild idR zum aktuellen view gehört und daß finalImage
	// und tempImage die gleichen Dimensionen haben.
	if ((Vol::getVolBehaviour() & Vol::GO_BEHAVIOUR_ZOOM) != 0)
	{
		tempDX = imageZoomFactor;
		tempDY = imageZoomFactor;
	} else
	{
		tempDX = 1.0f;
		tempDY = 1.0f;
	}
	tempXSave = tempImageWidth * 0.5 - (tempImageWidth * tempDX * 0.5);
	tempY = tempImageHeight * 0.5 - (tempImageHeight * tempDY * 0.5);
	
    for (y = 0; y < tempImage->getSizeY(); y++)   // finalImage Größe == tempImage Größe
	{
	    // xPtr = yPtr;
	    imagePtr = imagePtrY;
		tempX    = tempXSave;
	    for (x = 0; x < tempImage->getSizeX(); x++)
		{
			tempImage->sample(tempX,tempY,sampleVal);
			val = (*greyTF)[(volFloat)sampleVal];
		    *imagePtr = val;
		    // xPtr += dx;
		    imagePtr += imagedx;
			tempX += tempDX;
		}
		tempY += tempDY;
	    //yPtr += dy;
	    imagePtrY += imagedy;
	}

	finalImageMutex.unlock();
    return finalImage;
}

void
goVolumeNavigator::optGreyValues ()
{

	goSignal2D<volFloat> *tempImage = 0;
	tempImage = getTempImage();
	if (!tempImage)
	{
		goError::print("goVolumeNavigator::optGreyValues()","temp image is null pointer");
		return;
	}
    /*
     * Calculate the grey transfer function and set it.
     */
    goSize_t i,j;

    volFloat min = *tempImage->getPtr(0,0);
    volFloat max = min;
    // goSize_t tempImageSize = tempImageWidth * tempImageHeight;
    for (i = 0; i < tempImage->getSizeY(); i++)
		for (j = 0; j < tempImage->getSizeX(); j++)
		{
			volFloat __v = *tempImage->getPtr(j,i);
			min = min < __v ? min : __v;
			max = max > __v ? max : __v;
	    }
	greyTFMin = min;
	greyTFMax = max;

//	greyTFMin = 0;
//	greyTFMax = 1;
	cout << "image min = " << min << ", image max = " << max << endl;

    goInt32 y_min = 0;		// min colour
    goInt32 y_max = 255;	// max colour
	if (greyTF)
	{
		delete greyTF;
	}
	greyTF = new goTransferFunction<volFloat, goUInt32>;
    for (i = 0; i < (goSize_t)(greyTFPointsX.getSize() - 1); i++)
	{
	    greyTF->addSegment ( greyTFMin + (greyTFMax - greyTFMin) * greyTFPointsX[i], 
			    (goInt32)(y_min + (y_max - y_min) * greyTFPointsY[i]), 
			    greyTFMin + (greyTFMax - greyTFMin) * greyTFPointsX[i + 1], 
			    (goInt32)(y_min + (y_max - y_min) * greyTFPointsY[i + 1]) );
	}
	
}

void
goVolumeNavigator::saveImage (const char* filename)
{
    goSignal2D<goUInt32> *image = 0;
    try 
	{
	     image = getFinalImage ();
	}
    catch (...)
	{
	    throw;
	    return;
	}
    goSignal2D<goInt32> img (image->getSizeX(), image->getSizeY(), image->getBorderX(), image->getBorderY());
    goSize_t x,y;
    for (y = 0; y < image->getSizeY(); y++)
	{
	    for (x = 0; x < image->getSizeX(); x++)
		{
		    *img.getPtr(x,y) = *image->getPtr(x,y);
		}
	}
    goFileIO::writePGM(filename, img);
}

void
goVolumeNavigator::moveViewTo (go3Vector<volFloat>& newPos)
{
    viewVolume.setEyePos(newPos);
	viewVolume.update();
}

void
goVolumeNavigator::moveView (go3Vector<volFloat>& diff)
{
    go3Vector<volFloat> tmp;
    go3Vector<volFloat> moveVector;
    moveVector = viewVolume.getNormal();
    moveVector *= diff.z;
    tmp = viewVolume.getU();
    tmp *= diff.x;
    moveVector += tmp;
    tmp = viewVolume.getV();
    tmp *= diff.y;
    moveVector += tmp;
    
    tmp = viewVolume.getEyePos();
    tmp += moveVector;
    viewVolume.setEyePos(tmp);
    viewVolume.update();

#ifdef GO_VN_LINEAR_PREDICTION
	goViewVolume pView;
	pView = viewVolume;
	tmp += moveVector;
	pView.setEyePos(tmp);
	pView.update();
	predict(pView);
#endif

}

void
goVolumeNavigator::rotateView (volFloat angle, GO_ROTATION_AXIS ax)
{
	viewVolume.rotate (angle, ax);
	viewVolume.update();
#ifdef GO_VN_LINEAR_PREDICTION
	goViewVolume pView;
	pView = viewVolume;
	pView.update();
	pView.rotate(angle,ax);
	pView.update();
	predict(pView);
#endif
}

void
goVolumeNavigator::setViewDepth (goSize_t d)
{
    viewVolume.setDepth (d);
    viewVolume.update();
}

void
goVolumeNavigator::setViewEyeDistance (volFloat d)
{
    viewVolume.setEyeDistance (d);
    viewVolume.update();
}

void
goVolumeNavigator::setSampleDistance (goFloat x, goFloat y, goFloat z)
{
	viewVolume.setSampleDistanceX (x);
	viewVolume.setSampleDistanceY (y);
	viewVolume.setSampleDistanceZ (z);
	viewVolume.update();
//     switch (dataType)
// 	{
// 	case GO_INT8:
// 	    ((goViewManager<goInt8>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_UINT8:
// 	    ((goViewManager<goUInt8>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_INT16:
// 	    ((goViewManager<goInt16>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_UINT16:
// 	    ((goViewManager<goUInt16>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_INT32:
// 	    ((goViewManager<goInt32>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_UINT32:
// 	    ((goViewManager<goUInt32>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_FLOAT:
// 	    ((goViewManager<goFloat>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	case GO_DOUBLE:
// 	    ((goViewManager<goDouble>*)vm)->setRendererSampleDistance (x,y,z);
// 	    break;
// 	default:
// 	    goError::print("goVolumeNavigator::setSampleDistance()","Unknown data type!");
// 	    break;
// 	}
}

#define GO_VN_SETMAXRES(__TYPE,__r)	\
{									\
	goViewManager<__TYPE>* temp_vm = (goViewManager<__TYPE>*)vm;	\
	temp_vm->setMaxResolution(__r);									\
}

void
goVolumeNavigator::setMaxResolution (int r)
{
	switch (dataType)
	{
		case GO_INT8:
			GO_VN_SETMAXRES(goInt8, r);
			break;
		case GO_UINT8:
			GO_VN_SETMAXRES(goUInt8, r);
			break;
		case GO_INT16:
			GO_VN_SETMAXRES(goInt16, r);
			break;
		case GO_UINT16:
			GO_VN_SETMAXRES(goUInt16, r);
			break;
#if 1
		case GO_INT32:
			GO_VN_SETMAXRES(goInt32, r);
			break;
		case GO_UINT32:
			GO_VN_SETMAXRES(goUInt32, r);
			break;
		case GO_FLOAT:
			GO_VN_SETMAXRES(goFloat, r);
			break;
		case GO_DOUBLE:
			GO_VN_SETMAXRES(goDouble, r);
			break;
#endif
		default:
			goError::print("goVolumeNavigator::setMaxResolution()","Unknown data type");
			return;
			break;
	}
	// Set the factor images get zoomed by if wished for
	imageZoomFactor = 1 / (float)(1 << (getFileInfo().stages - r));	
}

void
goVolumeNavigator::setScreenSize (int w, int h)
{
    screenWidth = w;
    screenHeight = h;
}

void 
goVolumeNavigator::setTransferFunctionPoints (goArray<volFloat>* pointsX, goArray<volFloat>* pointsY, GO_TF_TYPE t)
{
    switch (t)
	{
	case GO_TF_GREY:
	    greyTFPointsX = *pointsX;
	    greyTFPointsY = *pointsY;
	    break;
	case GO_TF_OPACITY:
	    opacityTFPointsX = *pointsX;
	    opacityTFPointsY = *pointsY;
	    setTransferFunctions();
	    break;
	case GO_TF_DENSITY:
	    densityTFPointsX = *pointsX;
	    densityTFPointsY = *pointsY;
	    setTransferFunctions();
	    break;
	}
}

const goVolumeFileInfo&
goVolumeNavigator::getFileInfo() 
{
	switch (dataType)
	{
		case GO_INT8:
			return ((goViewManager<goInt8>*)vm)->getFileInfo();
			break;
		case GO_UINT8:
			return ((goViewManager<goUInt8>*)vm)->getFileInfo();
			break;
		case GO_INT16:
			return ((goViewManager<goInt16>*)vm)->getFileInfo();
			break;
		case GO_UINT16:
			return ((goViewManager<goUInt16>*)vm)->getFileInfo();
			break;
#if 1
		case GO_INT32:
			return ((goViewManager<goInt32>*)vm)->getFileInfo();
			break;
		case GO_UINT32:
			return ((goViewManager<goUInt32>*)vm)->getFileInfo();
			break;
		case GO_FLOAT:
			return ((goViewManager<goFloat>*)vm)->getFileInfo();
			break;
		case GO_DOUBLE:
			return ((goViewManager<goDouble>*)vm)->getFileInfo();
			break;
#endif
		default:
			goError::print("goVolumeNavigator::getFileInfo()","Unknown data type");	
	}
}

void
goVolumeNavigator::waitForRenderer()
{
	switch(dataType)
	{
		case GO_INT8:
			((goViewManager<goInt8>*)vm)->getRenderer().wait();
			break;
		case GO_UINT8:
			((goViewManager<goUInt8>*)vm)->getRenderer().wait();
			break;
		case GO_INT16:
			((goViewManager<goInt16>*)vm)->getRenderer().wait();
			break;
		case GO_UINT16:
			((goViewManager<goUInt16>*)vm)->getRenderer().wait();
			break;
#if 1
		case GO_INT32:
			((goViewManager<goInt32>*)vm)->getRenderer().wait();
			break;
		case GO_UINT32:
			((goViewManager<goUInt32>*)vm)->getRenderer().wait();
			break;
		case GO_FLOAT:
			((goViewManager<goFloat>*)vm)->getRenderer().wait();
			break;
		case GO_DOUBLE:
			((goViewManager<goDouble>*)vm)->getRenderer().wait();
			break;
#endif
		default:
			goError::print("goVolumeNavigator::waitForRenderer()","Unknown data type");	
			break;
	}
}

bool
goVolumeNavigator::rendererBusy()
{
	switch(dataType)
	{
		case GO_INT8:
			return ((goViewManager<goInt8>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_UINT8:
			return ((goViewManager<goUInt8>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_INT16:
			return ((goViewManager<goInt16>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_UINT16:
			return ((goViewManager<goUInt16>*)vm)->getRenderer().rendererBusy();
			break;
#if 1
		case GO_INT32:
			return ((goViewManager<goInt32>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_UINT32:
			return ((goViewManager<goUInt32>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_FLOAT:
			return ((goViewManager<goFloat>*)vm)->getRenderer().rendererBusy();
			break;
		case GO_DOUBLE:
			return ((goViewManager<goDouble>*)vm)->getRenderer().rendererBusy();
			break;
#endif
		default:
			goError::print("goVolumeNavigator::rendererBusy()","Unknown data type");	
	}
	return false;
}

void
goVolumeNavigator::motion(goNavSlot::motionType t, void* arg)
{
	switch(t)
	{
		case (goNavSlot::ROTATION):
		// Rotation wants a vector with all angles
		{
			go3Vector<volFloat> *angles;
			angles = (go3Vector<volFloat>*)arg;
			motionMutex.lock();
			volFloat pi_ = M_PI / 180.0f;
			rotationSpeed.x = angles->x * 15 * pi_; // Max. Rotation 
			rotationSpeed.y = angles->y * 15 * pi_;
			rotationSpeed.z = angles->z * 15 * pi_;
			motionMutex.unlock();
			motionSema.inc();  // indicate motion change
			break;
		}
		case (goNavSlot::TRANSLATION):
			// Translation wants a vector with all translation offsets	
			go3Vector<volFloat> *offsets;
			offsets = (go3Vector<volFloat>*)arg;
			motionMutex.lock();
			translationSpeed.x = offsets->x * 20;  // Max. Translation 20 Voxel/T
			translationSpeed.y = offsets->y * 20;
			translationSpeed.z = offsets->z * 20;
			motionMutex.unlock();
			motionSema.inc();  // indicate motion change
			break;
		default:
			goError::print("goVolumeNavigator::motion()","Unknown motion type");
			break;	
	}
}

void
goVolumeNavigator::motionUpdater()
{
	goError::note("goVolumeNavigator::motionUpdater()","Started.");
	bool update_me;
	while(true)  // wir laufen als thread
	{
		// Auf Bewegung warten
		motionSema.dec();
#if _GODEBUG >= 4	
		cout << getClassName() << ": Received motion change\n";
#endif		
		waitForRenderer();
#if _GODEBUG >= 4		
		cout << getClassName() << ": Waited for renderer successfully\n";
#endif		
		
		motionMutex.lock();
		update_me = false;
		if (rotationSpeed.x != 0)
		{
			update_me = true;
			rotateView(rotationSpeed.x, GO_ROTATION_X);
		}
		if (rotationSpeed.y != 0)
		{
			update_me = true;
			rotateView(rotationSpeed.y, GO_ROTATION_Y);
		}
		if (rotationSpeed.z != 0)
		{
			update_me = true;
			rotateView(rotationSpeed.z, GO_ROTATION_Z);
		}
		if ( (translationSpeed.x != 0) ||
			 (translationSpeed.y != 0) ||
			 (translationSpeed.z != 0) )
		{
			update_me = true;
			moveView(translationSpeed);
		}
		motionMutex.unlock();
		if (update_me)
			updateView(update_view_renders);
	}
}

#define GO_VN_SETTF(__TYPE) {											  \
    goTransferFunction<__TYPE, volFloat> __alpha;								  \
    goTransferFunction<__TYPE, volFloat> __color;								  \
    goViewManager<__TYPE>*               __temp_vm;								  \
    __temp_vm         = (goViewManager<__TYPE>*)vm;								  \
    volFloat	__min = __temp_vm->getFileInfo().minimum;							  \
    volFloat    __max = __temp_vm->getFileInfo().maximum;							  \
    densityTFMin  = 0;											  \
    densityTFMax  = fabs(__max - __min);											  \
    opacityTFMin  = __min;											  \
    opacityTFMax  = __max;											  \
    int __i;													  \
    for (__i = 0; __i < (opacityTFPointsX.getSize() - 1); __i++)					  \
	{													  \
	    __alpha.addSegment ( (__TYPE)(__min + (__max - __min) * opacityTFPointsX[__i]), 			  \
				 (opacityMapMin + (opacityMapMax - opacityMapMin) * opacityTFPointsY[__i]), 	  \
				 (__TYPE)(__min + (__max - __min) * opacityTFPointsX[__i + 1]), 		  \
				 (opacityMapMin + (opacityMapMax - opacityMapMin) * opacityTFPointsY[__i + 1]) ); \
	}													  \
    for (__i = 0; __i < (densityTFPointsX.getSize() - 1); __i++)					  \
	{													  \
	    __color.addSegment ( (__TYPE)(__min + (__max - __min) * densityTFPointsX[__i]), 			  \
				 (densityMapMin + (densityMapMax - densityMapMin) * densityTFPointsY[__i]), 				  \
				 (__TYPE)(__min + (__max - __min) * densityTFPointsX[__i + 1]), 		  \
				 (densityMapMin + (densityMapMax - densityMapMin) * densityTFPointsY[__i + 1]) );			  \
	}													  \
    __temp_vm->setRendererAlpha (__alpha);									  \
    __temp_vm->setRendererColor (__color);									  \
    														  \
}

void
goVolumeNavigator::setTransferFunctions ()
{
    switch (dataType)
	{
	case GO_INT8:
	    GO_VN_SETTF(goInt8);
	    break;
	case GO_UINT8:
	    GO_VN_SETTF(goUInt8);
	    break;
	case GO_INT16:
	    GO_VN_SETTF(goInt16);
	    break;
	case GO_UINT16:
	    GO_VN_SETTF(goUInt16);
	    break;
#if 1
	case GO_INT32:
	    GO_VN_SETTF(goInt32);
	    break;
	case GO_UINT32:
	    GO_VN_SETTF(goUInt32);
	    break;
	case GO_FLOAT:
	    GO_VN_SETTF(goFloat);
	    break;
	case GO_DOUBLE:
	    GO_VN_SETTF(goDouble);
	    break;
#endif
	default:
	    goError::print("goVolumeNavigator::setTransferFunctions()","Unknown type.");
	    break;
	}
}

void
goVolumeNavigator::TEST(int sel)
{
	switch(dataType)
	{
		case GO_INT8:
			((goViewManager<goInt8>*)vm)->TEST(sel);
			break;
		case GO_UINT8:
			((goViewManager<goUInt8>*)vm)->TEST(sel);
			break;
		case GO_INT16:
			((goViewManager<goInt16>*)vm)->TEST(sel);
			break;
		case GO_UINT16:
			((goViewManager<goUInt16>*)vm)->TEST(sel);
			break;
			
		case GO_INT32:
			((goViewManager<goInt32>*)vm)->TEST(sel);
			break;
		case GO_UINT32:
			((goViewManager<goUInt32>*)vm)->TEST(sel);
			break;
			
		case GO_FLOAT:
			((goViewManager<goFloat>*)vm)->TEST(sel);
			break;
		case GO_DOUBLE:
			((goViewManager<goDouble>*)vm)->TEST(sel);
			break;
	}
}

#ifdef _GODEBUG_VOLUMEFILE
void
goVolumeNavigator::debugVolumeFile()
{
	if (dataType != GO_INT16)
	{
		cout << "only debugging of int16 files is possible\n";
		return;
	}
	goVolumeFile<goInt16> *file = ((goViewManager<goInt16>*)vm)->getVolumeFile();
	
	goSignal3D<goInt16> b(32,32,32,0,0,0);
	goArray<int> stages;
	goArray<int> minStages;
	goArray<goSize_t> indices;
	goArray<void*> signalPtrs;

	cout << getClassName() << ": Volumefile debugging\n";

	b.shiftLeftDiff(1);
	b.shiftRightSize(1);
	stages.resize(1);
	minStages.resize(1);
	indices.resize(1);
	signalPtrs.resize(1);
	stages[0] = 2;
	minStages[0] = 0;
	indices[0] = 0;
	signalPtrs[0] = &b;

	file->readTransBlocks(indices,signalPtrs,stages,minStages);
	int x,y,z;
	z = 0;
	cout << "\tenter a key when you think the block is loaded";
	char c; cin >> c;
	for (y = 0; y < b.getSizeY(); y++)
	{
		cout << "\t";
		for (x = 0; x < b.getSizeX(); x++)
		{
			cout << *b.getPtr(x,y,z) << " ";
		}
		cout << "\n";
	}
	// return;
	stages[0] = 3;
	minStages[0] = 3;
	indices[0] = 0;
	signalPtrs[0] = &b;

	b.shiftLeftSize(1);
	b.shiftRightDiff(1);

	file->readTransBlocks(indices,signalPtrs,stages,minStages);
	z = 0;
	cout << "\tenter a key when you think the block is loaded";
	cin >> c;
	for (y = 0; y < b.getSizeY(); y++)
	{
		cout << "\t";
		for (x = 0; x < b.getSizeX(); x++)
		{
			cout << *b.getPtr(x,y,z) << " ";
		}
		cout << "\n";
	}
}
#endif

}











