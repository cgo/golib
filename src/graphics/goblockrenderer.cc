#include <goblockrenderer.h>
#include <go4vector.h>
#include <goexception.h>

#include <go3dalgo.h>
#include <gomath.h>		// MIN/MAX macros
#include <math.h>		// ceil()
#include <govolumerenderermacros.h>

#include <time.h>  // clock()

namespace Vol {

template <class T>
goBlockRenderer<T>::goBlockRenderer() : goVolumeRenderer<T>()
{
  setClassName ("goBlockRenderer");
  tempImage = 0;
  alphaImage = 0;
  finalImage = 0;
  eyePos.x = 0;
  eyePos.y = 0;
  eyePos.z = 0;
  resolutionManager = 0;
  REQUEST_THREAD_CANCEL = false;
  RENDERER_BUSY = false;
}

template <class T>
goBlockRenderer<T>::~goBlockRenderer()
{
  if (tempImage)
  {
	  delete tempImage;
  }
   if (alphaImage)
   {
       delete alphaImage;
   }
   if (finalImage)
   {
       delete finalImage;
   }
}

template <class T>
void
goBlockRenderer<T>::init()
{
    this->cancel();		// cancel any eventually running rendering thread.

    if (!resolutionManager)
	{
	    goString s;
	    s = "goBlockRenderer::init() does not have a goResolutionManager set.";
	    throw goExceptionString(s);
	    return;
	}

    /*
     * Create new images if necessary
     */
    if ( !tempImage || (tempImage->getSizeX() != getImageWidth()) || (tempImage->getSizeY() != getImageHeight()) )
	{
	    if (tempImage)
		{
		    delete tempImage;
		    delete alphaImage;
		    delete finalImage;
		    tempImage = 0;
		    alphaImage = 0;
		    finalImage = 0;
		}
    
	    goSize_t borderSize = MAX(getBlockProvider()->getBlockSize().x,
				      getBlockProvider()->getBlockSize().y);
	    borderSize = MAX(borderSize, getBlockProvider()->getBlockSize().z);
	    borderSize <<= 1;
	    tempImage  = new goSignal2D<volFloat> (getImageWidth(), getImageHeight(), borderSize, borderSize);
	    alphaImage = new goSignal2D<volFloat> (getImageWidth(), getImageHeight(), borderSize, borderSize);
	    finalImage = new goSignal2D<goUInt32> (getImageWidth(), getImageHeight(), borderSize, borderSize);

	    tempImage->fill (0.0f);
	    finalImage->fill (0);
	}

    alphaImage->fill (1.0f);
//     goSize_t i;
//     for (i = 0; i < getImageWidth() * getImageHeight(); i++)
//     {
// 	tempImage[i]  = 0;
// 	alphaImage[i] = 0;
//     }
    
    getViewPlane().update();
    Tproj = getViewPlane().getViewMatrix();
    eyePos = getViewPlane().getEyePos();
    
    goBlockProvider<T> *provider = getBlockProvider();
    // Set the original block sizes as step sizes in world (volume) coordinates
    xStep   = provider->getBlockSize().x;
    yStep   = provider->getBlockSize().y;
    zStep   = provider->getBlockSize().z;
    go3Vector<volFloat> size = getSize();
    xBlocks = (goSize_t)ceil( size.x / xStep );
    yBlocks = (goSize_t)ceil( size.y / yStep );
    zBlocks = (goSize_t)ceil( size.z / zStep );
}

template <class T>
void
goBlockRenderer<T>::render (goArray<goSize_t>* indices)
{
  goSignal3D<T> *block;

  goSize_t blockIndex;
  goSize_t i;
  volFloat _1_y_x = 1 / (float)(xBlocks * yBlocks);
  volFloat _1_x   = 1 / (float)xBlocks;
  goSize_t _x_y   = xBlocks * yBlocks;

  goSize_t x,y,z;

  /* 
   * Position is the position of the middle of the volume.
   * Calculate start position at corner 0 of the volume:
   */
  go4Vector<volFloat> volumeStartPos;
  volumeStartPos.x = getPosition().x;
  volumeStartPos.y = getPosition().y;
  volumeStartPos.z = getPosition().z;
  volumeStartPos.t = 1.0f;
  go3Vector<volFloat> size = getSize();
  volumeStartPos.x -= (size.x) / 2.0f;
  volumeStartPos.y -= (size.y) / 2.0f;
  volumeStartPos.z -= (size.z) / 2.0f;
  
  clock_t t1,t2;
  t1 = clock();
  for (i = 0; i < (goSize_t)indices->getSize(); i++)
    {
      blockIndex = (*indices)[i];
      // Bounding box auf der viewplane berechnen
      // Alle strahlen darin clippen am aktuellen Block
      // Berechnen und abspeichern
      GO_VREXP_FINDBOUNDS_START()
      go4Vector<volFloat> blockStartPos;
      volFloat tempD = blockIndex * _1_y_x;
      blockStartPos = volumeStartPos;
      // Find out block position
      z = (goSize_t)(tempD);
      volFloat tempD2 = blockIndex - z * _x_y;
      y = (goSize_t)(tempD2 * _1_x);
      x = (goSize_t)(tempD2 - y * xBlocks);
      // World coordinates of current block's 0th corner
      blockStartPos.x += x * xStep;
      blockStartPos.y += y * yStep;
      blockStartPos.z += z * zStep;
      blockStartPos.t = 1;

//       cout << "Blockstartpos: " << 
// 	blockStartPos.x << "," << blockStartPos.y << "," << blockStartPos.z << endl;

      volFloat left, right, top, bottom;
      // Bounding box:
      GO_VREXP_FINDBOUNDS(blockStartPos, left, right, top, bottom);
      left -= 2;
      right += 2;
      top -= 2;
      bottom += 2;

      // cout << "block " << blockIndex << " projiziert auf " << left << "," << right << "," << top << "," << bottom << endl;

      /*
       * Start point on the viewplane, in world coordinates.
       */
      go3Vector<volFloat> u = getViewPlane().getU();
      go3Vector<volFloat> v = getViewPlane().getV();
      go3Vector<volFloat> planePointSave;
      go3Vector<volFloat> tempV;
      planePointSave	= getViewPlane().getPosition();
      tempV		= u;
      tempV		*= (goInt32)(left + 0.5);
      planePointSave	+= tempV;
      tempV		= v;
      tempV		*= (goInt32)(top + 0.5);
      planePointSave	+= tempV;
      // cout << "plane point (start): " << planePointSave << endl;
      
      /*
       * Schränke den Bereich der clip-Strahlen ein 
       * (nur das eben berechnete Fenster um die Projektion des Blocks)
       */
      y = (getImageHeight() >> 1) + (goInt32)(top + 0.5);
      x = (getImageWidth() >> 1) + (goInt32)(left + 0.5);
      goSize_t xMax = x + (goInt32)(right - left + 0.5);
      xMax = MIN(getImageWidth(),xMax);
      goSize_t yMax = y + (goInt32)(bottom - top + 0.5);
      yMax = MIN(getImageHeight(),yMax);
      goSize_t xStart = x;

      // cout << "Block " << i << " at " << " projecting to " << x << "," << y << " window " << xMax << "," << yMax << endl;

      block = getBlockProvider()->getBlock(blockIndex);
      T* blockPtr = block->getPtr();
      goPtrdiff_t dx = block->getXDiff();
      goPtrdiff_t dy = block->getYDiff();
      goPtrdiff_t dz = block->getZDiff();
//       volFloat dx = (volFloat)block->getXDiff();
//       volFloat dy = (volFloat)block->getYDiff();
//       volFloat dz = (volFloat)block->getZDiff();
//       dx *= block->getSizeX() / (float)xStep;			// Adapt diff values to block size
//       dy *= block->getSizeY() / (float)yStep;
//       dz *= block->getSizeZ() / (float)zStep;

      // EXPERIMENTAL -- adapt the step size to the stage (= resolution)
//       volFloat realXStep = block->getSizeX();
//       volFloat realYStep = block->getSizeY();
//       volFloat realZStep = block->getSizeZ();
//       volFloat realXStep = xStep;
//       volFloat realYStep = yStep;
//       volFloat realZStep = zStep;
      

      volFloat tempAlphaVal;
      volFloat tempImageVal;
      goPtrdiff_t blockDiff;
      go3Vector<volFloat> planePoint;
      go3Vector<volFloat> start,end;
      go3Vector<volFloat> rayStep;
      bool lineIsValid;
      GO_3D_CLIPLIANGBARSKY_START()
	// FOR (The bounding box on the view plane) DO
    for (; y < yMax; y++)
	{
	  planePoint = planePointSave;
	  for (x = xStart; x < xMax; x++)
	  {
		// tempAlphaVal = alphaImage[x + y * getImageWidth()];
		// tempImageVal =  tempImage[x + y * getImageWidth()];
		tempImageVal = *tempImage->getPtr (x,y);
		tempAlphaVal = *alphaImage->getPtr (x,y);
	    if (tempAlphaVal < 1.0f)
		{
		  rayStep = planePoint;
  		  rayStep -= eyePos;
  		  rayStep *= 10000;
		  GO_3D_CLIPLIANGBARSKY(blockStartPos.x, (blockStartPos.x + xStep),
					blockStartPos.y, (blockStartPos.y + yStep),
					blockStartPos.z, (blockStartPos.z + zStep),
					rayStep, planePoint,
					start, end, lineIsValid);
		  if (lineIsValid)
		    {
		      //cout << "valid line" << endl;
		      rayStep *= 1 / rayStep.abs();
		      end -= start;
		      // Length of the ray
	              z = (goSize_t)end.abs();
		      
		      // Calculate the relative start in a block and store in in start:
		      start += planePoint;
		      start.x -= blockStartPos.x;
		      start.y -= blockStartPos.y;
		      start.z -= blockStartPos.z;
    //		      cout << "start: " << start.x << "," << start.y << "," << start.z << endl;
		      volFloat alpha;
		      for ( ; z > 0; z--)
			{
			  blockDiff = (goPtrdiff_t)(start.x - 0.5) * dx +
			      (goPtrdiff_t)(start.y - 0.5) * dy +
			      (goPtrdiff_t)(start.z - 0.5) * dz;
			  // Akkumulieren
			  alpha = alphaFunction[*(blockPtr + blockDiff)];
			  tempImageVal *= (1 - alpha);
			  tempImageVal += colorFunction[*(blockPtr + blockDiff)] 
			      * alpha;
			  tempAlphaVal += alpha * (1 - tempAlphaVal);
			  
			  // MIP
  			  //if (tempImageVal < densityToFloat[*(blockPtr + blockDiff)])
			  // tempImageVal = densityToFloat[*(blockPtr + blockDiff)];
			  start.x += rayStep.x;
			  start.y += rayStep.y;
			  start.z += rayStep.z;
			}
		      //  		      if (tempImageVal < 1.0f)
		      //  			tempImage[x + y * getImageWidth()] = tempImageVal;
//  		      else 
//		        tempImage[x + y * getImageWidth()] = 1.0f;
		      //tempImage[x + y * getImageWidth()] = tempImageVal;
		      //alphaImage[x + y * getImageWidth()] = tempAlphaVal;
		      *tempImage->getPtr(x,y) = tempImageVal;
		      *alphaImage->getPtr(x,y) = tempAlphaVal;
		    }
		}
              planePoint += u;
	    }
	  planePointSave += v;
	}
      GO_3D_CLIPLIANGBARSKY_END();      
      GO_VREXP_FINDBOUNDS_END();
    }
  t2 = clock();
  cout << "Time for rendering loop: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
}

template <class T>
void
goBlockRenderer<T>::wait ()
{
	rThread.join();
}

template <class T>
void
goBlockRenderer<T>::cancel ()
{
	// rThread.cancel();   // zu brutal
	REQUEST_THREAD_CANCEL = true;
	wait();
	REQUEST_THREAD_CANCEL = false;
}

template <class T>
bool
goBlockRenderer<T>::rendererBusy()
{
	return this->RENDERER_BUSY;
}

};
template class Vol::goBlockRenderer<goInt8>;
template class Vol::goBlockRenderer<goUInt8>;
template class Vol::goBlockRenderer<goInt16>;
template class Vol::goBlockRenderer<goUInt16>;
template class Vol::goBlockRenderer<goInt32>;
template class Vol::goBlockRenderer<goUInt32>;
template class Vol::goBlockRenderer<goFloat>;
template class Vol::goBlockRenderer<goDouble>;





