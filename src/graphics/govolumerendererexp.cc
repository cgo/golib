#include <govolumerendererexp.h>
#include <go3vector.h>
#include <go44matrix.h>
#include <go4vector.h>

#include <go3dalgo.h>

#include <math.h>
#include <time.h>

namespace Vol {

/*
 * Parental advisory:
 * If you read the comments in this file, 
 * please excuse my toilet language at some 
 * points. If you are easily offended, become a priest or something
 * and do not read this source.
 */
template< class T,class OUT_T >
goVolumeRendererEXP<T,OUT_T>::goVolumeRendererEXP () {
//   viewPlane.setSize (256,256);
//   viewPlane.setPosition (128,128,-10);
//   go3Vector<volFloat> n;
//   viewPlane.setEyeDistance (4000);
   
//   position.x = 128;
//   position.y = 128;
//   position.z = 46;
  
//    densityToFloat.addSegment (500, .001f, 1000, .004f);
//    densityToFloat.addSegment (1500, .009f, 3000, .07f);
//  densityToFloat.addSegment (1, .001f, 128, .004f);
//  densityToFloat.addSegment (1, 1.0f, 128, 1.0f);
    densityToFloat.addSegment (1, 0.001, 256, .008);

  // MIP
  //  densityToFloat.addSegment (40, 0.00f, 4096, 1.0f);    

  //  densityToFloat.addSegment (10, 0.00001f, 139, 0.013f);    
  // densityToFloat.addSegment (1000, .08f, 2000, .002f);
  // floatToGrey.addSegment (0.5f, 100, 1.0f, 220);
  // floatToGrey.addSegment (0.1f, 50, 1.0f, 220);

  // Second must be larger than 10, else smear bug! (Check transfer function)
  floatToGrey.addSegment (0.1f, 11, 1.0f, 250);

  stages = 1;

  tempImage = 0;
  width = 0;
  height = 0;
}

template< class T,class OUT_T >
goVolumeRendererEXP<T,OUT_T>::~goVolumeRendererEXP () {
  if (tempImage)
    delete tempImage;
}

/*
 * Noch nicht fertig. Wird noch nicht benoetigt.
 */
#define GO_TRILIN_INTERPOLATE(__block, __coord, __target) {\
     T p0,p1,p2,p3,p4,p5,p6,p7;\
     goSize_t xp0,xp1,xp2,xp3,xp4,xp5,xp6,xp7;\
     goSize_t yp0,yp1,yp2,yp3,yp4,yp5,yp6,yp7;\
     goSize_t zp0,zp1,zp2,zp3,zp4,zp5,zp6,zp7;\
     volFloat x,y,z;\
     xp0 = __coord.x;\
     yp0 = __coord.y;\
     zp0 = __coord.z;\
     xp1 = xp0 + 1;\
     yp1 = yp0;\
     zp1 = zp0;\
     xp2 = xp0;\
     yp2 = yp0 + 1;\
     zp2 = zp0;\
     xp3 = xp1;\
     yp3 = yp2;\
     zp3 = zp0;\
\
     xp4 = xp0;\
     yp4 = yp0;\
     zp4 = zp0 + 1;\
     xp5 = xp1;\
     yp5 = yp1;\
     zp5 = zp4;\
     xp6 = xp2;\
     yp6 = yp2;\
     zp6 = zp4;\
     xp7 = xp3;\
     yp7 = yp3;\
     zp7 = zp4;\
\
		 /* Now we have the point coordinates. Calculate x,y,z in the\
		    2x2x2 cube and interpolate. */\
}



/* __d: double */
#define CLOSEST_INTEGER(__d) ((__d - (goInt32)__d) > 0.5) ? (goInt32)__d + 1 : (goInt32)__d


static void printProgress (float f) {
  cout << f * 100 << "% done." << endl; 
}

#define GO_VREXP_FINDBOUNDS_START() {\
   go4Vector<volFloat> __pos;

#define GO_VREXP_FINDBOUNDS_END() }

#define GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down) { \
  if (__pos.x < __left)				\
    {						\
      __left = __pos.x;				\
    } else					\
      {						\
        if (__pos.x > __right)			\
        {					\
          __right = __pos.x;			\
        }					\
      }						\
  if (__pos.y < __up)				\
    {						\
      __up = __pos.y;				\
    } else					\
      {						\
        if (__pos.y > __down)			\
        {					\
          __down = __pos.y;			\
        }					\
      }						\
}



#define GO_VREXP_FINDBOUNDS(__blockStart, \
			    __left, __right, \
			    __up, __down) {	\
  __pos		= __blockStart;			\
  __pos		*= Tproj;			\
  __pos.div();					\
  __left	= __pos.x;			\
  __right	= __left;			\
  __up		= __pos.y;			\
  __down	= __up;				\
						\
  __pos = __blockStart;				\
  __pos.x += xStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.y += yStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.x += xStep - 1;				\
  __pos.y += yStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep - 1;				\
  __pos.x += xStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep - 1;				\
  __pos.y += yStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep - 1;				\
  __pos.x += xStep - 1;				\
  __pos.y += yStep - 1;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
}


template< class T,class OUT_T >
void
goVolumeRendererEXP<T,OUT_T>::renderInit(OUT_T *img, 
					 goSize_t w,
					 goSize_t h)
{
  image = img;
  if (!tempImage)
    {
      tempImage = new volFloat[w * h];
    }
  else {
    if ((w != width) || (h != height))
    {
      delete[] tempImage;
      tempImage = new volFloat[w * h];
    }
  }

  // Update viewplane to calculate eye position, view matrix etc.
  viewPlane.update();

  width = w;
  height = h;
  
  // Temp variables
  go44Matrix<volFloat>	tempM;
  go3Vector<volFloat>	tempV3;
  go4Vector<volFloat>	tempV4;

  go3Vector<volFloat> planeSize;
  planeSize = viewPlane.getSize();
  normal = viewPlane.getNormal();
  // Unit vectors in x/y direction on the viewplane
  u3 = viewPlane.getU();
  v3 = viewPlane.getV();

  // Eye position
  eyePos = viewPlane.getEyePos();

  // Projection matrix
  Tproj = viewPlane.getViewMatrix();
  

  /* Calculate distance between the middle of each block 
   * and the eye point and sort blocks
   * (scheint zu funzen)
   */
  // Step sizes in x/y/z directions for each block in world (volume) 
  // coordinates
  xStep   = provider->getBlockSize().x;
  yStep   = provider->getBlockSize().y;
  zStep   = provider->getBlockSize().z;
  xBlocks = (goSize_t)(size.x / xStep + 0.5);
  yBlocks = (goSize_t)(size.y / yStep + 0.5);
  zBlocks = (goSize_t)(size.z / zStep + 0.5);

  curPos.x = (position.x - (size.x / 2.0f)) + xStep / 2.0f;
  curPos.y = (position.y - (size.y / 2.0f)) + yStep / 2.0f;
  curPos.z = (position.z - (size.z / 2.0f)) + zStep / 2.0f;
  curPosStart = curPos;
  goSize_t x,y,z;
  goSize_t blockIndex = 0;

  blockIndices.resize(0);
  blockDistances.resize(0);
  for (z = 0; z < zBlocks; z++)
    {
      curPos.y = curPosStart.y;
      for (y = 0; y < yBlocks; y++)
	{
	  curPos.x = curPosStart.x;
	  for (x = 0; x < xBlocks; x++)
	    {
	      // Calculate distance to eye
	      tempV3 = eyePos;
    	      tempV3 -= curPos;
              blockDistances += tempV3.abs();
  	      blockIndices += blockIndex;
	      
	      curPos.x += xStep;
	      blockIndex++;
	    }
	  curPos.y += yStep;
	}
      curPos.z += zStep;
    }
  // Sort both arrays using the distances as sort keys.
  blockDistances.sort (blockIndices);
  
  // Ok ... now we have an array telling us in which order to render the blocks.
  // Next, render them.
  
}


template< class T,class OUT_T >
void
goVolumeRendererEXP<T,OUT_T>::render ()
{
  volFloat* tip = tempImage;
  goSize_t x,y,z;
  for (x = 0; x < (width * height); x++)
    {
      *tip = 0.0f;
      tip++;
    }

  /*
   * Render all the blocks front to back.
   */
  goSize_t i,j,k;
  // goSize_t countX,countY,rayCount;
  go4Vector<volFloat> curPos4;
  go4Vector<volFloat> curPosStart4;
  //  volFloat front;
  //  volFloat back;
  curPosStart4.x = (position.x - (size.x) / 2.0f);
  curPosStart4.y = (position.y - (size.y) / 2.0f);
  curPosStart4.z = (position.z - (size.z) / 2.0f);
  goSignal3D<T> *block;

  go3Vector<volFloat> rayStep;

  go3Vector<volFloat> planePoint;
  go3Vector<volFloat> planePointSave;
  // Calculate start position on the view plane with respect to
  // world (=volume) coordinates ...
  go3Vector<volFloat>	tempV3;

  planePointSave  = viewPlane.getPosition();
  tempV3      = u3;
  tempV3     *= (width >> 1);
  planePointSave -= tempV3;
  tempV3      = v3;
  tempV3     *= (height >> 1);
  planePointSave -= tempV3;
  k = 0;
  clock_t t1,t2;
  
  t1 = clock();
  volFloat _1_y_x = 1 / (float)(xBlocks * yBlocks);
  volFloat _1_x   = 1 / (float)xBlocks;
  goSize_t _x_y   = xBlocks * yBlocks;
  GO_VREXP_FINDBOUNDS_START()

    cout << "Rendering " << blockIndices.getSize() << " blocks" << endl;
  goSize_t blockIndex;
  volFloat tempD, tempD2;
  for (i = 0; i < (goSize_t)blockIndices.getSize(); i++)
    {
      // printProgress (i / (float)blockIndices.getSize());
      /*
       * 1. Project block corner points on viewplane
       * 2. Clip all rays in the resulting bounding box with the current block
       * 3. Accumulate the clipped sub-rays into the accumulation buffer
       */
      // Position of corner 0 of first block
      blockIndex = blockIndices[i];
      
      tempD = blockIndex * _1_y_x;
      curPos4 = curPosStart4;
      // Find out block position
      z = (goSize_t)(tempD);
      tempD2 = blockIndex - z * _x_y;
      y = (goSize_t)(tempD2 * _1_x);
      x = (goSize_t)(tempD2 - y * xBlocks);
      // World coordinates of current block's 0th corner
      curPos4.x += x * xStep;
      curPos4.y += y * yStep;
      curPos4.z += z * zStep;
      curPos4.t = 1;
      // cout << "Index: " << blockIndex << endl;
      // cout << "Current pos " << curPos4.x << "," << curPos4.y << "," << curPos4.z << "," << curPos4.t << endl;
      // Project block on viewplane and determine bounds
      volFloat left, right, up, down;
	GO_VREXP_FINDBOUNDS(curPos4, left, right, up, down);
	left -= 2;
	right += 2;
	up -= 2;
	down += 2;
      planePointSave	= viewPlane.getPosition();
      tempV3		= u3;
      tempV3		*= (goInt32)(left + 0.5);
      planePointSave	+= tempV3;
      tempV3		= v3;
      tempV3		*= (goInt32)(up + 0.5);
      planePointSave	+= tempV3;
      // cout << "plane point (start): " << planePointSave << endl;
      
      /*
       * Schränke den Bereich der clip-Strahlen ein (nur ein Fenster um die Projektion des Blocks)
       */
      y = (height >> 1) + (goInt32)(up + 0.5);
      x = (width >> 1) + (goInt32)(left + 0.5);
      goSize_t xMax = x + (goInt32)(right - left + 0.5);
      goSize_t yMax = y + (goInt32)(down - up + 0.5);
      goSize_t xStart = x;

      // cout << "Block " << i << " at " << " projecting to " << x << "," << y << " window " << xMax << "," << yMax << endl;

      go3Vector<volFloat> start,end;
      bool lineIsValid;
      block = provider->getBlock(blockIndex);
      T* blockPtr = block->getPtr();
      goPtrdiff_t dx = block->getXDiff();
      goPtrdiff_t dy = block->getYDiff();
      goPtrdiff_t dz = block->getZDiff();

      // EXPERIMENTAL -- adapt the step size to the stage (= resolution)
      xStep = block->getSizeX();
      yStep = block->getSizeY();
      zStep = block->getSizeZ();

      volFloat tempImageVal;
      goPtrdiff_t blockDiff;
      GO_3D_CLIPLIANGBARSKY_START()
      for (; y < yMax; y++)
	{
	  planePoint = planePointSave;
	  for (x = xStart; x < xMax; x++)
	    {
	      tempImageVal = tempImage[x + y * width];
	      if (tempImageVal < 1.0f)
		{
  		  rayStep = planePoint;
  		  rayStep -= eyePos;
  		  rayStep *= 10000;
		  GO_3D_CLIPLIANGBARSKY(curPos4.x, (curPos4.x + xStep),
					curPos4.y, (curPos4.y + yStep),
					curPos4.z, (curPos4.z + zStep),
					rayStep, planePoint,
					start, end, lineIsValid);
		  if (lineIsValid)
		    {
		      //cout << "valid line" << endl;
		      rayStep *= 1 / rayStep.abs();
		      end -= start;
		      z = (goSize_t)end.abs();
		      // Adapt length to size of the block (depending on the stage)
		      // z >>= stage;
		      start += planePoint;
		      start.x -= curPos4.x;
		      start.y -= curPos4.y;
		      start.z -= curPos4.z;
//  		  volFloat blockDiffD = start.x * dx + start.y * dy + start.z * dz;
//  		  volFloat blockOffsetD = rayStep.x * dx + rayStep.y * dy + rayStep.z * dz;
		      for ( ; z > 0; z--)
			{
			  blockDiff = (goPtrdiff_t)(start.x - 0.5) * dx +
			    (goPtrdiff_t)(start.y - 0.5) * dy +
			    (goPtrdiff_t)(start.z - 0.5) * dz;
			  // Akkumulieren
			  tempImageVal += densityToFloat[*(blockPtr + blockDiff)];
			  // MIP
  			  //if (tempImageVal < densityToFloat[*(blockPtr + blockDiff)])
			  // tempImageVal = densityToFloat[*(blockPtr + blockDiff)];
			  start.x += rayStep.x;
			  start.y += rayStep.y;
			  start.z += rayStep.z;
			}
		      if (tempImageVal < 1.0f)
			tempImage[x + y * width] = tempImageVal;
		      else 
			tempImage[x + y * width] = 1.0f;
		    }
		}
	      planePoint += u3;
	    }
	  planePointSave += v3;
	}
      GO_3D_CLIPLIANGBARSKY_END();      
      curPos4 = curPosStart4;
    }
  GO_VREXP_FINDBOUNDS_END()
  t2 = clock();
  cout << "Time for render poo poo: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  // compressed->printTimes();

  // Fill the image buffer
  OUT_T *imagePtr = image;
  volFloat *tempImagePtr = tempImage;
   for (j = 0; j < (height * width); j++)
     {
       *imagePtr = floatToGrey[*tempImagePtr];
       imagePtr++;
       tempImagePtr++;
     }
  y = 0;
  z = 1;
  // z <<= (provider->getStages() - stage);
//   int z2 = z;
//   cout << "z = " << z << endl;
//   for (i = 0; i < height; i++)
//     {
//       x = 0;
//       for (j = 0; j < width; j++)
// 	{
// 	  *(imagePtr + j + i*width) = floatToGrey[*(tempImagePtr + x + y * width)];
// 	  z--;
// 	  if (z == 0)
// 	    {
// 	      z = 1;
// 	      z <<= (provider->getStages() - stage);
// 	      x++;
// 	    }
// 	}
//       z2--;
//       if (z2 == 0)
// 	{
// 	  z2 = 1;
// 	  z2 <<= (provider->getStages() - stage);
// 	  y++;
// 	}
//     }

  cout << "End of rendering." << endl;
  cout << "----------------------------------" << endl;

}

}

template Vol::goVolumeRendererEXP<goInt16,goInt16>;
template Vol::goVolumeRendererEXP<goInt8,goInt16>;
template Vol::goVolumeRendererEXP<goUInt8,goInt16>;








