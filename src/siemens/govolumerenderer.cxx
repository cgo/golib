#include <govolumerenderer.h>
#include <govolumerendererconfig.h>
#include <go3vector.h>
#include <math.h>

template< class T,class OUT_T >
goVolumeRenderer<T,OUT_T>::goVolumeRenderer () {
  go3DInfo i;
  i.position.x = 128;
  i.position.y = 128;
  i.position.z = -10;
  i.size.x = 256;
  i.size.y = 256;
  i.size.z = 0;
  viewPlane.setInfo (i);
  go3Vector<goDouble> n;
  viewPlane.setEyeDistance (4000);
   
  volumeInfo.position.x = 128;
  volumeInfo.position.y = 128;
  volumeInfo.position.z = 46;
  

  // densityToFloat.addSegment (10, 0.05f, 250, 0.2f);
  
  densityToFloat.addSegment (50, 0.005f, 1000, 0.07f);
  densityToFloat.addSegment (1000, .08f, 2000, .002f);
  // floatToGrey.addSegment (0.5f, 100, 1.0f, 220);
  // floatToGrey.addSegment (0.1f, 50, 1.0f, 220);
  floatToGrey.addSegment (0.1f, 50, 1.0f, 220);

  stages = 1;
}

template< class T,class OUT_T >
goVolumeRenderer<T,OUT_T>::~goVolumeRenderer () {
}

template< class T,class OUT_T >
void
goVolumeRenderer<T,OUT_T>::
set3DBlockInfo (go3DInfo &i) {
  volumeInfo = i;
}

/*
 * Noch nicht fertig. Wird wahrscheinlich fuers Praktikum nicht benoetigt.
 */
#define GO_TRILIN_INTERPOLATE(__block, __coord, __target) {\
     T p0,p1,p2,p3,p4,p5,p6,p7;\
     goSize_t xp0,xp1,xp2,xp3,xp4,xp5,xp6,xp7;\
     goSize_t yp0,yp1,yp2,yp3,yp4,yp5,yp6,yp7;\
     goSize_t zp0,zp1,zp2,zp3,zp4,zp5,zp6,zp7;\
     goDouble x,y,z;\
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


/*!
 * Macro used by calculateDisplayBoundary to project 3D points onto
 * the given viewplane in order to determine the display boundaries.
 * point is the point, viewPlane the viewplane, upx the unit vector in
 * x direction on the plane, upy the pendent in y direction, x and y are
 * the variables to store the x and y value in.
 * In tempZ, the z coordinate relative to the viewplane origin is
 * calculated, in order to enable calculation of the nearest and
 * farthest point.
 */
#define PROJECT_POINT(point, viewPlane, upx, upy, x, y, tempZ) {\
  go3Vector<goDouble> tempP = point;\
  tempP -= viewPlane.getInfo().position;\
  goDouble Ez = viewPlane.getEyeDistance();\
  goDouble p_z = tempP * viewPlane.getNormal();\
  goDouble factP = Ez / (float)(Ez + p_z);\
  goDouble p_x = tempP * upx;\
  goDouble p_y = tempP * upy;\
  tempZ = p_z;\
  p_x *= factP;\
  p_y *= factP;\
  x = p_x;\
  y = p_y;\
}

static
inline
void
calculateDisplayBoundary (const go3DInfo* volumeInfo, 
			  const go3DViewPlane* viewPlane,
			  const go3Vector<goDouble> &upx,
			  const go3Vector<goDouble> &upy,
			  goDouble &maxX, goDouble &minX,
			  goDouble &maxY, goDouble &minY,
			  goDouble &maxZ, goDouble &minZ)
{
  go3Vector<goDouble> p1, p2, p3, p4, p5, p6, p7, p8;
  goDouble volX = volumeInfo->position.x;
  goDouble volY = volumeInfo->position.y;
  goDouble volZ = volumeInfo->position.z;
  goDouble volSizeX_2 = volumeInfo->size.x * 0.5;
  goDouble volSizeY_2 = volumeInfo->size.y * 0.5;
  goDouble volSizeZ_2 = volumeInfo->size.z * 0.5;

  p1.x = volX - volSizeX_2;
  p1.y = volY - volSizeY_2;
  p1.z = volZ - volSizeZ_2;
  p2.x = volX + volSizeX_2;
  p2.y = volY - volSizeY_2;
  p2.z = volZ - volSizeZ_2;
  p3.x = volX - volSizeX_2;
  p3.y = volY + volSizeY_2;
  p3.z = volZ - volSizeZ_2;
  p4.x = volX + volSizeX_2;
  p4.y = volY + volSizeY_2;
  p4.z = volZ - volSizeZ_2;

  p5.x = volX - volSizeX_2;
  p5.y = volY - volSizeY_2;
  p5.z = volZ + volSizeZ_2;
  p6.x = volX + volSizeX_2;
  p6.y = volY - volSizeY_2;
  p6.z = volZ + volSizeZ_2;
  p7.x = volX - volSizeX_2;
  p7.y = volY + volSizeY_2;
  p7.z = volZ + volSizeZ_2;
  p8.x = volX + volSizeX_2;
  p8.y = volY + volSizeY_2;
  p8.z = volZ + volSizeZ_2;

  goDouble x[8],y[8];
  goDouble depth[8];
  PROJECT_POINT(p1, (*viewPlane), upx, upy, x[0], y[0], depth[0]);
  PROJECT_POINT(p2, (*viewPlane), upx, upy, x[1], y[1], depth[1]);
  PROJECT_POINT(p3, (*viewPlane), upx, upy, x[2], y[2], depth[2]);
  PROJECT_POINT(p4, (*viewPlane), upx, upy, x[3], y[3], depth[3]);
  PROJECT_POINT(p5, (*viewPlane), upx, upy, x[4], y[4], depth[4]);
  PROJECT_POINT(p6, (*viewPlane), upx, upy, x[5], y[5], depth[5]);
  PROJECT_POINT(p7, (*viewPlane), upx, upy, x[6], y[6], depth[6]);
  PROJECT_POINT(p8, (*viewPlane), upx, upy, x[7], y[7], depth[7]);
  
  goIndex_t i;
  maxX = x[0]; minX = x[0];
  maxY = y[0]; minY = y[0];
  // goDouble z[8];
  /* z[0] = p1.z; z[1] = p2.z;
     z[2] = p3.z; z[3] = p4.z;
     z[4] = p5.z; z[5] = p6.z;
     z[6] = p7.z; z[7] = p8.z; */
  maxZ = depth[0]; minZ = depth[0];
  for (i = 1; i < 8; i++) 
    {
      if (x[i] > maxX)
	{
	  maxX = x[i];
	}
      if (x[i] < minX)
	{
	  minX = x[i];
	}
      if (y[i] > maxY)
	{
	  maxY = y[i];
	}
      if (y[i] < minY)
	{
	  minY = y[i];
	}
      if (depth[i] > maxZ) 
	{
	  maxZ = depth[i];
	}
      if (depth[i] < minZ) 
	{
	  minZ = depth[i];
	}
    }
}

  

/* __d: double */
#define CLOSEST_INTEGER(__d) ((__d - (goInt32)__d) > 0.5) ? (goInt32)__d + 1 : (goInt32)__d

#define GET_VOXEL_DIRECT() {\
  voxel = *( volume->getPtr(currentRayPointX,\
	  		    currentRayPointY,\
			    currentRayPointZ) );\
}

#define GET_VOXEL_COMPRESSED() {\
  blockX = currentRayPointX >> shiftX;\
  blockY = currentRayPointY >> shiftY;\
  blockZ = currentRayPointZ >> shiftZ;\
  blockPtr = compressed->get3DBlock (blockX \
                                     + blockY * blocksPerX \
                                     + blockZ * blocksPerX * blocksPerY, stage);\
  posX = (currentRayPointX & posXMask) >> (stages - stage); \
  posY = (currentRayPointY & posYMask) >> (stages - stage); \
  posZ = (currentRayPointZ & posZMask) >> (stages - stage); \
  /* OPTIMIEREN */ \
  voxel = *(blockPtr->getPtr (posX, posY, posZ));\
}

static void printProgress (float f) {
  cout << f * 100 << "% done." << endl; 
}

template< class T,class OUT_T >
void
goVolumeRenderer<T,OUT_T>::render (OUT_T *image, 
				   goSize_t width, 
				   goSize_t height,
				   goSize_t stage) {

#ifndef GO_VOLUME_COMPRESSED
  volumeInfo.size.x = volume->getSizeX();
  volumeInfo.size.y = volume->getSizeY();
  volumeInfo.size.z = volume->getSizeZ();
#endif

  cout << densityToFloat[2000] << endl;
  cout << densityToFloat[4000] << endl;
  cout << densityToFloat[-1000] << endl;
  cout << floatToGrey[0.1] << endl;
  cout << floatToGrey[1.0] << endl;
  // return;
  
  goDouble planeDX, planeDY;
  go3Vector<goDouble> eyePos;

  /* Calculate step ratio in x and y directions */
  planeDX = viewPlane.getInfo().size.x / (float)width;
  planeDY = viewPlane.getInfo().size.y / (float)height;
  
  /* Always look at the object */
  go3Vector<goDouble> normal;
  goDouble len;
  normal = volumeInfo.position;
  normal -= viewPlane.getInfo().position;
  len = 1 / normal.abs();
  normal *= len;
  viewPlane.setNormal (normal);

  /* Set up vector. This holds only for the tests using only y/z movements. 
   * Bald entfernen und ---*-VORSISCHT RITSCHIE, VORSISCHT!!-*---
   */
  eyePos.x = 1;
  eyePos.y = 0;
  eyePos.z = 0;
  eyePos.cross (normal);
  eyePos *= 1 / eyePos.abs();
  viewPlane.setUp (eyePos);

  /* Calculate eye position */
  goDouble eyeDistance = viewPlane.getEyeDistance();
  eyePos = normal;
  eyePos *= (goDouble)-eyeDistance;
  eyePos += viewPlane.getInfo().position;


  /* Calculate vectors in x and y directions of length width/2 and height/2 */
  go3Vector<goDouble> upx = viewPlane.getUp();
  go3Vector<goDouble> upy = upx;
  upx.cross (normal);
  upx *= -1.0 / upx.abs();
  upy *= -1.0 / upy.abs();

  /*
   * The ray casting loop. This is for clarity, not for speed (yet).
   */
  goDouble accVal;
  T voxel;

  goSize_t sizeX = (goSize_t)volumeInfo.size.x;
  goSize_t sizeY = (goSize_t)volumeInfo.size.y;
  goSize_t sizeZ = (goSize_t)volumeInfo.size.z;
#ifdef GO_VOLUME_COMPRESSED
  register goSize_t blocksPerX;
  register goSize_t blocksPerY;
  register goSize_t blocksPerZ;
  goSize_t blockX, blockY, blockZ;
  register int shiftX;
  register int shiftY;
  register int shiftZ;
  register goInt32 posX,posY,posZ;
  register goInt32 posXMask, posYMask, posZMask;
  go3DBlock<T> *blockPtr;

  shiftX = compressed->getBlockSizeLog().x;
  shiftY = compressed->getBlockSizeLog().y;
  shiftZ = compressed->getBlockSizeLog().z;
  blocksPerX = sizeX >> shiftX;
  blocksPerY = sizeY >> shiftY;
  blocksPerZ = sizeZ >> shiftZ;
  posXMask = (1 << shiftX) - 1;
  posYMask = (1 << shiftY) - 1;
  posZMask = (1 << shiftZ) - 1;
  blockX = 1;
  goSize_t i;
  for (i = 0; i < (stages - stage); i++) 
    {
      posXMask = posXMask & (~blockX);
      posYMask = posYMask & (~blockX);
      posZMask = posZMask & (~blockX);
      blockX = blockX << 1;
    }
  cout << "Mask values: " << endl;
  cout << dec << posXMask << "," << posYMask << "," << posZMask << dec << endl;
#endif

  /* Maybe remove register? */
  goIndex_t currentRayPointX = 0;
  goIndex_t currentRayPointY = 0;
  goIndex_t currentRayPointZ = 0;
//    register goIndex_t currentRayPointX = 0;
//    register goIndex_t currentRayPointY = 0;
//    register goIndex_t currentRayPointZ = 0;
  
  goDouble upperLeftBoundaryX;
  goDouble upperLeftBoundaryY;
  goDouble lowerRightBoundaryX;
  goDouble lowerRightBoundaryY;
  goDouble depthBoundaryMax;
  goDouble depthBoundaryMin;
  goDouble planeDistance;
  calculateDisplayBoundary (&volumeInfo, &viewPlane, 
			    upx, upy,
			    (goDouble&)lowerRightBoundaryX,
			    (goDouble&)upperLeftBoundaryX,
			    (goDouble&)lowerRightBoundaryY,
			    (goDouble&)upperLeftBoundaryY,
			    (goDouble&)depthBoundaryMax,
			    (goDouble&)depthBoundaryMin);

  goDouble viewPlaneWidth = viewPlane.getInfo().size.x;
  goDouble viewPlaneHeight = viewPlane.getInfo().size.y;
  lowerRightBoundaryX += viewPlaneWidth * 0.5;
  upperLeftBoundaryX  += viewPlaneWidth * 0.5;
  lowerRightBoundaryY += viewPlaneHeight * 0.5;
  upperLeftBoundaryY  += viewPlaneHeight * 0.5;
  if (lowerRightBoundaryX > viewPlaneWidth) 
    {
      lowerRightBoundaryX = viewPlaneWidth;
    } else {
      if (lowerRightBoundaryX < 0) 
	{
	  lowerRightBoundaryX = 0;
	}
    }
  if (upperLeftBoundaryX > viewPlaneWidth) 
    {
      upperLeftBoundaryX = viewPlaneWidth;
    } else {
      if (upperLeftBoundaryX < 0) 
	{
	  upperLeftBoundaryX = 0;
	}
    }
  if (upperLeftBoundaryY > viewPlaneHeight) 
    {
      upperLeftBoundaryY = viewPlaneHeight;
    } else {
      if (upperLeftBoundaryY < 0) 
	{
	  upperLeftBoundaryY = 0;
	}
    }
  if (lowerRightBoundaryY > viewPlaneHeight) 
    {
      lowerRightBoundaryY = viewPlaneHeight;
    } else {
      if (lowerRightBoundaryY < 0) 
	{
	  lowerRightBoundaryY = 0;
	}
    }
  if (depthBoundaryMin < 0) 
    {
      depthBoundaryMin = 0;
    }
  if (depthBoundaryMax < 0) 
    {
      depthBoundaryMax = 0;
    }
  planeDistance = depthBoundaryMin;
  cout << "Boundaries: " << endl;
  cout << "\t" << upperLeftBoundaryX << "," << upperLeftBoundaryY << endl;
  cout << "\t" << lowerRightBoundaryX << "," << lowerRightBoundaryY << endl;


  go3Vector<goDouble> tempY;
  go3Vector<goDouble> tempX;

  /* Calculate upper left point in the view plane to start with */
  go3Vector<goDouble> currentPlanePoint = viewPlane.getInfo().position;
  tempX = upx;
  tempX *= upperLeftBoundaryX;
  tempY = upy;
  tempY *= upperLeftBoundaryY;
  currentPlanePoint += tempX;
  currentPlanePoint += tempY;

  tempX = upx;
  tempY = upy;
  tempX *= viewPlaneWidth * 0.5;
  tempY *= viewPlaneHeight * 0.5;
  currentPlanePoint -= tempX;
  currentPlanePoint -= tempY;

  cout << "Start point after correction: " << currentPlanePoint << endl;
  goSize_t x,y,z;
  goSize_t startX, startY;
  goSize_t endX, endY;
  startX = (goSize_t)(upperLeftBoundaryX / planeDX);
  startY = (goSize_t)(upperLeftBoundaryY / planeDY);
  endX   = (goSize_t)(lowerRightBoundaryX / planeDX);
  endY   = (goSize_t)(lowerRightBoundaryY / planeDY);
//    startX = (goSize_t)(upperLeftBoundaryX);
//    startY = (goSize_t)(upperLeftBoundaryY);
//    endX   = (goSize_t)(lowerRightBoundaryX);
//    endY   = (goSize_t)(lowerRightBoundaryY);
  go3Vector<goDouble> rayStep;
  go3Vector<goDouble> currentRayPoint;
  go3Vector<goDouble> currentPointSave = currentPlanePoint;
  go3Vector<goDouble> tempPoint;

  /* move and scale the viewplane in front of the volume */
  currentPlanePoint = normal;
  currentPlanePoint *= planeDistance;
  currentPlanePoint += viewPlane.getInfo().position;

  upx *= (eyeDistance + planeDistance) / (eyeDistance);
  upy *= (eyeDistance + planeDistance) / (eyeDistance);
  tempX = upx;
  tempX *= upperLeftBoundaryX;
  tempY = upy;
  tempY *= upperLeftBoundaryY;
  currentPlanePoint += tempX;
  currentPlanePoint += tempY;
  tempX = upx;
  tempY = upy;
  tempX *= viewPlaneWidth * 0.5;
  tempY *= viewPlaneHeight * 0.5;
  currentPlanePoint -= tempX;
  currentPlanePoint -= tempY;
  currentPointSave = currentPlanePoint;
  /* done moving and scaling */

  cout << "planeDistance = " << planeDistance << endl;
  cout << "Start point: " << currentPlanePoint << endl;
  cout << "upx = " << upx << endl << "upy = " << upy << endl;
  cout << "upx.abs() = " << upx.abs() << endl;
  cout << "upy.abs() = " << upy.abs() << endl;
  cout << "Normal = " << normal << endl;
  cout << "eyeDistance = " << eyeDistance << endl;
  cout << "eyePos = " << eyePos << endl;
  cout << "viewPlane position = " << viewPlane.getInfo().position << endl;
  cout << "Volume position = " << volumeInfo.position << endl;
  cout << "Maximum ray lenght " << depthBoundaryMax - depthBoundaryMin << endl;

  upx *= planeDX;
  upy *= planeDY;
  for (y = startY; y < endY; y++) {
    currentPlanePoint = currentPointSave;
    for (x = startX; x < endX; x++) {
      z = (goSize_t)depthBoundaryMin;
      accVal = 0;
      currentRayPoint = currentPlanePoint;
      rayStep = currentPlanePoint;
      rayStep -= eyePos;
      rayStep *= 1.0 / (float)rayStep.abs();
      while ( (z++ < depthBoundaryMax) && (accVal < 1.0) ) {
	// while ( (z++ < maxDepth) ) {
	currentRayPointX = CLOSEST_INTEGER(currentRayPoint.x);
	currentRayPointY = CLOSEST_INTEGER(currentRayPoint.y);
	currentRayPointZ = CLOSEST_INTEGER(currentRayPoint.z);
	
	if ( currentRayPointX >= 0 ) {
	  if ( sizeX > (goSize_t)currentRayPointX ) {
	    if ( currentRayPointY >= 0 ) {
	      if ( sizeY > (goSize_t)currentRayPointY ) {
		if ( currentRayPointZ >= 0 ) {
		  if ( sizeZ > (goSize_t)currentRayPointZ ) {
#ifdef GO_VOLUME_COMPRESSED
		    GET_VOXEL_COMPRESSED();
#else
		    GET_VOXEL_DIRECT();
#endif		    
		    accVal += densityToFloat[voxel];
		  }
		}
	      }
	    }
	  }
	}
	currentRayPoint += rayStep;
      }
      // cout << x << "," << y << ": currentRayPoint: " << currentRayPoint << endl;
      image[(width * y) + x] = floatToGrey[accVal < 1.0 ? accVal : 1.0];
      // image[(width * y) + x] = 200;
      // cout << x << "," << y << endl;
      // accVal = accVal * 220;
      // image[width * y + x] = (OUT_T)accVal;
      currentPlanePoint += upx;
    }
    printProgress (y / (float)height);
    currentPointSave += upy;
  }
  image[width * startY + startX] = 255;
  image[width * endY + endX] = 255;
}


template goVolumeRenderer<goInt16,goInt16>;
template goVolumeRenderer<goInt8,goInt16>;
template goVolumeRenderer<goUInt8,goInt16>;








