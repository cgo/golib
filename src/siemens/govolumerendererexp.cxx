#include <govolumerendererexp.h>
#include <govolumerendererconfig.h>
#include <go3vector.h>
#include <go44matrix.h>
#include <go4vector.h>

#include <go3dalgo.h>

#include <math.h>

/*
 * Parental advisory:
 * If you read the comments in this file, 
 * please excuse my toilet language at some 
 * points. If you are easily offended, become a priest or something
 * and do not read this source.
 */
template< class T,class OUT_T >
goVolumeRendererEXP<T,OUT_T>::goVolumeRendererEXP () {
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
  
//    densityToFloat.addSegment (500, .001f, 1000, .004f);
//    densityToFloat.addSegment (1500, .009f, 3000, .07f);

  // MIP
  densityToFloat.addSegment (40, 0.00f, 250, 1.0f);    

  // densityToFloat.addSegment (10, 0.00001f, 139, 0.013f);    
  // densityToFloat.addSegment (1000, .08f, 2000, .002f);
  // floatToGrey.addSegment (0.5f, 100, 1.0f, 220);
  // floatToGrey.addSegment (0.1f, 50, 1.0f, 220);

  // Second must be larger than 10, else smear bug! (Check transfer function)
  floatToGrey.addSegment (0.0f, 11, 1.0f, 250);

  stages = 1;
}

template< class T,class OUT_T >
goVolumeRendererEXP<T,OUT_T>::~goVolumeRendererEXP () {
}

template< class T,class OUT_T >
void
goVolumeRendererEXP<T,OUT_T>::
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
 * UNUSED IN EXP RENDERER -- USES MATRIX OPERATIONS INSTEAD
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

/*
 * UNUSED IN EXP RENDERER
 */
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

#define GO_VREXP_FINDBOUNDS_START() {\
   go4Vector<goDouble> __pos;

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
goVolumeRendererEXP<T,OUT_T>::render (OUT_T *image, 
				      goSize_t width, 
				      goSize_t height,
				      goSize_t stage) {
  goDouble *tempImage = new goDouble[width * height];
  go3Vector<goDouble> u3, v3, normal;
  
  // Temp variables
  go44Matrix<goDouble>	tempM;
  go3Vector<goDouble>	tempV3;
  go4Vector<goDouble>	tempV4;
  goDouble		tempD, tempD2;

  go3Vector<goDouble> planeSize;
  planeSize = viewPlane.getInfo().size;
  normal = viewPlane.getNormal();
  // Unit vector in x direction on the viewplane
  u3 = normal;
  v3 = viewPlane.getUp();
  u3.cross (v3);
  u3 *= 1 / u3.abs();
  
  // Unit vector in y direction on the viewplane
  v3 *= -1 / v3.abs();

  // Eye position
  go3Vector<goDouble> eyePos;
  eyePos = normal;
  eyePos *= -viewPlane.getEyeDistance();
  eyePos += viewPlane.getPosition();

  cout << "eyePos: " << eyePos << endl;
  cout << "normal: " << normal << endl;
  cout << "u3: " << u3 << endl;
  cout << "v3: " << v3 << endl;
  cout << "planePos: " << viewPlane.getPosition() << endl;

  // Sample distance for each ray in unit lengths.
  goDouble sampleDistance = 1.0f;

  // View matrix
  go44Matrix<goDouble> Tm ( 1, 0, 0, -eyePos.x,
			    0, 1, 0, -eyePos.y,
			    0, 0, 1, -eyePos.z,
			    0, 0, 0, 1 );
			   
  go44Matrix<goDouble> B ( u3.x, v3.x, normal.x, 0,
			   u3.y, v3.y, normal.y, 0,
			   u3.z, v3.z, normal.z, 1,
			   0,    0,    0,        1 );


  // Make B the base change matrix from the world coordinate system
  // to the plane coordinate system.
  B.transpose();
  tempM = B;

  tempM *= Tm;
  // Projection Matrix
  go44Matrix<goDouble> Tproj ( 1, 0, 0, 0,
			       0, 1, 0, 0,
			       0, 0, 1, 0,
			       0, 0, 1 / viewPlane.getEyeDistance(), 0 );
  // Make Tproj the projection matrix to project a point onto the viewplane.
  Tproj *= tempM;
  

  /* Calculate distance between the middle of each block 
   * and the eye point and sort blocks
   * (scheint zu funzen)
   */
  // Step sizes in x/y/z directions for each block in world (volume) 
  // coordinates
  goDouble xStep   = compressed->getBlockSize().x;
  goDouble yStep   = compressed->getBlockSize().y;
  goDouble zStep   = compressed->getBlockSize().z;
  goSize_t xBlocks = (goSize_t)(volumeInfo.size.x / xStep + 0.5);
  goSize_t yBlocks = (goSize_t)(volumeInfo.size.y / yStep + 0.5);
  goSize_t zBlocks = (goSize_t)(volumeInfo.size.z / zStep + 0.5);
  go3Vector<goDouble> curPos, curPosStart;
  curPos.x = (volumeInfo.position.x - (volumeInfo.size.x / 2.0f)) + xStep / 2.0f;
  curPos.y = (volumeInfo.position.y - (volumeInfo.size.y / 2.0f)) + yStep / 2.0f;
  curPos.z = (volumeInfo.position.z - (volumeInfo.size.z / 2.0f)) + zStep / 2.0f;
  curPosStart = curPos;
  goSize_t x,y,z;
  goSize_t blockIndex = 0;
  goArray<goSize_t>	blockIndices;
  goArray<goDouble>	blockDistances;
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

  goDouble* tip = tempImage;
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
  go4Vector<goDouble> curPos4;
  go4Vector<goDouble> curPosStart4;
  //  goDouble front;
  //  goDouble back;
  curPosStart4.x = (volumeInfo.position.x - (volumeInfo.size.x) / 2.0f);
  curPosStart4.y = (volumeInfo.position.y - (volumeInfo.size.y) / 2.0f);
  curPosStart4.z = (volumeInfo.position.z - (volumeInfo.size.z) / 2.0f);
  go3DBlock<T> *block;
  // Pre calculate block boundaries on the image raster?
  go3Vector<goDouble> rayStep;
  // goSize_t *blockBoundaries;
  //  goDouble *rayStepsX, *rayStepsY, *rayStepsZ;
  // blockBoundaries = new goSize_t[blockIndices.getSize() << 2];
//    rayStepsX	  = new goDouble[height * width];
//    rayStepsY	  = new goDouble[height * width];
//    rayStepsZ	  = new goDouble[height * width];

  go3Vector<goDouble> planePoint;
  go3Vector<goDouble> planePointSave;
  // Calculate start position on the view plane with respect to
  // world (=volume) coordinates ...
  planePointSave  = viewPlane.getPosition();
  tempV3      = u3;
  tempV3     *= (width >> 1);
  planePointSave -= tempV3;
  tempV3      = v3;
  tempV3     *= (height >> 1);
  planePointSave -= tempV3;
  k = 0;
  clock_t t1,t2;
//    t1 = clock();
//    for (i = 0; i < height; i++)
//      {
//        planePoint = planePointSave;
//        for (j = 0; j < width; j++)
//  	{
//  	  rayStep = planePoint;
//  	  rayStep -= eyePos;
//  	  // Just for testing ... make it large.
//  	  rayStep *= 10000;
//  	  rayStepsX[k] = rayStep.x;
//  	  rayStepsY[k] = rayStep.y;
//  	  rayStepsZ[k] = rayStep.z;
//  	  k++;
//  	  planePoint += u3;
//  	}
//        planePointSave += v3;
//      }
//    t2 = clock();
//    cout << "Time for calculating raySteps: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;

  goDouble width_2 = (goDouble)(width >> 1);
  goDouble height_2 = (goDouble)(height >> 1);
  t1 = clock();
  goDouble _1_y_x = 1 / (float)(xBlocks * yBlocks);
  goDouble _1_x   = 1 / (float)xBlocks;
  goSize_t _x_y   = xBlocks * yBlocks;
  GO_VREXP_FINDBOUNDS_START()

  for (i = 0; i < (goSize_t)blockIndices.getSize(); i++)
    {
      printProgress (i / (float)blockIndices.getSize());
      /*
       * 1. Project block corner points on viewplane
       * 2. Clip all rays in the resulting bounding box with the current block
       * 3. Accumulate the clipped sub-rays into the accumulation buffer
       */
      // Position of corner 0 of first block
      blockIndex = blockIndices[i];

      tempD = blockIndex * _1_y_x;
      curPos4 = curPosStart4;
      // Divisionen bei Gelegenheit wegoptimieren (mitsortieren beim
      //  Sortieren der Blöcke weiter oben)
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
      goDouble left, right, up, down;
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

      go3Vector<goDouble> start,end;
      bool lineIsValid;
      block = compressed->get3DBlock(blockIndex, stage);
      T* blockPtr = block->getPtr();
      goPtrdiff_t dx = block->getXDiff();
      goPtrdiff_t dy = block->getYDiff();
      goPtrdiff_t dz = block->getZDiff();

      // EXPERIMENTAL -- adapt the step size to the stage (= resolution)
      xStep = block->getSizeX();
      yStep = block->getSizeY();
      zStep = block->getSizeZ();

      goDouble tempImageVal;
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
//  		  goDouble blockDiffD = start.x * dx + start.y * dy + start.z * dz;
//  		  goDouble blockOffsetD = rayStep.x * dx + rayStep.y * dy + rayStep.z * dz;
		      for ( ; z > 0; z--)
			{
			  blockDiff = (goPtrdiff_t)(start.x - 0.5) * dx +
			    (goPtrdiff_t)(start.y - 0.5) * dy +
			    (goPtrdiff_t)(start.z - 0.5) * dz;
			  // Akkumulierendes raycasting
			  //			  tempImageVal += densityToFloat[*(blockPtr + blockDiff)];
			  // MIP
  			  if (tempImageVal < densityToFloat[*(blockPtr + blockDiff)])
  			    tempImageVal = densityToFloat[*(blockPtr + blockDiff)];
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
  compressed->printTimes();

  // Fill the image buffer
  OUT_T *imagePtr = image;
  goDouble *tempImagePtr = tempImage;
//    for (j = 0; j < (height * width); j++)
//      {
//        *imagePtr = floatToGrey[*tempImagePtr];
//        imagePtr++;
//        tempImagePtr++;
//      }
  y = 0;
  z = 1;
  z <<= (compressed->getStages() - stage);
  int z2 = z;
  cout << "z = " << z << endl;
  for (i = 0; i < height; i++)
    {
      x = 0;
      for (j = 0; j < width; j++)
	{
	  *(imagePtr + j + i*width) = floatToGrey[*(tempImagePtr + x + y * width)];
	  z--;
	  if (z == 0)
	    {
	      z = 1;
	      z <<= (compressed->getStages() - stage);
	      x++;
	    }
	}
      z2--;
      if (z2 == 0)
	{
	  z2 = 1;
	  z2 <<= (compressed->getStages() - stage);
	  y++;
	}
    }

  /*
   * The following part has nothing to do with the rendering and was used for
   * debugging and testing purposes.
   * Delete it when the time comes.
   * May the force be with you.
   */
  // Projection test
  go4Vector<goDouble> p1;
  p1.x = 32;
  p1.y = 32;
  p1.z = 0;
  p1.t = 1;
  go4Vector<goDouble> p2;
  p2  = p1;
  cout << p2.x << "," << p2.y << "," << p2.z << "," << p2.t << endl;
  p2 *= Tproj;

  cout << "Projection matrix:" << endl;
  goSize_t p,q;
  for (p = 0; p < 4; p++) 
    {
      for (q = 0; q < 4; q++)
	{
	  cout << Tproj.elem(p,q) << ",";
	}
      cout << endl;
    }


//    clock_t t1,t2;
//    t1 = clock();
//    for (i = 0; i < 256000; i++)
//      {
//        p2 = Tproj * p1;
//      }
//    t2 = clock();
//    cout << "Time for 256000 projections: " << (t2 - t1) / (float)CLOCKS_PER_SEC << endl;

  p2.div();
  cout << "projects onto" << endl;
  cout << p2.x << "," << p2.y << "," << p2.z << "," << p2.t << endl;

  // Clean up.
  delete[] tempImage;
  //  delete[] blockBoundaries;
//    delete[] rayStepsX;
//    delete[] rayStepsY;
//    delete[] rayStepsZ;
}


template goVolumeRendererEXP<goInt16,goInt16>;
template goVolumeRendererEXP<goInt8,goInt16>;
template goVolumeRendererEXP<goUInt8,goInt16>;








