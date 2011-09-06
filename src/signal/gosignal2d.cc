/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal2d.h>
#include <gomatrix.h>

#define __GOSIG2D_MIN(x,y) {\
  ((x < y) ? x : y) \
}

template <class T>
goSignal2D<T>::goSignal2D (goSize_t width, goSize_t height, 
			   goSize_t border_x, goSize_t border_y,
			   goSize_t blockWidth, goSize_t blockHeight) 
    :
    goObjectBase ()
{
  sizeX = width;
  sizeY = height;
  blockSizeX = blockWidth;
  blockSizeY = blockHeight;
  borderX = border_x;
  borderY = border_y;
  data = new goArray<T>;
  data->resize ((width + (borderX << 1)) * (height + (borderY << 1)) );
  total_memory_usage = sizeof(T) * data->getSize();
}

template <class T>
goSignal2D<T>::~goSignal2D () {
  // data->resize(0);
  delete data;
}

template <class T>
T*
goSignal2D<T>::getPtr (goIndex_t x, goIndex_t y) {
  return (&data->getPtr()[(y + borderY) * (sizeX + (borderX << 1)) + borderX + x]);
}

template <class T>
T*
goSignal2D<T>::getPtrStart () {
  return &data->getPtr()[(borderY) * (sizeX + (borderX << 1)) + borderX];
}

template <class T>
goPtrOffset_t
goSignal2D<T>::getOffsetY () {
  goPtrOffset_t offX = getOffsetX();
  return ( offX * ( (getBorderX() << 1) + getSizeX() ) );
}

template <class T>
void
goSignal2D<T>::resize (goSize_t width, goSize_t height) {
  goArray<T> *newdata = new goArray<T>;
  newdata->resize ( (height + (borderY << 1)) * (width + (borderX << 1)) );
  goSize_t x, y;
  goPtrOffset_t offsetX = getOffsetX();
  goPtrOffset_t offsetY = getOffsetY();
  goPtrOffset_t newOffsetX = 1;
  goPtrOffset_t newOffsetY = newOffsetX * ( (borderX << 1) + width );

  T* linePtr;
  T* linePtrStart;
  T* newLinePtr;
  T* newLinePtrStart;

  linePtrStart    = linePtr    = getPtr (0,0);
  newLinePtrStart = newLinePtr = &newdata->getPtr()[borderY * ((borderX << 1) + width) + borderX];
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      *newLinePtr = *linePtr;
      newLinePtr += newOffsetX;
      linePtr += offsetX;
    }
    newLinePtr = newLinePtrStart = newLinePtrStart + newOffsetY;
    linePtr = linePtrStart = linePtrStart + offsetY;
  }

  sizeX = width;
  sizeY = height;
  goArray<T> *tmp = data;
  data = newdata;
  tmp->resize (0);
  delete tmp;
}

template <class T>
goSignal2D<T>&
goSignal2D<T>::operator-= (goSignal2D<T>& other) {
  T* linePtr;
  T* oLinePtr;
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;
  int oLinePtrOffsetX = 1;
  int oLinePtrOffsetY = (2 * other.getBorderX()) * oLinePtrOffsetX;;

  linePtr  = getPtr(0,0);
  oLinePtr = other.getPtr(0,0);

  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      *linePtr -= *oLinePtr;
      linePtr = linePtr + linePtrOffsetX;
      oLinePtr = oLinePtr + oLinePtrOffsetX;
    }
    linePtr = linePtr + linePtrOffsetY;
    oLinePtr = oLinePtr + oLinePtrOffsetY;
  }
  return *this;
}

template <class T>
goSignal2D<T>&
goSignal2D<T>::operator+= (goSignal2D<T>& other) {
  T* linePtr;
  T* oLinePtr;
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;
  int oLinePtrOffsetX = 1;
  int oLinePtrOffsetY = (2 * other.getBorderX()) * oLinePtrOffsetX;;

  linePtr  = getPtr(0,0);
  oLinePtr = other.getPtr(0,0);

  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      *linePtr += *oLinePtr;
      linePtr = linePtr + linePtrOffsetX;
      oLinePtr = oLinePtr + oLinePtrOffsetX;
    }
    linePtr = linePtr + linePtrOffsetY;
    oLinePtr = oLinePtr + oLinePtrOffsetY;
  }
  return *this;
}

template <class T>
goSignal2D<T>&
goSignal2D<T>::operator*= (T value) {
  T* linePtr;
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;

  linePtr  = getPtr(0,0);

  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      *linePtr *= value;
      linePtr = linePtr + linePtrOffsetX;
    }
    linePtr = linePtr + linePtrOffsetY;
  }
  return *this;
}

template <class T>
goSignal2D<T>&
goSignal2D<T>::operator= (goSignal2D<T>& other) {
  T* linePtr;
  T* oLinePtr;
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;
  int oLinePtrOffsetX = 1;
  int oLinePtrOffsetY = (2 * other.getBorderX()) * oLinePtrOffsetX;;

  linePtr  = getPtr(0,0);
  oLinePtr = other.getPtr(0,0);

  goSize_t x,y;
  
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      *linePtr = *oLinePtr;
      linePtr = linePtr + linePtrOffsetX;
      oLinePtr = oLinePtr + oLinePtrOffsetX;
    }
    linePtr = linePtr + linePtrOffsetY;
    oLinePtr = oLinePtr + oLinePtrOffsetY;
  }
  return *this;
}

/**
 * other lies on this, (xrel,yrel) denotes the upper left corner of other relative to this.
 * (this' upper left corner lies in (0,0), coordinates are like screen coordinates)
 */
template <class T>
T
goSignal2D<T>::meanDiff ( goSignal2D<T>& other, 
			  goIndex_t xrel, goIndex_t yrel ) {
  goDouble diff;

  T* linePtr;
  T* oLinePtr;
  T* endPtr;
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;
  int oLinePtrOffsetX = 1;
  int oLinePtrOffsetY = (2 * other.getBorderX()) * oLinePtrOffsetX;;

  goSize_t deltaX;  
  goSize_t deltaY;

  if (xrel < 0) {
    deltaX = other.getSizeX() + xrel;
    if (yrel < 0) {
      deltaY   = other.getSizeY() + yrel;
      linePtr  = getPtr(0,0);
      oLinePtr = other.getPtr(-xrel, -yrel);
      endPtr   = getPtr(other.getSizeX() + xrel, other.getSizeY() + yrel);
    } else {
      deltaY   = getSizeY() - yrel;
      linePtr  = getPtr(0, yrel);
      oLinePtr = other.getPtr(-xrel, 0);
      endPtr   = getPtr(other.getSizeX() + xrel, getSizeY());
    }
  } else {
    deltaX = getSizeX() - xrel;
    if (yrel < 0) {
      deltaY   = other.getSizeY() + yrel;
      linePtr  = getPtr(xrel, 0);
      oLinePtr = other.getPtr(0, -yrel);
      endPtr   = getPtr(getSizeX(), other.getSizeY() + yrel);
    } else {
      deltaY   = getSizeY() - yrel;
      linePtr  = getPtr(xrel, yrel);
      oLinePtr = other.getPtr(0, 0);
      endPtr   = getPtr(getSizeX(), getSizeY());
    }
  }

  goSize_t x,y;

  T* linePtrStart  = linePtr;
  T* oLinePtrStart = oLinePtr;

  goFloat size = (goFloat)(deltaX * deltaY);

  diff = 0.0f;
  for (y = 0; y < deltaY; y++) {
    for (x = 0; x < deltaX; x++) {
      diff += (goDouble)fabs ((goDouble) (*linePtr - *oLinePtr)) / size;
      linePtr = linePtr + linePtrOffsetX;
      oLinePtr = oLinePtr + oLinePtrOffsetX;
    }
    linePtr = linePtrStart = linePtrStart + linePtrOffsetY;
    oLinePtr = oLinePtrStart = oLinePtrStart + oLinePtrOffsetY;
  }
  
  return (T)diff;
  
}


/**
 * m: 3x3 transformation matrix applied to other
 */
#if 0
template <class T>
T
goSignal2D<T>::meanDiff ( goSignal2D<T>& other, 
			  goMatrix<goDouble>& m ) 
{
  goDouble diff;
  
  T* linePtr  = getPtr (0,0);
  int linePtrOffsetX = 1;
  int linePtrOffsetY = (2 * borderX) * linePtrOffsetX;
  
  register goSize_t deltaX = __GOSIG2D_MIN(getSizeX(), other.getSizeX());  
  register goSize_t deltaY = __GOSIG2D_MIN(getSizeY(), other.getSizeY());  
  
  register goSize_t x,y;

  T* linePtrStart  = linePtr;

  goFloat size = (goFloat)(deltaX * deltaY);

  diff = 0.0f;

  goMatrix<goDouble> otherPixel(3, 1);
  otherPixel[0][0] = 0;
  otherPixel[1][0] = 0;
  otherPixel[2][0] = 1.0;

  goMatrix<goDouble> tempMatrix(3,1);

  for (y = 0; y < deltaY; y++) {
    for (x = 0; x < deltaX; x++) {
      tempMatrix = otherPixel;
      // cout << "other: " << otherPixel[0][0] << ", " << otherPixel[1][0] << endl;
      tempMatrix = m * otherPixel;
      // cout << "temp: " << tempMatrix[0][0] << ", " << tempMatrix[1][0] << endl << endl;
      tempMatrix[0][0] /= tempMatrix[2][0];
      tempMatrix[1][0] /= tempMatrix[2][0];
      tempMatrix[2][0] = 1.0;
      if ( (tempMatrix[0][0] >= 0) && (tempMatrix[0][0] < (goDouble)getSizeX()) 
	   && (tempMatrix[1][0] >= 0) && (tempMatrix[1][0] < (goDouble)getSizeY()) ) {
	diff += (goDouble)fabs ((goDouble) (*getPtr ((goIndex_t)tempMatrix[0][0], 
						     (goIndex_t)tempMatrix[1][0]) - 
					    *other.getPtr(x, y))) / size;
      }
      linePtr = linePtr + linePtrOffsetX;
      otherPixel[0][0] += 1;
      
    }
    otherPixel[0][0] = 0;
    otherPixel[1][0] += 1;
    linePtr = linePtrStart = linePtrStart + linePtrOffsetY;
  }
  
  return (T)diff;
}
#endif

template <class T>
void
goSignal2D<T>::fill (T val) {
    goIndex_t i;
    for (i = 0; i < data->getSize(); i++) {
	(*data)[i] = val;
    }
}

/* Optimize for pointer arithmetic in y direction. */
#define SIGNAL2D_COPY(startX1, startY1, endX1, endY1, startX2, startY2, endX2, endY2, source, target) {\
   {\
	T* localPtr1;\
        T* localPtr2;\
        goIndex_t localY, localX, localY2;\
        goPtrOffset_t localOffX1 = source.getOffsetX();\
        goPtrOffset_t localOffX2 = target.getOffsetX();\
        localY2 = startY2;\
         for (localY = startY1; localY <= endY1; localY++, localY2++) {\
           localPtr1 = source.getPtr (startX1, localY);\
           localPtr2 = target.getPtr (startX2, localY2);\
           for (localX = startX1; localX <= endX1; localX++) {\
              *localPtr2 = *localPtr1;\
              localPtr1 += localOffX1;\
              localPtr2 += localOffX2;\
           }\
        }\
   }\
}


template <class T>
void
goSignal2D<T>::put (goSignal2D<T>& s, goIndex_t x, goIndex_t y) {
  SIGNAL2D_COPY(0, 0, (s.getSizeX() - 1), (s.getSizeY() - 1),
		x, y, (x + s.getSizeX() - 1), (y + s.getSizeY() - 1),
		s, (*this));
}



template <class T>
void
goSignal2D<T>::interpolateBorders (bool interpolate) {
  T* ptr  = getPtr (-1,0);
  T* ptr2 = getPtr (getSizeX(), 0);
  int offsetX = getOffsetX();
  int offsetY = getOffsetY();

  goSize_t sx, sy;
  sx = getSizeX();
  sy = getSizeY();
  goSize_t i;
  for (i = 0; i < sy; i++) {
    *ptr  = *(ptr + offsetX);
    *ptr2 = *(ptr2 - offsetX);
    ptr  += offsetY;
    ptr2 += offsetY;
  }
  ptr  = getPtr (-1,-1);
  ptr2 = getPtr (-1,sy);
  for (i = 0; i < (sx + 2); i++) {
    *ptr  = *(ptr + offsetY);
    *ptr2 = *(ptr2 - offsetY);
    ptr  += offsetX;
    ptr2 += offsetX;
  }
}

template <class T>
void
goSignal2D<T>::interpolateBorders (goIndex_t xleft, goIndex_t xright, goIndex_t yup, goIndex_t ydown) {
  T* ptr  = getPtr (-1,0);
  T* ptr2 = getPtr (getSizeX(), 0);
  int offsetX = getOffsetX();
  int offsetY = getOffsetY();

  goSize_t sx, sy;
  sx = getSizeX();
  sy = getSizeY();
  goSize_t i;
  for (i = 0; i < sy; i++) {
    *ptr  = *(ptr + (offsetX * (xleft + 1)) );
    *ptr2 = *(ptr2 - (offsetX * (xright + 1)) );
    ptr  += offsetY;
    ptr2 += offsetY;
  }
  ptr  = getPtr (0,-1);
  ptr2 = getPtr (0,sy);
  for (i = 0; i < sx; i++) {
    *ptr  = *(ptr + (offsetY * (yup + 1)) );
    *ptr2 = *(ptr2 - (offsetY * (ydown + 1)) );
    ptr  += offsetX;
    ptr2 += offsetX;
  }
}


template <class T>
void
goSignal2D<T>::interpolateBordersPeriodic () {
  SIGNAL2D_COPY( 0,0,(getBorderX() - 1), (getSizeY() - 1), 
		 (getSizeX()), 0, (getSizeX() + getBorderX() - 1), (getSizeY() - 1) , (*this), (*this));		 
  SIGNAL2D_COPY( (getSizeX() - getBorderX()), 0, (getSizeX() - 1), (getSizeY() - 1),
		 (-getBorderX()), 0, -1, (getSizeY() - 1) , (*this), (*this) );		 
  SIGNAL2D_COPY( 0, 0, (getSizeX() - 1), (getBorderY() - 1),
		 0, (getSizeY()), (getSizeX() - 1), (getSizeY() + getBorderY() - 1) , (*this), (*this) );
  SIGNAL2D_COPY( 0, (getSizeY() - getBorderY()), (getSizeX() - 1), (getSizeY() - 1),
		 0, (-getBorderY()), (getSizeX() - 1), -1, (*this), (*this) );
}




template class goSignal2D<goInt8>;
template class goSignal2D<goUInt8>;
template class goSignal2D<goInt16>;
template class goSignal2D<goUInt16>;
template class goSignal2D<goInt32>;
template class goSignal2D<goUInt32>;
template class goSignal2D<goFloat>;
template class goSignal2D<goDouble>;







