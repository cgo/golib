#include <gosignal2d.h>
#include <math.h>

#define __GOSIG2D_MIN(x,y) {\
  ((x < y) ? x : y) \
}

template <class T>
goSignal2D<T>::goSignal2D (goSize_t width, goSize_t height, 
			   goSize_t blockWidth, goSize_t blockHeight) {
  sizeX = width;
  sizeY = height;
  blockSizeX = blockWidth;
  blockSizeY = blockHeight;
  borderX = 16;
  borderY = 16;
  data = new goArray<T>;
  data->resize ((width + (borderX << 1)) * (height + (borderY << 1)) );
}

template <class T>
goSignal2D<T>::~goSignal2D () {
  data->resize(0);
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
        int localOffX1 = source.getOffsetX();\
        int localOffX2 = target.getOffsetX();\
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
  SIGNAL2D_COPY(0, 0, (goIndex_t)(s.getSizeX() - 1), (goIndex_t)(s.getSizeY() - 1),
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
  ptr  = getPtr (0,-1);
  ptr2 = getPtr (0,sy);
  for (i = 0; i < sx; i++) {
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
  SIGNAL2D_COPY( 0,0, (goIndex_t)(getBorderX() - 1), (goIndex_t)(getSizeY() - 1), 
		 (getSizeX()), 0, (getSizeX() + getBorderX() - 1), (getSizeY() - 1) , (*this), (*this));		 
  SIGNAL2D_COPY( (goIndex_t)(getSizeX() - getBorderX()), 0, (goIndex_t)(getSizeX() - 1), (goIndex_t)(getSizeY() - 1),
		 (goIndex_t)(-getBorderX()), 0, -1, (getSizeY() - 1) , (*this), (*this) );		 
  SIGNAL2D_COPY( 0, 0, (goIndex_t)(getSizeX() - 1), (goIndex_t)(getBorderY() - 1),
		 0, (getSizeY()), (getSizeX() - 1), (getSizeY() + getBorderY() - 1) , (*this), (*this) );
  SIGNAL2D_COPY( 0, (goIndex_t)(getSizeY() - getBorderY()), (goIndex_t)(getSizeX() - 1), (goIndex_t)(getSizeY() - 1),
		 0, (goIndex_t)(-getBorderY()), (goIndex_t)(getSizeX() - 1), -1, (*this), (*this) );
}



template class goSignal2D<goFloat>;
template class goSignal2D<goInt32>;






