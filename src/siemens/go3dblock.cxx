// #include ".\go3dblock.h"
// #include "stdafx.h"
#include <go3dblock.h>
#include <goerror.h>

//template class go3DBlock <int>;
//template class go3DBlock <unsigned char>;

template< class T >
go3DBlock<T>::go3DBlock () {
  ptr = 0;
  xDiff = yDiff = zDiff = 0;
  setSize (0,0,0);
}

template< class T >
go3DBlock<T>::go3DBlock (go3DBlock &other) {
  *this = other;
}

template< class T >
go3DBlock<T>::~go3DBlock () {
}

template< class T >
void
go3DBlock<T>::make (goSize_t x, goSize_t y, goSize_t z) {
  ptr = new T[x * y * z];
  xDiff = 1;
  yDiff = x;
  zDiff = yDiff * y;
  setSize (x,y,z);
}

template< class T >
void
go3DBlock<T>::make (go3DBlock *other) {
  this->make (other->getSizeX(), other->getSizeY(), other->getSizeZ());
}

template< class T >
void
go3DBlock<T>::destroy () {
  delete[] ptr;
}

template< class T >
bool
go3DBlock<T>::read (ifstream &f) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("go3DBlock::read()","No block memory allocated or zero size indicated. No data read.");
    return false;
  }
  goSize_t k;
  T *p = ptr;
  /* Actually useless to use nested loops, but hey, this is just for loading... */
  for (k = 0; k < getSizeZ(); k++) {
    readSlice (f, (goIndex_t)k);
  }
  return true;
}

template< class T >
bool
go3DBlock<T>::readSlice (ifstream &f, goIndex_t slice) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("go3DBlock::read()","No block memory allocated or zero size indicated. No data read.");
    return false;
  }
  goSize_t i,j;
  goPtrdiff_t dx = getXDiff();
  T *p = ptr + (slice * getZDiff());
  /* Actually useless to use nested loops, but hey, this is just for loading... */
  for (j = getSizeY(); j > 0; j--) {
    if (f.eof()) {
      goError::print ("go3DBlock::readSlice()","Early EOF detected.");
      return false;
    }
    for (i = getSizeX(); i > 0; i--) {
      f.read ((char*)p, sizeof(T));
      p += dx;
    }
  }
  return true;
}

template< class T >
bool
go3DBlock<T>::write (ofstream &f) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("go3DBlock::write()","No block memory allocated or zero size indicated. No data written.");
    return false;
  }

  goSize_t k;
  for (k = 0; k < getSizeZ(); k++) {
    writeSlice (f, (goIndex_t)k);
  }
  return true;
}

template< class T >
bool
go3DBlock<T>::writeSlice (ofstream &f, goIndex_t slice) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("go3DBlock::writeSlice()","No block memory allocated or zero size indicated. No data written.");
    return false;
  }
  goSize_t i,j;
  goPtrdiff_t dx = getXDiff();
  T *p = ptr + (slice * getZDiff());
  for (j = getSizeY(); j > 0; j--) {
    for (i = getSizeX(); i > 0; i--) {
      f.write ((const char*)p, sizeof(T));
      p += dx;
    }
  }
  return true;
}

template< class T >
go3DBlock<T>&
go3DBlock<T>::operator= (go3DBlock<T> &other) {
  xDiff = other.getXDiff ();
  yDiff = other.getYDiff ();
  zDiff = other.getZDiff ();
  ptr = other.getPtr ();
  setSize (other.getSizeX(), other.getSizeY(), other.getSizeZ());
  return *this;
}

/* Optimierbar. */
template< class T >
bool
go3DBlock<T>::operator== (go3DBlock<T> &other) {
  goSize_t x,y,z;
  goSize_t thisX, thisY, thisZ;
  x = other.getSizeX();
  y = other.getSizeY();
  z = other.getSizeZ();
  thisX = getSizeX();
  thisY = getSizeY();
  thisZ = getSizeZ();

  if ( (thisX != x) || (thisY != y) || (thisZ != z) ) {
    return false;
  }
  
  goPtrdiff_t oxd = other.getXDiff();
  goPtrdiff_t oyd = other.getYDiff();
  goPtrdiff_t ozd = other.getZDiff();
  goPtrdiff_t xd  = getXDiff();
  goPtrdiff_t yd  = getYDiff();
  goPtrdiff_t zd  = getZDiff();

  T *op;
  T *p;

  for (z; z > 0; z--) {
    y = getSizeY();
    for (y; y > 0; y--) {
      x = getSizeX();
      p  = getPtr (x-1, y-1, z-1);
      op = other.getPtr (x-1, y-1, z-1);
      for (x; x > 0; x--) {
	if ( *p != *op ) {
	  return false;
	}
	p  -= xd;
	op -= oxd;
      }
    }
  }
  return true;
}

template< class T >
T
go3DBlock<T>::getMaximum()
{
  T *p = ptr;
  T *py = p,*pz = p;
  goSize_t x,y,z;
  T maxVal = *p;
  for (z = 0; z < zSize; z++)
    {
      py = pz;
      for (y = 0; y < ySize; y++)
	{
	  p = py;
	  for (x = 0; x < xSize; x++)
	    {
	      if (*p > maxVal)
		{
		  maxVal = *p;
		}
	    }
	}
    }
  return maxVal;
}

template< class T >
T
go3DBlock<T>::getMinimum()
{
  T *p = ptr;
  T *py = p,*pz = p;
  goSize_t x,y,z;
  T minVal = *p;
  for (z = 0; z < zSize; z++)
    {
      py = pz;
      for (y = 0; y < ySize; y++)
	{
	  p = py;
	  for (x = 0; x < xSize; x++)
	    {
	      if (*p < minVal)
		{
		  minVal = *p;
		}
	    }
	}
    }
  return minVal;
}

template class go3DBlock< goInt8 >;
template class go3DBlock< goUInt8 >;
template class go3DBlock< goInt16 >;
template class go3DBlock< goInt32 >;
