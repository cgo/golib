#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <goerror.h>
#include <gosignalmacros.h>
#include <go3vector.h>
#include <string.h> // bzero()

template< class T >
goSignal3D<T>::goSignal3D () {
  ptr = 0;
  real_ptr = 0;
  xDiff = yDiff = zDiff = 0;
  setSize (0,0,0);
}

template< class T >
goSignal3D<T>::goSignal3D (goSize_t x, goSize_t y, goSize_t z,
			   goSize_t border_x, goSize_t border_y, goSize_t border_z)
{
  this->make (x,y,z, border_x, border_y, border_z);
}


template< class T >
goSignal3D<T>::goSignal3D (goSignal3D &other) {
  *this = other;
}

template< class T >
goSignal3D<T>::~goSignal3D () {
}


template<class T>
goSize_t
goSignal3D<T>::memoryUsage()
{
    if (real_ptr) 
	{
	    return (goSize_t)(sizeof(T) * getSizeX() * getSizeY() * getSizeZ());
	}
    return 0;
}

template< class T >
void
goSignal3D<T>::make (goSignal3D *other) {
  this->make (other->getSizeX(), other->getSizeY(), other->getSizeZ(),
	      other->getBorderX(), other->getBorderY(), other->getBorderZ());
}


template< class T >
bool
goSignal3D<T>::read (ifstream &f, bool no_extra_memory) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("goSignal3D::read()","No block memory allocated or zero size indicated. No data read.");
    return false;
  }
  if (!no_extra_memory)		// Load everything, then copy
  {
	  T buffer[xSize * ySize * zSize];
	  T *b_ptr;
	  b_ptr = &buffer[0];
	  goSize_t bufSize = xSize * ySize * zSize * sizeof(T);
	  bzero((void*)b_ptr, bufSize);
	  f.read ((char*)b_ptr, bufSize);
	  GO_SIGNAL3D_EACHELEMENT(*__ptr = *(b_ptr++), (*this), T);
  }
  else   // Use readSlice to load every element
  {
  	  /* Actually useless to use nested loops, but hey, this is just for loading... */
  	  goSize_t k;
  	  for (k = 0; k < getSizeZ(); k++) 
	  {
  	      readSlice (f, (goIndex_t)k, no_extra_memory);
  	  }
  }
  return true;
}

template< class T >
bool
goSignal3D<T>::readSlice (ifstream &f, goIndex_t slice, bool no_extra_memory) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("goSignal3D::readSlice()","No block memory allocated or zero size indicated. No data read.");
    return false;
  }
  goSize_t i,j;
  goPtrdiff_t dx = getXDiff();
  goPtrdiff_t dy = getYDiff();
  T *p2 = ptr + (slice * getZDiff());
  T *p;
  /*
   * Use nested loops since this is also used for subsignals and more 
   * exotic signals not having exactly linear storage of the data.
   * First, load everything in the local buffer, then copy the data.
   */
  if (!no_extra_memory)
  {
	  T buffer[xSize * ySize];
	  goSize_t bufSize = sizeof(T) * xSize * ySize;
	  bzero((void*)&buffer[0], bufSize);
	  f.read ((char*)&buffer[0], bufSize);
	  int ptr = 0;
	  for (j = getSizeY(); j > 0; j--) 
	  {
      	  p = p2;
      	  for (i = getSizeX(); i > 0; i--) {
			*p = buffer[ptr++];
      	    p += dx;
      	  }
      	  p2 += dy;
  	  }
   	  if (f.eof()) 
	  {
   	    goError::print ("goSignal3D::readSlice()","Early EOF detected. Some data might be
				corrupted.");
   	    return false;
   	  }
  } 
  else  // Don't use extra memory, load everything element-by-element
 	  {
 	  for (j = getSizeY(); j > 0; j--) {
 	    p = p2;
 	    for (i = getSizeX(); i > 0; i--) {
 	      f.read ((char*)p, sizeof(T));
 	      // cout << "Read line " << j << " element " << i << ": " << *p << endl;
 	      // f >> *p;
 	      p += dx;
 	    }
 	    p2 += dy;
 	    if (f.eof()) {
 	      goError::print ("goSignal3D::readSlice()","Early EOF detected.");
 	      return false;
 	    }
 	  }
  }
  return true;
}

template< class T >
bool
goSignal3D<T>::write (ofstream &f, bool no_extra_memory) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("goSignal3D::write()","No block memory allocated or zero size indicated. No data written.");
    return false;
  }
  if (!no_extra_memory)		// Load everything, then copy
  {
      T buffer[xSize * ySize * zSize];
      T *b_ptr;
      b_ptr = &buffer[0];
      GO_SIGNAL3D_EACHELEMENT(*(b_ptr++) = *__ptr, (*this), T);
      f.write ((const char*)&buffer[0], sizeof(T) * xSize * ySize * zSize);
  }
  else
  {
  	  goSize_t k;
  	  for (k = 0; k < getSizeZ(); k++) {
  	    writeSlice (f, (goIndex_t)k, no_extra_memory);
  	  }
  }
  return true;
}

template< class T >
bool
goSignal3D<T>::writeSlice (ofstream &f, goIndex_t slice, bool no_extra_memory) {
  if ( (!ptr) || ( (xSize * ySize * zSize) <= 0 ) ) {
    goError::print("goSignal3D::writeSlice()","No block memory allocated or zero size indicated. No data written.");
    return false;
  }
  goSize_t i,j;
  goPtrdiff_t dx = getXDiff();
  goPtrdiff_t dy = getYDiff();
  T *p = ptr + (slice * getZDiff());
  T *p2 = p;
  if (!no_extra_memory)
  {
	  T buffer[xSize * ySize];
	  int ptr = 0;
	  for (j = getSizeY(); j > 0; j--) 
	  {
      	  p = p2;
      	  for (i = getSizeX(); i > 0; i--) {
			buffer[ptr++] = *p;
      	    p += dx;
      	  }
      	  p2 += dy;
  	  }
	  f.write ((const char*)&buffer[0], xSize * ySize * sizeof(T));
  } 
  else 
  {
  	  for (j = getSizeY(); j > 0; j--) {
  	    p = p2;
  	    for (i = getSizeX(); i > 0; i--) {
  	      // cout << "writing line " << j << " element " << i << " = " << *p << endl;
  	      f.write ((const char*)p, sizeof(T));
  	      // f << *p << " ";
  	      p += dx;
  	    }
  	    // f << "\n";
  	    p2 += dy;
  	  }
  }
  return true;
}

template< class T >
void
goSignal3D<T>::setPtr(T *p)
{
    ptr = p;
}

template< class T >
goSignal3D<T>&
goSignal3D<T>::operator= (goSignal3D<T> &other) {
  xDiff = other.getXDiff ();
  yDiff = other.getYDiff ();
  zDiff = other.getZDiff ();
  ptr = other.getPtr ();
  setSize (other.getSizeX(), other.getSizeY(), other.getSizeZ());
  return *this;
}

template< class T >
bool
goSignal3D<T>::operator== (goSignal3D<T> &other) {
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
  goPtrdiff_t xd  = getXDiff();

  T *op;
  T *p;

  for (; z > 0; z--) {
    y = getSizeY();
    for (; y > 0; y--) {
      x = getSizeX();
      p  = getPtr (x-1, y-1, z-1);
      op = other.getPtr (x-1, y-1, z-1);
      for (; x > 0; x--) {
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
goSize_t
goSignal3D<T>::getSize()
{
  return sizeof(T) * ((xSize + (borderX << 1)) * (ySize + (borderY << 1)) * (zSize + (borderZ << 1)));
}

template< class T >
T
goSignal3D<T>::getMaximum()
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
	      p += xDiff;
	    }
	  py += yDiff;
	}
      pz += zDiff;
    }
  return maxVal;
}

template< class T >
T
goSignal3D<T>::getMinimum()
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
	      p += xDiff;
	    }
	  py += yDiff;
	}
      pz += zDiff;
    }
  return minVal;
}

template< class T >
void
goSignal3D<T>::fill (T value)
{
  GO_SIGNAL3D_EACHELEMENT(*__ptr = value, (*this), T);
}

template<class T>
void
goSignal3D<T>::interpolateBorders()
{
	goSubSignal3D<T> source;
	goSubSignal3D<T> target;
	source.setParent (this);
	source.setDiff (getXDiff(), getYDiff(), getZDiff());
	target.setParent (this);
	target.setDiff (getXDiff(), getYDiff(), getZDiff());
	
	source.setSize (getSizeX(), getSizeY(), 1);
	target.setSize (getSizeX(), getSizeY(), 1);
	source.setPosition (0, 0, 0);
	target.setPosition (0, 0, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
	source.setPosition (0, 0, getSizeZ() - 1);
	target.setPosition (0, 0, getSizeZ());
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);

	source.setSize (getSizeX(), 1, getSizeZ() + 2);
	target.setSize (getSizeX(), 1, getSizeZ() + 2);
	source.setPosition (0, 0, -1);
	target.setPosition (0, -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
	source.setPosition (0, getSizeY() - 1, -1);
	target.setPosition (0, getSizeY(), -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);

	source.setSize (1, getSizeY() + 2, getSizeZ() + 2);
	target.setSize (1, getSizeY() + 2, getSizeZ() + 2);
	source.setPosition (0, -1, -1);
	target.setPosition (-1, -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
	source.setPosition (getSizeX() - 1, -1, -1);
	target.setPosition (getSizeX(), -1, -1);
    GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), source, target, T, T);
}		


template <class T>
void
goSignal3D<T>::interpolateFromSignal (goSignal3D<T>& other, Neighbour n)
/*
 * Currently, only 1 voxel wide areas are copied from the "other" signal. If you
 * want to use this e.g. for cyclic filtering with filter width > 2, you have to
 * extend this method accordingly. It shouldn't be hard, but it has to be done.
 */
{
	bool interpolate = false;
    if ( (other.getSizeX() != getSizeX()) ||
         (other.getSizeY() != getSizeY()) ||
         (other.getSizeZ() != getSizeZ()) )
    {
#if _GODEBUG >= 3
		cout << "signal3D: interpolateFromSignal: uneven sizes of signals!" << endl;
#endif
		interpolate = true;
    }
    goSubSignal3D<T> subSource;
    goSubSignal3D<T> subTarget;
    subSource.setParent (&other);
    subTarget.setParent (this);
    subSource.setDiff (other.getXDiff(), other.getYDiff(), other.getZDiff());
    subTarget.setDiff (this->getXDiff(), this->getYDiff(), this->getZDiff());
    switch (n)
    {
	case LEFT: 	subSource.setSize (1, other.getSizeY(), other.getSizeZ());
			subSource.setPosition ((int)other.getSizeX() - 1, 0, 0);
			subTarget.setSize (1, this->getSizeY(), this->getSizeZ());
			subTarget.setPosition (-1, 0, 0);
			subSource.rotateAxes(); subSource.rotateAxes();
			subTarget.rotateAxes(); subTarget.rotateAxes();
			break;
	case RIGHT:	subSource.setSize (1, other.getSizeY(), other.getSizeZ());
			subSource.setPosition (0, 0, 0);
			subTarget.setSize (1, this->getSizeY(), this->getSizeZ());
			subTarget.setPosition (this->getSizeX(), 0, 0);
			subSource.rotateAxes(); subSource.rotateAxes();
			subTarget.rotateAxes(); subTarget.rotateAxes();
			break;
	case TOP:	subSource.setSize (other.getSizeX(), 1, other.getSizeZ());
			subSource.setPosition (0, getSizeY() - 1, 0);
			subTarget.setSize (this->getSizeX(), 1, this->getSizeZ());
			subTarget.setPosition (0, -1, 0);
			subSource.rotateAxes();
			subTarget.rotateAxes();
			break;
	case BOTTOM:	subSource.setSize (other.getSizeX(), 1, other.getSizeZ());
			subSource.setPosition (0, 0, 0);
			subTarget.setSize (this->getSizeX(), 1, this->getSizeZ());
			subTarget.setPosition (0, this->getSizeY(), 0);
			subSource.rotateAxes();
			subTarget.rotateAxes();
			break;
	case FRONT:	subSource.setSize (other.getSizeX(), other.getSizeY(), 1);
			subSource.setPosition (0, 0, getSizeZ() - 1);
			subTarget.setSize (this->getSizeX(), this->getSizeY(), 1);
			subTarget.setPosition (0, 0, -1);
			break;
	case BACK:	subSource.setSize (other.getSizeX(), other.getSizeY(), 1);
			subSource.setPosition (0, 0, 0);
			subTarget.setSize (this->getSizeX(), this->getSizeY(), 1);
			subTarget.setPosition (0, 0, this->getSizeZ());
			break;
	case EDGE1:
		subSource.setSize (other.getSizeX(), 1, 1);
		subSource.setPosition (0, other.getSizeY() - 1, other.getSizeZ() - 1);
		subTarget.setSize (this->getSizeX(), 1, 1);
		subTarget.setPosition (0, -1, -1);
		break;
	case EDGE2:
		subSource.setSize (1, 1, other.getSizeZ());
		subSource.setPosition (0, other.getSizeY() - 1, 0);
		subTarget.setSize (1, 1, this->getSizeZ());
		subTarget.setPosition (this->getSizeX(), -1, 0);
		subSource.rotateAxes();
		subTarget.rotateAxes();
		break;	
	case EDGE3:
		subSource.setSize (other.getSizeX(), 1, 1);
		subSource.setPosition (0, other.getSizeY() - 1, 0);
		subTarget.setSize (this->getSizeX(), 1, 1);
		subTarget.setPosition (0, -1, this->getSizeZ());
		break;	
	case EDGE4:
		subSource.setSize (1, 1, other.getSizeZ());
		subSource.setPosition (other.getSizeX() - 1, other.getSizeY() - 1, 0);
		subTarget.setSize (1, 1, this->getSizeZ());
		subTarget.setPosition (-1, -1, 0);
		subSource.rotateAxes();
		subTarget.rotateAxes();
		break;	
	
	case EDGE5:
		subSource.setSize (other.getSizeX(), 1, 1);
		subSource.setPosition (0, 0, other.getSizeZ() - 1);
		subTarget.setSize (this->getSizeX(), 1, 1);
		subTarget.setPosition (0, this->getSizeY(), -1);
		break;
	case EDGE6:
		subSource.setSize (1, 1, other.getSizeZ());
		subSource.setPosition (0, 0, 0);
		subTarget.setSize (1, 1, this->getSizeZ());
		subTarget.setPosition (this->getSizeX(), this->getSizeY(), 0);
		subSource.rotateAxes();
		subTarget.rotateAxes();
		break;	
	case EDGE7:
		subSource.setSize (other.getSizeX(), 1, 1);
		subSource.setPosition (0, 0, 0);
		subTarget.setSize (this->getSizeX(), 1, 1);
		subTarget.setPosition (0, this->getSizeY(), this->getSizeZ());
		break;	
	case EDGE8:
		subSource.setSize (1, 1, other.getSizeZ());
		subSource.setPosition (other.getSizeX() - 1, 0, 0);
		subTarget.setSize (1, 1, this->getSizeZ());
		subTarget.setPosition (-1, this->getSizeY(), 0);
		subSource.rotateAxes();
		subTarget.rotateAxes();
		break;	

	case EDGE9:
		subSource.setSize (1, other.getSizeY(), 1);
		subSource.setPosition (0, 0, other.getSizeZ() - 1);
		subTarget.setSize (1, this->getSizeY(), 1);
		subTarget.setPosition (this->getSizeX(), 0, -1);
		subSource.rotateAxes(); subSource.rotateAxes();
		subTarget.rotateAxes(); subTarget.rotateAxes();
		break;
	case EDGE10:
		subSource.setSize (1, other.getSizeY(), 1);
		subSource.setPosition (0, 0, 0);
		subTarget.setSize (1, this->getSizeY(), 1);
		subTarget.setPosition (this->getSizeX(), 0, this->getSizeZ());
		subSource.rotateAxes(); subSource.rotateAxes();
		subTarget.rotateAxes(); subTarget.rotateAxes();
		break;	
	case EDGE11:
		subSource.setSize (1, other.getSizeY(), 1);
		subSource.setPosition (other.getSizeX() - 1, 0, 0);
		subTarget.setSize (1, this->getSizeY(), 1);
		subTarget.setPosition (-1, 0, this->getSizeZ());
		subSource.rotateAxes(); subSource.rotateAxes();
		subTarget.rotateAxes(); subTarget.rotateAxes();
		break;	
	case EDGE12:
		subSource.setSize (1, other.getSizeY(), 1);
		subSource.setPosition (other.getSizeX() - 1, 0, other.getSizeZ() - 1);
		subTarget.setSize (1, this->getSizeY(), 1);
		subTarget.setPosition (-1, 0, -1);
		subSource.rotateAxes(); subSource.rotateAxes();
		subTarget.rotateAxes(); subTarget.rotateAxes();
		break;	
	case CORNER1:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(other.getSizeX()-1, other.getSizeY()-1, other.getSizeZ()-1);
		subTarget.setPosition(-1,-1,-1);
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER2:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(0, other.getSizeY()-1, other.getSizeZ()-1);
		subTarget.setPosition(this->getSizeX(),-1,-1);
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER3:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(0, other.getSizeY()-1, 0);
		subTarget.setPosition(this->getSizeX(), -1, this->getSizeZ());
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER4:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(other.getSizeX()-1, other.getSizeY()-1, 0);
		subTarget.setPosition(-1,-1,this->getSizeZ());
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER5:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(other.getSizeX()-1, 0, other.getSizeZ()-1);
		subTarget.setPosition(-1,this->getSizeY(),-1);
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER6:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(0, 0, other.getSizeZ()-1);
		subTarget.setPosition(this->getSizeX(), this->getSizeY(), -1);
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER7:
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(0, 0, 0);
		subTarget.setPosition(this->getSizeX(), this->getSizeY(), this->getSizeZ());
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
	case CORNER8:					
		subSource.setSize(1,1,1);
		subTarget.setSize(1,1,1);
		subSource.setPosition(other.getSizeX()-1, 0, 0);
		subTarget.setPosition(-1, this->getSizeY(), this->getSizeZ());
		*subTarget.getPtr(0,0,0) = *subSource.getPtr(0,0,0);
		return;
    }
	if (!interpolate)
	{
    	GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), subSource, subTarget, T, T);
	}
	else 
	{
	    if ( (subSource.getSizeZ() == 1) && (subSource.getSizeY() == 1) )
		{
			//This is no interpolation for source>target, because source doesn't get filtered
			goDouble dx = subSource.getSizeX() / (float)subTarget.getSizeX();
			goDouble px = 0;
			goDouble d;
			T A,B;
			goInt32 x;
			for (x = 0; x < (goInt32)(subTarget.getSizeX() - 1); x++)
			{
				A = *subSource.getPtr ((goInt32)px,0,0);
				B = *subSource.getPtr ((goInt32)px + 1, 0, 0);
				d = px - (goInt32)px;
				*subTarget.getPtr (x,0,0) = (T) (A + (B - A) * d);
				px += dx;
			}
			*subTarget.getPtr (x, 0, 0) = *subSource.getPtr (subSource.getSizeX() - 1, 0, 0);
		}
		goDouble dx = subSource.getSizeX() / (float)subTarget.getSizeX();
		goDouble dy = subSource.getSizeY() / (float)subTarget.getSizeY();
		go3Vector<goDouble> point;
		goSize_t x,y;
		point.z = 0;
		point.y = 0;
		for (y = 0; y < subTarget.getSizeY(); y++)
		{
			point.x = 0;
			for (x = 0; x < subTarget.getSizeX(); x++)
			{
				T a = 0,b = 0,c = 0,d = 0;
				goInt32  ix, iy;
				goDouble rx, ry;
				ix = (goInt32)point.x;
				iy = (goInt32)point.y;
				rx = point.x - ix;
				ry = point.y - iy;
				a = *subSource.getPtr (ix,iy,0);
				if (x < (subTarget.getSizeX() - 1))
				{
					b = *subSource.getPtr (ix + 1,iy,0);
				}
				if (y < (subTarget.getSizeY() - 1))
				{
					c = *subSource.getPtr (ix, iy + 1,0);
					if (x < (subTarget.getSizeX() - 1))
					{
						d = *subSource.getPtr (ix + 1, iy + 1,0);
					}
				}
				goDouble C;
				SIGNAL3D_bilinear (a,b,c,d,rx,ry,C);
				*subTarget.getPtr (x,y,0) = (T)C;
				point.x += dx;
			}
			point.y += dy;
		}
	}
}



template class goSignal3D< goInt8 >;
template class goSignal3D< goUInt8 >;
template class goSignal3D< goInt16 >;
template class goSignal3D< goUInt16 >;
template class goSignal3D< goInt32 >;
template class goSignal3D< goUInt32 >;
template class goSignal3D< goInt64 >;
template class goSignal3D< goFloat >;
template class goSignal3D< goDouble >;

