#include <gosubsignal3d.h>
#include <gosignal3d.h>
#include <godefs.h>
#include <goerror.h>
#include <gotypes.h>

#include <config.h>

template< class T >
goSubSignal3D<T>::goSubSignal3D ()
  : goSignal3D<T>() {
  parent = 0;
  position.x = 0;
  position.y = 0;
  position.z = 0;
  parentXDiff = 0;
  parentYDiff = 0;
  parentZDiff = 0;
  borderX = 0;
  borderY = 0;
  borderZ = 0;
}

template< class T >
goSubSignal3D<T>::goSubSignal3D (goSignal3D<T> *b) 
  : goSignal3D<T> () {
  setParent (b);
  setPosition (0,0,0);
}

template< class T >
goSubSignal3D<T>::~goSubSignal3D () {
}


template< class T >
void
goSubSignal3D<T>::setPosition (int x,
			       			   int y,
			       			   int z) 
{
	goPosition p;
    p.x = x;
    p.y = y;
    p.z = z;
    setPosition (p);
}

template< class T >
void
goSubSignal3D<T>::setPositionSub (int x,
				  int y,
				  int z) {
  goPtrdiff_t offset = zDiff * z + yDiff * y + xDiff * x;
  // cout << hex << "Setting to offset " << offset << ", coordinates " << x << "," << y << "," << z << endl;
  this->setPtr (parent->getPtr() + offset);
  // cout << "parent->getPtr(): " << hex << parent->getPtr() << endl;
  // cout << "this->getPtr(): " << this->getPtr() << dec << endl;
}

template< class T >
void
goSubSignal3D<T>::setPosition (goPosition &p) {
  position = p;
#ifdef GO_SUBSIGNAL3D_DEBUG
  if (!parent) {
    goError::print ("goSubSignal3D::setPosition()","No parent set.");
    return;
  }
#endif
  goPtrdiff_t offset = parentZDiff * p.z + parentYDiff * p.y + parentXDiff * p.x;
  setPtr (parent->getPtr() + offset);
}

template<class T>
void
goSubSignal3D<T>::move(int dir)
{
	switch(dir)
	{
		case GO_DIRECTION_X: position.x += 1; break;
		case GO_DIRECTION_Y: position.y += 1; break;
		case GO_DIRECTION_Z: position.z += 1; break;
		case GO_DIRECTION_X_NEGATIVE: position.x -= 1; break;
		case GO_DIRECTION_Y_NEGATIVE: position.y -= 1; break;
		case GO_DIRECTION_Z_NEGATIVE: position.z -= 1; break;
	    default: break;	
	}
	setPosition(position.x, position.y, position.z);
}

template< class T >
void
goSubSignal3D<T>::setParent (goSignal3D<T> *p)
{
  parent = p;
  parentXDiff = p->getXDiff();
  parentYDiff = p->getYDiff();
  parentZDiff = p->getZDiff();
  real_ptr = p->getRealPtr();
  ptr = p->getPtr();
}

template class goSubSignal3D<goInt8>;
template class goSubSignal3D<goUInt8>;
template class goSubSignal3D<goInt16>;
template class goSubSignal3D<goUInt16>;
template class goSubSignal3D<goInt32>;
template class goSubSignal3D<goUInt32>;
template class goSubSignal3D<goInt64>;
template class goSubSignal3D<goFloat>;
template class goSubSignal3D<goDouble>;
template class goSubSignal3D<void*>;





