#include <gosubsignal3d.h>
#include <gosignal3dbase.h>
#include <godefs.h>
#include <goerror.h>
#include <gotypes.h>

#include <config.h>
#include <assert.h>

template< class T >
goSubSignal3D<T>::goSubSignal3D ()
  : goSignal3DBase<T>() 
{
  parent        = NULL;
  position.x    = 0;
  position.y    = 0;
  position.z    = 0;
}

template< class T >
goSubSignal3D<T>::goSubSignal3D (goSignal3DBase<T> *b, goSize_t x, goSize_t y, goSize_t z) 
  : goSignal3DBase<T> () 
{
  setSize     (x, y, z);
  setParent   (b);
  setPosition (0,0,0);
}

template< class T >
goSubSignal3D<T>::~goSubSignal3D () 
{
    // avoid deletion of the data -- we didn't actually allocate anything!
    parent   = NULL;
    ptr      = NULL;
    real_ptr = NULL;
    xDiff    = NULL;
    yDiff    = NULL;
    zDiff    = NULL;
    myXJump  = NULL;
    myYJump  = NULL;
    myZJump  = NULL;
}

template< class T >
void
goSubSignal3D<T>::setPosition (goIndex_t x,
			       			   goIndex_t y,
			       			   goIndex_t z) 
{
	goPosition p;
    p.x = x;
    p.y = y;
    p.z = z;
    setPosition (p);
}

template< class T >
void
goSubSignal3D<T>::setPosition (goPosition &p) 
{
    if (!parent)
    {
        return;
    }
    
    assert (parent != NULL);

    position = p;
#ifdef GO_SUBSIGNAL3D_DEBUG
    if (!parent) {
        goError::print ("goSubSignal3D::setPosition()","No parent set.");
        return;
    }
#endif
    
    myXJump = parent->getXJump() + (goPtrdiff_t)p.x;
    myYJump = parent->getYJump() + (goPtrdiff_t)p.y;
    myZJump = parent->getZJump() + (goPtrdiff_t)p.z;
    xDiff   = parent->getXDiff() + (goPtrdiff_t)p.x;
    yDiff   = parent->getYDiff() + (goPtrdiff_t)p.y;
    zDiff   = parent->getZDiff() + (goPtrdiff_t)p.z;
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
goSubSignal3D<T>::setParent (goSignal3DBase<T> *p)
{
  parent   = p;
  real_ptr = p->getRealPtr();
  ptr      = p->getPtr(0, 0, 0);
  setPosition (0, 0, 0);
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

