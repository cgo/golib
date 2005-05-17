#include <gosubsignal3d.h>
#include <gosignal3dbase.h>
#include <godefs.h>
#include <goerror.h>
#include <gotypes.h>
#include <gomath.h>

#include <goconfig.h>
#include <assert.h>

template< class T >
goSubSignal3D<T>::goSubSignal3D ()
  : 
    goSignal3DBase<T>(),
    position (0, 0, 0),
    parent   (NULL),
    skipX    (0),
    skipY    (0),
    skipZ    (0),
    deleteX  (false),
    deleteY  (false),
    deleteZ  (false)
{
}

template< class T >
goSubSignal3D<T>::goSubSignal3D (goSignal3DBase<T> *b, goSize_t x, goSize_t y, goSize_t z) 
  : 
    goSignal3DBase<T>(),
    position (0, 0, 0),
    parent   (NULL),
    skipX    (0),
    skipY    (0),
    skipZ    (0),
    deleteX  (false),
    deleteY  (false),
    deleteZ  (false)
{
  assert (b);
  this->setParent   (b);
  this->setSize     (x, y, z, b->getChannelCount());
  this->setPosition (0,0,0);
}

template< class T >
goSubSignal3D<T>::~goSubSignal3D () 
{
    // avoid deletion of the data in case we didn't actually allocate anything!
    this->parent   = NULL;
    this->ptr      = NULL;
    this->real_ptr = NULL;
    if (!deleteX)
    {
        this->xDiff    = NULL;
        this->myXJump  = NULL;
    }
    if (!deleteY)
    {
        this->yDiff    = NULL;
        this->myYJump  = NULL;
    }
    if (!deleteZ)
    {
        this->zDiff    = NULL;
        this->myZJump  = NULL;
    }
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
    
    if (deleteX)
    {
        delete[] this->real_myXJump;
        this->real_myXJump = 0;
        this->myXJump = 0;
        delete[] this->real_xDiff;
        this->xDiff = NULL;
        this->real_xDiff = NULL;
    }
    if (deleteY)
    {
        delete[] this->real_myYJump;
        this->real_myYJump = 0;
        this->myYJump = 0;
        delete[] this->real_yDiff;
        this->yDiff = NULL;
        this->real_yDiff = NULL;
    }
    if (deleteZ)
    {
        delete[] this->real_myZJump;
        this->real_myZJump = 0;
        this->myZJump = 0;
        delete[] this->real_zDiff;
        this->zDiff = NULL;
        this->real_zDiff = NULL;
    }
    if (skipX == 0)
    {
        deleteX = false;
        this->myXJump = parent->getXJump() + (goPtrdiff_t)p.x;
        this->xDiff   = parent->getXDiff() + (goPtrdiff_t)p.x;
    }
    else
    {
        deleteX = true;
        goIndex_t xArraySize = this->getSizeX() + 2*this->getBorderX();
        // FIXME: Enable own borders (optionally)! Necessary for DWT.
        this->real_myXJump = new goPtrdiff_t [xArraySize];
        this->myXJump      = this->real_myXJump + this->getBorderX();
        this->real_xDiff   = new goPtrdiff_t [xArraySize];
        this->xDiff        = this->real_xDiff + this->getBorderX();
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getXJump() + (goPtrdiff_t)p.x;
        goPtrdiff_t* parentDiffP = parent->getXDiff() + (goPtrdiff_t)p.x;
        for (k = 0; k < this->getSizeX(); ++k)
        {
            goPtrdiff_t diff = 0;
            goSize_t i;
            for (i = 0; i <= skipX; ++i)
            {
                diff += parentDiffP [i];
            }
            this->xDiff [k]   = diff;
            this->myXJump [k] = *parentJumpP;
            parentJumpP = parentJumpP + 1 + skipX;
            parentDiffP = parentDiffP + 1 + skipX;
        }
        this->periodize (GO_X);
    }
    if (skipY == 0)
    {
        deleteY = false;
        this->myYJump = parent->getYJump() + (goPtrdiff_t)p.y;
        this->yDiff   = parent->getYDiff() + (goPtrdiff_t)p.y;
    }
    else
    {
        deleteY = true;
        goSize_t arraySize = this->getSizeY() + 2*this->getBorderY();
        this->real_myYJump = new goPtrdiff_t [arraySize];
        this->myYJump      = this->real_myYJump + this->getBorderY();
        this->real_yDiff   = new goPtrdiff_t [arraySize];
        this->yDiff        = this->real_yDiff + this->getBorderY();
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getYJump() + (goPtrdiff_t)p.y;
        goPtrdiff_t* parentDiffP = parent->getYDiff() + (goPtrdiff_t)p.y;
        for (k = 0; k < this->getSizeY(); ++k)
        {
            goPtrdiff_t diff = 0;
            goSize_t i;
            for (i = 0; i <= skipY; ++i)
            {
                diff += parentDiffP [i];
            }
            this->myYJump [k] = *parentJumpP;
            this->yDiff [k]   = diff;
            parentJumpP = parentJumpP + 1 + skipY;
            parentDiffP = parentDiffP + 1 + skipY;
        }
        this->periodize (GO_Y);
    }
    if (skipZ == 0)
    {
        deleteZ = false;
        this->myZJump = parent->getZJump() + (goPtrdiff_t)p.z;
        this->zDiff   = parent->getZDiff() + (goPtrdiff_t)p.z;
    }
    else
    {
        deleteZ = true;
        goSize_t arraySize = this->getSizeZ() + 2*this->getBorderZ();
        this->real_myZJump = new goPtrdiff_t [arraySize];
        this->myZJump      = this->real_myZJump + this->getBorderZ();
        this->real_zDiff   = new goPtrdiff_t [arraySize];
        this->zDiff        = this->real_zDiff + this->getBorderZ();
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getZJump() + (goPtrdiff_t)p.z;
        goPtrdiff_t* parentDiffP = parent->getZDiff() + (goPtrdiff_t)p.z;
        for (k = 0; k < this->getSizeZ(); ++k)
        {
            goPtrdiff_t diff = 0;
            goSize_t i;
            for (i = 0; i <= skipZ; ++i)
            {
                diff += parentDiffP [i];
            }
            this->myZJump [k] = *parentJumpP;
            this->zDiff [k]   = diff;
            parentJumpP = parentJumpP + 1 + skipZ;
            parentDiffP = parentDiffP + 1 + skipZ;
        }
        this->periodize (GO_Z);
    }
}

template<class T>
void
goSubSignal3D<T>::setSkip (goSize_t _skipX, goSize_t _skipY, goSize_t _skipZ)
{
    skipX = _skipX;
    skipY = _skipY;
    skipZ = _skipZ;
    this->setPosition (this->getPosition());
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

template <class T>
void
goSubSignal3D<T>::shiftLeftSize (int n, int axes)
{
    goSize_t sx = this->getSizeX();
    goSize_t sy = this->getSizeY();
    goSize_t sz = this->getSizeZ();

    if (axes & GO_X)
        sx = sx << n;
    if (axes & GO_Y)
        sy = sy << n;
    if (axes & GO_Z)
        sz = sz << n;
    this->setSize (sx, sy, sz, this->getChannelCount());
}

template <class T>
void
goSubSignal3D<T>::shiftRightSize (int n, int axes)
{
    goSize_t sx = this->getSizeX();
    goSize_t sy = this->getSizeY();
    goSize_t sz = this->getSizeZ();

    if (axes & GO_X)
        if (sx > 1) sx = sx >> n;
    if (axes & GO_Y)
        if (sy > 1) sy = sy >> n;
    if (axes & GO_Z)
        if (sz > 1) sz = sz >> n;
    this->setSize (sx, sy, sz, this->getChannelCount());
}

template <class T>
void
goSubSignal3D<T>::shiftLeftDiff (int n, int axes)
{
    goSize_t newSkipX = skipX;
    goSize_t newSkipY = skipY;
    goSize_t newSkipZ = skipZ;
     
    if (axes & GO_X)
        newSkipX = this->getSizeX() > 1 ? ((skipX + 1) << n) - 1 : skipX;
    if (axes & GO_Y)
        newSkipY = this->getSizeY() > 1 ? ((skipY + 1) << n) - 1 : skipY;
    if (axes & GO_Z)
        newSkipZ = this->getSizeZ() > 1 ? ((skipZ + 1) << n) - 1 : skipZ;
    this->setSkip (newSkipX, newSkipY, newSkipZ); 
    // this->setSkip (skipX + (1 << n) - 1, skipY + (1 << n) - 1, skipZ + (1 << n) - 1);
}

template <class T>
void
goSubSignal3D<T>::shiftRightDiff (int n, int axes)
{
    goSize_t newSkipX = skipX;
    goSize_t newSkipY = skipY;
    goSize_t newSkipZ = skipZ;

    if (axes & GO_X)
        if (skipX > 1) newSkipX = ((skipX + 1) >> n) - 1;
    if (axes & GO_Y)
        if (skipY > 1) newSkipY = ((skipY + 1) >> n) - 1;
    if (axes & GO_Z)
        if (skipZ > 1) newSkipZ = ((skipZ + 1) >> n) - 1;
    this->setSkip (newSkipX, newSkipY, newSkipZ); 
    // this->setSkip (skipX + (1 << n) - 1, skipY + (1 << n) - 1, skipZ + (1 << n) - 1);
    // this->setSkip ((skipX + 1) >> n, (skipY + 1) >> n, (skipZ + 1) >> n);
}

void
goSubSignal3D<void>::setParent (goSignal3DBase<void> *p)
{
  if (p == 0)
  {
      return;
  }
  this->setBorder (goMath::min(this->getSizeX(),(goSize_t)32),
                   goMath::min(this->getSizeY(),(goSize_t)32),
                   goMath::min(this->getSizeZ(),(goSize_t)32));
  this->setDataType (p->getDataType().getID());
  this->parent   = p;
  this->real_ptr = p->getRealPtr();
  this->ptr      = p->getPtr(0, 0, 0);
  this->setPosition (0, 0, 0);
  this->myChannelCount = p->getChannelCount();
  this->myChannel = p->getChannel();
  if (this->myChannelOffset)
  {
      delete [] this->myChannelOffset;
      this->myChannelOffset = NULL;
  }
  this->myChannelOffset = new goPtrdiff_t[this->getChannelCount()];
  goSize_t i;
  for (i = 0; i < this->getChannelCount(); ++i)
  {
    this->myChannelOffset[i] = p->getChannelOffset(i);
  }
}

template< class T >
void
goSubSignal3D<T>::setParent (goSignal3DBase<T> *p)
{
  this->setBorder (goMath::min(this->getSizeX(),(goSize_t)4),
                   goMath::min(this->getSizeY(),(goSize_t)4),
                   goMath::min(this->getSizeZ(),(goSize_t)4));
  this->parent   = p;
  this->real_ptr = p->getRealPtr();
  this->ptr      = p->getPtr(0, 0, 0);
  this->setPosition (0, 0, 0);
  this->myChannelCount = p->getChannelCount();
  this->myChannel = p->getChannel();
  if (this->myChannelOffset)
  {
      delete [] this->myChannelOffset;
      this->myChannelOffset = NULL;
  }
  this->myChannelOffset = new goPtrdiff_t[this->getChannelCount()];
  goSize_t i;
  for (i = 0; i < this->getChannelCount(); ++i)
  {
    this->myChannelOffset[i] = p->getChannelOffset(i);
  }
}

template <class T>
goSignal3DBase<T>* 
goSubSignal3D<T>::getParent ()
{
    return parent;
}

template <class T>
goPosition&
goSubSignal3D<T>::getPosition ()
{
    return position;
}

template class goSubSignal3D<goInt8>;
template class goSubSignal3D<goUInt8>;
template class goSubSignal3D<goInt16>;
template class goSubSignal3D<goUInt16>;
template class goSubSignal3D<goInt32>;
template class goSubSignal3D<goUInt32>;
#ifdef HAVE_INT64
template class goSubSignal3D<goInt64>;
#endif
template class goSubSignal3D<goFloat>;
template class goSubSignal3D<goDouble>;
template class goSubSignal3D<void*>;
template class goSubSignal3D<void>;
