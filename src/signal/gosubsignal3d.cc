#include <gosubsignal3d.h>
#include <gosignal3dbase.h>
#include <godefs.h>
#include <goerror.h>
#include <gotypes.h>

#include <config.h>
#include <assert.h>

template< class T >
goSubSignal3D<T>::goSubSignal3D ()
  : 
    goSignal3DBase<T>(),
    parent   (NULL),
    position (0, 0, 0),
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
    parent   (NULL),
    position (0, 0, 0),
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
        delete this->myXJump;
        this->myXJump = 0;
        delete this->xDiff; 
        this->xDiff = NULL;
    }
    if (deleteY)
    {
        delete this->myYJump;
        this->myYJump = 0;
        delete this->yDiff; 
        this->yDiff = NULL;
    }
    if (deleteZ)
    {
        delete this->myZJump;
        this->myZJump = 0;
        delete this->zDiff; 
        this->zDiff = NULL;
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
        goSize_t xArraySize = parent->getSizeX() / (1 + skipX);
        this->myXJump = new goPtrdiff_t [xArraySize];
        this->xDiff = new goPtrdiff_t [xArraySize];
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getXJump();
        goPtrdiff_t* parentDiffP = parent->getXDiff();
        for (k = 0; k < xArraySize; ++k)
        {
            goPtrdiff_t diff = 0;
            goIndex_t i;
            for (i = 0; i <= skipX; ++i)
            {
                diff += parentDiffP [i];
            }
            this->xDiff [k]   = diff;
            this->myXJump [k] = *parentJumpP;
            parentJumpP = parentJumpP + 1 + skipX;
            parentDiffP = parentDiffP + 1 + skipX;
        }
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
        goSize_t arraySize = parent->getSizeY() / (1 + skipY);
        this->myYJump = new goPtrdiff_t [arraySize];
        this->yDiff = new goPtrdiff_t [arraySize];
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getYJump();
        goPtrdiff_t* parentDiffP = parent->getYDiff();
        for (k = 0; k < arraySize; ++k)
        {
            goPtrdiff_t diff = 0;
            goIndex_t i;
            for (i = 0; i <= skipY; ++i)
            {
                diff += parentDiffP [i];
            }
            this->myYJump [k] = *parentJumpP;
            this->yDiff [k]   = diff;
            parentJumpP = parentJumpP + 1 + skipY;
            parentDiffP = parentDiffP + 1 + skipY;
        }
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
        goSize_t arraySize = parent->getSizeZ() / (1 + skipZ);
        this->myZJump = new goPtrdiff_t [arraySize];
        this->zDiff = new goPtrdiff_t [arraySize];
        goSize_t k;
        goPtrdiff_t* parentJumpP = parent->getZJump();
        goPtrdiff_t* parentDiffP = parent->getZDiff();
        for (k = 0; k < arraySize; ++k)
        {
            goPtrdiff_t diff = 0;
            goIndex_t i;
            for (i = 0; i <= skipZ; ++i)
            {
                diff += parentDiffP [i];
            }
            this->myZJump [k] = *parentJumpP;
            this->zDiff [k]   = diff;
            parentJumpP = parentJumpP + 1 + skipZ;
            parentDiffP = parentDiffP + 1 + skipZ;
        }
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
goSubSignal3D<T>::shiftLeftSize (int n)
{
    goSize_t sx = this->getSizeX();
    goSize_t sy = this->getSizeY();
    goSize_t sz = this->getSizeZ();

    sx = sx << n;
    sy = sy << n;
    sz = sz << n;
    this->setSize (sx, sy, sz, this->getChannelCount());
}

template <class T>
void
goSubSignal3D<T>::shiftRightSize (int n)
{
    goSize_t sx = this->getSizeX();
    goSize_t sy = this->getSizeY();
    goSize_t sz = this->getSizeZ();

    if (sx > 1) sx = sx >> n;
    if (sy > 1) sy = sy >> n;
    if (sz > 1) sz = sz >> n;
    this->setSize (sx, sy, sz, this->getChannelCount());
}

template <class T>
void
goSubSignal3D<T>::shiftLeftDiff (int n)
{
    // FIXME: Are these right now? 
    this->setSkip (((skipX + 1) << n) - 1, ((skipY + 1) << n) - 1, ((skipZ + 1) << n) - 1); 
    // this->setSkip (skipX + (1 << n) - 1, skipY + (1 << n) - 1, skipZ + (1 << n) - 1);
}

template <class T>
void
goSubSignal3D<T>::shiftRightDiff (int n)
{
    int newSkipX = 0;
    int newSkipY = 0;
    int newSkipZ = 0;
    if (skipX > 1) newSkipX = ((skipX + 1) >> n) - 1;
    if (skipY > 1) newSkipY = ((skipY + 1) >> n) - 1;
    if (skipZ > 1) newSkipZ = ((skipZ + 1) >> n) - 1;
    this->setSkip (newSkipX, newSkipY, newSkipZ); 
    // this->setSkip (skipX + (1 << n) - 1, skipY + (1 << n) - 1, skipZ + (1 << n) - 1);
    // this->setSkip ((skipX + 1) >> n, (skipY + 1) >> n, (skipZ + 1) >> n);
}

void
goSubSignal3D<void>::setParent (goSignal3DBase<void> *p)
{
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
template class goSubSignal3D<goInt64>;
template class goSubSignal3D<goFloat>;
template class goSubSignal3D<goDouble>;
template class goSubSignal3D<void*>;
template class goSubSignal3D<void>;
