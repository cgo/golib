#include <go3dsubblock.h>
#include <godefs.h>
#include <goerror.h>

template< class T >
go3DSubBlock<T>::go3DSubBlock ()
  : go3DBlock<T>() {
  parent = 0;
  position.x = 0;
  position.y = 0;
  position.z = 0;
  parentXDiff = 0;
  parentYDiff = 0;
  parentZDiff = 0;
}

template< class T >
go3DSubBlock<T>::go3DSubBlock (go3DBlock<T> *b) 
  : go3DBlock<T> () {
  setParent (b);
  setPosition (0,0,0);
}

template< class T >
go3DSubBlock<T>::~go3DSubBlock () {
}

template< class T >
void
go3DSubBlock<T>::setParent (go3DBlock<T> *b) {
  parent = b;
  setPosition (0,0,0);
  setPtr (parent->getPtr());

  parentXDiff = parent->getXDiff();
  parentYDiff = parent->getYDiff();
  parentZDiff = parent->getZDiff();
  setDiff (parentXDiff, parentYDiff, parentZDiff);  
}

template< class T >
void
go3DSubBlock<T>::setPosition (goIndex_t x,
			      goIndex_t y,
			      goIndex_t z) {
  goPosition p;
  p.x = x;
  p.y = y;
  p.z = z;
  setPosition (p);
}

template< class T >
void
go3DSubBlock<T>::setPosition (goPosition &p) {
  position = p;
  if (!parent) {
    goError::print ("go3DSubBlock::setPosition()","No parent set.");
    return;
  }
  goPtrdiff_t offset = parentZDiff * p.z + parentYDiff * p.y + parentXDiff * p.x;
  setPtr (parent->getPtr() + offset);
}

template< class T >
void
go3DSubBlock<T>::move (int dir) {
  switch (dir) {
  case GO_DIRECTION_X: ptr += parentXDiff; position.x++; break;
  case GO_DIRECTION_Y: ptr += parentYDiff; position.y++; break;
  case GO_DIRECTION_Z: ptr += parentZDiff; position.z++; break;
  case GO_DIRECTION_X_NEGATIVE: ptr -= parentXDiff; position.x--; break;
  case GO_DIRECTION_Y_NEGATIVE: ptr -= parentYDiff; position.y--; break;
  case GO_DIRECTION_Z_NEGATIVE: ptr -= parentZDiff; position.z--; break;
  default: goError::print("go3DSubBlock::move()","Invalid direction.");
    break;
  }
}

template go3DSubBlock< goInt8 >;
template go3DSubBlock< goUInt8 >;
template go3DSubBlock< goInt16 >;
template go3DSubBlock< goInt32 >;
