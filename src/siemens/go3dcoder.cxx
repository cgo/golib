#include <go3dcoder.h>
#include <godefs.h>

template< class T >
go3DCoder<T>::go3DCoder () {
  codedStream	= 0;
  block		= 0;
  this->ID	= GO_ID_3DCODER;
}

template< class T >
go3DCoder<T>::~go3DCoder () {
}

template< class T >
void
go3DCoder<T>::setCodedStream (goArray<T> *s, goSize_t idx) {
  streamIndex = idx;
  codedStream = s;
}

template< class T >
void
go3DCoder<T>::set3DBlock (go3DBlock<T> *b) {
  block = b;
}

template< class T >
goArray<T>*
go3DCoder<T>::getCodedStream () {
  return codedStream;
}

template< class T >
go3DBlock<T>*
go3DCoder<T>::get3DBlock () {
  return block;
}

template< class T >
void
go3DCoder<T>::init (void*) {
  
}

template< class T >
void
go3DCoder<T>::encode () {
  goIndex_t i,j,k;
  T *pz;
  T *py,*px;
  pz = block->getPtr();
  goSize_t x = block->getSizeX();
  goSize_t y = block->getSizeY();
  goSize_t z = block->getSizeZ();
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  goPtrdiff_t dz = block->getZDiff();
  for (k = (goIndex_t)z; k > 0; k--) {
    py = pz;
    for (j = (goIndex_t)y; j > 0; j--) {
      px = py;
      for (i = (goIndex_t)x; i > 0; i--) {
	(*codedStream) += *px;
	px += dx;
      }
      py += dy;
    }
    pz += dz;
  }
}

template< class T >
void
go3DCoder<T>::decode () {
  goIndex_t i,j,k;
  T *pz;
  T *py,*px;
  pz = block->getPtr();
  goSize_t x = block->getSizeX();
  goSize_t y = block->getSizeY();
  goSize_t z = block->getSizeZ();
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  goPtrdiff_t dz = block->getZDiff();
  goIndex_t arrayIdx = streamIndex;
  for (k = (goIndex_t)z; k > 0; k--) {
    py = pz;
    for (j = (goIndex_t)y; j > 0; j--) {
      px = py;
      for (i = (goIndex_t)x; i > 0; i--) {
	*px = (*codedStream)[arrayIdx++];
	px += dx;
      }
      py += dy;
    }
    pz += dz;
  }
}

template< class T >
int
go3DCoder<T>::getID () {
  return this->ID;
}

template class go3DCoder< goInt16 >;
template class go3DCoder< goInt32 >;











