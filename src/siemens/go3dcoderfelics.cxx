#include <go3dcoderfelics.h>
#include <felics.h>
#include <godefs.h>

template< class T >
go3DCoderFELICS<T>::go3DCoderFELICS ()
  : go3DCoder<T> () {
  this->ID = GO_ID_3DCODERFELICS;
}

template< class T >
go3DCoderFELICS<T>::~go3DCoderFELICS () {
}

template< class T >
void
go3DCoderFELICS<T>::init (void*) {
}

template< class T >
void
go3DCoderFELICS<T>::encode () {
  // codedStream->resize(0);
  goFELICSencode<T> (block, (1 << (relevantBits + 1)) - 1, 0, codedStream);
  //  goFELICSencode<T> (block, (1 << 17) - 1, 0, codedStream);
}

template< class T >
void
go3DCoderFELICS<T>::decode () {
  goFELICSdecode<T> (codedStream, streamIndex, (1 << (relevantBits + 1)) - 1, 0, block);
  //  goFELICSdecode<T> (codedStream, streamIndex, (1 << 17) - 1, 0, block);
}


template< class T >
void
go3DCoderFELICS<T>::
setRelevantBits (goSize_t b) {
  relevantBits = b;
}


template class go3DCoderFELICS< goInt16 >;
template class go3DCoderFELICS< goInt32 >;


