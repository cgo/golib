#ifndef GOFELICS_H
#define GOFELICS_H

#include <gotypes.h>

template< class T >
void 
goFELICSencode (go3DBlock<T> *block, 
		T maxval, int maxk,
		goArray<T> *array);

template< class T >
void 
goFELICSdecode (goArray<T> *array, goSize_t startIdx,
		T maxval, int maxk,
		go3DBlock<T> *block);

#endif
