#ifndef __GOTYPES_H__
#define __GOTYPES_H__

// Put this into gotypes.h when this class is added to golib.
typedef unsigned int goPtrdiff_t;
typedef goPtrdiff_t goPtrOffset_t;
typedef char goInt8;
typedef unsigned char goUInt8;
typedef short goInt16;
typedef unsigned short goUInt16;
typedef int goInt32;
typedef unsigned int goUInt32;
typedef unsigned int goSize_t;
typedef int goIndex_t;
typedef float goFloat;
typedef double goDouble;

// #define go3DSize_t goArray<goSize_t>
#include <go3vector.h>
#define go3DSize_t go3Vector<goSize_t>

/* Type of the actual data. */
#ifndef T
// #define T signed long int
#endif

/* Types used for S-Transform calculations. */
#define GO_STRANSFORM_IN_T goInt16
#define GO_STRANSFORM_OUT_T goInt32

#endif



