/*! \file gotypes.h 
 * type definitions for libGo  <br>
 * You should only use the type definitions for fixed bit length where really necessary.
 * Else, use the C types (that vary in length dependent on the architecture you are using).
 */

#ifndef GOTYPES_H
#define GOTYPES_H


#include <stdlib.h>	//for size_t
#include "config.h"
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_IEEE754_H
#include <ieee754.h>
#endif

#define GO_PROJECT_NAME		"libGo"
#define GO_PROJECT_VERSION	"0.3"

/* Standard types */
#if SIZEOF_CHAR == 1
/*!
 * \addtogroup types
 * @{
 */
///
typedef unsigned char goUInt8;
///
typedef char goSint8;
///
typedef char goInt8;
#endif
#if SIZEOF_SHORT_INT == 2
///
typedef unsigned short goUInt16;
///
typedef short goSint16;
///
typedef short goInt16;
#endif
#if SIZEOF_INT == 4
///
typedef unsigned int goUInt32;
///
typedef int goSint32;
///
typedef int goInt32;
#elif SIZEOF_SHORT_INT == 4
///
typedef unsigned short int goUInt32;
///
typedef short int goSint32;
///
typedef short int goInt32;
#endif

/* Hopyfully, one of the following will do :-) */
#if SIZEOF_LONG_INT == 8
///
///
typedef unsigned long goUInt64;
///
///
typedef long goSint64;
///
///
typedef long goInt64;
#elif SIZEOF_LONG_LONG_INT == 8
///
///
typedef unsigned long long goUInt64;
///
///
typedef long long goSint64;
///
///
typedef long long goInt64;
#endif
//

/* index types */
//
///
typedef goInt32 goIndex_t;
//

/* floats: */
//
///
///
typedef double goDouble;
#ifdef HAVE_IEEE754_H
///
typedef ieee754_double goIeee_double;
#else 
typedef goDouble goIeee_double;
#endif
///
typedef float	goFloat;
//

/** Misc types: */
typedef size_t	goSize_t;
#include <go3vector.h>
#define goSize3D go3Vector<goSize_t>

/** Pointer types: */
///
typedef int goPtrOffset_t;
///
typedef goPtrOffset_t goPtrdiff_t;

/** Types mainly for signal handling: */
///
typedef double goTime_t;

/*!
 * Clock tick type
 */
#ifdef HAVE_TIME_H
typedef clock_t goClock_t;
#else
typedef unsigned long long goClock_t;
#endif

///
enum GO_TYPE {
  GO_INT8,
  GO_INT16,
  GO_INT32,
  GO_UINT8,
  GO_UINT16,
  GO_UINT32,
  GO_FLOAT,
  GO_DOUBLE
};
/** @}*/
#endif /* __GOTYPES_H__ */




