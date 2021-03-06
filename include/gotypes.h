/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*! \file gotypes.h 
 * type definitions for libGo  <br>
 * You should only use the type definitions for fixed bit length where really necessary.
 * Else, use the C types (that vary in length dependent on the architecture you are using).
 */

#ifndef GOTYPES_H
#define GOTYPES_H


#include <stdlib.h>	//for size_t
#include <goconfig.h>
#ifdef HAVE_TIME_H
#include <time.h>
#endif

#define GO_PROJECT_NAME		"libGo"
#define GO_PROJECT_VERSION	"0.4.0"

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
///
typedef goInt8 goByte;
///
typedef goUInt8 goUByte;
#endif
#if SIZEOF_SHORT_INT == 2
///
typedef unsigned short goUInt16;
///
typedef short goInt16;
#endif
#if SIZEOF_INT == 4
///
typedef unsigned int goUInt32;
///
typedef int goInt32;
#elif SIZEOF_SHORT_INT == 4
///
typedef unsigned short int goUInt32;
///
typedef short int goInt32;
#endif

/* Hopyfully, one of the following will do :-) */
#ifdef HAVE_INT64
# if SIZEOF_LONG_INT == 8
///
///
typedef unsigned long goUInt64;
///
///
typedef long goInt64;
# elif SIZEOF_LONG_LONG_INT == 8
///
///
typedef unsigned long long goUInt64;
///
///
typedef long long goInt64;
# endif
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
typedef float	goFloat;
//

/** Misc types: */
typedef size_t	goSize_t;

#ifndef GO3VECTOR_H_INSIDE
#include <go3vector.h>
typedef go3Vector<goSize_t> goSize3D;
#endif

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

/*!
 * \brief Type enumerators.
 */
enum {
  GO_INT8,
  GO_INT16,
  GO_INT32,
  GO_INT64,
  GO_UINT8,
  GO_UINT16,
  GO_UINT32,
  GO_UINT64,
  GO_FLOAT,
  GO_DOUBLE,
  GO_VOID_POINTER,
  GO_COMPLEX_SINGLE,
  GO_COMPLEX_DOUBLE
};

typedef int goTypeEnum;

/** @}*/
#endif /* __GOTYPES_H__ */
