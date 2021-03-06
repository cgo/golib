/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/* config.h.in -- libGo config */


/* Type sizes */
#define SIZEOF_CHAR 1
#define SIZEOF_SHORT_INT 2
#define SIZEOF_INT 4
#define SIZEOF_LONG_INT 4
#define SIZEOF_LONG_LONG_INT 8
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8

/* header files */
#define HAVE_IEEE754_H 1
#define HAVE_DIRENT_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1

/* functions */
#define HAVE_TMPNAM_R 1
#define HAVE_UNLINK 1
#define HAVE_MODF 1

/* libraries */
#define HAVE_LIBPTHREAD 1
/* #undef HAVE_LIBQT  */
/* #undef HAVE_LIBTIFF  */
/* #undef HAVE_LIBFFTW  */
/* #undef HAVE_LIBSDL  */
/* #undef HAVE_LIBJPEG  */
#define HAVE_LIBIL 1
#define HAVE_LIBILU 1
/* #undef HAVE_LIBILUT  */

#ifdef HAVE_LIBQT
#define USE_QT
#endif

/* #undef HAVE_NASM  */

/* Stuff not yet tested by configure */
#define GO_HAVE_MMX 1
#define GO_HAVE_3DNOW 1
// Version 0.2.1
// #define GO_DIPLOMARBEIT 1
#undef GO_DIPLOMARBEIT
// Undefine this to disable multiple threads in the volume navigation system.
// That's for debugging and should not be turned off unless you know exactly what you are doing.
// The system will not behave as you expect it to when you don't use threads.
#define MULTIPLE_THREADS 1
#define BLOCKSTORE_USE_HASHCACHE 1
/* #undef BLOCKSTORE_USE_HASHTABLE */
// #define BLOCKSTORE_USE_ARRAY 1
#define GO_VN_LINEAR_PREDICTION 1
/* Debugging */
#define _GODEBUG 1
// #define _GODEBUG_SBLOCKRENDERER_SEGFAULT 1
#undef _GODEBUG_SBLOCKRENDERER_SEGFAULT 
#undef _GODEBUG_VOLUMEFILE
