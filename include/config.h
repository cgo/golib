/* include/config.h.  Generated automatically by configure.  */
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

/* libraries */
#define HAVE_LIBPTHREAD 1
/* #undef HAVE_LIBQT */
#define HAVE_LIBTIFF 1
/* #undef HAVE_LIBFFTW */
#define HAVE_LIBSDL 1
/* #undef HAVE_LIBJPEG */

#ifdef HAVE_LIBQT
#define USE_QT
#endif

/* #undef HAVE_NASM */

/* Stuff not yet tested by configure */
#define GO_HAVE_MMX 1
#define GO_HAVE_3DNOW 1
#define MULTIPLE_THREADS 1
#define BLOCKSTORE_USE_HASHCACHE 1
#define GO_DIPLOMARBEIT 1
/* #undef BLOCKSTORE_USE_HASHTABLE */
// #define BLOCKSTORE_USE_ARRAY 1
#define GO_VN_LINEAR_PREDICTION 1

/* Benchmarking */
#define GO_BENCHMARK 1

/* Debugging */
#define _GODEBUG 0 
// #define _GODEBUG_SBLOCKRENDERER_SEGFAULT 1
/* #undef _GODEBUG_SBLOCKRENDERER_SEGFAULT */ 
/* #undef _GODEBUG_VOLUMEFILE */
