INCLUDE             (CheckLibraryExists)
INCLUDE             (CheckTypeSize)
INCLUDE             (CheckIncludeFile)
INCLUDE             (CheckFunctionExists)
# Apparently buggy:
# INCLUDE             (FindThreads)

PROJECT             (Go)
SET                 (TARGETNAME Go)
SET                 (SOURCES 
 src/data/goarrayi.cc 
 src/data/golisti.cc 
 src/data/goheapi.cc 
 src/data/gotreei.cc 
 src/data/goqueuei.cc 
 src/data/gostring.cc
 src/math/go3vectori.cc 
 src/math/go44matrix.cc 
 src/math/go4vectori.cc 
 src/math/gomatrixi.cc 
 src/math/gocomplex.cc 
 src/math/goposition.cc 
 src/math/goquaternion.cc 
 src/math/golinearalgebra.cc 
 src/math/gopdf.cpp 
 src/math/gogausspdf.cpp 
 src/math/gopoint.cpp 
 src/math/gradient.cpp 
 src/math/gomath.cpp 
 src/math/divnormalizedgrad2d.cpp
 src/misc/goconfigfile.cc 
 src/misc/goconfigfileaccess.cc 
 src/misc/godate.cc 
 src/misc/goerror.cc 
 src/misc/goexception.cc 
 src/misc/gofileio.cc 
 src/misc/goobjectbase.cc 
 src/misc/gostatusobject.cc 
 src/misc/gotransferfunction.cc 
 src/misc/goobjectmessage.cc 
 src/misc/goglobal.cpp 
 src/misc/golog.cpp
 src/network/goconnection.cc
 src/network/gonetobject.cc 
 src/network/gonetwork.cc 
 src/network/goserver.cc
 src/nifty/gorandom.cc
 src/signal/gosignal3dbase.cpp 
 src/signal/gosubsignal3d.cc 
 src/signal/gosignal3d.cc 
 src/signal/gofilter3d.cpp 
 src/signal/godwt.cc 
 src/signal/gohaar3d.cpp 
 src/signal/godwt3d.cpp 
 src/signal/gofilter1d.cpp 
 src/signal/gosignalhelper.cpp 
 src/signal/gohistogram.cpp 
 src/signal/gosignal3diterator.cpp 
 src/signal/gosignal3dgenericiterator.cpp
 src/system/godirectory.cc
 src/system/goprocess.cc
 src/system/goenvironment.cpp
 src/thread/gothread.cc 
 src/thread/goconsumer.cc 
 src/thread/goproducer.cc 
 src/thread/gosynchronized.cc 
 src/thread/gothreadobject.cc
 src/types/gotype.cpp)

SET                 (EXTRA_LIB_PATH /home/christian/Software/lib)
SET                 (EXTRA_INCLUDE_PATH /home/christian/Software/include)
SET                 (EXTRA_LIBS "")
SET                 (GOPATH $ENV{GOPATH})
# FILE                (GLOB SOURCES *.cpp *.cxx)

CHECK_TYPE_SIZE("char"          SIZEOF_CHAR)
CHECK_TYPE_SIZE("short int"     SIZEOF_SHORT_INT)
CHECK_TYPE_SIZE("int"           SIZEOF_INT)
CHECK_TYPE_SIZE("long int"      SIZEOF_LONG_INT)
CHECK_TYPE_SIZE("long long int" SIZEOF_LONG_LONG_INT)
CHECK_TYPE_SIZE("float"         SIZEOF_FLOAT)
CHECK_TYPE_SIZE("double"        SIZEOF_DOUBLE)

# We have 64 bit integers:
IF (${SIZEOF_LONG_LONG_INT} MATCHES 8)
    SET (HAVE_INT64 1)
ENDIF (${SIZEOF_LONG_LONG_INT} MATCHES 8)

CHECK_INCLUDE_FILE (ieee754.h   HAVE_IEEE754_H)
CHECK_INCLUDE_FILE (dirent.h    HAVE_DIRENT_H)
CHECK_INCLUDE_FILE (time.h      HAVE_TIME_H)
CHECK_INCLUDE_FILE (unistd.h    HAVE_UNISTD_H)
CHECK_INCLUDE_FILE (sys/stat.h  HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE (sys/types.h  HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE (errno.h  HAVE_ERRNO_H)

CHECK_FUNCTION_EXISTS (tmpnam_r HAVE_TMPNAM_R)
CHECK_FUNCTION_EXISTS (unlink   HAVE_UNLINK)
CHECK_FUNCTION_EXISTS (modf     HAVE_MODF)
CHECK_FUNCTION_EXISTS (getenv   HAVE_GETENV)
CHECK_FUNCTION_EXISTS (stat     HAVE_STAT)
CHECK_FUNCTION_EXISTS (mkdir    HAVE_MKDIR)


#EXEC_PROGRAM        (guile-config ARGS link OUTPUT_VARIABLE GUILE_LIBS)
#EXEC_PROGRAM        (guile-config ARGS compile OUTPUT_VARIABLE GUILE_CFLAGS)
#EXEC_PROGRAM        (swig ARGS 
#                        -I./ 
#                        -I$ENV{GOPATH}/include 
#                        -I$ENV{SWSPATH}/include 
#                        -guile -c++ -Linkage module -scm ${IFILE})
# FIND_LIBRARY        (GOLIB NAMES Go PATHS $ENV{GOPATH}/../lib ${GOPATH}/lib)
#CHECK_LIBRARY_EXISTS (IL ilInit 
#                      "$ENV{LD_LIBRARY_PATH};/usr/lib;/usr/local/lib;${EXTRA_LIB_PATH}"
#                      HAVE_LIBIL)

CHECK_LIBRARY_EXISTS (IL ilInit "" HAVE_LIBIL)
IF (${HAVE_LIBIL} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lIL")
ENDIF (${HAVE_LIBIL} MATCHES 1)
CHECK_LIBRARY_EXISTS (ILU iluInit "" HAVE_LIBILU)
IF (${HAVE_LIBILU} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lILU")
ENDIF (${HAVE_LIBILU} MATCHES 1)
CHECK_LIBRARY_EXISTS (fftw fftw_create_plan "" HAVE_LIBFFTW)
IF (${HAVE_LIBFFTW} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lfftw")
ENDIF (${HAVE_LIBFFTW} MATCHES 1)
CHECK_LIBRARY_EXISTS (nsl connect "" HAVE_LIBNSL)
IF (${HAVE_LIBNSL} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lnsl")
ENDIF (${HAVE_LIBNSL} MATCHES 1)
CHECK_LIBRARY_EXISTS (socket connect "" HAVE_LIBSOCKET)
IF (${HAVE_LIBSOCKET} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lsocket")
ENDIF (${HAVE_LIBSOCKET} MATCHES 1)

CHECK_LIBRARY_EXISTS (pthread pthread_create "" HAVE_LIBPTHREAD)
IF (${HAVE_LIBPTHREAD} MATCHES 1)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lpthread")
ENDIF (${HAVE_LIBPTHREAD} MATCHES 1)

#IF (DEFINED ${CMAKE_USE_PTHREADS_INIT})
#    MESSAGE (STATUS "Using pthreads!")
#    SET (HAVE_LIBPTHREAD 1)
#ENDIF (DEFINED ${CMAKE_USE_PTHREADS_INIT})

INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     $ENV{GOPATH}/include)
  
# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
CONFIGURE_FILE      (${GOPATH}/include/goconfig.h.cmake.in
                     ${GOPATH}/include/goconfig.h)
ADD_LIBRARY         (${TARGETNAME} SHARED ${SOURCES})
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
INSTALL_TARGETS     (/lib ${TARGETNAME})

