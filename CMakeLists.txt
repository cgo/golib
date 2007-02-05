INCLUDE             (CheckLibraryExists)
INCLUDE             (CheckTypeSize)
INCLUDE             (CheckIncludeFile)
INCLUDE             (CheckFunctionExists)
# Apparently buggy:
# INCLUDE             (FindThreads)

SET (BUILD_GUILE_MODULE FALSE CACHE BOOL "Enable/disable building the guile module")
SET (BUILD_PYTHON_MODULE FALSE CACHE BOOL "Enable/disable building the python module")
SET (PYTHON_INSTALL_DIR $ENV{HOME}/.python CACHE PATH "Directory which holds python modules -- often pointed to by the environment variable PYTHONPATH")
SET (BUILD_MATLAB_MODULE FALSE CACHE BOOL "Enable/disable building the matlab module")
SET (BUILD_MATLAB_MEX FALSE CACHE BOOL "Enable/disable building the matlab mex modules")
SET (BUILD_GUI_MODULE FALSE CACHE BOOL "Enable/disable building the GUI module")
SET (BUILD_GUI_MODULE_OPENGL FALSE CACHE BOOL "Enable/disable building the OpenGL widget (may have problems with matlab)")
SET (BUILD_GL_MODULE FALSE CACHE BOOL "Enable/disable building the OpenGL module")

PROJECT             (Go)
SET                 (TARGETNAME Go)
SET                 (SOURCES 
 src/data/goarrayi.cc 
 # src/data/golisti.cc 
 # src/data/goheapi.cc 
 # src/data/gotreei.cc 
 src/data/gobtreei.cpp 
 src/data/goqueuei.cc
 src/data/gostring.cc
 src/data/gohashtablei.cpp
 src/math/goaffinetransform.cpp
 src/math/affinematch.cpp
 src/math/getknee.cpp
 src/math/goresample.cpp
 src/math/govector.cpp
 src/math/gokmeans.cpp 
 src/math/gokmeansspatial.cpp 
 src/math/go3vectori.cc 
 src/math/go44matrix.cc 
 src/math/go4vectori.cc 
 src/math/gomatrixi.cc 
 src/math/gocomplex.cc 
 src/math/goposition.cc 
 src/math/goquaternion.cc 
 # src/math/golinearalgebra.cc 
 # src/math/gopdf.cpp 
 src/math/gogausspdf.cpp 
 src/math/gopoint.cpp 
 src/math/gradient.cpp 
 src/math/gomath.cpp 
 src/math/divnormalizedgrad2d.cpp
 src/math/goeigenvalue.cpp
 src/math/cg.cpp
 src/math/gobiorthowavelet.cpp
 src/math/divergence.cpp
 src/math/vectormult.cpp
 # src/math/mean.cpp
 # src/math/variance.cpp
 src/math/integrate.cpp
 src/math/getroots.cpp
 src/misc/gosingleplot.cpp
 src/misc/gomultiplotter.cpp
 src/misc/goconfigfile.cc 
 src/misc/goconfigfileaccess.cc 
 src/misc/godate.cc 
 src/misc/goexception.cc 
 src/misc/gofileio.cc 
 src/misc/goobjectbase.cc 
 src/misc/gostatusobject.cc 
 src/misc/gotransferfunction.cc 
 src/misc/goobjectmessage.cc 
 src/misc/goglobal.cpp 
 src/misc/golog.cpp
 src/misc/goplot.cpp
 src/misc/govideocapture.cpp
 src/network/goconnection.cc
 src/network/gonetobject.cc 
 src/network/gonetwork.cc 
 src/network/goserver.cc
 src/nifty/gorandom.cc
 src/signal/gosignal3dref.cpp
 src/signal/gogaussimage.cpp
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
 # src/signal/gosignal3dgenericiterator.cpp
 src/system/godirectory.cc
 src/system/goprocess.cc
 src/system/goenvironment.cpp
 src/thread/gothread.cc 
 src/thread/goconsumer.cc 
 src/thread/goproducer.cc 
 src/thread/gosynchronized.cc 
 src/thread/gothreadobject.cc
 src/types/gotype.cpp
 src/math/gopointcloud.cpp
 src/math/gocurve.cpp
 # src/math/gonubs.cpp
 src/seispack/bakvec.c
 src/seispack/balanc.c
 src/seispack/balbak.c
 src/seispack/bandr.c
 src/seispack/bandv.c
 src/seispack/bisect.c
 src/seispack/bqr.c
 src/seispack/cbabk2.c
 src/seispack/cbal.c
 src/seispack/cdiv.c
 src/seispack/cg.c
 src/seispack/ch.c
 src/seispack/cinvit.c
 src/seispack/combak.c
 src/seispack/comhes.c
 src/seispack/comlr2.c
 src/seispack/comlr.c
 src/seispack/comqr2.c
 src/seispack/comqr.c
 src/seispack/cortb.c
 src/seispack/corth.c
 src/seispack/csroot.c
 src/seispack/elmbak.c
 src/seispack/elmhes.c
 src/seispack/eltran.c
 src/seispack/epslon.c
 src/seispack/figi2.c
 src/seispack/figi.c
 src/seispack/hqr2.c
 src/seispack/hqr.c
 src/seispack/htrib3.c
 src/seispack/htribk.c
 src/seispack/htrid3.c
 src/seispack/htridi.c
 src/seispack/imtql1.c
 src/seispack/imtql2.c
 src/seispack/imtqlv.c
 src/seispack/invit.c
 src/seispack/minfit.c
 src/seispack/ortbak.c
 src/seispack/orthes.c
 src/seispack/ortran.c
 src/seispack/pythag.c
 src/seispack/qzhes.c
 src/seispack/qzit.c
 src/seispack/qzval.c
 src/seispack/qzvec.c
 src/seispack/ratqr.c
 src/seispack/rebakb.c
 src/seispack/rebak.c
 src/seispack/reduc2.c
 src/seispack/reduc.c
 src/seispack/rg.c
 src/seispack/rgg.c
 src/seispack/rsb.c
 src/seispack/rs.c
 src/seispack/rsgab.c
 src/seispack/rsgba.c
 src/seispack/rsg.c
 src/seispack/rsm.c
 src/seispack/rsp.c
 src/seispack/rst.c
 src/seispack/rt.c
 src/seispack/svd.c
 src/seispack/tinvit.c
 src/seispack/tql1.c
 src/seispack/tql2.c
 src/seispack/tqlrat.c
 src/seispack/trbak1.c
 src/seispack/trbak3.c
 src/seispack/tred1.c
 src/seispack/tred2.c
 src/seispack/tred3.c
 src/seispack/tridib.c
 src/seispack/tsturm.c
 src/seispack/r_sign.c)

SET (GOPATH ${PROJECT_SOURCE_DIR})

#
# Find out the machine type. Currently, assume only 
# x86 and amd64 machines are supported.
#
EXEC_PROGRAM        (uname ARGS -m OUTPUT_VARIABLE UNAME_M)
IF (UNAME_M MATCHES x86_64)
    SET (MACHINE_TYPE "amd64")
ELSE (UNAME_M MATCHES x86_64)
    SET (MACHINE_TYPE "x86")
ENDIF (UNAME_M MATCHES x86_64)
MESSAGE ("Machine type is ${MACHINE_TYPE}")

# This was introduced for guile 1.6.x. 1.8 should be threadsafe. Not tried
# though.
SET                 (GO_GUILE_MULTITHREAD 0)

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

# SET (CMAKE_REQUIRED_INCLUDES /usr/include /usr/local/include)

CHECK_INCLUDE_FILE (ieee754.h   HAVE_IEEE754_H)
CHECK_INCLUDE_FILE (dirent.h    HAVE_DIRENT_H)
CHECK_INCLUDE_FILE (time.h      HAVE_TIME_H)
CHECK_INCLUDE_FILE (unistd.h    HAVE_UNISTD_H)
CHECK_INCLUDE_FILE (sys/stat.h  HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE (sys/types.h  HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE (errno.h  HAVE_ERRNO_H)
CHECK_INCLUDE_FILE (linux/videodev2.h  HAVE_LINUX_VIDEODEV2_H)
CHECK_INCLUDE_FILE (linux/videodev.h  HAVE_LINUX_VIDEODEV_H)
CHECK_INCLUDE_FILE (f2c.h  HAVE_F2C_H)
CHECK_INCLUDE_FILE (clapack.h  HAVE_CLAPACK_H)
SET (HAVE_LINUX_VIDEODEV_H 1)

IF (NOT HAVE_F2C_H)
    MESSAGE ("f2c.h not found --- please install f2c.")
ENDIF (NOT HAVE_F2C_H)
IF (NOT HAVE_CLAPACK_H)
    MESSAGE ("clapack.h not found --- please install it, e.g. through the ATLAS library.")
ENDIF (NOT HAVE_CLAPACK_H)

CHECK_FUNCTION_EXISTS (tmpnam_r HAVE_TMPNAM_R)
CHECK_FUNCTION_EXISTS (unlink   HAVE_UNLINK)
CHECK_FUNCTION_EXISTS (modf     HAVE_MODF)
CHECK_FUNCTION_EXISTS (getenv   HAVE_GETENV)
CHECK_FUNCTION_EXISTS (stat     HAVE_STAT)
CHECK_FUNCTION_EXISTS (mkdir    HAVE_MKDIR)

FIND_LIBRARY (ATLAS NAMES atlas PATHS /usr/lib /usr/local/lib /usr/lib64 ${GOPATH}/extra/lib)
FIND_LIBRARY (CBLAS NAMES cblas PATHS /usr/lib /usr/local/lib /usr/lib64 ${GOPATH}/extra/lib)
FIND_LIBRARY (LAPACK NAMES lapack PATHS /usr/lib /usr/local/lib /usr/lib64 ${GOPATH}/extra/lib)
FIND_LIBRARY (DEVIL NAMES IL PATHS /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
FIND_LIBRARY (DEVILU NAMES ILU PATHS /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
FIND_LIBRARY (PTHREAD NAMES pthread PATHS /usr/lib /usr/local/lib)

#CHECK_LIBRARY_EXISTS (atlas ATL_cputime "${GOPATH}/extra" HAVE_LIBATLAS)
#IF (${HAVE_LIBATLAS} MATCHES 1)
#    SET (EXTRA_LIBS "${EXTRA_LIBS} -latlas")
#ENDIF (${HAVE_LIBATLAS} MATCHES 1)
#CHECK_LIBRARY_EXISTS (cblas cblas_dgemm "${GOPATH}/extra" HAVE_LIBCBLAS)
#IF (${HAVE_LIBATLAS} MATCHES 1)
#    SET (EXTRA_LIBS "${EXTRA_LIBS} -lcblas")
#ENDIF (${HAVE_LIBATLAS} MATCHES 1)


#CHECK_LIBRARY_EXISTS (IL ilInit "" HAVE_LIBIL)
#IF (${HAVE_LIBIL} MATCHES 1)
#    SET (EXTRA_LIBS "${EXTRA_LIBS} -lIL")
#ENDIF (${HAVE_LIBIL} MATCHES 1)
#CHECK_LIBRARY_EXISTS (ILU iluInit "" HAVE_LIBILU)
#IF (${HAVE_LIBILU} MATCHES 1)
#    SET (EXTRA_LIBS "${EXTRA_LIBS} -lILU")
#ENDIF (${HAVE_LIBILU} MATCHES 1)
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

INCLUDE_DIRECTORIES (# ${EXTRA_INCLUDE_PATH}
                     ${GOPATH}/include
                     ${GOPATH}/extra/include)

##                 
## If your system compiler is a gcc 4.x,
## set this explicitly to a 3.x version if using Matlab.
## Matlab links to the old libstdc++ version 5 which
## causes severe crashes when using it with libstdc++ version 6.
##                 
IF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")
                     
# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
IF (LAPACK)
    LINK_LIBRARIES (${LAPACK})
ENDIF (LAPACK)
IF (CBLAS)
    LINK_LIBRARIES (${CBLAS})
ENDIF (CBLAS)
IF (ATLAS)
    LINK_LIBRARIES (${ATLAS})
ENDIF (ATLAS)
IF (DEVIL)
    SET (HAVE_LIBIL 1)
    LINK_LIBRARIES (${DEVIL})
ENDIF (DEVIL)
IF (DEVILU)
    SET (HAVE_LIBILU 1)
    LINK_LIBRARIES (${DEVILU})
ENDIF (DEVILU)
IF (PTHREAD)
    SET (HAVE_LIBPTHREAD 1)
    LINK_LIBRARIES (${PTHREAD})
ENDIF (PTHREAD)

CONFIGURE_FILE      (${GOPATH}/include/goconfig.h.cmake.in
                     ${GOPATH}/include/goconfig.h)

ADD_LIBRARY         (${TARGETNAME} SHARED ${SOURCES})
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
# SET_TARGET_PROPERTIES (${TARGETNAME} PROPERTIES LINK_FLAGS "-L${MATPACK_PATH} -L/usr/X11R6/lib -lXpm")
INSTALL_TARGETS     (/lib ${TARGETNAME})
# If the the environment variable MATLAB is set, assume the matlab module
# should be built. This creates the build directory, but it is not
# automatically descended into. The user has to do that.
IF (BUILD_MATLAB_MODULE)
    IF(DEFINED ENV{MATLAB})
        MESSAGE ("MATLAB path is $ENV{MATLAB}")
        SET (EXTRA_INCLUDE_PATH $ENV{HOME}/Software/include)
        ADD_SUBDIRECTORY(${PROJECT_SOURCE_DIR}/matlab)
    ELSE(DEFINED ENV{MATLAB})
        MESSAGE ("*** MATLAB environment variable was not declared. ***")
    ENDIF(DEFINED ENV{MATLAB})
ENDIF (BUILD_MATLAB_MODULE)

IF (BUILD_GUILE_MODULE)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/swig)
ENDIF (BUILD_GUILE_MODULE)

IF (BUILD_PYTHON_MODULE)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/python)
    IF (NOT EXISTS ${PYTHON_INSTALL_DIR})
        MAKE_DIRECTORY (${PYTHON_INSTALL_DIR})
    ENDIF (NOT EXISTS ${PYTHON_INSTALL_DIR})
ENDIF (BUILD_PYTHON_MODULE)

IF (BUILD_GUI_MODULE)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/gui)
ENDIF (BUILD_GUI_MODULE)

IF (BUILD_GL_MODULE)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/gl)
ENDIF (BUILD_GL_MODULE)

