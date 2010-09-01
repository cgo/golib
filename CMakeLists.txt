CMAKE_MINIMUM_REQUIRED (VERSION 2.6)
INCLUDE             (CheckLibraryExists)
INCLUDE             (CheckTypeSize)
INCLUDE             (CheckIncludeFile)
INCLUDE             (CheckFunctionExists)
INCLUDE             (FindPkgConfig)
# Apparently buggy:
# INCLUDE             (FindThreads)

SET (HAVE_MATLAB_GEQ_2007A TRUE CACHE BOOL "Have Matlab >= 2007a? Also see BUILD_WITH_GCC_3_3")
SET (BUILD_WITH_GCC_3_3 FALSE CACHE BOOL "Enable/disable gcc-3.3 (for matlab < 2007a)")
SET (BUILD_WITH_GCC_3_4 FALSE CACHE BOOL "Enable/disable gcc-3.4 (for matlab >= 2006b)")
# SET (BUILD_GUILE_MODULE FALSE CACHE BOOL "Enable/disable building the guile module")
SET (BUILD_PYTHON_MODULE FALSE CACHE BOOL "Enable/disable building the golib python extension")
SET (BUILD_PYTHON_EMBED FALSE CACHE BOOL "Enable/disable python embedding module (Gopython)")
SET (PYTHON_INSTALL_DIR $ENV{HOME}/.python CACHE PATH "Directory which holds python modules -- often pointed to by the environment variable PYTHONPATH")
SET (BUILD_MATLAB_MODULE FALSE CACHE BOOL "Enable/disable building the matlab module (Gomatlab)")
SET (BUILD_MATLAB_MEX FALSE CACHE BOOL "Enable/disable building the matlab mex modules")
SET (BUILD_GUI_MODULE FALSE CACHE BOOL "Enable/disable building the GUI module (Gogui)")
SET (BUILD_GUI_MODULE_OPENGL FALSE CACHE BOOL "Enable/disable building the OpenGL widget (needs Gogl)")
SET (BUILD_GL_MODULE FALSE CACHE BOOL "Enable/disable building the OpenGL module (Gogl)")
SET (LINK_GFORTRAN TRUE CACHE BOOL "Link gfortran library (for atlas 3.6)")

IF (HAVE_MATLAB_GEQ_2007A)
    ADD_DEFINITIONS (-DHAVE_MATLAB_GEQ_2007A)
    SET (HAVE_MATLAB_GEQ_2007A 1)
ENDIF (HAVE_MATLAB_GEQ_2007A)

PROJECT             (Go)
SET                 (TARGETNAME Go)

## Aufteilung:
# thread: Thread-Klasse
# data/misc/base: Datenstrukturen, Objekte, basic file io, log, object messages, exceptions, types, ...
# math: Mathematik-Klassen und -Funktionen (keine goSignal!)
# signal: goSignal3D und alle Funktionen Signalverarbeitungsklassen;
#         Bilder laden / speichern
# system: Process, Directory, ...
# plot: Plotting
# gui
# gl
# matlab
SET                 (SOURCES 
 # src/math/mean.cpp
 # src/math/variance.cpp

 # src/math/contours.cpp
 src/misc/goofffile.cpp
 # src/misc/plot.cpp
 # src/misc/gognuplot.cpp      # not plotting in windows!
 # src/misc/gosingleplot.cpp
 # src/misc/gomultiplotter.cpp
 # src/misc/goconfigfile.cc    # not in windows! (deprecated anyway)
 # src/misc/goconfigfileaccess.cc 
 src/misc/godate.cc # DEPRECATE
 src/misc/gofileio.cc 
 # src/misc/gostatusobject.cc  # not in windows!
 src/misc/gotransferfunction.cc  # DEPRECATE -- apparently only used in the Diplomarbeit renderer
 # src/misc/goplot.cpp  # not in windows!
 # src/misc/govideocapture.cpp # not in windows!
 # src/network/goconnection.cc # not in windows!
 # src/network/gonetobject.cc  # not in windows!
 # src/network/gonetwork.cc # not in windows!
 # src/network/goserver.cc # not in windows!
 # src/signal/gosignal3dgenericiterator.cpp
 # src/system/godirectory.cc # not in windows!
 # src/system/goprocess.cc  # not in windows!
 src/system/goenvironment.cpp
 src/thread/goconsumer.cc # DEPRECATE
 src/thread/goproducer.cc # DEPRECATE
 # src/thread/gosynchronized.cc # not in windows!
 # src/math/gonubs.cpp
 ### For general SVD:
 ## BLAS part:
 #src/clapack/blas/lsame.c
 #src/clapack/blas/xerbla.c
 #src/clapack/blas/s_copy.c  ## in f2c
 #src/clapack/blas/s_cmp.c   ## in f2c
 )

## Base and stuff
SET (SOURCES ${SOURCES}
 src/misc/goobjectmessage.cc 
 src/misc/goglobal.cpp 
 src/misc/golog.cpp
 src/misc/goobjectbase.cc 
 src/misc/goexception.cc 
 src/nifty/gorandom.cc
 src/types/gotype.cpp)

## System, threads
SET (SOURCES ${SOURCES}
    src/thread/gothreadobject.cc
    src/thread/gothread.cc)

## Data containers
SET (SOURCES ${SOURCES}
 src/data/goarrayi.cc 
 # src/data/golisti.cc 
 # src/data/goheapi.cc 
 # src/data/gotreei.cc 
 src/data/gobtreei.cpp 
 src/data/goqueuei.cc
 src/data/gostring.cc
 src/data/gohashtablei.cpp)

## goMath
SET (SOURCES ${SOURCES}
 src/math/goreparam.cpp
 src/math/covariance.cpp
 src/math/gomanifold.cpp
 src/math/lapack.cpp
 src/math/planelinecut.cpp
 src/math/barycentric.cpp
 src/math/gocubicspline.cpp
 src/math/gocubicsplinend.cpp
 src/math/viewsphere.cpp
 src/math/pairwisedistances.cpp
 src/math/gosvd.cpp
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
 src/math/gomatrix.cpp 
 src/math/gocomplex.cc 
 src/math/goposition.cc 
 src/math/goquaternion.cc 
 # src/math/golinearalgebra.cc 
 # src/math/gopdf.cpp 
 src/math/gogausspdf.cpp 
 src/math/gopoint.cpp  ## Deprecate this, do not use
 src/math/gomath.cpp 
 src/math/goeigenvalue.cpp
 src/math/cg.cpp
 src/math/integrate.cpp
 src/math/getroots.cpp
 src/math/gopointcloud.cpp
 src/math/gocurve.cpp
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

## goSignal
SET (SOURCES ${SOURCES}
    src/signal/smooth.cpp
    src/signal/convert.cpp
    src/signal/canny.cpp
    src/signal/defaultsize.cpp
    src/signal/sobel2d.cpp
    ## goSignal namespace hinzufuegen:
    src/signal/minmax.cpp
    src/signal/crop.cpp
    src/signal/gosignaloperation.cpp
 src/signal/centerofmass.cpp
 src/signal/transform.cpp
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
 src/signal/binaryimage.cpp
 ## Bei math Namespace aendern:
 src/math/godenoise.cpp
 src/math/stencil.cpp
 src/math/gradient.cpp 
 src/math/divnormalizedgrad2d.cpp
 src/math/divergence.cpp
 src/math/gobiorthowavelet.cpp
 src/math/vectormult.cpp)

# Cairo plotting
SET (SOURCES ${SOURCES}
       src/plot/cairoplot.cpp
       src/plot/graph.cpp
       src/plot/object2dbox.cpp
       src/plot/object2dtext.cpp
       src/plot/object2dimage.cpp
       src/plot/object2dpoints.cpp
       src/plot/texttraits.cpp
       src/plot/plot.cpp
       src/plot/graphaxis.cpp)

IF (NOT WIN32)
    SET (SOURCES ${SOURCES}
  src/misc/plot.cpp
  src/misc/gognuplot.cpp      # not plotting in windows!
  src/misc/gosingleplot.cpp
  src/misc/gomultiplotter.cpp
  src/misc/goconfigfile.cc    # not in windows! (deprecated anyway)
  src/misc/goconfigfileaccess.cc 
  src/misc/goplot.cpp  # not in windows!
  src/misc/govideocapture.cpp # not in windows!
  src/system/goprocess.cc  # not in windows!
  # Can I remove any of these permanently?
  src/system/godirectory.cc # not in windows!
  src/network/goconnection.cc # not in windows!
  src/network/gonetobject.cc  # not in windows!
  src/network/gonetwork.cc # not in windows!
  src/network/goserver.cc # not in windows!
  src/thread/gosynchronized.cc # not in windows!
 # src/misc/gostatusobject.cc  # not in windows!
 )
ENDIF (NOT WIN32)


IF (BUILD_PYTHON_EMBED)
    ## SET (SOURCES src/python/gopython.cpp ${SOURCES})
    include (FindSWIG)
    # FIND_PROGRAM (SWIG_EXECUTABLE NAMES swig)
    ## Create include file for swig runtime
    EXEC_PROGRAM (${SWIG_EXECUTABLE} ARGS "-python -external-runtime ${PROJECT_SOURCE_DIR}/include/golib_swig_python.h")
ENDIF (BUILD_PYTHON_EMBED)

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
    MESSAGE ("clapack.h not found --- please install it through the ATLAS library.")
ENDIF (NOT HAVE_CLAPACK_H)

CHECK_FUNCTION_EXISTS (tmpnam_r HAVE_TMPNAM_R)
CHECK_FUNCTION_EXISTS (unlink   HAVE_UNLINK)
CHECK_FUNCTION_EXISTS (modf     HAVE_MODF)
CHECK_FUNCTION_EXISTS (getenv   HAVE_GETENV)
CHECK_FUNCTION_EXISTS (stat     HAVE_STAT)
CHECK_FUNCTION_EXISTS (mkdir    HAVE_MKDIR)

#FIND_LIBRARY (ATLAS NAMES atlas PATHS ${GOPATH}/extra/lib)
#FIND_LIBRARY (CBLAS NAMES cblas PATHS ${GOPATH}/extra/lib)
IF (WIN32)
        # FIND_LIBRARY (LAPACK_ATLAS NAMES lapack_atlas PATHS /usr/lib /usr/lib64 /usr/lib/atlas ${GOPATH}/extra/lib)
        FIND_LIBRARY (F2C NAMES f2c PATHS $ENV{HOME}/lib ${GOPATH}/extra/lib)
        FIND_LIBRARY (ATLAS NAMES atlas PATHS $ENV{HOME}/lib ${GOPATH}/extra/lib)
        FIND_LIBRARY (F77BLAS NAMES f77blas PATHS $ENV{HOME}/lib ${GOPATH}/extra/lib)
        FIND_LIBRARY (LAPACK NAMES lapack PATHS $ENV{HOME}/lib /usr/lib/atlas/sse2 /usr/lib/atlas ${GOPATH}/extra/lib)
        FIND_LIBRARY (BLAS NAMES cblas blas PATHS $ENV{HOME}/lib /usr/lib/atlas/sse2 /usr/lib/atlas ${GOPATH}/extra/lib)
        FIND_LIBRARY (DEVIL NAMES DevIL IL PATHS $ENV{HOME}/lib /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
        FIND_LIBRARY (DEVILU NAMES ILU PATHS $ENV{HOME}/lib /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
        FIND_LIBRARY (PTHREAD NAMES pthread PATHS $ENV{HOME}/lib /usr/lib /usr/local/lib)
ELSE (WIN32)
    FIND_LIBRARY (LAPACK_ATLAS NAMES lapack_atlas PATHS /usr/lib /usr/lib64 /usr/lib/atlas ${GOPATH}/extra/lib)
    FIND_LIBRARY (LAPACK NAMES lapack PATHS /usr/lib/atlas/sse2 /usr/lib/atlas ${GOPATH}/extra/lib)
    FIND_LIBRARY (BLAS NAMES blas PATHS /usr/lib/atlas/sse2 /usr/lib/atlas ${GOPATH}/extra/lib)
    FIND_LIBRARY (DEVIL NAMES IL PATHS /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
    FIND_LIBRARY (DEVILU NAMES ILU PATHS /usr/lib /usr/local/lib ${GOPATH}/extra/lib $ENV{HOME}/Software/lib) 
    FIND_LIBRARY (PTHREAD NAMES pthread PATHS /usr/lib /usr/local/lib)
ENDIF (WIN32)
#FIND_LIBRARY (F2C NAMES f2c PATHS /usr/lib /usr/local/lib)

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

IF (LINK_GFORTRAN)
    #    FIND_LIBRARY (GFORTRAN NAMES gfortran PATHS /usr/lib /usr/local/lib)
    #IF (GFORTRAN)
    #    SET (EXTRA_LIBS "${EXTRA_LIBS} ${GFORTRAN}")
    #ELSE (GFORTRAN)
    #    MESSAGE ("*** LINK_GFORTRAN selected, but gfortran library not found! ***")
    #ENDIF (GFORTRAN)
    SET (EXTRA_LIBS "${EXTRA_LIBS} -lgfortran")
ENDIF (LINK_GFORTRAN)

SET (EXTRA_INCLUDE_PATH "")

## Cairo and Pango (for use of the goPlot library)
pkg_search_module (CAIRO REQUIRED cairo)
pkg_search_module (PANGO REQUIRED pango)
pkg_search_module (PANGOCAIRO REQUIRED pangocairo)

IF (CAIRO_FOUND)
    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${CAIRO_INCLUDE_DIRS})
    LINK_LIBRARIES (${CAIRO_LDFLAGS})
    SET (HAVE_CAIRO 1)
ENDIF (CAIRO_FOUND)

IF (PANGO_FOUND)
    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${PANGO_INCLUDE_DIRS})
    LINK_LIBRARIES (${PANGO_LDFLAGS})
    SET (HAVE_PANGO 1)
ENDIF (PANGO_FOUND)

IF (PANGOCAIRO_FOUND)
    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${PANGOCAIRO_INCLUDE_DIRS})
    LINK_LIBRARIES (${PANGOCAIRO_LDFLAGS})
    SET (HAVE_PANGOCAIRO 1)
ENDIF (PANGOCAIRO_FOUND)

set (PCINCLUDES "-I${GOPATH}/include -I${GOPATH}/plot/include -I${GOPATH}/extra/include -I$ENV{HOME}/include")
INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${GOPATH}/include
                     ${GOPATH}/plot/include
                     ${GOPATH}/extra/include
                     $ENV{HOME}/include) # Only in windows!
                        

##                 
## If your system compiler is a gcc 4.x,
## set this explicitly to a 3.x version if using Matlab.
## Matlab links to the old libstdc++ version 5 which
## causes severe crashes when using it with libstdc++ version 6.
##                 
#IF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")
#    SET (CMAKE_CXX_COMPILER g++-3.3)
#    SET (CMAKE_C_COMPILER gcc-3.3)
#ENDIF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")

## For now, always set to 3.3
IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)
                     
# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)

IF (LAPACK_ATLAS)
    LINK_LIBRARIES (${LAPACK_ATLAS})
ENDIF (LAPACK_ATLAS)
IF (LAPACK)
    LINK_LIBRARIES (${LAPACK})
ENDIF (LAPACK)
IF (F77BLAS)
    LINK_LIBRARIES (${F77BLAS})
ENDIF (F77BLAS)
IF (BLAS)
    LINK_LIBRARIES (${BLAS})
ENDIF (BLAS)
IF (CBLAS)
    LINK_LIBRARIES (${CBLAS})
ENDIF (CBLAS)
IF (ATLAS)
    LINK_LIBRARIES (${ATLAS})
ENDIF (ATLAS)
IF (F2C)
    LINK_LIBRARIES (${F2C})
ENDIF (F2C)
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

# pkg-config file
set (PCLIBS "-lGo")
set (PCLIBDIR "-L${CMAKE_INSTALL_PREFIX}/lib")
CONFIGURE_FILE (${GOPATH}/golib.pc.cmakeconfig
                ${GOPATH}/golib.pc
                @ONLY)

ADD_LIBRARY         (${TARGETNAME} SHARED ${SOURCES})
STRING (STRIP ${EXTRA_LIBS} EXTRA_LIBS)  ## Remove trailing and leading whitespaces (leads to error in newer cmake)
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})

INSTALL_TARGETS     (/lib ${TARGETNAME})

#IF (BUILD_PYTHON_EMBED)
#    SET_TARGET_PROPERTIES (${TARGETNAME} PROPERTIES LINK_FLAGS ${PYTHON_LDFLAGS})
#ENDIF (BUILD_PYTHON_EMBED)

IF (BUILD_PYTHON_EMBED OR BUILD_PYTHON_MODULE)
    include (FindPythonLibs)
    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${PYTHON_INCLUDE_PATH})
    #EXEC_PROGRAM (python ARGS "-c \"import distutils.sysconfig; print distutils.sysconfig.get_config_var('INCLUDEPY')\"" OUTPUT_VARIABLE PYTHON_INCLUDE)
    #SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${PYTHON_INCLUDE})
    #EXEC_PROGRAM (python ARGS "-c \"import distutils.sysconfig; print distutils.sysconfig.get_config_var('BLDLIBRARY')\"" OUTPUT_VARIABLE PYTHON_LDFLAGS)
ENDIF (BUILD_PYTHON_EMBED OR BUILD_PYTHON_MODULE)

IF (BUILD_PYTHON_EMBED)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/pythonembed)
ENDIF (BUILD_PYTHON_EMBED)

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
    SET(HAVE_MATLAB 1)
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

## Python install target
#ADD_CUSTOM_TARGET   (install-py cp ${PYTHON_FILES} ${PYTHON_INSTALL_DIR})
