# INCLUDE (FindSWIG)
#PROJECT             (golib_python)

SET (TARGET _golib.so)

FIND_PROGRAM (SWIG_EXECUTABLE NAMES swig)

# SET (PYTHON_INSTALL_DIR $ENV{HOME}/.python CACHE PATH "Directory which holds python modules -- often pointed to by the environment variable PYTHONPATH")

#EXEC_PROGRAM        (${GUILE_CONFIG} ARGS link OUTPUT_VARIABLE GUILE_LIBS)
#EXEC_PROGRAM        (${GUILE_CONFIG} ARGS compile OUTPUT_VARIABLE GUILE_CFLAGS)
#EXEC_PROGRAM        (chicken-config ARGS -shared -libs OUTPUT_VARIABLE CHICKEN_LIBS)
EXEC_PROGRAM        (${SWIG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
                        ARGS -I../include -python -fcompact -addextern -c++ golib.i )
#FIND_LIBRARY        (GOLIB NAMES Go PATHS $ENV{GOPATH}/../lib ${GOPATH}/lib)
#SET                 (GOMATLAB_LIB "-lGomatlab")
#LINK_LIBRARIES      (${GOLIB})
#INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../include /usr/include/python2.4)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../include ${PYTHON_INCLUDE})
#ADD_DEFINITIONS     ()
ADD_LIBRARY         (_golib MODULE golib_wrap.cxx golibpython.cpp)
SET_TARGET_PROPERTIES (_golib PROPERTIES PREFIX "")
ADD_DEPENDENCIES (_golib Go)
TARGET_LINK_LIBRARIES (_golib Go)
#INSTALL_TARGETS     (/lib _golib)

IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)

SET (PYTHON_FILES ${CMAKE_CURRENT_SOURCE_DIR}/golib.py ${TARGET})

ADD_CUSTOM_TARGET   (install-py cp ${PYTHON_FILES} ${PYTHON_INSTALL_DIR})
