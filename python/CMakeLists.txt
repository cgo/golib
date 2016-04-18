INCLUDE (FindSWIG)
#PROJECT             (golib_python)

SET (TARGET _golib)

# FIND_PROGRAM (SWIG_EXECUTABLE NAMES swig)

# SET (PYTHON_INSTALL_DIR $ENV{HOME}/.python CACHE PATH "Directory which holds python modules -- often pointed to by the environment variable PYTHONPATH")

#EXEC_PROGRAM        (${GUILE_CONFIG} ARGS link OUTPUT_VARIABLE GUILE_LIBS)
#EXEC_PROGRAM        (${GUILE_CONFIG} ARGS compile OUTPUT_VARIABLE GUILE_CFLAGS)
#EXEC_PROGRAM        (chicken-config ARGS -shared -libs OUTPUT_VARIABLE CHICKEN_LIBS)
add_custom_command (OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/golib_wrap.cxx
                    COMMAND ${SWIG_EXECUTABLE} 
                        ARGS -v -v -I../include -python -fcompact -addextern -c++ golib.i
                        DEPENDS golib.i
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
#EXEC_PROGRAM        (${SWIG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
#                        ARGS -v -I../include -python -fcompact -addextern -c++ golib.i )

#FIND_LIBRARY        (GOLIB NAMES Go PATHS $ENV{GOPATH}/../lib ${GOPATH}/lib)
#SET                 (GOMATLAB_LIB "-lGomatlab")
#LINK_LIBRARIES      (${GOLIB})
#INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../include /usr/include/python2.4)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../include ${PYTHON_INCLUDE_PATH})
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

SET (PYTHON_FILES ${CMAKE_CURRENT_SOURCE_DIR}/golib.py)
install (FILES ${PYTHON_FILES} DESTINATION ${PYTHON_INSTALL_DIR})
install (TARGETS ${TARGET} DESTINATION ${PYTHON_INSTALL_DIR})
