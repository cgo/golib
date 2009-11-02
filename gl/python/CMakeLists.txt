# INCLUDE (FindSWIG)

SET (TARGET _gogl)

#FIND_PROGRAM (SWIG_EXECUTABLE NAMES swig)

add_custom_command (OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/gogl_wrap.cxx
                    COMMAND ${SWIG_EXECUTABLE} 
                        ARGS -I../include -python -fcompact -addextern -c++ gogl.i
                        DEPENDS gogl.i
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
#EXEC_PROGRAM        (${SWIG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
#                        ARGS -I../../include -I../include -python -c++ gogl.i)
#FIND_LIBRARY        (GOLIB NAMES Go PATHS $ENV{GOPATH}/../lib ${GOPATH}/lib)
#SET                 (GOGL_LIB "-lGogl")
#LINK_LIBRARIES      (${GOLIB} ${GOGL_LIB})
#INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../../include /usr/include/python2.4)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../../include ${CMAKE_CURRENT_SOURCE_DIR}/../include ${PYTHON_INCLUDE_PATH})
ADD_LIBRARY         (${TARGET} SHARED gogl_wrap.cxx)
ADD_DEPENDENCIES    (${TARGET} Go Gogl)
TARGET_LINK_LIBRARIES (${TARGET} Go Gogl)
SET_TARGET_PROPERTIES (${TARGET} PROPERTIES PREFIX "")
#INSTALL_TARGETS     (/lib _golib)

IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)

SET (PYTHON_FILES ${CMAKE_CURRENT_SOURCE_DIR}/gogl.py)
install (FILES ${PYTHON_FILES} DESTINATION ${PYTHON_INSTALL_DIR})
install (TARGETS ${TARGET} DESTINATION ${PYTHON_INSTALL_DIR})
# ADD_CUSTOM_TARGET   (install-py cp ${PYTHON_FILES} ${PYTHON_INSTALL_DIR})
