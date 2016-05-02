INCLUDE (FindOpenGL)
SET                 (TARGETNAME Gogl)
SET                 (SOURCES 
                             src/texture.cpp
                             src/textureimage.cpp
                             src/animation.cpp
                             src/waypoint.cpp
                             src/scene.cpp
                             src/offfile.cpp
                             src/helper.cpp
                             src/camera.cpp
                             src/light.cpp
                             src/material.cpp
                             src/object.cpp
                             src/drawableobject.cpp
                             src/meshobject.cpp)

# If the the environment variable MATLAB is set, add include path.
#IF($ENV{MATLAB})
#    SET (EXTRA_INCLUDE_PATH $ENV{MATLAB}/extern/include)
#    ADD_DEFINITIONS (-DHAVE_MATLAB)
#ENDIF($ENV{MATLAB})

#SET (GOPATH $ENV{GOPATH})

#FIND_LIBRARY (GOLIB NAMES Go PATHS ${GOPATH}/../lib /usr/local/lib)
#FIND_LIBRARY (GOMATLAB NAMES Gomatlab PATHS ${GOPATH}/../lib /usr/local/lib)
#SET (EXTRA_LIBS ${GOLIB} ${GOMATLAB})

SET (EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL_LIBRARIES})

INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${CMAKE_CURRENT_SOURCE_DIR}/include
                     ${GOPATH}/include
                     ${OPENGL_INCLUDE_DIR})
                     
IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)

if (APPLE)
  find_library (OPENGL OpenGL)
  link_libraries (${OPENGL})
endif (APPLE)

# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)

IF (BUILD_PYTHON_MODULE)
    ADD_SUBDIRECTORY (${PROJECT_SOURCE_DIR}/gl/python)
ENDIF (BUILD_PYTHON_MODULE)

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})
ADD_DEPENDENCIES (${TARGETNAME} Go)
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT} Go)
INSTALL_TARGETS     (/lib ${TARGETNAME})

#
# pkg-config file

set (GLPCLIBS "-lGogl")
set (GLPCINCLUDES "-I${GOPATH}/gl/include")

set (GOGL_PKG_CONFIG_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" CACHE PATH "Where to install pkg-config files")

set (GLPCLIBDIR "-L${CMAKE_INSTALL_PREFIX}/lib")

CONFIGURE_FILE (${GOPATH}/gl/gogl.pc.cmakeconfig
                ${GOPATH}/gl/gogl.pc
                @ONLY)
INSTALL (FILES ${GOPATH}/gl/gogl.pc DESTINATION ${GOGL_PKG_CONFIG_INSTALL_DIR})
