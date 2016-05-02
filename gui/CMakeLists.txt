INCLUDE             (FindPkgConfig)

SET                 (TARGETNAME Gogui)
SET                 (SOURCES 
  src/interactivedraw.cpp
  src/interactivedrawobject.cpp
  src/textoutput.cpp
  src/imagecontrol.cpp
  src/controldialog.cpp
  src/controlsbox.cpp
  src/cannycontrol.cpp
  src/cairoplot.cpp
  src/warning.cpp
  src/plotview.cpp
  src/multivectorinput.cpp
  src/vectorinput.cpp
  src/imageview.cpp
  src/draw.cpp
  src/control.cpp
  src/helper.cpp
  src/about.cpp
  src/mainwindow.cpp
  src/countfilenames.cpp)

## If libGo contains the video module:
if (MODULE_VIDEO)
  set (SOURCES ${SOURCES}
    src/videocapturecontrol.cpp)
endif (MODULE_VIDEO)


SET (GOGUI_USE_CAIRO TRUE CACHE BOOL "Use Cairo in the GUI")

IF (BUILD_GUI_MODULE_OPENGL)
    SET (SOURCES ${SOURCES} 
          src/scenecontrol.cpp
          src/sceneview.cpp)
ENDIF (BUILD_GUI_MODULE_OPENGL)


IF (BUILD_GUI_MODULE_OPENGL)
    ADD_DEFINITIONS (-DGOGUI_MAKE_OPENGL)
    SET (SOURCES ${SOURCES}
            src/glanimation.cpp
            src/offviewcontrol.cpp          
            src/offview.cpp
            src/glwidget.cpp
            src/gllightinput.cpp
            src/globjectinput.cpp
            src/gldrawableobjectinput.cpp
            src/glmaterialinput.cpp)
        #    IF (WIN32)
        #SET (GTKGL_INCLUDE_DIR ${GTK_DIR}/include)
        #SET (GTKGL_LIB_DIR ${GTK_DIR}/lib)
        #ELSE (WIN32)
    #PKGCONFIG (gtkglext-1.0 GTKGL_INCLUDE_DIR GTKGL_LIB_DIR GTKGL_LDFLAGS GTKGL_CFLAGS)
    #pkg_search_module (GTKGL REQUIRED gtkglext-1.0)
    #ENDIF (WIN32)

    if (APPLE)
      target_link_libraries(${OPENGL})
    endif(APPLE)
ENDIF (BUILD_GUI_MODULE_OPENGL)

IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)


# PKGCONFIG (gtkmm-2.4 GTKMM_INCLUDE_DIR GTKMM_LIB_DIR GTKMM_LDFLAGS GTKMM_CFLAGS)
pkg_search_module (GTKMM REQUIRED gtkmm-3.0)
# Funnily, gtkmm does not include these:
ADD_DEFINITIONS (-DHAVE_GTK_3_0 -DHAVE_GTK_2_4)

SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKMM_LDFLAGS} ${GDKMM_LDFLAGS})

#IF (GTKGL_INCLUDE_DIRS AND BUILD_GUI_MODULE_OPENGL)
#    SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKGL_LDFLAGS})
#    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${GTKGL_INCLUDE_DIRS})
#    ADD_DEFINITIONS (${GTKGL_CFLAGS})
#ENDIF (GTKGL_INCLUDE_DIRS AND BUILD_GUI_MODULE_OPENGL)

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})

set_property (TARGET ${TARGETNAME} PROPERTY CXX_STANDARD 11) 

IF (BUILD_GUI_MODULE_OPENGL)
    ADD_DEPENDENCIES (${TARGETNAME} Gogl)
    TARGET_LINK_LIBRARIES (${TARGETNAME} Gogl)
    INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../gl/include)
    # MESSAGE ("******************************** GOGL include dir: ${CMAKE_CURRENT_SOURCE_DIR}/../gl/include")
ENDIF (BUILD_GUI_MODULE_OPENGL)

ADD_DEPENDENCIES (${TARGETNAME} Go)
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT} Go)
INSTALL_TARGETS     (/lib ${TARGETNAME})

INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${CMAKE_CURRENT_SOURCE_DIR}/include
                     ${GOPATH}/include
                     ${GOPATH}/plot/include)

#IF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")
#    SET (CMAKE_CXX_COMPILER g++-3.3)
#    SET (CMAKE_C_COMPILER gcc-3.3)
#ENDIF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")

# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
ADD_DEFINITIONS (${GTKMM_CFLAGS} ${GDKMM_CFLAGS} -DGTKMM_GUI)

#
# pkg-config file

set (GUIPCLIBS "-lGogui")
set (GUIPCINCLUDES "-I${GOPATH}/gui/include -I${GOPATH}/gl/include -I${GOPATH}/tools/lapacke/include")
if (BUILD_GL_MODULE)
    set (GUIPCLIBS "${GUIPCLIBS} -lGogl")
endif (BUILD_GL_MODULE)

set (GOGUI_PKG_CONFIG_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" CACHE PATH "Where to install pkg-config files")

set (GUIPCLIBDIR "-L${CMAKE_INSTALL_PREFIX}/lib")

CONFIGURE_FILE (${GOPATH}/gui/gogui.pc.cmakeconfig
                ${GOPATH}/gui/gogui.pc
                @ONLY)
INSTALL (FILES ${GOPATH}/gui/gogui.pc DESTINATION ${GOGUI_PKG_CONFIG_INSTALL_DIR})
