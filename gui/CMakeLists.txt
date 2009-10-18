SET                 (TARGETNAME Gogui)
SET                 (SOURCES 
    src/interactivedraw.cpp
    src/textoutput.cpp
    src/imagecontrol.cpp
    src/controldialog.cpp
    src/controlsbox.cpp
    src/cannycontrol.cpp
                             src/cairoplot.cpp
                             src/warning.cpp
                             src/plotview.cpp
                             src/videocapturecontrol.cpp
                             src/multivectorinput.cpp
                             src/vectorinput.cpp
                             src/imageview.cpp
                             src/draw.cpp
                             src/control.cpp
                             src/helper.cpp
                             src/about.cpp
                             src/mainwindow.cpp
                             src/countfilenames.cpp)

SET (GOGUI_USE_CAIRO TRUE CACHE BOOL "Use Cairo in the GUI")

IF (BUILD_GUI_MODULE_OPENGL)
    SET (SOURCES ${SOURCES} 
          src/scenecontrol.cpp
          src/sceneview.cpp)
ENDIF (BUILD_GUI_MODULE_OPENGL)

                         #IF (WIN32)
                         #    SET (GTK_DIR CACHE PATH "c:/GTK")
                         #ENDIF (WIN32)
IF (WIN32)
    ## Copied from UsePkgConfig and modified
    #FIND_PROGRAM(PKGCONFIG_EXECUTABLE NAMES pkg-config PATHS /c/GTK/bin)
    SET (PKGCONFIG_EXECUTABLE c:/GTK/bin/pkg-config)
    MACRO(PKGCONFIG _package _include_DIR _link_DIR _link_FLAGS _cflags)
    # reset the variables at the beginning
      SET(${_include_DIR})
      SET(${_link_DIR})
      SET(${_link_FLAGS})
      SET(${_cflags})

      # if pkg-config has been found
      IF(PKGCONFIG_EXECUTABLE)

        EXEC_PROGRAM(${PKGCONFIG_EXECUTABLE} ARGS ${_package} --exists RETURN_VALUE _return_VALUE OUTPUT_VARIABLE _pkgconfigDevNull )

        # and if the package of interest also exists for pkg-config, then get the information
        IF(NOT _return_VALUE)

          EXEC_PROGRAM(${PKGCONFIG_EXECUTABLE} ARGS ${_package} --variable=includedir 
            OUTPUT_VARIABLE ${_include_DIR} )
          string(REGEX REPLACE "[\r\n]" " " ${_include_DIR} "${${_include_DIR}}")
        

          EXEC_PROGRAM(${PKGCONFIG_EXECUTABLE} ARGS ${_package} --variable=libdir 
            OUTPUT_VARIABLE ${_link_DIR} )
          string(REGEX REPLACE "[\r\n]" " " ${_link_DIR} "${${_link_DIR}}")

          EXEC_PROGRAM(${PKGCONFIG_EXECUTABLE} ARGS ${_package} --libs 
            OUTPUT_VARIABLE ${_link_FLAGS} )
          string(REGEX REPLACE "[\r\n]" " " ${_link_FLAGS} "${${_link_FLAGS}}")

          EXEC_PROGRAM(${PKGCONFIG_EXECUTABLE} ARGS ${_package} --cflags 
            OUTPUT_VARIABLE ${_cflags} )
          string(REGEX REPLACE "[\r\n]" " " ${_cflags} "${${_cflags}}")

        ENDIF(NOT _return_VALUE)

      ENDIF(PKGCONFIG_EXECUTABLE)

    ENDMACRO(PKGCONFIG _include_DIR _link_DIR _link_FLAGS _cflags)

    MARK_AS_ADVANCED(PKGCONFIG_EXECUTABLE)
ELSE (WIN32)
    INCLUDE (UsePkgConfig)
ENDIF (WIN32)

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
    PKGCONFIG (gtkglext-1.0 GTKGL_INCLUDE_DIR GTKGL_LIB_DIR GTKGL_LDFLAGS GTKGL_CFLAGS)
        #ENDIF (WIN32)
ENDIF (BUILD_GUI_MODULE_OPENGL)

IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)


    PKGCONFIG (gtkmm-2.4 GTKMM_INCLUDE_DIR GTKMM_LIB_DIR GTKMM_LDFLAGS GTKMM_CFLAGS)
    EXEC_PROGRAM (${PKGCONFIG_EXECUTABLE} OUTPUT_VARIABLE o)
    MESSAGE ("${o}")
    MESSAGE ("${GTKMM_INCLUDE_DIR}")
    IF (NOT GTKMM_CFLAGS)
        MESSAGE ("Did not find gtkmm-2.4, but it is necessary.")
    ENDIF (NOT GTKMM_CFLAGS)
    ADD_DEFINITIONS (-DHAVE_GTK_2_4)

    SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKMM_LDFLAGS})

IF (GTKGL_INCLUDE_DIR AND BUILD_GUI_MODULE_OPENGL)
    SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKGL_LDFLAGS})
    SET (EXTRA_INCLUDE_PATH ${EXTRA_INCLUDE_PATH} ${GTKGL_INCLUDE_DIR})
    ADD_DEFINITIONS (${GTKGL_CFLAGS})
ENDIF (GTKGL_INCLUDE_DIR AND BUILD_GUI_MODULE_OPENGL)

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})

IF (BUILD_GUI_MODULE_OPENGL)
    ADD_DEPENDENCIES (${TARGETNAME} Gogl)
    TARGET_LINK_LIBRARIES (${TARGETNAME} Gogl)
    INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../gl/include)
    MESSAGE ("******************************** GOGL include dir: ${CMAKE_CURRENT_SOURCE_DIR}/../gl/include")
ENDIF (BUILD_GUI_MODULE_OPENGL)

ADD_DEPENDENCIES (${TARGETNAME} Go)
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
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
ADD_DEFINITIONS (${GTKMM_CFLAGS} -DGTKMM_GUI)

