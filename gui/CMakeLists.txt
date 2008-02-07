INCLUDE (UsePkgConfig)
PROJECT             (Gogui)
SET                 (TARGETNAME Gogui)
SET                 (SOURCES 
                             src/scenecontrol.cpp
                             src/sceneview.cpp
                             src/multivectorinput.cpp
                             src/vectorinput.cpp
                             src/imageview.cpp
                             src/draw.cpp
                             src/control.cpp
                             src/helper.cpp
                             src/about.cpp
                             src/mainwindow.cpp
                             src/countfilenames.cpp)

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
    PKGCONFIG (gtkglext-1.0 GTKGL_INCLUDE_DIR GTKGL_LIB_DIR GTKGL_LDFLAGS GTKGL_CFLAGS)
ENDIF (BUILD_GUI_MODULE_OPENGL)

IF (BUILD_WITH_GCC_3_3)
    SET (CMAKE_CXX_COMPILER g++-3.3)
    SET (CMAKE_C_COMPILER gcc-3.3)
ENDIF (BUILD_WITH_GCC_3_3)
IF (BUILD_WITH_GCC_3_4)
    SET (CMAKE_CXX_COMPILER g++-3.4)
    SET (CMAKE_C_COMPILER gcc-3.4)
ENDIF (BUILD_WITH_GCC_3_4)


# If the the environment variable MATLAB is set, add include path.
#IF($ENV{MATLAB})
#    SET (EXTRA_INCLUDE_PATH $ENV{MATLAB}/extern/include)
#    ADD_DEFINITIONS (-DHAVE_MATLAB)
#ENDIF($ENV{MATLAB})

#SET (GOPATH $ENV{GOPATH})

#FIND_LIBRARY (GOLIB NAMES Go PATHS ${GOPATH}/../lib /usr/local/lib)
#FIND_LIBRARY (GOMATLAB NAMES Gomatlab PATHS ${GOPATH}/../lib /usr/local/lib)
#SET (EXTRA_LIBS ${GOLIB} ${GOMATLAB})

#FIND_LIBRARY (GTKGLEXTMM NAMES gtkglextmm-x11-1.0)
#FIND_LIBRARY (GDKGLEXTMM NAMES gdkglextmm-x11-1.0)
#SET (EXTRA_INCLUDE_PATH /usr/include/gtkglextmm-1.0)

    PKGCONFIG (gtkmm-2.4 GTKMM_INCLUDE_DIR GTKMM_LIB_DIR GTKMM_LDFLAGS GTKMM_CFLAGS)
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


INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${Gogui_SOURCE_DIR}/include
                     ${Gogl_SOURCE_DIR}/include
                     ${GOPATH}/include)
                     
#IF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")
#    SET (CMAKE_CXX_COMPILER g++-3.3)
#    SET (CMAKE_C_COMPILER gcc-3.3)
#ENDIF (BUILD_MATLAB_MODULE AND ${MACHINE_TYPE} STREQUAL "x86")

# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
ADD_DEFINITIONS (${GTKMM_CFLAGS} -DGTKMM_GUI)

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
INSTALL_TARGETS     (/lib ${TARGETNAME})
