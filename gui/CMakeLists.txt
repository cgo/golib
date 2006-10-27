INCLUDE (UsePkgConfig)
PROJECT             (Gogui)
SET                 (TARGETNAME Gogui)
SET                 (SOURCES src/offviewcontrol.cpp 
                             src/offview.cpp
                             src/glwidget.cpp
                             src/control.cpp
                             src/helper.cpp
                             src/about.cpp
                             src/mainwindow.cpp
                             src/countfilenames.cpp)

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
    PKGCONFIG (gtkglext-1.0 GTKGL_INCLUDE_DIR GTKGL_LIB_DIR GTKGL_LDFLAGS GTKGL_CFLAGS)
    IF (NOT GTKMM_CFLAGS)
        MESSAGE ("Did not find gtkmm-2.4, but it is necessary.")
    ENDIF (NOT GTKMM_CFLAGS)
    ADD_DEFINITIONS (-DHAVE_GTK_2_4)

    SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKMM_LDFLAGS} ${GTKGL_LDFLAGS})

INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${Gogui_SOURCE_DIR}/include
                     ${Gogl_SOURCE_DIR}/include
                     ${GOPATH}/include)
                     
                 #SET (CMAKE_CXX_COMPILER g++-3.3.6)
                 #SET (CMAKE_C_COMPILER gcc-3.3.6)

# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
ADD_DEFINITIONS (${GTKMM_CFLAGS} -DGTKMM_GUI ${GTKGL_CFLAGS})

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
INSTALL_TARGETS     (/lib ${TARGETNAME})
