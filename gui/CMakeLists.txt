INCLUDE (UsePkgConfig)
PROJECT             (Gogui)
SET                 (TARGETNAME Gogui)
SET                 (SOURCES src/control.cpp
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

    PKGCONFIG (gtkmm-2.4 GTKMM_INCLUDE_DIR GTKMM_LIB_DIR GTKMM_LDFLAGS GTKMM_CFLAGS)
    IF (NOT GTKMM_CFLAGS)
        MESSAGE ("Did not find gtkmm-2.4, but it is necessary.")
    ENDIF (NOT GTKMM_CFLAGS)
    ADD_DEFINITIONS (-DHAVE_GTK_2_4)

    SET (EXTRA_LIBS ${EXTRA_LIBS} ${GTKMM_LDFLAGS})

INCLUDE_DIRECTORIES (${EXTRA_INCLUDE_PATH}
                     ${Gogui_SOURCE_DIR}/include
                     ${GOPATH}/include)
                     
                 #SET (CMAKE_CXX_COMPILER g++-3.3.6)
                 #SET (CMAKE_C_COMPILER gcc-3.3.6)

# Add -MT flag!
IF(UNIX)
 ADD_DEFINITIONS     (-Wall -ansi $ENV{CPPFLAGS} -D_REENTRANT)
ENDIF(UNIX)
ADD_DEFINITIONS (${GTKMM_CFLAGS} -DGTKMM_GUI)

ADD_LIBRARY (${TARGETNAME} SHARED ${SOURCES})
TARGET_LINK_LIBRARIES  (${TARGETNAME} ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
INSTALL_TARGETS     (/lib ${TARGETNAME})
