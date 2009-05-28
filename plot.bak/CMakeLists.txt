INCLUDE             (CheckLibraryExists)
INCLUDE             (CheckTypeSize)
INCLUDE             (CheckIncludeFile)
INCLUDE             (CheckFunctionExists)
INCLUDE             (UsePkgConfig)

set (TARGET goplot)

set (SOURCES)

PKGCONFIG (gtkmm-2.4 GTKMM_INCLUDE_DIR GTKMM_LIB_DIR GTKMM_LDFLAGS GTKMM_CFLAGS)
# PKGCONFIG (gtkglext-1.0 GTKGL_INCLUDE_DIR GTKGL_LIB_DIR GTKGL_LDFLAGS GTKGL_CFLAGS)

include_directories ($ENV{CPPFLAGS} ${GTKMM_INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include) 
add_library (${TARGET} SHARED ${SOURCES})
add_definitions (${GTKMM_CFLAGS})
target_link_libraries (${TARGET} ${GTKMM_LDFLAGS})
install_targets (/lib ${TARGET})
