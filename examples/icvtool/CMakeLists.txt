include (FindPkgConfig)
include (CheckLibraryExists)

set (TARGET icvtool)
set (SOURCES icvtool.cpp gaussimagecontrol.cpp)

## Cairo and Pango (for use of the goPlot library)
pkg_search_module (CAIRO REQUIRED cairo)
pkg_search_module (PANGO REQUIRED pango)
pkg_search_module (GTKMM REQUIRED gtkmm-3.0)
pkg_search_module (GOLIB REQUIRED golib)
# pkg_search_module (GOGUI REQUIRED gogui)

# include_directories ($ENV{GOPATH}/include $ENV{GOPATH}/gui/include ${CAIRO_INCLUDE_DIRS} ${PANGO_INCLUDE_DIRS} ${GTKMM_INCLUDE_DIRS})
include_directories (${GOLIB_INCLUDE_DIRS} ${CAIRO_INCLUDE_DIRS} ${PANGO_INCLUDE_DIRS} ${GTKMM_INCLUDE_DIRS})


add_executable (${TARGET} ${SOURCES})
target_link_libraries (${TARGET} 
                       ${GOLIB_LDFLAGS}
                       ${CAIRO_LDFLAGS} 
                       ${PANGO_LDFLAGS} 
                       ${GTKMM_LDFLAGS})
set_property (TARGET ${TARGET} PROPERTY CXX_STANDARD 11) 
