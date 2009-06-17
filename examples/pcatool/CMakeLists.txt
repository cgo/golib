include (FindPkgConfig)
include (CheckLibraryExists)

set (TARGET pcatool)
set (SOURCES pcatool.cpp)

## Cairo and Pango (for use of the goPlot library)
pkg_search_module (CAIRO REQUIRED cairo)
pkg_search_module (PANGO REQUIRED pango)
pkg_search_module (GTKMM REQUIRED gtkmm-2.4)

find_library (GOLIB NAMES Go PATHS $ENV{HOME}/pgm/lib /space/gosch/lib)
find_library (GOGUI NAMES Gogui PATHS $ENV{HOME}/pgm/lib /space/gosch/lib)
find_library (LIBSHAPE NAMES shape PATHS $ENV{HOME}/pgm/lib /space/gosch/lib)

include_directories ($ENV{GOPATH}/include $ENV{GOPATH}/gui/include ${CAIRO_INCLUDE_DIRS} ${PANGO_INCLUDE_DIRS} ${GTKMM_INCLUDE_DIRS})
#include_directories ($ENV{HOME}/Documents/partial-shapes/libshape/include)
include_directories ($ENV{HOME}/Work/partial-shapes/libshape/include)

add_executable (${TARGET} ${SOURCES})
target_link_libraries (${TARGET} 
                       ${GOLIB}
                       ${GOGUI}
                       ${LIBSHAPE})
                       #${CAIRO_LDFLAGS} 
                       #${PANGO_LDFLAGS} 
                       #${GTKMM_LDFLAGS})
