include (FindPkgConfig)
include (CheckLibraryExists)

set(CMAKE_CXX_STANDARD 11)

set (TARGET cairoplot)
set (SOURCES cairoplot.cpp)

## Cairo and Pango (for use of the goPlot library)
pkg_search_module (GTKMM REQUIRED gtkmm-2.4)
pkg_search_module (GOLIB REQUIRED golib)
pkg_search_module (GOGUI REQUIRED gogui)
pkg_search_module (PANGO REQUIRED pangocairo)

# find_library (GOLIB NAMES Go PATHS $ENV{HOME}/Software/lib)
# find_library (GOGUI NAMES Gogui PATHS $ENV{HOME}/Software/lib)

include_directories (${GOLIB_INCLUDE_DIRS} ${GOGUI_INCLUDE_DIRS} ${GTKMM_INCLUDE_DIRS})

add_executable (${TARGET} ${SOURCES})
target_link_libraries (
  ${TARGET}
  ${GOLIB_LDFLAGS}
  ${GOGUI_LDFLAGS})
                       #${GOGUI})
                       #${CAIRO_LDFLAGS}
                       #${PANGO_LDFLAGS}
                       #${GTKMM_LDFLAGS})
