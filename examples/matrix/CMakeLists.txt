find_library (GOLIB NAMES Go PATHS $ENV{GOPATH}/pgm/lib)
include_directories ($ENV{GOPATH}/include)

add_executable (matrix2 matrix2.cpp)
target_link_libraries (matrix2 ${GOLIB})
