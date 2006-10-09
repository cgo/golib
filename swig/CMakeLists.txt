INCLUDE (FindSWIG)
PROJECT             (golib_guile)

SET (SCM_INSTALL_DIR $ENV{HOME}/.guile-modules CACHE PATH "Directory which holds guile modules -- often pointed to by the environment variable GUILE_LOAD_PATH")

EXEC_PROGRAM        (guile-config ARGS link OUTPUT_VARIABLE GUILE_LIBS)
EXEC_PROGRAM        (guile-config ARGS compile OUTPUT_VARIABLE GUILE_CFLAGS)
#EXEC_PROGRAM        (chicken-config ARGS -shared -libs OUTPUT_VARIABLE CHICKEN_LIBS)
EXEC_PROGRAM        (${SWIG_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}
                        ARGS -I../include -Linkage passive -scmstub -guile -c++ -scm -proxy golib.i)
FIND_LIBRARY        (GOLIB NAMES Go PATHS $ENV{GOPATH}/../lib ${GOPATH}/lib)
SET                 (GOMATLAB_LIB "-lGomatlab")
LINK_LIBRARIES      (${GUILE_LIBS} ${GOLIB} ${GOMATLAB_LIB})
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../include)
ADD_DEFINITIONS     (${GUILE_CFLAGS})
ADD_LIBRARY         (golib_guile MODULE golib_wrap.cxx golibguile.cpp)
INSTALL_TARGETS     (/lib golib_guile)

SET (CMAKE_CXX_COMPILER g++-3.3)

IF (NOT EXISTS ${SCM_INSTALL_DIR})
    MAKE_DIRECTORY (${SCM_INSTALL_DIR})
ENDIF (NOT EXISTS ${SCM_INSTALL_DIR})

SET (SCM_FILES ${CMAKE_CURRENT_SOURCE_DIR}/golib_guile.scm
               ${CMAKE_CURRENT_SOURCE_DIR}/golib_guile-primitive.scm CACHE FILEPATH "scm files to copy to SCM_INSTALL_DIR")

ADD_CUSTOM_TARGET   (install-scm cp ${SCM_FILES} ${SCM_INSTALL_DIR})
