SET (SOURCES ${PROJECT_SOURCE_DIR}/matlab/mex/videocapture/videocapture.cpp)

SET (TARGET videocapture${MEX_EXTENSION})
# MESSAGE ("MEX target: ${TARGET}")

FIND_LIBRARY (GOLIB  NAMES Go  PATHS ${GOPATH}/../lib)

SET (MEX_CPPFLAGS -I${GOPATH}/include -I${EXTRA_INCLUDE_PATH})
SET (MEX_LDFLAGS $ENV{LDFLAGS} -lGo -lGomatlab)

ADD_CUSTOM_TARGET (${TARGET} ALL
                    COMMAND mex ${MEX_CPPFLAGS} ${MEX_LDFLAGS} ${SOURCES}
                    DEPENDS ${SOURCES})
INSTALL_TARGETS (/lib ${TARGET})
