#ifndef GOGL_HELPER_H
#define GOGL_HELPER_H

#include <GL/gl.h>
#include <gosignal3d.h>
#include <govector.h>

namespace goGL
{
    bool getGLBuffer (goSignal3D<void>& ret);
    bool viewSphere  (goFloat phi, goFloat theta, goFloat radius,
                      goVectorf* positionRet = 0, goVectorf* upRet = 0);
    bool findNeighbouringVertices (goSize_t Nvertices, const goFixedArray<goVector<int> >& faces, goMatrix<goIndex_t>& ret);
}

#endif
