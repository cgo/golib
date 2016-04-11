/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_HELPER_H
#define GOGL_HELPER_H

#include <gogl/gl.h>
#include <gosignal3d.h>
#include <govector.h>
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

namespace goGL
{
/** @addtogroup gl
 * @{
 */
    bool getGLBuffer (goSignal3D<void>& ret);
    bool viewSphere  (goFloat phi, goFloat theta, goFloat radius,
                      goVectorf* positionRet = 0, goVectorf* upRet = 0);
    bool findNeighbouringVertices (goSize_t Nvertices, const goFixedArray<goVector<int> >& faces, goMatrix<goIndex_t>& ret);
/** 
 * @}
 */
}

#endif
