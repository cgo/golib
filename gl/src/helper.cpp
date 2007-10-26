#include <gogl/helper.h>
#include <GL/glu.h>
#include <golog.h>
#include <gomatrix.h>

static int check_gl_error (const char* name)
{
    int n = 0;
    if ((n = glGetError()) != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
    return n;
}

bool goGL::getGLBuffer (goSignal3D<void>& ret)
{
    glReadBuffer (GL_FRONT);
    check_gl_error ("glReadBuffer");
    GLint type = GL_UNSIGNED_BYTE;
    GLenum format = GL_RGB;
    switch (ret.getChannelCount())
    {
        case 1: format = GL_LUMINANCE; break;
        case 3: format = GL_RGB; break;
        case 4: format = GL_RGBA; break;
        default: return false; break;
    }
    switch (ret.getDataType().getID())
    {
        case GO_UINT8: type = GL_UNSIGNED_BYTE; break;
        case GO_INT8: type = GL_BYTE; break;
        case GO_UINT16: type = GL_UNSIGNED_SHORT; break;
        case GO_INT16: type = GL_SHORT; break;
        case GO_UINT32: type = GL_UNSIGNED_INT; break;
        case GO_INT32: type = GL_INT; break;
        case GO_FLOAT: type = GL_FLOAT; break;
        default: goLog::warning ("goGL::getGLBuffer(): only data types up to float are allowed.");
                 return false; break;
    }
    GLint viewport[4];
    glGetIntegerv (GL_VIEWPORT, viewport);
    if (ret.getSize() != goSize3D (viewport[2], viewport[3], 1) ||
        ret.getSize() != ret.getBlockSize())
    {
        ret.make (goSize3D (viewport[2], viewport[3], 1),
                  goSize3D (viewport[2], viewport[3], 1),
                  ret.getBorderSize (),
                  ret.getChannelCount ());
    }
    glPixelStorei (GL_PACK_ALIGNMENT, 1);
    glReadPixels (viewport[0], viewport[1], viewport[2], viewport[3], 
            format, type, ret.getPtr());
    // ret.flip (GO_Y);  // flipping is buggy.
    check_gl_error ("glReadPixels");
    return true;
}


/** 
 * @brief Set OpenGL camera on view sphere, looking at the origin.
 * 
 * @param phi      in [-pi,pi]
 * @param theta    in [0,2*pi]
 * @param radius 
 * @param positionRet  3D position, return value
 * @param upRet        3D up-vector, return value
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::viewSphere (goFloat phi, goFloat theta, goFloat radius,
                       goVectorf* positionRet, goVectorf* upRet)
{
    //= Spherical to cartesian coordinates
    GLfloat sin_theta = ::sin(theta);

    //= Only rotation of phi from x towards y axis
    go3Vector<goFloat> v1 (radius * ::cos(phi), radius * ::sin(phi), 0.0f);
    //= Complete rotation, first phi towards y, then the result towards z axis
    go3Vector<goFloat> v2 (v1.x * sin_theta, v1.y * sin_theta, radius * ::cos(theta));
    //= Create up vector as cross product
    go3Vector<goFloat> up (v2);

    up.cross (v1);
    up *= 1.0f / up.abs();
    if (v2.z < 0.0f)
        up *= -1.0f;

    glLoadIdentity ();
    gluLookAt (v2.x, v2.y, v2.z,
               0.0f, 0.0f, 0.0f,
               up.x, up.y, up.z);
    if (positionRet)
    {
        positionRet->resize (3);
        (*positionRet)[0] = v2.x;
        (*positionRet)[1] = v2.y;
        (*positionRet)[2] = v2.z;
    }
    if (upRet)
    {
        upRet->resize (3);
        (*upRet)[0] = up.x;
        (*upRet)[1] = up.y;
        (*upRet)[2] = up.z;
    }

    return true;
}

/* 
 * Unused, not useful, remove.
 *
 * @brief Find neighbouring vertices for each vertex in a triangle mesh.
 * 
 * @note Triangles may not occur twice in the faces array, e.g. in permuted form.
 *
 * @param Nvertices 
 * @param faces Actually triangles (must be triangles)
 * @param ret Matrix, containing for each vertex i the neighbouring vertices in row i.
 * The matrix is Nvertices X 6. If a vertex has less than 6 neighbours, the remaining
 * entries are -1.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::findNeighbouringVertices (goSize_t Nvertices, const goFixedArray<goVector<int> >& faces, goMatrix<goIndex_t>& ret)
{
    goSize_t Nfaces = faces.getSize();
    ret.resize (Nvertices, 6);
    ret.fill (-1);
    for (goSize_t i = 0; i < Nvertices; ++i)
    {
        goSize_t neigh_index = 0;
        for (goSize_t j = 0; j < Nfaces; ++j)
        {
            const goVector<int>& v = faces[j];
            for (goSize_t k = 0; k < v.getSize(); ++k)
            {
                if (v[k] == i)  // contains vertex
                {
                    //= Add all except i to row i in ret, and do not add
                    //= already added points.
                    for (goSize_t k2 = 0; k2 < v.getSize(); ++k2)
                    {
                        if (v[k2] != i) 
                        {
                            bool contained = false;
                            for (goSize_t k3 = 0; k3 < 6; ++k3)
                            {
                                if (ret(i,k3) == v[k2]) 
                                {
                                    contained = true;
                                    break;
                                }
                            }
                            if (!contained)
                            {
                                ret(i,neigh_index) = v[k2];
                                ++neigh_index;
                            }
                        }
                    }
                    break;
                }
            }
            //= Sicher ist sicher .. sollte aber nicht passieren.
            if (neigh_index >= 6)
                break;
        }
    }
}

