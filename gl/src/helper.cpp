#include <gogl/helper.h>
#include <golog.h>

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
