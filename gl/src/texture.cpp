/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/texture.h>
#include <gosignalhelper.h>


goGL::Texture::Texture (const goSignal3DBase<void>& image)
    : myName (0)
{
    this->init (image);
}

goGL::Texture::~Texture ()
{
    glDeleteTextures (1, &this->myName);
}

GLuint goGL::Texture::getName () const
{
    return this->myName;
}

void goGL::Texture::init (const goSignal3DBase<void>& image)
{
    int id = image.getDataType().getID();
    GLenum type = GL_UNSIGNED_BYTE;
    switch (id)
    {
        case GO_UINT8: type = GL_UNSIGNED_BYTE; break;
        case GO_INT8: type = GL_BYTE; break;
        case GO_UINT16: type = GL_UNSIGNED_SHORT; break;
        case GO_INT16: type = GL_SHORT; break;
        case GO_UINT32: type = GL_UNSIGNED_INT; break;
        case GO_INT32: type = GL_INT; break;
        case GO_FLOAT: type = GL_FLOAT; break;
        default: goLog::error ("goGL::Texture::init(): type not supported.");
                 return; break;
    }

    goSignal3D<void> image2;
    const goSignal3DBase<void>* image_ptr = 0;
    if (image.getSize() == image.getBlockSize())
    {
        image_ptr = &image;
    }
    else
    {
        image2.setDataType (id);
        image2.make (image.getSize(), image.getSize(), goSize3D(4,4,0), image.getChannelCount());
        goCopySignal (&image, &image2);
        image_ptr = &image2;
    }

    GLenum format = GL_LUMINANCE;
    GLenum internal_format = GL_INTENSITY;
    switch (image_ptr->getChannelCount())
    {
        case 1: format = GL_LUMINANCE; 
                internal_format = GL_RGB;
                // printf ("Intensity\n");
                break;
        case 3: format = GL_RGB;
                internal_format = GL_RGB;
                // printf ("RGB\n");
                break;
        case 4: format = GL_RGBA; 
                internal_format = GL_RGBA;
                //printf ("RGBA\n");
                break;
        default: goLog::error ("goGL::Texture::init(): unsupported format.");
                 return; break;
    }

    glGenTextures (1, &this->myName);
    if (glGetError() != GL_NO_ERROR)
        printf ("glGenTextures error\n");
    glBindTexture (GL_TEXTURE_2D, this->myName);
    if (glGetError() != GL_NO_ERROR)
        printf ("glBindTexture error\n");
    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
    //... add parameters if needed
    
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_REPEAT);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_REPEAT);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    
    glTexImage2D (GL_TEXTURE_2D, 0, internal_format, image_ptr->getSizeX(), image_ptr->getSizeY(), 0, format, type, image_ptr->getPtr());
    GLenum er = glGetError();
    if (er != GL_NO_ERROR)
    {
        printf ("glTexImage error\n");
        printf ("%s\n", (const char*)gluErrorString (er));
    }
}
