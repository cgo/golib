/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_TEXTUREIMAGE_H
#define GOGL_TEXTUREIMAGE_H

#include <gogl/drawableobject.h>
#include <gosignal3dbase.h>

namespace goGL
{
    class TextureImagePrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Represents an OpenGL quad (a plane)
     * with a texture given by an image.
     * The image is tiled into suitably sized texture tiles.
     */
    class TextureImage : public DrawableObject
    {
        public:
            TextureImage ();
            virtual ~TextureImage ();

            void setImage (const goSignal3DBase<void>& image);

            virtual bool operator () () const;
            virtual bool draw () const;
            virtual bool init (); //= Do stuff like creating lists etc.

        private:
            TextureImage (const TextureImage&);
            TextureImage& operator= (const TextureImage&);

        private:
            TextureImagePrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
