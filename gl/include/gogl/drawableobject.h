/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_DRAWABLEOBJECT_H
#define GOGL_DRAWABLEOBJECT_H

#include <gogl/gl.h>
#include <gogl/object.h>
#include <gogl/material.h>

namespace goGL
{
    class DrawableObjectPrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Drawable OpenGL object.
     */
    class DrawableObject : public Object
    {
        public:
            virtual ~DrawableObject ();

            virtual bool operator () () const = 0;
            virtual bool draw () const = 0;
            virtual bool init () = 0; //= Do stuff like creating lists etc.

            virtual bool setRenderParameters () const;
            
            void                  setMaterial (const goGL::Material& mat);
            goGL::Material&       getMaterial ();
            const goGL::Material& getMaterial () const;
            void setLighting (bool l);
            void setShadeModel (GLenum m);

            bool getLighting () const;
            GLenum getShadeModel () const;

            bool toList (int listName);

            virtual bool writeASCII (FILE* f) const;
            virtual bool readASCII (FILE* f);

            DrawableObject& operator= (const DrawableObject& o);

        protected:
            DrawableObject ();
            DrawableObject (const DrawableObject& o);

        private:
            DrawableObjectPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
