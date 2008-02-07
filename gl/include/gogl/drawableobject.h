#ifndef GOGL_DRAWABLEOBJECT_H
#define GOGL_DRAWABLEOBJECT_H

#include <gogl/object.h>
#include <gogl/material.h>
#include <GL/gl.h>

namespace goGL
{
    class DrawableObjectPrivate;

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
};

#endif
