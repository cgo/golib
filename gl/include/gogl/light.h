#ifndef GOGL_LIGHT_H
#define GOGL_LIGHT_H

#include <GL/gl.h>
#include <goobjectbase.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
    class Light : public goObjectBase
    {
        public:
            Light (GLenum light_enum = GL_LIGHT0);
            virtual ~Light ();

            bool setPosition () const;
            virtual bool operator() () const;

            goVectorf myPosition;
            goVectorf myAmbient;
            goVectorf mySpecular;
            goVectorf myDiffuse;
            GLenum    myLightEnum;

            bool      myEnabled;
    };
};

#endif
