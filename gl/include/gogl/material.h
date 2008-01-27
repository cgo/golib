#ifndef GOGL_MATERIAL_H
#define GOGL_MATERIAL_H

#include <GL/gl.h>
#include <goobjectbase.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
    class Material : public goObjectBase
    {
        public:
            Material ();
            virtual ~Material ();

            virtual bool operator() () const;

            goVectorf myAmbient;
            goVectorf mySpecular;
            goVectorf myDiffuse;
            goFloat   myShininess;
            goVectorf myEmission;
            GLenum    myFace;
    };
};

#endif
