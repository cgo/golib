/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_MATERIAL_H
#define GOGL_MATERIAL_H

#include <GL/gl.h>
#include <goobjectbase.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Material object for OpenGL.
     */
    class Material : public goObjectBase
    {
        public:
            Material ();
            virtual ~Material ();

            virtual bool operator() () const;

            bool writeASCII (FILE* f) const;
            bool writeASCII (const char* filename) const;
            bool readASCII (FILE* f);
            bool readASCII (const char* filename);

            goVectorf myAmbient;
            goVectorf mySpecular;
            goVectorf myDiffuse;
            goFloat   myShininess;
            goVectorf myEmission;
            GLenum    myFace;
    };
/** 
 * @}
 */
};

#endif
