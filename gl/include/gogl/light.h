/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_LIGHT_H
#define GOGL_LIGHT_H

#include <gogl/gl.h>
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
     * @brief Light object for OpenGL.
     */
    class Light : public goObjectBase
    {
        public:
            Light (GLenum light_enum = GL_LIGHT0);
            virtual ~Light ();

            bool setPosition () const;
            virtual bool operator() () const;

            virtual bool writeASCII (FILE* f) const;
            virtual bool writeASCII (const char* filename) const;
            virtual bool readASCII (FILE* f);
            virtual bool readASCII (const char* filename);

            goVectorf myPosition;
            goVectorf myAmbient;
            goVectorf mySpecular;
            goVectorf myDiffuse;
            GLenum    myLightEnum;

            bool      myEnabled;
    };
/** 
 * @}
 */
};

#endif
