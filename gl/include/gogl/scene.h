/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_SCENE_H
#define GOGL_SCENE_H

#include <gogl/object.h>
#include <gogl/drawableobject.h>
#include <gogl/light.h>
#include <gogl/camera.h>
#include <goautoptr.h>

namespace goGL
{
    class ScenePrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief OpenGL scene class.
     */
    class Scene : public Object
    {
        public:
            Scene ();
            virtual ~Scene ();

            bool render ();
            bool add (goAutoPtr<goGL::DrawableObject> o);
            bool removeObject (int i);

            virtual bool writeASCII (FILE* f) const;
            virtual bool readASCII (FILE* f);
            virtual bool writeASCII (const char* filename) const;
            virtual bool readASCII (const char* filename);

            goSize_t                        getObjectCount () const;
            goAutoPtr<goGL::DrawableObject> getObject (goSize_t i);

            goSize_t               getLightCount () const;
            goAutoPtr<goGL::Light> getLight (goSize_t i);

            goAutoPtr<goGL::Camera> getCamera ();

            const goVectorf& getClearColour () const;
            void             setClearColour (const goVectorf& cc);
            const goVectorf& getAmbient () const;
            void             setAmbient (const goVectorf& a);

        private:
            Scene (const Scene& o);
            Scene& operator= (const Scene& o);

        private:
            ScenePrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
