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

        private:
            Scene (const Scene& o);
            Scene& operator= (const Scene& o);

        private:
            ScenePrivate* myPrivate;
    };
};

#endif
