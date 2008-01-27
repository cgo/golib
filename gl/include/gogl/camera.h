#ifndef GOGL_CAMERA_H
#define GOGL_CAMERA_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goGL
{
    class Camera : public goObjectBase
    {
        public:
            Camera (int width, int height, goFloat fov_angle=30.0f, goFloat xy_aspect=-1.0f, goFloat near_clip=0.1f, goFloat far_clip=1000.0f);
            virtual ~Camera ();

            bool setProjection () const;
            bool setViewingTransformation () const;
            virtual bool operator() () const;

            int myWidth;
            int myHeight;
            goFloat myFOVAngle;
            goFloat myXYAspect;
            goFloat myNearClip;
            goFloat myFarClip;

            goVectorf myPosition;
            goVectorf myLookat;
            goVectorf myUp;
    };
};

#endif