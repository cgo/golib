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
            Camera (int width = 640, int height = 480, goFloat fov_angle=30.0f, goFloat xy_aspect=-1.0f, goFloat near_clip=0.1f, goFloat far_clip=1000.0f);
            virtual ~Camera ();

            bool setProjection () const;
            bool setViewingTransformation () const;
            virtual bool operator() () const;

            goFloat viewPortWidth (const goVectorf& at) const;
            goFloat viewPortHeight (const goVectorf& at) const;

            virtual bool writeASCII (FILE* f) const;
            virtual bool writeASCII (const char* filename) const;
            virtual bool readASCII (FILE* f);
            virtual bool readASCII (const char* filename);

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
