#ifndef GOGUI_VIDEOCAPTURECONTROL_H
#define GOGUI_VIDEOCAPTURECONTROL_H

#include <gogui/control.h>
#include <goautoptr.h>
#include <gofunctor.h>
#include <gosignal3d.h>

namespace goGUI
{

    class VideoCaptureControlPrivate;

    class VideoCaptureControl : public Control
    {
        public:
            VideoCaptureControl ();
            virtual ~VideoCaptureControl ();

            // bool capture (goSignal3D<void>& target);
            void capture ();
            void setTarget (goAutoPtr<goSignal3D<void> > target);
            void contCaptureToggle ();

            goCaller0<int>& capturedCaller ();

        private:
            VideoCaptureControlPrivate* myPrivate;
    };
};

#endif
