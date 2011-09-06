/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_VIDEOCAPTURECONTROL_H
#define GOGUI_VIDEOCAPTURECONTROL_H

#include <gogui/control.h>
#include <goautoptr.h>
#include <gofunctor.h>
#include <gosignal3d.h>
#include <govideocapture.h>

namespace goGUI
{

    class VideoCaptureControlPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Control for video capturing.
     *
     * Provides its own goVideoCapture (see getVideoCapture).
     */
    class VideoCaptureControl : public Control
    {
        public:
            VideoCaptureControl ();
            virtual ~VideoCaptureControl ();

            // bool capture (goSignal3D<void>& target);
            void capture ();
            void setTarget (goAutoPtr<goSignal3DBase<void> > target);
            goAutoPtr<goSignal3DBase<void> >  getTarget ();
            void contCaptureToggle ();
            void swapRGBToggle ();

            goCaller0<void>& capturedCaller ();

            goVideoCapture& getVideoCapture ();

            void setWhiteness ();
            void setBrightness ();
            void setColour ();
            void setHue ();
            void setContrast ();

        private:
            VideoCaptureControlPrivate* myPrivate;
    };
/** \} */
};

#endif
