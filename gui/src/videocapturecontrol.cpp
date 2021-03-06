/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/videocapturecontrol.h>
#include <govideocapture.h>
#include <gosignalmacros.h>
#include <gotimerobject.h>
#include <gomath.h>

namespace goGUI
{
    class VideoCaptureControlPrivate
    {
        public:
            VideoCaptureControlPrivate ()
                : vc (),
                  target (0),
                  captureButton ("Capture"),
                  contCapture ("Continuous"),
                  swapRGB ("Swap R/B"),
                  capturedCaller (),
                  brightnessScale (),
                  hueScale (),
                  colourScale (),
                  contrastScale (),
                  whitenessScale (),
                  fpsButton ()
            {
                vc.setDevice ("/dev/video0");
                vc.open ();
                vc.getSettings ();
                vc.setCaptureSize (640, 480);
                vc.setSettings ();

                fpsButton.set_digits (0);
                fpsButton.set_range (1.0, 10000000.0);
                fpsButton.set_increments (1, 5);
                fpsButton.set_value (10);

                //printf ("Settings:");
                //printf ("%f, %f, %f, %f\n", vc.getBrightness(), vc.getColour(), vc.getHue(), vc.getContrast());

                brightnessScale.set_range (0.0, 1.0);
                hueScale.set_range (0.0, 1.0);
                colourScale.set_range (0.0, 1.0);
                contrastScale.set_range (0.0, 1.0);
                whitenessScale.set_range (0.0, 1.0);

                brightnessScale.set_value (0.5);
                hueScale.set_value (0.5);
                colourScale.set_value (0.5);
                contrastScale.set_value (0.5);
                whitenessScale.set_value (0.5);

                brightnessScale.set_digits (4);
                hueScale.set_digits (4);
                colourScale.set_digits (4);
                contrastScale.set_digits (4);
                whitenessScale.set_digits (4);

                brightnessScale.set_increments (0.001, 0.1);
                hueScale.set_increments (0.001, 0.1);
                colourScale.set_increments (0.001, 0.1);
                contrastScale.set_increments (0.001, 0.1);
                whitenessScale.set_increments (0.001, 0.1);

                brightnessScale.set_update_policy (Gtk::UPDATE_CONTINUOUS);
                hueScale.set_update_policy (Gtk::UPDATE_CONTINUOUS);
                colourScale.set_update_policy (Gtk::UPDATE_CONTINUOUS);
                contrastScale.set_update_policy (Gtk::UPDATE_CONTINUOUS);
                whitenessScale.set_update_policy (Gtk::UPDATE_CONTINUOUS);

            };

            ~VideoCaptureControlPrivate () 
            {
                vc.close ();
            };

            //= Deprecated. Does not get used anymore.
            void channelSwap ()
            {
                if (target.isNull())
                    return;

                if (target->getDataType().getID() != GO_UINT8)
                {
                    goLog::error ("goVideoCapture: channel swapping only for uint8. Not swapping.");
                    return;
                }

                if (target->getChannelCount() < 3)
                    return;
                target->setChannel (0);
                goUInt8 temp = 0;
                size_t s = sizeof (goUInt8);
                size_t ss = 2 * s;
                GO_SIGNAL3D_EACHELEMENT_GENERIC (temp = *(goUInt8*)(__ptr + ss);
                        *(goUInt8*)(__ptr + ss) = *(goUInt8*)__ptr;
                        *(goUInt8*)(__ptr) = temp;, (*target));
            };

            goVideoCapture vc;
            goAutoPtr<goSignal3DBase<void> > target;

            Gtk::Button      captureButton;
            Gtk::CheckButton contCapture;
            Gtk::CheckButton swapRGB;

            goCaller0<void>  capturedCaller;

            Gtk::HScale      brightnessScale;
            Gtk::HScale      hueScale;
            Gtk::HScale      colourScale;
            Gtk::HScale      contrastScale;
            Gtk::HScale      whitenessScale;

            Gtk::SpinButton  fpsButton;
    };
};

goGUI::VideoCaptureControl::VideoCaptureControl () 
    : Control ("Video Capture"),
      myPrivate (0)
{
    myPrivate = new VideoCaptureControlPrivate;

    Gtk::VBox* mainBox = Gtk::manage (new Gtk::VBox);
    {
        Gtk::Table* table = Gtk::manage (new Gtk::Table);
        table->attach (myPrivate->captureButton, 0, 2, 0, 1);
        table->attach (myPrivate->contCapture, 0, 1, 1, 2);
        table->attach (myPrivate->fpsButton, 1, 2, 1, 2);
        table->attach (myPrivate->swapRGB, 0, 1, 2, 3);
        mainBox->pack_start (*table);
        table->show_all ();
    }
    {
        Gtk::VBox* hbox = Gtk::manage (new Gtk::VBox);
        hbox->pack_start (myPrivate->brightnessScale);
        hbox->pack_start (myPrivate->hueScale);
        hbox->pack_start (myPrivate->colourScale);
        hbox->pack_start (myPrivate->contrastScale);
        hbox->pack_start (myPrivate->whitenessScale);
        mainBox->pack_start (*hbox);
    }
    this->add (*mainBox);

    myPrivate->captureButton.signal_clicked().connect (sigc::mem_fun (*this, &VideoCaptureControl::capture));
    myPrivate->contCapture.signal_toggled().connect (sigc::mem_fun (*this, &VideoCaptureControl::contCaptureToggle));
    myPrivate->swapRGB.signal_toggled().connect (sigc::mem_fun (*this, &VideoCaptureControl::swapRGBToggle));

    myPrivate->whitenessScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setWhiteness));
    myPrivate->brightnessScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setBrightness));
    myPrivate->colourScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setColour));
    myPrivate->hueScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setHue));
    myPrivate->contrastScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setContrast));

    this->setWhiteness ();
    this->setBrightness ();
    this->setContrast ();
    this->setHue ();
    this->setColour ();
}

goGUI::VideoCaptureControl::~VideoCaptureControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** 
 * @brief Get the goVideoCapture object.
 * 
 * @return Reference to the goVideoCapture object.
 */
goVideoCapture& goGUI::VideoCaptureControl::getVideoCapture ()
{
    return myPrivate->vc;
}

//bool goGUI::VideoCaptureControl::capture (goSignal3D<void>& target)
//{
//    return myPrivate->vc.grab (target);
//}

/** 
 * @brief Capture a frame.
 *
 * capturedCaller() is called after the frame has been captured.
 */
void goGUI::VideoCaptureControl::capture ()
{
    if (!myPrivate->target.isNull())
    {
        const int rgb_channels[] = {0, 1, 2};
        const int bgr_channels[] = {2, 1, 0};
        if (myPrivate->swapRGB.get_active())
        {
            myPrivate->vc.grab (*myPrivate->target, bgr_channels);
        }
        else
        {
            myPrivate->vc.grab (*myPrivate->target, rgb_channels);
        }
        //{
        //    myPrivate->channelSwap ();
        //}
        myPrivate->capturedCaller ();
    }
}

/** 
 * @brief Toggle continuous capture.
 *
 * During continuous capture, Gtk::Main::iteration() is called 
 * <b>after each capture</b>. Then the loop waits for a set time
 * and repeats.
 */
void goGUI::VideoCaptureControl::contCaptureToggle ()
{
    if (myPrivate->contCapture.get_active())
    {
        struct timespec t_req, t_remain;
        t_req.tv_sec = 0;
        // t_req.tv_nsec = 10000000;
            
        t_req.tv_nsec = 10000;

        goTimerObject timer;

        while (myPrivate->contCapture.get_active())
        {
            int fps = myPrivate->fpsButton.get_value_as_int ();
            timer.startTimer ();
            this->capture ();
            timer.stopTimer ();
            long int nsec = 1000000000 / fps - (long int) (timer.getTimerSeconds() * 1e9);
            t_req.tv_nsec = goMath::max<long int> (0, nsec);
            while (Gtk::Main::events_pending())
            {
                Gtk::Main::iteration ();
            }
            
            nanosleep (&t_req, &t_remain);
        }
    }
}

void goGUI::VideoCaptureControl::swapRGBToggle ()
{
    if (myPrivate->contCapture.get_active())
    {
        // FIXME
    }
}

/** 
 * @brief Set the target goSignal3DBase to hold the image data.
 * 
 * @param target Pointer to the target. Must be 3-channel GO_UINT8 data.
 */
void goGUI::VideoCaptureControl::setTarget (goAutoPtr<goSignal3DBase<void> > target)
{
    myPrivate->target = target;
}

/** 
 * @brief Get the pointer to the target image.
 * 
 * @return An autoptr to the target image. Note is can be Null if no target has been set.
 */
goAutoPtr<goSignal3DBase<void> >  goGUI::VideoCaptureControl::getTarget ()
{
    return myPrivate->target;
}

/** 
 * @brief This caller gets called every time capture() has successfully been called.
 * 
 * @return The caller.
 */
goCaller0<void>& goGUI::VideoCaptureControl::capturedCaller ()
{
    return myPrivate->capturedCaller;
}

void goGUI::VideoCaptureControl::setWhiteness ()
{
    myPrivate->vc.setWhiteness (myPrivate->whitenessScale.get_value());
}

void goGUI::VideoCaptureControl::setBrightness ()
{
    myPrivate->vc.setBrightness (myPrivate->brightnessScale.get_value());
}

void goGUI::VideoCaptureControl::setColour ()
{
    myPrivate->vc.setColour (myPrivate->colourScale.get_value());
}

void goGUI::VideoCaptureControl::setContrast ()
{
    myPrivate->vc.setContrast (myPrivate->contrastScale.get_value());
}

void goGUI::VideoCaptureControl::setHue ()
{
    myPrivate->vc.setHue (myPrivate->hueScale.get_value());
}
