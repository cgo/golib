#include <gogui/videocapturecontrol.h>
#include <govideocapture.h>

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
                  capturedCaller (),
                  brightnessScale (),
                  hueScale (),
                  colourScale (),
                  contrastScale (),
                  whitenessScale ()
            {
                vc.setDevice ("/dev/video0");
                vc.open ();
                vc.getSettings ();
                vc.setCaptureSize (640, 480);
                vc.setSettings ();

                printf ("Settings:");
                printf ("%f, %f, %f, %f\n", vc.getBrightness(), vc.getColour(), vc.getHue(), vc.getContrast());

                brightnessScale.set_increments (0.001, 0.1);
                hueScale.set_increments (0.001, 0.1);
                colourScale.set_increments (0.001, 0.1);
                contrastScale.set_increments (0.001, 0.1);
                whitenessScale.set_increments (0.001, 0.1);

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

                brightnessScale.set_digits (3);
                hueScale.set_digits (3);
                colourScale.set_digits (3);
                contrastScale.set_digits (3);
                whitenessScale.set_digits (3);
            };

            ~VideoCaptureControlPrivate () 
            {
                vc.close ();
            };

            goVideoCapture vc;
            goAutoPtr<goSignal3D<void> > target;

            Gtk::Button      captureButton;
            Gtk::CheckButton contCapture;

            goCaller0<int> capturedCaller;

            Gtk::HScale      brightnessScale;
            Gtk::HScale      hueScale;
            Gtk::HScale      colourScale;
            Gtk::HScale      contrastScale;
            Gtk::HScale      whitenessScale;
    };
};

goGUI::VideoCaptureControl::VideoCaptureControl () 
    : Control ("Video Capture"),
      myPrivate (0)
{
    myPrivate = new VideoCaptureControlPrivate;

    Gtk::VBox* mainBox = Gtk::manage (new Gtk::VBox);
    {
        Gtk::HBox* hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->captureButton);
        hbox->pack_start (myPrivate->contCapture);
        mainBox->pack_start (*hbox);
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

    myPrivate->whitenessScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setWhiteness));
    myPrivate->brightnessScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setBrightness));
    myPrivate->colourScale.signal_value_changed().connect (sigc::mem_fun (*this, &VideoCaptureControl::setColour));
}

goGUI::VideoCaptureControl::~VideoCaptureControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

//bool goGUI::VideoCaptureControl::capture (goSignal3D<void>& target)
//{
//    return myPrivate->vc.grab (target);
//}

void goGUI::VideoCaptureControl::capture ()
{
    if (!myPrivate->target.isNull())
    {
        myPrivate->vc.grab (*myPrivate->target);
        myPrivate->capturedCaller ();
    }
}

void goGUI::VideoCaptureControl::contCaptureToggle ()
{
    if (myPrivate->contCapture.get_active())
    {
        struct timespec t_req, t_remain;
        t_req.tv_sec = 0;
        t_req.tv_nsec = 100000000;

        while (myPrivate->contCapture.get_active())
        {
            this->capture ();
            while (Gtk::Main::events_pending())
            {
                Gtk::Main::iteration ();
            }
            
            nanosleep (&t_req, &t_remain);
        }
    }
}

void goGUI::VideoCaptureControl::setTarget (goAutoPtr<goSignal3D<void> > target)
{
    myPrivate->target = target;
}

goCaller0<int>& goGUI::VideoCaptureControl::capturedCaller ()
{
    return myPrivate->capturedCaller;
}

void goGUI::VideoCaptureControl::setWhiteness ()
{
    myPrivate->vc.setWhiteness ((int)(myPrivate->whitenessScale.get_value() * 65535.0));
}

void goGUI::VideoCaptureControl::setBrightness ()
{
    myPrivate->vc.setBrightness ((int)(myPrivate->brightnessScale.get_value() * 65535.0));
}

void goGUI::VideoCaptureControl::setColour ()
{
    myPrivate->vc.setColour ((int)(myPrivate->colourScale.get_value() * 65535.0));
}
