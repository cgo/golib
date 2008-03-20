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
                  capturedCaller ()
            {
                vc.setDevice ("/dev/video0");
                vc.open ();
                vc.getSettings ();
                vc.setCaptureSize (640, 480);
                vc.setSettings ();
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
    };
};

goGUI::VideoCaptureControl::VideoCaptureControl () 
    : Control ("Video Capture"),
      myPrivate (0)
{
    myPrivate = new VideoCaptureControlPrivate;

    {
        Gtk::HBox* hbox = Gtk::manage (new Gtk::HBox);
        hbox->pack_start (myPrivate->captureButton);
        hbox->pack_start (myPrivate->contCapture);
        this->add (*hbox);
    }

    myPrivate->captureButton.signal_clicked().connect (sigc::mem_fun (*this, &VideoCaptureControl::capture));
    myPrivate->contCapture.signal_toggled().connect (sigc::mem_fun (*this, &VideoCaptureControl::contCaptureToggle));
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
