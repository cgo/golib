#include "vcgui.h"
#include <gofunctor.h>
#include <gogui/imageview.h>
#include <gogui/videocapturecontrol.h>
#include <gtkmm.h>

#include <gopython.h>

class VCGuiPrivate
{
    public:
        VCGuiPrivate () 
            : imageView (), imageContainer (), vccontrol ()
        {
        };
        ~VCGuiPrivate () {};

        goGUI::ImageView           imageView;
        Gtk::ScrolledWindow        imageContainer;
        goGUI::VideoCaptureControl vccontrol;
};

VCGui::VCGui () 
    : goGUI::MainWindow (), myPrivate (0)
{
    myPrivate = new VCGuiPrivate;
    this->set_title ("Video Capture");
    this->addFileAbout ("Video capture example\nusing golib 0.5 and gogui\n\n(C) Christian Gosch");
    this->getFileMenu()->append (*Gtk::manage (new Gtk::SeparatorMenuItem));
    this->addFileQuit ();
    this->addControl (myPrivate->vccontrol);

    myPrivate->imageContainer.add (myPrivate->imageView);
    this->getPaned().add1 (myPrivate->imageContainer);

    int w = myPrivate->vccontrol.getVideoCapture().getCaptureWidth ();
    int h = myPrivate->vccontrol.getVideoCapture().getCaptureHeight ();

    myPrivate->imageView.setImage (w, h, goPlot::Object2DImage::RGB24);

    myPrivate->vccontrol.setTarget (myPrivate->imageView.getImage());
    myPrivate->vccontrol.capturedCaller().connect (goMemberFunction<VCGui, int> (this, &VCGui::redrawImage));

    printf ("myPrivate->imageView.getImage() size: %d %d\n", myPrivate->imageView.getImage()->getSizeX(), myPrivate->imageView.getImage()->getSizeY());

    this->show_all ();

    goPython::init ();
    goPython::run ("import golib\n");
    goPython::set ("the_image", myPrivate->imageView.getImage(), false);
}

VCGui::~VCGui ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

int VCGui::redrawImage ()
{
    // goPython::runFile ("imagescript.py");
    // myPrivate->imageView.setImage (myPrivate->imageView.getImage());
    myPrivate->imageView.queue_draw ();
    return 0;
}

int main (int argc, char* argv[])
{
    Gtk::Main m (argc, argv);
    VCGui gui;
    m.run (gui);
}
