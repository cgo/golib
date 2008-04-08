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

    myPrivate->vccontrol.setTarget (myPrivate->imageView.getImage());
    myPrivate->vccontrol.capturedCaller().connect (goMemberFunction<VCGui, int> (this, &VCGui::redrawImage));

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
    goPython::runFile ("imagescript.py");
    myPrivate->imageView.queue_draw ();
    return 0;
}
