#include <gogui/cannycontrol.h>
#include <gtkmm.h>
#include <gosignal.h>

namespace goGUI
{
    class CannyControlPrivate
    {
        public:
            CannyControlPrivate ()
                : label_thresh1 ("High thresh "),
                  label_thresh2 ("Low thresh "),
                  spin_thresh1 (),
                  spin_thresh2 (),
                  button_run ("Run"),
                  table (),
                  edgeMap (0),
                  image (0),
                  imageCreationCaller ()
            {
                spin_thresh1.set_digits (2);
                spin_thresh2.set_digits (2);
                spin_thresh1.set_increments (1.0, 10.0);
                spin_thresh2.set_increments (1.0, 10.0);
                spin_thresh1.set_range (0.0, 65535.0);
                spin_thresh2.set_range (0.0, 65535.0);
                spin_thresh1.set_value (80.0);
                spin_thresh2.set_value (40.0);

                table.attach (label_thresh1, 0, 1, 0, 1, Gtk::SHRINK, Gtk::SHRINK);
                table.attach (label_thresh2, 0, 1, 1, 2, Gtk::SHRINK, Gtk::SHRINK);
                table.attach (spin_thresh1, 1, 2, 0, 1, Gtk::SHRINK, Gtk::SHRINK);
                table.attach (spin_thresh2, 1, 2, 1, 2, Gtk::SHRINK, Gtk::SHRINK);
                table.attach (button_run, 0, 1, 2, 3, Gtk::FILL, Gtk::FILL);
                table.set_homogeneous (false);
            }

            Gtk::Label label_thresh1;
            Gtk::Label label_thresh2;
            Gtk::SpinButton spin_thresh1;
            Gtk::SpinButton spin_thresh2;
            Gtk::Button button_run;
            Gtk::Table table;

            goAutoPtr<goSignal3DBase<void> > edgeMap;
            goAutoPtr<goSignal3DBase<void> > image;
            goCaller1<void, goAutoPtr<goSignal3DBase<void> > > imageCreationCaller;
    };
};

goGUI::CannyControl::CannyControl ()
    : Control ("Canny edge detection")
{
    myPrivate = new CannyControlPrivate;

    this->add (myPrivate->table);
    myPrivate->table.show_all_children ();
    myPrivate->button_run.signal_clicked().connect (sigc::mem_fun (*this, &CannyControl::run));
}

goGUI::CannyControl::~CannyControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::CannyControl::run ()
{
    if (myPrivate->image.isNull())
    {
        this->warning ("Image is NULL.\nNot running.");
        return;
    }

    if (myPrivate->edgeMap.isNull())
    {
        goSignal3D<void>* s = new goSignal3D<void>;
        myPrivate->edgeMap = s;
        s->setDataType (GO_UINT8);
        s->make (myPrivate->image->getSize(), myPrivate->image->getBlockSize(), goSize3D (8, 8, 0), 1);
    }
    myPrivate->edgeMap->setObjectName (myPrivate->image->getObjectName() + " Canny edge map");

    if (!goSignal::canny (*myPrivate->image, *myPrivate->edgeMap, myPrivate->spin_thresh1.get_value(), myPrivate->spin_thresh2.get_value()))
    {
        this->warning ("Canny failed.");
        return;
    }

    //= FIXME Note that egde map is only _newly_ created if it was null before --- it is the same goSignal3D
    //=       otherwise.
    myPrivate->imageCreationCaller (myPrivate->edgeMap);
}

void goGUI::CannyControl::setImage (goAutoPtr<goSignal3DBase<void> > im)
{
    myPrivate->image = im;
}

void goGUI::CannyControl::setEdgeMap (goAutoPtr<goSignal3DBase<void> > em)
{
    myPrivate->edgeMap = em;
}

goCaller1<void, goAutoPtr<goSignal3DBase<void> > >& goGUI::CannyControl::getImageCreationCaller ()
{
    return myPrivate->imageCreationCaller;
}
