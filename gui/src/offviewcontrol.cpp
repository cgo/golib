#include <gogui/offviewcontrol.h>
#include <gogui/helper.h>

namespace goGUI
{
    class OFFViewControlPrivate
    {
        public:
            OFFViewControlPrivate ()
                : loadButton ("Load OFF"),
                  alignButton ("Align"),
                  phi    (-180.0f, 181.0f, 1.0f),
                  theta  (0.0f, 181.0f, 1.0f),
                  radius (),
                  view (0),
                  vbox (),
                  tips ()
            {
                this->radius.set_range (0.0, 10000.0);
                this->radius.set_digits (1);
                this->radius.set_increments (1.0, 10.0);
                this->radius.set_value (5.0);
            };
            ~OFFViewControlPrivate ()
            {
            };

            Gtk::Button loadButton;
            Gtk::Button alignButton;
            Gtk::HScale phi;
            Gtk::HScale theta;
            Gtk::SpinButton radius;
            Gtk::VBox   vbox;
            Gtk::Tooltips tips;

            goGUI::OFFView* view;
    };
}

goGUI::OFFViewControl::OFFViewControl ()
    : goGUI::Control ("OFFView"), myPrivate (0)
{
    myPrivate = new goGUI::OFFViewControlPrivate;
    
    myPrivate->tips.enable ();

    myPrivate->vbox.pack_start (myPrivate->phi, Gtk::PACK_SHRINK);
    myPrivate->vbox.pack_start (myPrivate->theta, Gtk::PACK_SHRINK);
    myPrivate->vbox.pack_start (myPrivate->radius, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->phi, "phi angle");
    myPrivate->tips.set_tip (myPrivate->theta, "theta angle");
    myPrivate->tips.set_tip (myPrivate->radius, "view sphere radius");
    
    Gtk::HBox* buttonBox = Gtk::manage (new Gtk::HBox);
    buttonBox->pack_start (myPrivate->loadButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->loadButton, "Load OFF file");
    buttonBox->pack_start (myPrivate->alignButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->alignButton, "Align object with principal axes");
    myPrivate->vbox.pack_start (*buttonBox, Gtk::PACK_SHRINK);
    this->add (myPrivate->vbox);

    myPrivate->phi.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->theta.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->radius.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->loadButton.signal_clicked().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::loadOFF));
    myPrivate->alignButton.signal_clicked().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::align));
}

goGUI::OFFViewControl::~OFFViewControl ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::OFFViewControl::setOFFView (goGUI::OFFView* view)
{
    myPrivate->view = view;
}

void goGUI::OFFViewControl::angleChanged ()
{
    goVectorf angles (3);
    angles[0] = myPrivate->phi.get_value();
    angles[1] = myPrivate->theta.get_value();
    angles[2] = myPrivate->radius.get_value();
    if (myPrivate->view)
    {
        myPrivate->view->setRotation (angles);
        myPrivate->view->GLWidgetBegin ();
        myPrivate->view->glDraw ();
        myPrivate->view->GLWidgetEnd ();
    }
}

void goGUI::OFFViewControl::addWidget (Gtk::Widget& w)
{
    myPrivate->vbox.pack_start (w, Gtk::PACK_SHRINK);
}

void goGUI::OFFViewControl::loadOFF ()
{
    if (!myPrivate->view)
    {
        this->warning ("No view set.");
        return;
    }
    goString filename;
    static goString lastFilename;
    if (!goGUI::getFilenameOpen (filename, lastFilename, "Open"))
    {
        return;
    }
    filename.getPathName (lastFilename);
    myPrivate->phi.set_value (0.0f);
    myPrivate->theta.set_value (0.0f);
    myPrivate->radius.set_value (0.0f);
    myPrivate->view->load (filename.toCharPtr());
}

void goGUI::OFFViewControl::align ()
{
    if (!myPrivate->view)
    {
        this->warning ("No view set.");
        return;
    }
    
    myPrivate->view->align ();
}

goGUI::OFFView* goGUI::OFFViewControl::getOFFView ()
{
    return myPrivate->view;
}

void goGUI::OFFViewControl::setRotation (const goVectorf& r)
{
    if (r.getSize() < 3)
    {
        return;
    }
    myPrivate->phi.set_value (r[0]);
    myPrivate->theta.set_value (r[1]);
    myPrivate->radius.set_value (r[2]);
}

void goGUI::OFFViewControl::getRotation (goVectorf& r) const
{
    if (r.getSize() < 3)
    {
        r.resize (3);
    }
    r[0] = myPrivate->phi.get_value();
    r[1] = myPrivate->theta.get_value();
    r[2] = myPrivate->radius.get_value();
}
