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
                  angleX (0.0f, 360.0f, 1.0f),
                  angleY (0.0f, 360.0f, 1.0f),
                  angleZ (0.0f, 360.0f, 1.0f),
                  view (0),
                  vbox (),
                  tips ()
            {
            };
            ~OFFViewControlPrivate ()
            {
            };

            Gtk::Button loadButton;
            Gtk::Button alignButton;
            Gtk::HScale angleX;
            Gtk::HScale angleY;
            Gtk::HScale angleZ;
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

    myPrivate->vbox.pack_start (myPrivate->angleX, Gtk::PACK_SHRINK);
    myPrivate->vbox.pack_start (myPrivate->angleY, Gtk::PACK_SHRINK);
    myPrivate->vbox.pack_start (myPrivate->angleZ, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->angleX, "X angle");
    myPrivate->tips.set_tip (myPrivate->angleY, "Y angle");
    myPrivate->tips.set_tip (myPrivate->angleZ, "Z angle");
    
    Gtk::HBox* buttonBox = Gtk::manage (new Gtk::HBox);
    buttonBox->pack_start (myPrivate->loadButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->loadButton, "Load OFF file");
    buttonBox->pack_start (myPrivate->alignButton, Gtk::PACK_SHRINK);
    myPrivate->tips.set_tip (myPrivate->alignButton, "Align object with principal axes");
    myPrivate->vbox.pack_start (*buttonBox, Gtk::PACK_SHRINK);
    this->add (myPrivate->vbox);

    myPrivate->angleX.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->angleY.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
    myPrivate->angleZ.signal_value_changed().connect (sigc::mem_fun (*this, &goGUI::OFFViewControl::angleChanged));
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
    angles[0] = myPrivate->angleX.get_value();
    angles[1] = myPrivate->angleY.get_value();
    angles[2] = myPrivate->angleZ.get_value();
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
    myPrivate->angleX.set_value (0.0f);
    myPrivate->angleY.set_value (0.0f);
    myPrivate->angleZ.set_value (0.0f);
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
    myPrivate->angleX.set_value (r[0]);
    myPrivate->angleY.set_value (r[1]);
    myPrivate->angleZ.set_value (r[2]);
}

void goGUI::OFFViewControl::getRotation (goVectorf& r) const
{
    if (r.getSize() < 3)
    {
        r.resize (3);
    }
    r[0] = myPrivate->angleX.get_value();
    r[1] = myPrivate->angleY.get_value();
    r[2] = myPrivate->angleZ.get_value();
}
