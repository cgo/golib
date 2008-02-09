#include <gogui/globjectinput.h>
#include <gogui/glmaterialinput.h>

namespace goGUI
{
    class GLObjectInputPrivate
    {
        public:
            GLObjectInputPrivate () 
                : objectVectorInput (),
                  // objectInputChangedSignal (),
                  objectInputChangedCaller (),
                  box (),
                  object (0)
            {};
            ~GLObjectInputPrivate () {};

            GLObjectVectorInput objectVectorInput;
            // sigc::signal<void>  objectInputChangedSignal;
            goCaller0<int> objectInputChangedCaller;

            Gtk::VBox box;

            goAutoPtr<goGL::Object> object;
    };

    static int GLObjectVectorInputV_[] = {3, 4, 3};
};

goGUI::GLObjectInput::GLObjectInput ()
    : Gtk::Frame ("Object Input"),
      myPrivate (0)
{
    myPrivate = new GLObjectInputPrivate;
    myPrivate->box.pack_start (myPrivate->objectVectorInput, Gtk::PACK_SHRINK);
    this->add (myPrivate->box);
    this->show_all ();

    // myPrivate->objectVectorInput.signalChanged().connect (sigc::mem_fun (*this, &GLObjectInput::inputChangedSlotObject));
    myPrivate->objectVectorInput.callerChanged().connect (goMemberFunction<GLObjectInput,int> (this, &GLObjectInput::inputChangedSlotObject));
}

goGUI::GLObjectInput::~GLObjectInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::GLObjectInput::set (const goGL::Object& o)
{
    myPrivate->objectVectorInput.set (o);
}

void goGUI::GLObjectInput::get (goGL::Object& o)
{
    myPrivate->objectVectorInput.get (o);
}


int goGUI::GLObjectInput::inputChangedSlotObject ()
{
    if (!myPrivate->object.isNull())
        this->get (*myPrivate->object);
    // myPrivate->objectInputChangedSignal ();
    return myPrivate->objectInputChangedCaller ();
}

//sigc::signal<void>& goGUI::GLObjectInput::signalObjectInputChanged ()
//{
//    return myPrivate->objectInputChangedSignal;
//}

goCaller0<int>& goGUI::GLObjectInput::callerObjectInputChanged ()
{
    return myPrivate->objectInputChangedCaller;
}

Gtk::Box* goGUI::GLObjectInput::getBox ()
{
    return &myPrivate->box;
}

void goGUI::GLObjectInput::setObject (goAutoPtr<goGL::Object> o)
{
    myPrivate->object = o;
    if (!o.isNull())
        this->set (*o);
}

goAutoPtr<goGL::Object> goGUI::GLObjectInput::getObject ()
{
    return myPrivate->object;
}

void goGUI::GLObjectInput::updateInput ()
{
    if (!myPrivate->object.isNull())
        this->set (*myPrivate->object);
}

// ################################################################################

goGUI::GLObjectVectorInput::GLObjectVectorInput ()
    : MultiVectorInput (GLObjectVectorInputV_, 3),
      myInputChangedCaller ()
{
    this->getInput (0).set_label ("Translation (x, y, z)");
    this->getInput (1).set_label ("Rotation (angle, x axis, y axis, z axis)");
    this->getInput (2).set_label ("Scale (x, y, z)");

    // this->signalChanged().connect (sigc::mem_fun (*this, &GLObjectVectorInput::inputChangedSlotObject));
    this->show_all ();
}

goGUI::GLObjectVectorInput::~GLObjectVectorInput ()
{
}

void goGUI::GLObjectVectorInput::set (const goGL::Object& o)
{
    this->getInput (0).setVector (o.getTranslation ());
    this->getInput (1).setVector (o.getRotation ());
    this->getInput (2).setVector (o.getScale ());
}

void goGUI::GLObjectVectorInput::get (goGL::Object& o)
{
    goVectorf v;
    this->getInput (0).getVector (v);
    o.setTranslation (v);
    this->getInput (1).getVector (v);
    o.setRotation (v);
    this->getInput (2).getVector (v);
    o.setScale (v);
}

int goGUI::GLObjectVectorInput::inputChangedSlotObject ()
{
    this->myInputChangedCaller ();
    return 0;
}

//sigc::signal<void>& goGUI::GLObjectVectorInput::signalObjectInputChanged ()
//{
//    return this->myInputChangedSignal;
//}

goCaller0<int>& goGUI::GLObjectVectorInput::callerObjectInputChanged ()
{
    return this->myInputChangedCaller;
}

