#include <gogui/vectorinput.h>
#include <gofixedarray.h>
#include <gomath.h>

#include <limits>

namespace goGUI
{
    class VectorInputPrivate
    {
        public:
            VectorInputPrivate (int n, Gtk::Box& mother)
                : spinButtons (n),
                  connections (n),
                  signalChangedVector (),
                  signalChanged (),
                  frame (0),
                  box (0)
            {
                for (goSize_t i = 0; i < spinButtons.getSize(); ++i)
                {
                    spinButtons[i].set_digits (3);
                    spinButtons[i].set_increments (0.01, 0.1);
                    spinButtons[i].set_range (-std::numeric_limits<double>::max(), std::numeric_limits<double>::max());
                    spinButtons[i].set_value (0.0);
                    mother.pack_start (spinButtons[i], Gtk::PACK_SHRINK);
                }
            };
            ~VectorInputPrivate () {};

            goFixedArray<Gtk::SpinButton> spinButtons;
            goFixedArray<sigc::connection> connections;

            sigc::signal<void, goVectorf> signalChangedVector;
            sigc::signal<void>            signalChanged;

            Gtk::Frame* frame;
            Gtk::Box* box;
    };
};

goGUI::VectorInput::VectorInput (const char* title, int n, int direction)
    : Gtk::Frame (),
      myPrivate (0)
{
    Gtk::Box* box = 0;
    if (direction == 0)
    {
        box = Gtk::manage (new Gtk::HBox);
    }
    else
    {
        box = Gtk::manage (new Gtk::VBox);
    }

    myPrivate = new VectorInputPrivate (n, *box);
    myPrivate->box = box;
    if (title)
    {
        this->set_label (title);
    }
    this->add (*box);

    this->connectAll ();

    this->show_all();
}

void goGUI::VectorInput::connectAll ()
{
    for (goSize_t i = 0; i < myPrivate->spinButtons.getSize(); ++i)
    {
        if (myPrivate->connections[i].connected())
        {
            myPrivate->connections[i].disconnect();
        }
        myPrivate->connections[i] = myPrivate->spinButtons[i].signal_value_changed().connect (sigc::mem_fun (*this, &VectorInput::valueChangedSlot));
    }
}

void goGUI::VectorInput::disconnectAll ()
{
    for (goSize_t i = 0; i < myPrivate->spinButtons.getSize(); ++i)
    {
        if (myPrivate->connections[i].connected())
        {
            myPrivate->connections[i].disconnect();
        }
    }
}

void goGUI::VectorInput::valueChangedSlot ()
{
    // goVectorf v(0);
    // this->getVector (v);
    // myPrivate->signalChangedVector (v);
    myPrivate->signalChanged ();
}

goGUI::VectorInput::~VectorInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

//sigc::signal<void, goVectorf>& goGUI::VectorInput::signalChangedVector ()
//{
//    return myPrivate->signalChangedVector;
//}

sigc::signal<void>& goGUI::VectorInput::signalChanged ()
{
    return myPrivate->signalChanged;
}

//void goGUI::VectorInput::setLabel (const char* text)
//{
//    if (!myPrivate->frame)
//    {
//        myPrivate->frame = Gtk::manage (new Gtk::Frame);
//        myPrivate->frame->add (*myPrivate->box);
//        this->add (*myPrivate->frame);
//    }
//    myPrivate->frame->set_label (text);
//}

void goGUI::VectorInput::getVector (goVectorf& v) const
{
    goSize_t sz = myPrivate->spinButtons.getSize();
    if (v.getSize() != sz)
    {
        v.resize (sz);
    }
    for (goSize_t i = 0; i < sz; ++i)
    {
        v[i] = myPrivate->spinButtons[i].get_value ();
    }
}

void goGUI::VectorInput::getVector (goVectord& v) const
{
    goSize_t sz = myPrivate->spinButtons.getSize();
    if (v.getSize() != sz)
    {
        v.resize (sz);
    }
    for (goSize_t i = 0; i < sz; ++i)
    {
        v[i] = myPrivate->spinButtons[i].get_value ();
    }
}

void goGUI::VectorInput::setVector (const goVectorf& v)
{
    this->disconnectAll ();
    goSize_t sz = goMath::min (myPrivate->spinButtons.getSize(), v.getSize());
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->spinButtons[i].set_value (v[i]);
        myPrivate->spinButtons[i].update ();
    }
    this->connectAll ();
}

void goGUI::VectorInput::setVector (const goVectord& v)
{
    this->disconnectAll ();
    goSize_t sz = goMath::min (myPrivate->spinButtons.getSize(), v.getSize());
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->spinButtons[i].set_value (v[i]);
        myPrivate->spinButtons[i].update ();
    }
    this->connectAll ();
}
