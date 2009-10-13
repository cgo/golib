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

/** 
 * @brief Constructor.
 * 
 * @param title Title shown in the frame around the input (default: 0)
 * @param n Number of elements in the vector (default: 4)
 * @param direction If 0, elements are laid out horizontally (default), otherwise they are laid out vertically.
 */
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

/** 
 * @brief Connects all input elements to the valueChangedSlot() method.
 */
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

/** 
 * @brief Disconnects all input elements so no signals are sent on change.
 */
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

/** 
 * @brief Set the number of digits shown.
 * 
 * @param d Number of digits shown in the input elements.
 */
void goGUI::VectorInput::setDigits (int d)
{
    goSize_t sz = myPrivate->spinButtons.getSize ();
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->spinButtons[i].set_digits (d);
    }
}

/** 
 * @brief Set the value range.
 *
 * All values are for all the input elements.
 * If setRange is not called, the minimal and maximal values
 * of the double type are used, and 0.01 and 0.1 are used as steps.
 *
 * @param low   Minimal value
 * @param high  Maximal value
 * @param step  Step
 * @param large_step  Page step 
 */
void goGUI::VectorInput::setRange (double low, double high, double step, double large_step)
{
    goSize_t sz = myPrivate->spinButtons.getSize ();
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->spinButtons[i].set_range (low, high);
        myPrivate->spinButtons[i].set_increments (step, large_step);
    }
}

/** 
 * @brief Called when a value changed.
 *
 * Emits the signalChanged() signal.
 */
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

/** 
 * @brief Emitted each time an input element changes.
 * 
 * @return 
 */
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

/** 
 * @brief Get the vector.
 * 
 * @param v On return, contains the vector elements. If its size does
 * not match the number of input elements, v will be resized.
 */
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

/** 
 * @brief Get the vector.
 * 
 * @param v On return, contains the vector elements. If its size does
 * not match the number of input elements, v will be resized.
 */
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

/** 
 * @brief Set the vector values.
 * 
 * @param v The vector. If v.getSize() is larger than the number of input elements,
 * only the values of v up to the number of input elements are used.
 */
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

/** 
 * @brief Set the vector values.
 * 
 * @param v The vector. If v.getSize() is larger than the number of input elements,
 * only the values of v up to the number of input elements are used.
 */
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
