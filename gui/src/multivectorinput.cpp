#include <gogui/multivectorinput.h>
#include <goexception.h>

namespace goGUI
{
    class MultiVectorInputPrivate
    {
        public:
            MultiVectorInputPrivate (const int* ns, int n) 
                : vectorInputs (n),
                  box (0),
                  changedCaller ()
                  //signalChanged ()
            {
                for (goSize_t i = 0; i < vectorInputs.getSize(); ++i)
                {
                    vectorInputs[i] = Gtk::manage (new goGUI::VectorInput (0, ns[i], 0));
                }
            };
            ~MultiVectorInputPrivate ()
            {
                //for (goSize_t i = 0; i < vectorInputs.getSize(); ++i)
                //{
                    // delete vectorInputs[i];
                //}
                vectorInputs.setSize (0);
            };

            goFixedArray<goGUI::VectorInput*> vectorInputs;

            Gtk::Box* box;

            // sigc::signal<void> signalChanged;
            goCaller0<int> changedCaller;
    };
};

/** 
 * @brief Constructor.
 * 
 * @param ns Array of integers, the \c i'th entry denotes the size of the \c i'th vector.
 * @param n Size of \c ns.
 */
goGUI::MultiVectorInput::MultiVectorInput (const int* ns, int n)
    : Gtk::Frame (),
      myPrivate (0)
{
    myPrivate = new MultiVectorInputPrivate (ns, n);

    myPrivate->box = Gtk::manage (new Gtk::VBox);

    for (goSize_t i = 0; i < myPrivate->vectorInputs.getSize(); ++i)
    {
        myPrivate->box->pack_start (*myPrivate->vectorInputs[i], Gtk::PACK_SHRINK);
        myPrivate->vectorInputs[i]->signalChanged().connect (sigc::mem_fun (*this, &MultiVectorInput::inputChangedSlot));
    }
    this->add (*myPrivate->box);
    this->show_all ();
}

goGUI::MultiVectorInput::~MultiVectorInput ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** 
 * @brief Get the \c index'th \c VectorInput
 * 
 * @param index Index of input.
 * 
 * @return Reference to a \c VectorInput
 */
goGUI::VectorInput& goGUI::MultiVectorInput::getInput (int index)
{
    if (index >= 0 && index < (int)myPrivate->vectorInputs.getSize())
    {
        return *myPrivate->vectorInputs[index];
    }
    else
    {
        goLog::error ("goGUI::MultiVectorInput::getInput(): index out of range.");
        throw goMathException (goMathException::OTHER);
    }
}

//sigc::signal<void>& goGUI::MultiVectorInput::signalChanged ()
//{
//    return myPrivate->signalChanged;
//}

/** 
 * @brief Get a caller that calls its connections every time the input gets changed.
 *
 * @note This was used in place of a sigc::signal here and in other places since 
 * in some configuration, an application using sigc crashed. It works with \c goCaller.
 * 
 * @return The goCaller object.
 */
goCaller0<int>& goGUI::MultiVectorInput::callerChanged ()
{
    return myPrivate->changedCaller;
}

void goGUI::MultiVectorInput::inputChangedSlot ()
{
    // myPrivate->signalChanged ();
    myPrivate->changedCaller ();
}
