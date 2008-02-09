#include <gogui/multivectorinput.h>

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

goGUI::VectorInput& goGUI::MultiVectorInput::getInput (int index)
{
    if (index >= 0 && index < (int)myPrivate->vectorInputs.getSize())
    {
        return *myPrivate->vectorInputs[index];
    }
    else
    {
        goLog::error ("goGUI::MultiVectorInput::getInput(): index out of range.");
    }
}

//sigc::signal<void>& goGUI::MultiVectorInput::signalChanged ()
//{
//    return myPrivate->signalChanged;
//}

goCaller0<int>& goGUI::MultiVectorInput::callerChanged ()
{
    return myPrivate->changedCaller;
}

void goGUI::MultiVectorInput::inputChangedSlot ()
{
    // myPrivate->signalChanged ();
    myPrivate->changedCaller ();
}
