#ifndef GOGUI_MULTIVECTORINPUT_H
#define GOGUI_MULTIVECTORINPUT_H

#include <gtkmm.h>
#include <gogui/vectorinput.h>
#include <gofixedarray.h>
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goGUI
{
    class MultiVectorInputPrivate;

    class MultiVectorInput : public Gtk::Frame
    {
        public:
            MultiVectorInput (const int* ns, int n);
            virtual ~MultiVectorInput ();

            goGUI::VectorInput& getInput (int index);
            void inputChangedSlot ();

            // sigc::signal<void>& signalChanged ();
            goCaller0<int>& callerChanged();

        private:
            MultiVectorInput (const MultiVectorInput&);
            MultiVectorInput& operator= (const MultiVectorInput&);

        private:
            MultiVectorInputPrivate* myPrivate;
    };
};

#endif
