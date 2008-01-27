#ifndef GOGUI_MULTIVECTORINPUT_H
#define GOGUI_MULTIVECTORINPUT_H

#include <gtkmm.h>
#include <gogui/vectorinput.h>
#include <gofixedarray.h>

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

            sigc::signal<void>& signalChanged ();

        private:
            MultiVectorInputPrivate* myPrivate;
    };
};

#endif
