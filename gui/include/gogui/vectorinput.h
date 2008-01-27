#ifndef GOGUI_VECTORINPUT_H
#define GOGUI_VECTORINPUT_H

#include <govector.h>
#include <gtkmm.h>

namespace goGUI
{
    class VectorInputPrivate;

    class VectorInput : public Gtk::Frame
    {
        public:
            VectorInput (const char* title = 0, int n = 4, int direction = 0);
            virtual ~VectorInput ();

            // void setLabel  (const char* text);
            void getVector (goVectorf& v) const;
            void getVector (goVectord& v) const;
            void setVector (const goVectorf& v);
            void setVector (const goVectord& v);

            void valueChangedSlot ();

            void connectAll ();
            void disconnectAll ();

            sigc::signal<void, goVectorf>& signalChangedVector ();
            sigc::signal<void>& signalChanged ();

        private:
            VectorInputPrivate* myPrivate;
    };
};

#endif
