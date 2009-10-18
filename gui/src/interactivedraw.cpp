#include <gogui/interactivedraw.h>
#include <iostream>

namespace goGUI
{
    class InteractiveDrawPrivate
    {
        public:
            InteractiveDrawPrivate ()
                : buttonPressConnection (),
                  buttonReleaseConnection (),
                  mode (InteractiveDraw::NONE)
            {
            }

            bool buttonPressed (GdkEventButton* event)
            {
                std::cout << "Button pressed\n";
                std::cout << event->x << ", " << event->y << "\n";
                return true;
            }

            bool buttonReleased (GdkEventButton* event)
            {
                std::cout << "Button released\n";
                std::cout << event->x << ", " << event->y << "\n";
                return true;
            }


            sigc::connection buttonPressConnection;
            sigc::connection buttonReleaseConnection;

            int mode;
    };
};


goGUI::InteractiveDraw::InteractiveDraw ()
    : myPrivate (0)
{
    myPrivate = new InteractiveDrawPrivate;
}

goGUI::InteractiveDraw::~InteractiveDraw ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::InteractiveDraw::setDrawWidget (Gtk::Widget* widget)
{
    if (myPrivate->buttonPressConnection.connected())
    {
        myPrivate->buttonPressConnection.disconnect ();
    }

    if (myPrivate->buttonReleaseConnection.connected())
    {
        myPrivate->buttonReleaseConnection.disconnect ();
    }

    if (!widget)
    {
        return;
    }

    myPrivate->buttonPressConnection = widget->signal_button_press_event().connect (sigc::mem_fun (*myPrivate, &InteractiveDrawPrivate::buttonPressed));
    myPrivate->buttonReleaseConnection = widget->signal_button_release_event().connect (sigc::mem_fun (*myPrivate, &InteractiveDrawPrivate::buttonReleased));
}
