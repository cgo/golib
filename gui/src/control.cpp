#include <gogui/control.h>

namespace goGUI
{

class ControlPrivate
{
    public:
        ControlPrivate () 
        {};
        ~ControlPrivate () {};
};


//===========================================================

Control::Control (const char* title)
    : Gtk::Frame ()
{
    myPrivate = new ControlPrivate;

    this->set_label (title);
    this->set_border_width (4);
}

Control::~Control ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void Control::warning (const char* text)
{
#ifdef HAVE_GTK_2
    Gtk::MessageDialog msg (text, Gtk::MESSAGE_WARNING, Gtk::BUTTONS_OK);
#elif defined HAVE_GTK_2_4
    Gtk::MessageDialog msg (text, false, Gtk::MESSAGE_WARNING, Gtk::BUTTONS_OK);
#endif
    msg.run ();
}

void Control::message (const char* text)
{
#ifdef HAVE_GTK_2
    Gtk::MessageDialog msg (text, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK);
#elif defined HAVE_GTK_2_4
    Gtk::MessageDialog msg (text, false, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK);
#endif
    msg.run ();
}

void Control::print (const char* text)
{
    // this->signal_print (goString(text));
}

};
