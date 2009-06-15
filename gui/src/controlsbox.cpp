#include <gogui/controlsbox.h>

namespace goGUI
{
    class ControlsBoxPrivate
    {
        public:
            ControlsBoxPrivate ()
                : myNotebook ()
            { }
            ~ControlsBoxPrivate () { }

            Gtk::Notebook myNotebook;
    };
};

goGUI::ControlsBox::ControlsBox (const char* title)
    : Control (title),
      myPrivate (0)
{
    myPrivate = new ControlsBoxPrivate;
    this->add (myPrivate->myNotebook);
}

goGUI::ControlsBox::~ControlsBox ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::ControlsBox::addControl (goGUI::Control& c)
{
    myPrivate->myNotebook.append_page (c, c.get_label (), "Menu label!");
}
