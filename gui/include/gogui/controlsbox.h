#ifndef GOGUI_CONTROLSBOX_H
#define GOGUI_CONTROLSBOX_H

#ifndef GOGUI_CONTROL_H
# include <gogui/control.h>
#endif

namespace goGUI
{
    class ControlsBoxPrivate;

    class ControlsBox : public Control
    {
        public:
            ControlsBox (const char* title = "Controls");
            virtual ~ControlsBox ();

            void addControl (goGUI::Control& c);

        private:
            ControlsBoxPrivate* myPrivate;
    };
};

#endif
