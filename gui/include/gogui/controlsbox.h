/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
