/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_CONTROL_H
#define GOGUI_CONTROL_H
#include <gtkmm.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif

namespace goGUI
{

class ControlPrivate;

/** @addtogroup gui
 * @{
 */
/** 
 * @brief Control frame base class.
 * This is a frame that can be used as a base for all sorts of 
 * control widgets, like "image control", "process control", ...
 */
class Control : public Gtk::Frame 
{
    public:
        Control (const char* title);
        virtual ~Control ();

        virtual void warning (const char* text);
        virtual void message (const char* text);
        virtual void print   (const char* text);

        //= Signals
        // SigC::Signal1<void,const goString&> signal_print;

    private:
        Control (const Control&);
        Control& operator= (const Control&);

    private:
        ControlPrivate* myPrivate;
};
/** 
 * @}
 */
};

#endif
