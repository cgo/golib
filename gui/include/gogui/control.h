#ifndef CONTROL_H
#define CONTROL_H
#include <gtkmm.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif

namespace goGUI
{

class ControlPrivate;

/** 
 * @brief Control frame base class.
 */
class Control : public Gtk::Frame 
{
    public:
        Control (const char* title);
        virtual ~Control ();

        virtual void warning (const char* text);
        virtual void print   (const char* text);

        //= Signals
        SigC::Signal1<void,const goString&> signal_print;

    private:
        ControlPrivate* myPrivate;
};
};

#endif
