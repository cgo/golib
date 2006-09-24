#ifndef GOGUI_COUNTFILENAMES_H
#define GOGUI_COUNTFILENAMES_H

#include <gtkmm.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

namespace goGUI
{

    /** 
     * @brief Widget that lets the user enter a base file name
     * and an extension and provides for a spin button to enter
     * numbers. The numbers are appended to the base file name and the whole
     * file name can be retrieved with getFilename().
     * increment() increments the number in the spin button.
     */
    class CountFilenames : public Gtk::HBox
    {
        public:
            CountFilenames ();
            virtual ~CountFilenames ();

            void setBase   (const goString& base);
            void setCount  (goIndex_t count);
            void setSuffix (const goString& suffix);

            void increment ();

            void getFilename (goString& fRet) const;
            
        protected:
            Gtk::Entry      myBaseEntry;
            Gtk::SpinButton myCount;
            Gtk::Entry      mySuffixEntry;
    };

};
#endif
