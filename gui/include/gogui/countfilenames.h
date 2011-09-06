/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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

/** @addtogroup gui
 * @{
 */
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
            void getBase (goString& ret) const;
            void getSuffix (goString& ret) const;
            goIndex_t getCount () const;
            
        protected:
            Gtk::Entry      myBaseEntry;
            Gtk::SpinButton myCount;
            Gtk::Entry      mySuffixEntry;
    };
/** 
 * @}
 */

};
#endif
