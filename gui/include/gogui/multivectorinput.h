/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_MULTIVECTORINPUT_H
#define GOGUI_MULTIVECTORINPUT_H

#include <gtkmm.h>
#include <gogui/vectorinput.h>
#include <gofixedarray.h>
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goGUI
{
    class MultiVectorInputPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input for multiple vectors.
     *
     * Contains several \c goGUI::VectorInput inputs.
     */
    class MultiVectorInput : public Gtk::Frame
    {
        public:
            MultiVectorInput (const int* ns, int n);
            virtual ~MultiVectorInput ();

            goGUI::VectorInput& getInput (int index);
            void inputChangedSlot ();

            // sigc::signal<void>& signalChanged ();
            goCaller0<int>& callerChanged();

        private:
            MultiVectorInput (const MultiVectorInput&);
            MultiVectorInput& operator= (const MultiVectorInput&);

        private:
            MultiVectorInputPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
