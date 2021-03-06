/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_VECTORINPUT_H
#define GOGUI_VECTORINPUT_H

#include <govector.h>
#include <gtkmm.h>

namespace goGUI
{
    class VectorInputPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input for a vector.
     *
     * For each entry in the vector, a spin button is created.
     * Each time an entry changes, \c signalChanged() is emitted.
     */
    class VectorInput : public Gtk::Frame
    {
        public:
            VectorInput (const char* title = 0, int n = 4, int direction = 0);
            virtual ~VectorInput ();

            // void setLabel  (const char* text);
            void getVector (goVectorf& v) const;
            void getVector (goVectord& v) const;
            void setVector (const goVectorf& v);
            void setVector (const goVectord& v);

            void setDigits (int d);
            void setRange (double low, double high, double step = 0.1, double large_step = 1.0);

            void valueChangedSlot ();

            void connectAll ();
            void disconnectAll ();

            // sigc::signal<void, goVectorf>& signalChangedVector ();
            sigc::signal<void>& signalChanged ();

        private:
            VectorInput (const VectorInput&);
            VectorInput& operator= (const VectorInput&);

        private:
            VectorInputPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
