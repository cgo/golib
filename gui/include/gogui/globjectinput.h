/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_GLOBJECTINPUT_H
#define GOGUI_GLOBJECTINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/object.h>
#include <gtkmm.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goGUI
{
    class GLObjectInputPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input widget for \c goGL::Object objects.
     *
     * Provides for translation, rotation, and scale inputs.
     */
    class GLObjectInput : public Gtk::Frame
    {
        public:
            GLObjectInput ();
            virtual ~GLObjectInput ();

            void setObject (goAutoPtr<goGL::Object> o);
            goAutoPtr<goGL::Object> getObject ();

            virtual void updateInput ();

            // sigc::signal<void>& signalObjectInputChanged ();
            goCaller0<int>& callerObjectInputChanged ();

        protected:
            void set (const goGL::Object& o);
            void get (goGL::Object& o);
            int inputChangedSlotObject ();
            Gtk::Box* getBox ();

        private:
            GLObjectInput (const GLObjectInput& o);
            GLObjectInput& operator= (const GLObjectInput&);

        private:
            GLObjectInputPrivate* myPrivate;
    };

    /** 
     * @brief Helper class for \c GLObjectInput.
     */
    class GLObjectVectorInput : public MultiVectorInput
    {
        public:
            GLObjectVectorInput ();
            virtual ~GLObjectVectorInput ();
            
            void set (const goGL::Object& o);
            void get (goGL::Object& o);
            int inputChangedSlotObject ();
            // sigc::signal<void>& signalObjectInputChanged ();
            goCaller0<int>& callerObjectInputChanged ();

        private:
            GLObjectVectorInput (const GLObjectVectorInput&);
            GLObjectVectorInput& operator= (const GLObjectVectorInput&);

        private:
            // sigc::signal<void> myInputChangedSignal;
            goCaller0<int> myInputChangedCaller;
    };
/** 
 * @}
 */
};

#endif

