/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_GLMATERIALINPUT_H
#define GOGUI_GLMATERIALINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/material.h>

namespace goGUI
{
    class GLMaterialInputPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input widget for \c goGL::Material objects.
     */
    class GLMaterialInput : public MultiVectorInput
    {
        public:
            GLMaterialInput ();
            virtual ~GLMaterialInput ();
            
            void set (const goGL::Material& m);
            void get (goGL::Material& m);
            int inputChangedSlotMaterial ();
            // sigc::signal<void>& signalChangedMaterial ();
            goCaller0<int>& callerChangedMaterial ();

        private:
            GLMaterialInput (const GLMaterialInput&);
            GLMaterialInput& operator= (const GLMaterialInput&);

        private:
            GLMaterialInputPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif

