#ifndef GOGUI_GLMATERIALINPUT_H
#define GOGUI_GLMATERIALINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/material.h>

namespace goGUI
{
    class GLMaterialInputPrivate;

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
};

#endif

