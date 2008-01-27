#ifndef GOGUI_GLLIGHTINPUT_H
#define GOGUI_GLLIGHTINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/light.h>

namespace goGUI
{
    class GLLightInputPrivate;

    class GLLightInput : public MultiVectorInput
    {
        public:
            GLLightInput ();
            virtual ~GLLightInput ();
            
            void set (const goGL::Light& light);
            void get (goGL::Light& light);
            void inputChangedSlotLight ();
            sigc::signal<void, goGL::Light>& signalChangedLight ();

        private:
            GLLightInputPrivate* myPrivate;
    };
};

#endif

