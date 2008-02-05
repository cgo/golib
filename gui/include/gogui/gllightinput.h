#ifndef GOGUI_GLLIGHTINPUT_H
#define GOGUI_GLLIGHTINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/light.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goGUI
{
    class GLLightInputPrivate;

    class GLLightInput : public MultiVectorInput
    {
        public:
            GLLightInput ();
            virtual ~GLLightInput ();
            
//            void setLight (goAutoPtr<goGL::Light> l);
//            goAutoPtr<goGL::Light> getLight ();

            void set (const goGL::Light& light);
            void get (goGL::Light& light);
            void inputChangedSlotLight ();
            sigc::signal<void, goGL::Light>& signalChangedLight ();

        private:
            GLLightInputPrivate* myPrivate;
    };
};

#endif

