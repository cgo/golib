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

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input widget for \c goGL::Light objects.
     */
    class GLLightInput : public MultiVectorInput
    {
        public:
            GLLightInput ();
            virtual ~GLLightInput ();
            
//            void setLight (goAutoPtr<goGL::Light> l);
//            goAutoPtr<goGL::Light> getLight ();

            void set (const goGL::Light& light);
            void get (goGL::Light& light);
            int inputChangedSlotLight ();
            // sigc::signal<void, goGL::Light>& signalChangedLight ();
            goCaller0<int>& callerChangedLight ();

        private:
            GLLightInput (const GLLightInput&);
            GLLightInput& operator= (const GLLightInput&);

        private:
            GLLightInputPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif

