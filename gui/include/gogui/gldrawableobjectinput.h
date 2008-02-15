#ifndef GOGUI_GLDRAWABLEOBJECTINPUT_H
#define GOGUI_GLDRAWABLEOBJECTINPUT_H

#include <gogui/globjectinput.h>
#include <gogl/object.h>
#include <gogl/drawableobject.h>
#include <gtkmm.h>

namespace goGUI
{
    class GLDrawableObjectInputPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Input object for \c goGL::DrawableObject objects.
     */
    class GLDrawableObjectInput : public GLObjectInput
    {
        public:
            GLDrawableObjectInput ();
            virtual ~GLDrawableObjectInput ();
           
            void setDrawableObject (goAutoPtr<goGL::DrawableObject> o);
            goAutoPtr<goGL::DrawableObject> getDrawableObject ();

            virtual void updateInput ();

            // sigc::signal<void>& signalDrawableObjectInputChanged ();
            goCaller0<int>& callerDrawableObjectInputChanged ();

        protected:
            void setDrawable (const goGL::DrawableObject& o);
            void getDrawable (goGL::DrawableObject& o);
            int inputChangedSlotDrawableObject ();

        private:
            GLDrawableObjectInput (const GLDrawableObjectInput&);
            GLDrawableObjectInput& operator= (const GLDrawableObjectInput&);

        private:
            GLDrawableObjectInputPrivate* myPrivate;
    };
/** 
 * @}
 */

};

#endif

