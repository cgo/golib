#ifndef GOGUI_SCENEVIEW_H
#define GOGUI_SCENEVIEW_H

#include <gogl/offfile.h>
#include <gogl/light.h>
#include <gogl/scene.h>
#include <gogl/meshobject.h>
#include <gogui/glwidget.h>

namespace goGUI
{
    class SceneViewPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Scene view widget, showing a \c goGL::Scene.
     */
class SceneView : public goGUI::GLWidget
{
    public: 
        SceneView ();
        virtual ~SceneView ();

        void loadOFF (const char* filename);

        virtual void glDraw ();

        void setRadius (goFloat);
        void setSphericalPosition (const goVectorf& phiThetaRadius);
        void setView (const goVectorf& position, const goVectorf& up, const goVectorf& focus);
        const goVectorf& getSphericalPosition () const;

        void setLight (const goGL::Light& light);
        void setScene (goAutoPtr<goGL::Scene> s);
        goAutoPtr<goGL::Scene> getScene ();

        void setActiveObject (goIndex_t i);

        sigc::signal<void>& signalChanged();
        sigc::signal<void>& signalChangedFinal();
        sigc::signal<void>& signalRotated();
        bool motionSlot (GdkEventMotion* e);
        bool buttonSlot (GdkEventButton* e);

    private:
        SceneView (const SceneView&);
        SceneView& operator= (const SceneView&);

    private:
        SceneViewPrivate* myPrivate;
};
/** 
 * @}
 */
}

#endif
