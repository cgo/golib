#ifndef GOGUI_OFFVIEW_H
#define GOGUI_OFFVIEW_H

#include <gogl/offfile.h>
#include <gogl/light.h>
#include <gogl/meshobject.h>
#include <gogui/glwidget.h>

#include <gofunctor.h>

namespace goGUI
{
    class OFFViewPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief View widget for OFF files (OpenGL).
     *
     * This is somewhat deprecated but still used; if
     * making something new, use \c SceneView instead.
     * This will not be removed in the near future, just do not use
     * it in new code.
     */
class OFFView : public goGUI::GLWidget
{
    public: 
        OFFView ();
        virtual ~OFFView ();

        void lighting ();
        void load (const char* filename);
        void align ();

        void setLight (const goGL::Light& light);
        goGL::Light& getLight ();

        virtual void glDraw ();

        void setRadius (goFloat);
        void setSphericalPosition (const goVectorf& phiThetaRadius);
        void setView (const goVectorf& position, const goVectorf& up, const goVectorf& focus);
        const goVectorf& getSphericalPosition () const;

        goGL::OFFFile& getOFFFile ();
        // goGL::MeshObject& getMesh ();

        sigc::signal<void>& signalChanged();
        sigc::signal<void>& signalChangedFinal();
        // sigc::signal<void>& signalRotated();
        goCaller0<int>&     callerRotated();
        bool motionSlot (GdkEventMotion* e);
        bool buttonSlot (GdkEventButton* e);

    private:
        OFFView (const OFFView&);
        OFFView& operator= (const OFFView&);

    private:
        goGL::OFFFile         off;
        int                   myList;

        OFFViewPrivate* myPrivate;
};
/** 
 * @}
 */
}

#endif
