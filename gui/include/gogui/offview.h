#ifndef GOGUI_OFFVIEW_H
#define GOGUI_OFFVIEW_H

#include <gogl/offfile.h>
#include <gogl/light.h>
#include <gogl/meshobject.h>
#include <gogui/glwidget.h>

namespace goGUI
{
    class OFFViewPrivate;

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

        sigc::signal<void> signalChanged();
        sigc::signal<void> signalChangedFinal();
        sigc::signal<void> signalRotated();
        bool motionSlot (GdkEventMotion* e);
        bool buttonSlot (GdkEventButton* e);

    private:
        goGL::OFFFile         off;
        int                   myList;

        OFFViewPrivate* myPrivate;
};
}

#endif
