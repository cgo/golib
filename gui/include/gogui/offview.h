#ifndef GOGUI_OFFVIEW_H
#define GOGUI_OFFVIEW_H

#include <gogl/offfile.h>
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

        virtual void glDraw ();

        void setRotation (const goVectorf& angles);
        const goVectorf& getRotation () const;

    private:
        goGL::OFFFile         off;
        int                   myList;
        goQuaternion<goFloat> myRotation; // This is not used.

        OFFViewPrivate* myPrivate;
};
}

#endif
