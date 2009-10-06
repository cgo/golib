#ifndef GAUSSIMAGECONTROL_H
#define GAUSSIMAGECONTROL_H


#include <gosignal3dbase.h>
#include <goautoptr.h>
#include <gogui/control.h>

class GaussImageControlPrivate;

class GaussImageControl : public goGUI::Control
{
    public:
        GaussImageControl ();
        virtual ~GaussImageControl ();

        void update (goAutoPtr<goSignal3DBase<void> >  image);

    private:
        GaussImageControlPrivate *myPrivate;
};

#endif
