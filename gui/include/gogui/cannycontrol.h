#ifndef GOGUI_CANNYCONTROL_H
#define GOGUI_CANNYCONTROL_H

#ifndef GOGUI_CONTROL_H
# include <gogui/control.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goGUI
{
    class CannyControlPrivate;

    class CannyControl : public Control
    {
        public:
            CannyControl ();
            virtual ~CannyControl ();

            void run ();

            void setImage   (goAutoPtr<goSignal3DBase<void> > im);
            void setEdgeMap (goAutoPtr<goSignal3DBase<void> > em);

            goCaller1<void, goAutoPtr<goSignal3DBase<void> > >& getImageCreationCaller ();

        private:
            CannyControlPrivate* myPrivate;

    };
};

#endif
