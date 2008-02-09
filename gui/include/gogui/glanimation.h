#ifndef GOGUI_GLANIMATION_H
#define GOGUI_GLANIMATION_H

#include <gtkmm.h>
#include <gogl/animation.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goGUI
{
    class GLAnimationPrivate;

    class GLAnimation : public Gtk::Frame
    {
        public:
            GLAnimation ();
            virtual ~GLAnimation ();

            void setAnimation (goAutoPtr<goGL::Animation> a);
            goAutoPtr<goGL::Animation> getAnimation ();
            goAutoPtr<goGL::Waypoint> getWaypoint ();

            void addWaypoint (const goGL::Waypoint& wp);
            void removeWaypoint (int wp_index);
            void prependWaypoint (const goGL::Waypoint& wp, int wp_index);
            void appendWaypoint (const goGL::Waypoint& wp, int wp_index);

            goAutoPtr<goGL::Waypoint> getSelectedWaypoint ();

            //= Slots
            void loadAnimation ();
            void saveAnimation ();
            void waypointSelected ();
            void tValueChanged ();

            int selectedWaypoint ();

            //sigc::signal<void>&     signalPositionChanged ();
            //sigc::signal<void>&     signalWaypointSelected ();

            goCaller0<int>&         waypointSelectedCaller ();
            goCaller0<int>&         positionChangedCaller ();

        private:
            GLAnimation (const GLAnimation& o);
            GLAnimation& operator= (const GLAnimation& o);


        private:
            GLAnimationPrivate* myPrivate;
    };
};

#endif
