#ifndef GOGL_WAYPOINT_H
#define GOGL_WAYPOINT_H

#include <gogl/object.h>

namespace goGL
{
    class WaypointPrivate;

    class Waypoint : public Object
    {
        public:
            Waypoint ();
            virtual ~Waypoint ();

            Waypoint (const Waypoint& o);
            Waypoint& operator= (const Waypoint& o);

        private:
            WaypointPrivate* myPrivate;
    };
};

#endif
