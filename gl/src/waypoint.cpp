#include <gogl/waypoint.h>

namespace goGL
{
    class WaypointPrivate
    {
        public:
            WaypointPrivate () {};
            ~WaypointPrivate () {};
    };
};

goGL::Waypoint::Waypoint ()
    : Object (),
      myPrivate (0)
{
    myPrivate = new WaypointPrivate;
}

goGL::Waypoint::~Waypoint ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goGL::Waypoint::Waypoint (const Waypoint& o)
    : Object (),
      myPrivate (0)
{
    myPrivate = new WaypointPrivate;
    *this = o;
}

goGL::Waypoint& goGL::Waypoint::operator= (const Waypoint& o)
{
    *(Object*)this = *(Object*)(&o);
    return *this;
}
