/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/waypoint.h>

namespace goGL
{
    class WaypointPrivate
    {
        public:
            WaypointPrivate () 
                : time (0.0) 
            {};
            ~WaypointPrivate () {};

            goDouble time;
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

void goGL::Waypoint::setTime (goDouble t)
{
    myPrivate->time = t;
}

goDouble goGL::Waypoint::getTime () const
{
    return myPrivate->time;
}
