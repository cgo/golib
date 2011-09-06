/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGL_WAYPOINT_H
#define GOGL_WAYPOINT_H

#include <gogl/object.h>

namespace goGL
{
    class WaypointPrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Waypoint to \c goGL::Animation objects.
     */
    class Waypoint : public Object
    {
        public:
            Waypoint ();
            virtual ~Waypoint ();

            void     setTime (goDouble t);
            goDouble getTime () const;

            Waypoint (const Waypoint& o);
            Waypoint& operator= (const Waypoint& o);

        private:
            WaypointPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
