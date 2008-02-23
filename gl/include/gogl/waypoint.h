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
