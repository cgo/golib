#ifndef GOGL_ANIMATION_H
#define GOGL_ANIMATION_H

#include <goobjectbase.h>
#include <gogl/waypoint.h>

namespace goGL
{
    class AnimationPrivate;

/** @addtogroup gl
 * @{
 */
    /** 
     * @brief Animation object.
     *
     * Given a set of waypoints (or "keyframes"),
     * interpolates between these points the translation and rotation
     * on a curve interpolating these waypoints.
     */
    class Animation : public goObjectBase
    {
        public:
            Animation ();
            virtual ~Animation ();

            Animation (const Animation& o);
            Animation& operator= (const Animation& o);

            void setSteps (goSize_t s);
            goSize_t getSteps () const;

            // goList<Waypoint>&       getWaypoints ();
            const goList<Waypoint>& getWaypoints () const;

            void            addWaypoint (const Waypoint& wp);
            // Waypoint&       getWaypoint (goIndex_t i);
            const Waypoint& getWaypoint (goIndex_t i) const;
            void setWaypoint (goIndex_t i, const goGL::Waypoint& wp);
            void insertWaypoint (goIndex_t i, const goGL::Waypoint& wp);
            void removeWaypoint (goIndex_t i);

            virtual bool writeASCII (FILE* f) const;
            virtual bool readASCII  (FILE* f);
            virtual bool writeASCII (const char* filename) const;
            virtual bool readASCII  (const char* filename);

            void initInterpolation ();
            void interpolate (goDouble t, Waypoint& ret);

        private:
            AnimationPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
