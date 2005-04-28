#ifndef GO_TIMEROBJECT_H
#define GO_TIMEROBJECT_H

#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>

/*!
 * Provides basic timing facilities.
 * Time can be taken and returned as clock ticks or seconds.
 * @note The real number of clock ticks spent between startTimer and stopTimer can differ slightly
 * from the difference between the start and stop time, since the time for returning from the
 * startTimer() member and entering the stopTimer() member are not taken into account.
 * Everything is inline, so depending on the compiler, this should not be used for VERY small
 * amounts of time.
 * @author Christian Gosch
 * @date 22.8.2001
 */
class
goTimerObject : public goObjectBase
{
 public:
    /// Starts the timer
    inline void startTimer();
    /// Stops the timer
    inline void stopTimer();
    /*!
     * @return The approximate clock ticks spent between startTimer() and stopTimer()
     */
    inline goClock_t getTimerClocks();
    /*!
     * @return The approximate time in seconds spent between startTimer() and stopTimer()
     */
    inline goDouble  getTimerSeconds();
 private:
    goClock_t timerObjectT1, timerObjectT2;
};

inline void
goTimerObject::startTimer()
{
#ifdef HAVE_TIME_H
    timerObjectT1 = clock();
#else
    timerObjectT1 = 0;
#endif
}

inline void
goTimerObject::stopTimer()
{
#ifdef HAVE_TIME_H
    timerObjectT2 = clock();
#else
    timerObjectT2 = 0;
#endif
}

inline goClock_t
goTimerObject::getTimerClocks()
{
    return timerObjectT2 - timerObjectT1;
}

inline goDouble
goTimerObject::getTimerSeconds ()
{
#ifdef HAVE_TIME_H
    return (timerObjectT2 - timerObjectT1) / (float)CLOCKS_PER_SEC;
#else
    return 0;
#endif
}

#endif
