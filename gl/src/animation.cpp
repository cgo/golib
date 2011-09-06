/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/animation.h>
#include <gofileio.h>
#include <gomath.h>

#include <assert.h>

namespace goGL
{
    class AnimationPrivate
    {
        public:
            AnimationPrivate ()
                : waypoints (),
                  positions (),
                  initialised (false),
                  accumLength (),
                  times (),
                  constantSpeedFromPosition (true),
                  autoTime (true),
                  steps (100)
            {};
            ~AnimationPrivate () {};

            goList<Waypoint> waypoints;
            goMatrixf positions; 

            bool initialised;

            goFixedArray<goDouble> accumLength;
            goFixedArray<goDouble> times;
            bool constantSpeedFromPosition; //= Use the positions to calculate frames with roughly constant speed --
                                            //= this only works when the position curve is regular, i.e. no subsequent positions
                                            //= are equal!
            bool autoTime;
            goSize_t steps;
    };
};

goGL::Animation::Animation ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new AnimationPrivate;
}

goGL::Animation::~Animation ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goGL::Animation::Animation (const Animation& o)
    : goObjectBase (), 
      myPrivate (0)
{
    myPrivate = new AnimationPrivate;
    *this = o;
}

goGL::Animation& goGL::Animation::operator= (const Animation& o)
{
    myPrivate->waypoints = o.getWaypoints ();

    return *this;
}

/** 
 * @brief 
 * @deprecated
 * 
 * @param s 
 */
void goGL::Animation::setSteps (goSize_t s)
{
    myPrivate->steps = s;
}

/** 
 * @brief 
 * @deprecated
 * 
 * @return 
 */
goSize_t goGL::Animation::getSteps () const
{
    return myPrivate->steps;
}

//goList<goGL::Waypoint>& goGL::Animation::getWaypoints ()
//{
//    return myPrivate->waypoints;
//}

/** 
 * @brief Get the list of waypoints.
 * 
 * @return The list of waypoints.
 */
const goList<goGL::Waypoint>& goGL::Animation::getWaypoints () const
{
    return myPrivate->waypoints;
}

/** 
 * @brief Add a waypoint to the end of the waypoint list.
 * 
 * @param wp Waypoint to add.
 */
void goGL::Animation::addWaypoint (const Waypoint& wp)
{
    myPrivate->waypoints.append (wp);
    myPrivate->initialised = false;
}

//goGL::Waypoint& goGL::Animation::getWaypoint (goIndex_t i)
//{
//    return myPrivate->waypoints (i)->elem;
//}

/** 
 * @brief Set a specific waypoint that already exists.
 * 
 * @param i   Index of the waypoints.
 * @param wp  Waypoint to copy the data from.
 */
void goGL::Animation::setWaypoint (goIndex_t i, const goGL::Waypoint& wp)
{
    myPrivate->waypoints(i)->elem = wp;
    myPrivate->initialised = false;
}

/** 
 * @brief Get a specific existing waypoint.
 * 
 * @param i Index of the waypoint.
 * 
 * @return The waypoint number \c i.
 */
const goGL::Waypoint& goGL::Animation::getWaypoint (goIndex_t i) const
{
    return myPrivate->waypoints (i)->elem;
}

/** 
 * @brief Prepends \c wp before waypoint \c i.
 *
 * If \c i is larger than the highest index, \c wp gets appended.
 *
 * @param i  Index
 * @param wp New waypoint
 */
void goGL::Animation::insertWaypoint (goIndex_t i, const goGL::Waypoint& wp)
{
    if (i < myPrivate->waypoints.getSize())
    {   
        goList<goGL::Waypoint>::Element* ne = new goList<goGL::Waypoint>::Element;
        ne->elem = wp;
        myPrivate->waypoints.prepend (ne, myPrivate->waypoints(i));
    }
    else
    {
       myPrivate->waypoints.append (wp);
    }
    myPrivate->initialised = false;
}

/** 
 * @brief Remove a specific waypoint.
 * 
 * @param i Index of the waypoint to remove.
 */
void goGL::Animation::removeWaypoint (goIndex_t i)
{
    myPrivate->waypoints.remove (myPrivate->waypoints(i));
    myPrivate->initialised = false;
}

/** 
 * @brief Set how to calculate the point in time for each waypoint.
 * 
 * If set to true, the time for each waypoint is calculated from its translation,
 * to yield a roughly constant speed.
 * If set to false, the time points set in the waypoints themselves are used.
 * @note No subsequent positions may be equal if this is set to true.
 * Set to false if you want to interpolate e.g. only rotations.
 * The default for this property is \c true.
 *
 * @param t Flag, true or false.
 */
void goGL::Animation::setConstantSpeedFromPosition (bool t)
{
    myPrivate->constantSpeedFromPosition = t;
}

/** 
 * @brief Get the constant speed flag.
 *
 * @see setConstantSpeedFromPosition().
 * 
 * @return The constant speed flag.
 */
bool goGL::Animation::getConstantSpeedFromPosition () const
{
    return myPrivate->constantSpeedFromPosition;
}

/** 
 * @brief Set whether to fill time points when initialising.
 * 
 * If set to \c true (default), \c initInterpolation() will fill the
 * time of each waypoint, uniformly filling the time between 0 and 1.
 *
 * If set to \c false, the times are untouched.
 *
 * @param t True of false.
 */
void goGL::Animation::setAutoTime (bool t)
{
    myPrivate->autoTime = t;
}

/** 
 * @brief Get the auto time flag.
 *
 * @seet setAutoTime()
 * 
 * @return The auto time flag.
 */
bool goGL::Animation::getAutoTime () const
{
    return myPrivate->autoTime;
}

/** 
 * @brief Write this animation to a file.
 * 
 * @param f File pointer.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::Animation::writeASCII (FILE* f) const
{
    goString s = "waypoints\n";
    bool ok = goFileIO::writeASCII (f, s);
    s = "";
    s += (int)myPrivate->waypoints.getSize();
    s += "\n";
    ok = ok && goFileIO::writeASCII (f, s);

    goList<Waypoint>::Element* el = myPrivate->waypoints.getFrontElement ();
    goSize_t i = 0;
    while (el && i < (goSize_t)myPrivate->waypoints.getSize())
    {
        ok = ok && el->elem.writeASCII (f);
        el = el->next;
        ++i;
    }

    return ok;
}

static inline bool CHECK_LINE (FILE* f, goString& s, const char* sought) {
    bool ok = goFileIO::readASCIILine (f, s);
    if (s != sought) 
    {
        goString t = "goGL::Animation::readASCII(): expected ";
        t += sought;
        t += ", got ";
        t += s;
        goLog::warning (t);
        return false;
    }
    return ok;
}

/** 
 * @brief Read animation into this object from a file.
 * 
 * @param f File pointer to read from.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::Animation::readASCII (FILE* f)
{
    myPrivate->waypoints.erase ();

    goString s;
    if (!CHECK_LINE (f, s, "waypoints"))
        return false;

    bool ok = true;
    ok = ok && goFileIO::readASCIILine (f, s);
    goSize_t N_wp = s.toInt();

    Waypoint wp;
    for (goSize_t i = 0; i < N_wp; ++i)
    {
        ok = ok && wp.readASCII (f);
        this->addWaypoint (wp);
    }
    
    myPrivate->initialised = false;

    return ok;
}

bool goGL::Animation::writeASCII (const char* fname) const
{
    FILE* f = ::fopen (fname, "w");
    if (!f) return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

bool goGL::Animation::readASCII (const char* fname)
{
    FILE* f = ::fopen (fname, "r");
    if (!f) return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}

/** 
 * @brief Initialise the interpolation.
 *
 * This gets automatically called by \c interpolate() if
 * something changed.
 */
void goGL::Animation::initInterpolation ()
{
    if (this->getWaypoints().getSize() < 1)
    {
        return;
    }

    goMatrixf& positions = myPrivate->positions;
    positions.resize (this->getWaypoints().getSize(), 3);

    goFixedArray<goDouble>& times = myPrivate->times;
    times.setSize (this->getWaypoints().getSize());

    goList<Waypoint>::Element* el = myPrivate->waypoints.getFrontElement ();
    goSize_t i = 0;
    while (el && i < positions.getRows())
    {
        positions.setRow (i, el->elem.getTranslation());
        if (this->getAutoTime())
            el->elem.setTime ((goDouble)i);
        ++i;
        el = el->next;
    }
    
    if (myPrivate->constantSpeedFromPosition)
    {
        //= FIXME: This is only used to estimate the accumLength array,
        //=        which would be the time line normally. It is only a quick hack, so fix it
        //=        by only calculating the lengths directly or by adding a "real" editable time line.
        goMatrixf resampled (0,0);
        goMath::resampleCubic<goFloat> (positions, resampled, positions.getRows(), false, &myPrivate->accumLength);

        //= Initialise points in time from the accumulated curve length at each position
        el = myPrivate->waypoints.getFrontElement();
        i = 0;
        goSize_t N = myPrivate->accumLength.getSize();
        goDouble lastL = myPrivate->accumLength[N - 1];
        if (lastL == 0.0)
        {
            goLog::warning ("goGL::Animation::initInterpolation(): lastL == 0. Not initialising.");
            return;
        }
        while (el && i < N)
        {
            el->elem.setTime (myPrivate->accumLength[i] / lastL);
            times[i] = el->elem.getTime ();
            ++i;
            el = el->next;
        }
    }
    else
    {
        //= Check if points in time are sensible and renormalise them to be between 0 and 1
        goDouble last_t = 1.0;
        {
            goList<Waypoint>::Element* e = myPrivate->waypoints.getTailElement ();
            if (e)
                last_t = e->elem.getTime ();
        }
        if (last_t == 0.0)
        {
            goLog::warning ("goGL::Animation::initInterpolation(): last_t == 0. Something is wrong with the time of the last waypoint. Not initialising.");
            return;
        }
        el = myPrivate->waypoints.getFrontElement ();
        if (el && el->elem.getTime() != 0.0)
            el->elem.setTime (0.0);
        el = el->next;
        times[0] = 0.0;
        i = 1;
        while (el)
        {
            el->elem.setTime (el->elem.getTime() / last_t); 
            if (el->elem.getTime() <= el->prev->elem.getTime())
            {
                goLog::warning ("goGL::Animation::initInterpolation(): points in time for waypoints must be increasing!");
            }
            times[i] = el->elem.getTime ();
            ++i;
            el = el->next;
        }
    }

    myPrivate->initialised = true;
}

/** 
 * @brief Interpolate a waypoint at time \f$ t \in [0,1] \f$.
 * 
 * Currently, the translation of the waypoint is interpolated using
 * cubic splines, and the rotation is interpolated by 
 * linear interpolation on the space of rotations SO3.
 *
 * @param t   Time in the interval [0,1].
 * @param ret The interpolated waypoint.
 */
void goGL::Animation::interpolate (goDouble t, Waypoint& ret)
{
    if (!myPrivate->initialised)
        this->initInterpolation ();

    //=
    //= For now, assume constant speed and interpolate resample with existing function.
    //=

    //= Total curve length
    //goDouble L = myPrivate->accumLength[myPrivate->accumLength.getSize() - 1];

    //= Length at which to interpolate
    //goDouble l = t * L;
    
    //= Find points which enclose l
    goSize_t i = 0;
    //goSize_t sz = myPrivate->accumLength.getSize();
    goSize_t sz = myPrivate->times.getSize ();
    while (i < sz && myPrivate->times[i] <= t)
    {
        ++i;
    }
    
    if (i >= sz)
    {
        i = sz - 1;
    }

    if (i < 1)
    {
        // goLog::error ("goGL::Animation::interpolate(): i < 1 but it should not.");
        return;
    }

    goVectorf pm1(0), p0(0), p1(0), p2(0);

    //= l should be between i-1 and i.

    if (i < 2)
    {
        myPrivate->positions.refRow (0, pm1);
    }
    else
    {
        myPrivate->positions.refRow (i-2, pm1);
    }
    if (i < 1)
    {
        myPrivate->positions.refRow (0, p0);
    }
    else
    {
        myPrivate->positions.refRow (i-1, p0);
    }
    myPrivate->positions.refRow (i, p1);
    if (i > myPrivate->positions.getRows() - 2)
    {
        myPrivate->positions.refRow (myPrivate->positions.getRows() - 1, p2);
    }
    else
    {
        myPrivate->positions.refRow (i+1, p2);
    }

    // goFixedArray<goDouble>& accumLength = myPrivate->accumLength;
    goFixedArray<goDouble>& times = myPrivate->times;

    //goDouble ll = accumLength[i] - accumLength[i - 1];
    goDouble ttt = times[i] - times[i - 1];

    assert (ttt != 0.0);

    //= Local t
    //goDouble tt = 1.0 - (accumLength[i] - l) / ll;
    goDouble tt = 1.0 - (times[i] - t) / ttt;
    //= Interpolate translation
    goMath::CubicSplineND<goFloat> spline (pm1, p0, p1, p2);
    goAutoPtr<goVectorf> s = spline ( tt );
    ret.setTranslation (*s);

    //= Interpolate rotation at tt between the rotation at waypoints i-1 and i.
    {
        goMath::SO3<goFloat> so3;
        goFloat _temp[] = {1.0f, 0.0f, 0.0f};
        goVectorf temp (_temp, 3, 1);
        goFloat _s0[9], _s1[9];
        goMatrixf s0 (_s0, 3, 3, 3), s1 (_s1, 3, 3, 3);
        s0.setIdentity (); s1.setIdentity ();
        goFloat _tangent[] = {1.0f, 0.0f, 0.0f};
        goVectorf tangent (_tangent, 3, 1);

        //= FIXME: This is slow. Replace; add a rotation and scale parameter matrix like for translation.
        const goVectorf& R0 = myPrivate->waypoints(i-1)->elem.getRotation ();
        const goVectorf& R1 = myPrivate->waypoints(i)->elem.getRotation ();
        printf ("R0, R1:\n");
        R0.print ();
        R1.print ();
        temp[0] = R0[1]; temp[1] = R0[2]; temp[2] = R0[3]; 
        if (temp.norm2() != 0.0f) temp *= 1.0f / temp.norm2() * R0[0] / 180.0f * M_PI;
        so3.matrix (temp, s0);

        temp[0] = R1[1]; temp[1] = R1[2]; temp[2] = R1[3]; 
        if (temp.norm2() != 0.0f) temp *= 1.0f / temp.norm2() * R1[0] / 180.0f * M_PI;
        so3.matrix (temp, s1);
        
        so3.log (s0, s1, tangent);

        printf ("s0:\n");
        s0.print ();
        printf ("s1:\n");
        s1.print ();
        printf ("s0 -- s1 at tt == %.3f\n", tt);
        so3.exp (s0, tangent * tt, s1);
        s1.print ();
        printf ("s0 -- s1 at tt == 0\n");
        so3.exp (s0, tangent * 0.0f, s1);
        s1.print ();
        printf ("s0 -- s1 at tt == 1\n");
        so3.exp (s0, tangent * 1.0f, s1);
        s1.print ();

        so3.exp (s0, tangent * tt, s1);
        so3.vector (s1, temp);
        goFloat angle = temp.norm2 ();
        if (angle != 0.0f)
        {
            temp /= angle;
        }
        ret.setRotation (angle * 180.0f / M_PI, temp[0], temp[1], temp[2]);
        printf ("Rotation:\n");
        ret.getRotation().print ();
    }
}
