#include <gogl/animation.h>
#include <gofileio.h>
#include <gomath.h>

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
                  steps (100)
            {};
            ~AnimationPrivate () {};

            goList<Waypoint> waypoints;
            goMatrixf positions; 

            bool initialised;

            goFixedArray<goDouble> accumLength;

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

void goGL::Animation::setSteps (goSize_t s)
{
    myPrivate->steps = s;
}

goSize_t goGL::Animation::getSteps () const
{
    return myPrivate->steps;
}

//goList<goGL::Waypoint>& goGL::Animation::getWaypoints ()
//{
//    return myPrivate->waypoints;
//}

const goList<goGL::Waypoint>& goGL::Animation::getWaypoints () const
{
    return myPrivate->waypoints;
}

void goGL::Animation::addWaypoint (const Waypoint& wp)
{
    myPrivate->waypoints.append (wp);
    myPrivate->initialised = false;
}

//goGL::Waypoint& goGL::Animation::getWaypoint (goIndex_t i)
//{
//    return myPrivate->waypoints (i)->elem;
//}

void goGL::Animation::setWaypoint (goIndex_t i, const goGL::Waypoint& wp)
{
    myPrivate->waypoints(i)->elem = wp;
    myPrivate->initialised = false;
}

const goGL::Waypoint& goGL::Animation::getWaypoint (goIndex_t i) const
{
    return myPrivate->waypoints (i)->elem;
}

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

void goGL::Animation::removeWaypoint (goIndex_t i)
{
    myPrivate->waypoints.remove (myPrivate->waypoints(i));
    myPrivate->initialised = false;
}

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

void goGL::Animation::initInterpolation ()
{
    if (this->getWaypoints().getSize() < 1)
    {
        return;
    }

    goSize_t steps = myPrivate->steps;
    goMatrixf& positions = myPrivate->positions;
    positions.resize (this->getWaypoints().getSize(), 3);

    goList<Waypoint>::Element* el = myPrivate->waypoints.getFrontElement ();
    goSize_t i = 0;
    while (el && i < positions.getRows())
    {
        positions.setRow (i, el->elem.getTranslation());
        ++i;
        el = el->next;
    }
    
    goMatrixf resampled (0,0);

    goMath::resampleCubic<goFloat> (positions, resampled, steps, false, &myPrivate->accumLength);

    myPrivate->initialised = true;
}

void goGL::Animation::interpolate (goDouble t, Waypoint& ret)
{
    if (!myPrivate->initialised)
        this->initInterpolation ();

    //=
    //= For now, assume constant speed and interpolate resample with existing function.
    //=

    //= Total curve length
    goDouble L = myPrivate->accumLength[myPrivate->accumLength.getSize() - 1];

    //= Length at which to interpolate
    goDouble l = t * L;
    
    //= Find points which enclose l
    goSize_t i = 0;
    goSize_t sz = myPrivate->accumLength.getSize();
    while (i < sz && myPrivate->accumLength[i] <= l)
    {
        ++i;
    }
    
    if (i < 1)
    {
        goLog::error ("goGL::Animation::interpolate(): i < 1 but it should not.");
        return;
    }
    if (i >= sz)
    {
        i = sz - 1;
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

    goFixedArray<goDouble>& accumLength = myPrivate->accumLength;

    goDouble ll = accumLength[i] - accumLength[i - 1];
    
    //= Local t
    goDouble tt = 1.0 - (accumLength[i] - l) / ll;
    //= Interpolate translation
    goMath::CubicSplineND<goFloat> spline (pm1, p0, p1, p2);
    goAutoPtr<goVectorf> s = spline ( tt );
    ret.setTranslation (*s);

    //= Interpolate rotation at tt between the rotation at waypoints i-1 and i.
    //...
}
