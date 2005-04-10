#include <godwt3d.h>
#include <gofilter1d.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>
#include <godefs.h>
#include <assert.h>

typedef goDouble godwt_t;

class goDWT3DPrivate
{
    public:
        goDWT3DPrivate ();
        ~goDWT3DPrivate ();
        
        goSignal3D<void>*     dwt;
        goSubSignal3D<void>   dwtAccess;
        int                   axes;       // or'ed GO_X, GO_Y, GO_Z
        goFilter1D            lowPass;
        goFilter1D            highPass;
        goFilter1D            lowPassReverse;
        goFilter1D            highPassReverse;
        goIndex_t             centerDWT;
        goIndex_t             centerIDWT;
};

goDWT3DPrivate::goDWT3DPrivate ()
    : dwt        (0),
      dwtAccess  (),
      axes       (GO_X|GO_Y|GO_Z),
      lowPass    (),
      highPass   (),
      lowPassReverse    (),
      highPassReverse   (),
      centerDWT  (0),
      centerIDWT (0)
{
    dwt = new goSignal3D<void> [8];
}

goDWT3DPrivate::~goDWT3DPrivate ()
{
    if (dwt)
    {
        delete [] dwt;
        dwt = NULL;
    }
}

goDWT3D::goDWT3D ()
    : goObjectBase (),
      myPrivate    (0)
{
    this->setClassName ("goDWT3D");
    myPrivate = new goDWT3DPrivate;
    assert (myPrivate);
    this->setFilter (D4);
}
        
goDWT3D::~goDWT3D ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

bool
goDWT3D::setFilter (int filterEnum)
{
    goArray<goFloat> mask;
    goIndex_t center = 0;
    goIndex_t centerReverse = 0;
    switch (filterEnum)
    {
        case HAAR:
            mask.resize(2);
            mask[0] = 1 / sqrt(2);
            mask[1] = 1 / sqrt(2);
            center = 0;
            centerReverse = 1;
            break;
        case D4:
            mask.resize(4);
            mask[0] = 0.48296291314453;
            mask[1] = 0.83651630373781;
            mask[2] = 0.22414386804201;
            mask[3] = -0.12940952255126;
            center = 2;
            centerReverse = 1;
            break;
        case D8:
            mask.resize(8);
            mask[0] = 0.23037781330890;
            mask[1] = 0.71484657055292;
            mask[2] = 0.63088076792986;
            mask[3] = -0.02798376941686;
            mask[4] = -0.18703481171909;
            mask[5] = 0.03084138183556;
            mask[6] = 0.03288301166689;
            mask[7] = -0.01059740178507;
            center = 4;
            centerReverse = 3;
            break;
        default:
            break;
    }
    goArray<goFloat> hiMask;
    goIndex_t i;
    int sign = -1;
    hiMask.resize (mask.getSize());
    goArray<goFloat> maskReverse;
    maskReverse.resize (mask.getSize());
    goArray<goFloat> hiReverse;
    hiReverse.resize (mask.getSize());
    for (i = 0; i < (goIndex_t)mask.getSize(); ++i)
    {
        hiMask[i] = sign * mask[mask.getSize() - 1 - i];
        sign *= -1;
    }
    for (i = 0; i < (goIndex_t)mask.getSize(); ++i)
    {
        hiReverse[i] = hiMask [mask.getSize() - 1 - i];
        maskReverse[i] = mask [mask.getSize() - 1 - i];
    }

    myPrivate->lowPass.setMask (mask);
    myPrivate->highPass.setMask (hiMask);
    myPrivate->lowPass.setCenter (center); 
    myPrivate->highPass.setCenter (center); 
    myPrivate->lowPassReverse.setMask (maskReverse);
    myPrivate->highPassReverse.setMask (hiReverse);
    myPrivate->lowPassReverse.setCenter (centerReverse); 
    myPrivate->highPassReverse.setCenter (centerReverse); 
    return true;
}

bool
goDWT3D::calculateDWT (goSignal3DBase<void>& sig, goTypeEnum dwtType)
{
    goLog::message("calculateDWT(): Currently only 2D.", this);
    goSignal3D<void>* dwt = myPrivate->dwt;
    goIndex_t i;
    
    for (i = 0; i < 8; ++i)
    {
        dwt[i].setDataType(dwtType);
    }
    
    goSize_t sx = sig.getSizeX();
    goSize_t sy = sig.getSizeY();
    goSize_t sz = sig.getSizeZ();
    if (myPrivate->axes & GO_X)
        sx = sx <= 1 ? sx : sx >> 1;
    if (myPrivate->axes & GO_Y)
        sy = sy <= 1 ? sy : sy >> 1;
    if (myPrivate->axes & GO_Z)
        sz = sz <= 1 ? sz : sz >> 1;


    goSignal3D<void> L;
    goSignal3D<void> H;
    L.setDataType (dwtType);
    H.setDataType (dwtType);
    downsampleFilter (sig, L, H);
    L.rotateAxes(); L.rotateAxes();
    H.rotateAxes(); H.rotateAxes();
    dwt[0].rotateAxes(); dwt[0].rotateAxes();
    dwt[1].rotateAxes(); dwt[1].rotateAxes();
    dwt[2].rotateAxes(); dwt[2].rotateAxes();
    dwt[3].rotateAxes(); dwt[3].rotateAxes();
    downsampleFilter (L, dwt[0], dwt[1]);   // LL,LH
    downsampleFilter (H, dwt[2], dwt[3]);   // HL,HH
    L.rotateAxes();
    H.rotateAxes();
    dwt[0].rotateAxes();
    dwt[1].rotateAxes();
    dwt[2].rotateAxes();
    dwt[3].rotateAxes();
    
    return true;
}

template <class T>
static void _combine2 (goSignal3DBase<void>& s1, goSignal3DBase<void>& s2, goSignal3DBase<void>& t)
{
    goSignal3DGenericIterator its1 (&s1);
    goSignal3DGenericIterator its2 (&s2);
    goSignal3DGenericIterator itt  (&t);

    while (!itt.endZ())
    {
        its1.resetY();
        its2.resetY();
        itt.resetY();
        while (!itt.endY())
        {
            its1.resetX();
            its2.resetX();
            itt.resetX();
            while (!itt.endX())
            {
                *(T*)*itt = *(T*)*its1 + *(T*)*its2;
                itt.incrementX();
                its1.incrementX();
                its2.incrementX();
            }
            itt.incrementY();
            its1.incrementY();
            its2.incrementY();
        }
        itt.incrementZ();
        its1.incrementZ();
        its2.incrementZ();
    }
}

static void _combine (goSignal3DBase<void>& s1, goSignal3DBase<void>& s2, goSignal3DBase<void>& t)
{
    switch (s1.getDataType().getID())
    {
        case GO_INT8:   _combine2<goInt8>   (s1, s2, t); break;
        case GO_UINT8:  _combine2<goUInt8>  (s1, s2, t); break;
        case GO_INT16:  _combine2<goInt16>  (s1, s2, t); break;
        case GO_UINT16: _combine2<goUInt16> (s1, s2, t); break;
        case GO_INT32:  _combine2<goInt32>  (s1, s2, t); break;
        case GO_UINT32: _combine2<goUInt32> (s1, s2, t); break;
        case GO_FLOAT:  _combine2<goFloat>  (s1, s2, t); break;
        case GO_DOUBLE: _combine2<goDouble> (s1, s2, t); break;
        default: goLog::warning("dwt3d: unknown data type."); break;
    }
}

bool
goDWT3D::upsampleFilter (goSignal3DBase<void>& L, goSignal3DBase<void>& H, goSignal3D<void>& target)
{
    if (L.getDataType().getID() != H.getDataType().getID())
    {
        goLog::warning("upsampleFilter(): input data types mismatch.",this);
        return false;
    }
    goSize_t sx = L.getSizeX() << 1;
    goSize_t sy = L.getSizeY();
    goSize_t sz = L.getSizeZ();

    target.setDataType (L.getDataType().getID());
    target.make (sx, sy, sz, 16, 16, 16, 32, 32, 32);

    goSignal3D<void> tempL;
    tempL.setDataType (L.getDataType().getID());
    tempL.make (&target);
    goSignal3D<void> tempH;
    tempH.setDataType (H.getDataType().getID());
    tempH.make (&target);
    
    goSubSignal3D<void> subsignal;
    subsignal.setSize (L.getSizeX(), L.getSizeY(), L.getSizeZ());
    subsignal.setParent (&tempL);
    subsignal.setSkip (1,0,0);
    if (!goCopySignal (&L, &subsignal))
        goLog::warning ("upsampleFilter(): copy failed.",this);
    subsignal.setPosition (1,0,0);
    goFillSignal (&subsignal, 0.0f); 
    myPrivate->lowPassReverse.filter (tempL);

    subsignal.setParent (&tempH);
    subsignal.setPosition (0,0,0);
    if (!goCopySignal (&H, &subsignal))
        goLog::warning ("upsampleFilter(): copy failed.",this);
    subsignal.setPosition (1,0,0);
    goFillSignal (&subsignal, 0.0f); 
    myPrivate->highPassReverse.filter (tempH);

    _combine (tempL, tempH, target);
    tempL.destroy();
    tempH.destroy();
    return true;
}

bool
goDWT3D::downsampleFilter (goSignal3DBase<void>& sig,
                           goSignal3D<void>& L, 
                           goSignal3D<void>& H)
                          
{
#if 0
    goSignal3D<void> temp (sig);
    int axes = myPrivate->axes;
    if (axes & GO_X)
        xFilter.filter (temp);
    temp.rotateAxes();
    temp.rotateAxes();
    if (axes & GO_Y)
        yFilter.filter (temp);
    temp.rotateAxes();
    temp.rotateAxes();
    if (axes & GO_Z)
        zFilter.filter (temp);
    temp.rotateAxes();
    temp.rotateAxes();
    goSubSignal3D<void> subsignal;
    subsignal.setSize (target.getSizeX(), target.getSizeY(), target.getSizeZ());
    subsignal.setSkip (axes & GO_X ? 1 : 0, axes & GO_Y ? 1 : 0 , axes & GO_Z ? 1 : 0);
    subsignal.setParent (&temp);
    goCopySignal (&subsignal, &target);
    return true;
#endif

    goSize_t sx = sig.getSizeX();
    sx = sx <= 1 ? sx : sx >> 1;
    goSignal3D<void> temp;
    temp = sig;
    goSubSignal3D<void> subsignal;
    L.make (sx, sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32);
    H.make (sx, sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32);
    
    myPrivate->lowPass.filter (temp);
    subsignal.setSize (L.getSizeX(), L.getSizeY(), L.getSizeZ());
    subsignal.setParent (&temp);
    subsignal.setSkip (1,0,0);
    goCopySignal (&subsignal, &L);

    temp = sig;
    myPrivate->highPass.filter (temp);
    subsignal.setSize (H.getSizeX(), H.getSizeY(), H.getSizeZ());
    subsignal.setParent (&temp);
    subsignal.setSkip (1,0,0);
    goCopySignal (&subsignal, &H);

    goString msg = "copy subsignal to H: max H = ";
    msg += (float)H.getMaximum();
    msg += " max subsignal = ";
    msg += (float)subsignal.getMaximum();
    msg += " max temp = ";
    msg += (float)temp.getMaximum();
    goLog::message (msg.toCharPtr());
   
    temp.destroy();
    return true;
}

bool
goDWT3D::dwt (goSignal3DBase<void>* sig, int axes, goTypeEnum dwtType)
{
    goLog::warning("dwt(): axes currently ignored -- only 2D!",this);
    myPrivate->axes = axes;
    return this->calculateDWT (*sig, dwtType);
}

bool
goDWT3D::dwt (goDWT3D& parentDWT, int axes)
{
    return this->dwt (&parentDWT.getDWT()[0], axes, parentDWT.getDWT()[0].getDataType().getID());
}

bool 
goDWT3D::idwt (goDWT3D& parentDWT)
{
    // We know this is a signal3d<void>
    return this->idwt (reinterpret_cast<goSignal3D<void>* > (&parentDWT.getDWT()[0]));
}

bool 
goDWT3D::idwt (goSignal3D<void>* target)
{
    if (!target)
    {
        goLog::warning("idwt(): target ist 0.", this);
        return false;
    }
    goSignal3D<void>* dwt = myPrivate->dwt;
    goSignal3D<void> tempH;
    goSignal3D<void> tempL;
    dwt[2].rotateAxes(); dwt[2].rotateAxes();
    dwt[3].rotateAxes(); dwt[3].rotateAxes();
    upsampleFilter (dwt[2], dwt[3], tempH);
    tempH.rotateAxes();
    dwt[2].rotateAxes();
    dwt[3].rotateAxes();
    
    dwt[0].rotateAxes(); dwt[0].rotateAxes();
    dwt[1].rotateAxes(); dwt[1].rotateAxes();
    upsampleFilter (dwt[0], dwt[1], tempL);
    tempL.rotateAxes();
    dwt[0].rotateAxes();
    dwt[1].rotateAxes();

    upsampleFilter (tempL, tempH, *target);
    return true;
}

goSignal3DBase<void>*
goDWT3D::getDWT ()
{
    return myPrivate->dwt;
}
