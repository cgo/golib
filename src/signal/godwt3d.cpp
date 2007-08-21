#include <godwt3d.h>
#include <gofilter1d.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>
#include <godefs.h>
#include <gofixedarray.h>
#include <goautoptr.h>
#include <assert.h>

typedef goDouble godwt_t;

static void _scaleValues (goSignal3DBase<void>& sig, goDouble value);
template <class T>
static void _scaleValues2 (goSignal3DBase<void>& sig, goDouble value);

class goDWT3DPrivate
{
    public:
        goDWT3DPrivate ();
        ~goDWT3DPrivate ();
        
        goFixedArray<goAutoPtr<goSignal3D<void> > >  dwt;
        goSubSignal3D<void>   dwtAccess;
        int                   axes;       // or'ed GO_X, GO_Y, GO_Z
        goFilter1D            lowPass;
        goFilter1D            highPass;
        goFilter1D            lowPassReverse;
        goFilter1D            highPassReverse;
        goIndex_t             centerDWT;
        goIndex_t             centerIDWT;
        bool                  frameMode;
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
      centerIDWT (0),
      frameMode (false)
{
    dwt.setSize (4);
    for (goSize_t i = 0; i < 4; ++i)
    {
        dwt[i] = goAutoPtr<goSignal3D<void> > (new goSignal3D<void>);
    }
}

goDWT3DPrivate::~goDWT3DPrivate ()
{
}

goDWT3D::goDWT3D ()
    : goObjectBase (),
      myPrivate    (0)
{
    this->setClassID(GO_DWT3D);
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

/* 
 * @brief For upsampling the filter masks when creating wavelet frames
 * 
 * @param mask Mask (will be replaced)
 * @param center Center (will be replaced)
 * 
 * @see NOTE in the source code.
 *
 * @return True if successful, false otherwise.
 */
static inline bool insertZeroTaps (goArray<goFloat>& mask, int &center)
{
    goSize_t sz = mask.getSize();
    goSize_t new_sz = sz + sz - 1;
    int new_center = 0;
    goArray<goFloat> new_mask (new_sz);
    goSize_t zero_counter = 0;
    goSize_t j = 0;
    for (goSize_t i = 0; i < sz;)
    {
        if (zero_counter == 0)
        {
            zero_counter = 1;
            new_mask[j] = mask[i];
            if (i == (goSize_t)center)
                new_center = j;
            ++i;
            ++j;
        }
        else
        {
            new_mask[j] = 0.0f;
            ++j;
            --zero_counter;
        }
    }
    //= NOTE: This is not necessarily _correct_; it is done to provide non-translated filter outputs
    //=       for the level set experiment.
    //if (new_sz & 1)
    //new_center = new_sz / 2;

    mask = new_mask;

//    for (goSize_t i = 0; i < mask.getSize(); ++i)
//    {
//        printf ("%f ", mask[i]);
//    }
//    printf ("\n");
//    printf ("New center: %d\n", new_center);
   
    center = new_center;
    return true;
}

bool
goDWT3D::setFilter (int filterEnum, int upsample)
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

    if (myPrivate->frameMode)
    {
        for (int i = 0; i < upsample; ++i)
        {
            insertZeroTaps (mask, center);
            insertZeroTaps (hiMask, center);
            insertZeroTaps (maskReverse, centerReverse);
            insertZeroTaps (hiReverse, centerReverse);
            if (mask.getSize() & 1)
            {
                center = mask.getSize() / 2;
                centerReverse = mask.getSize() / 2;
            }
            else
            {
                center = mask.getSize() / 2 + 1;
                centerReverse = mask.getSize() / 2;
            }
        }
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
    if (myPrivate->frameMode)
    {
//        myPrivate->lowPass.normalize();
//        myPrivate->highPass.normalize();
//        myPrivate->lowPassReverse.normalize();
//        myPrivate->highPassReverse.normalize();
    }
    goLog::message("calculateDWT(): Currently only 2D.", this);
    goFixedArray<goAutoPtr<goSignal3D<void> > >& dwt = myPrivate->dwt;
    goSize_t i;
    
    for (i = 0; i < dwt.getSize(); ++i)
    {
        dwt[i]->setDataType(dwtType);
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

    if (!myPrivate->frameMode)
    // Normal dwt operation
    {
        downsampleFilter (sig, L, H);
        L.rotateAxes(); L.rotateAxes();
        H.rotateAxes(); H.rotateAxes();
        dwt[0]->rotateAxes(); dwt[0]->rotateAxes();
        dwt[1]->rotateAxes(); dwt[1]->rotateAxes();
        dwt[2]->rotateAxes(); dwt[2]->rotateAxes();
        dwt[3]->rotateAxes(); dwt[3]->rotateAxes();
        downsampleFilter (L, *dwt[0], *dwt[1]);   // LL,LH
        downsampleFilter (H, *dwt[2], *dwt[3]);   // HL,HH
        L.rotateAxes();
        H.rotateAxes();
        dwt[0]->rotateAxes();
        dwt[1]->rotateAxes();
        dwt[2]->rotateAxes();
        dwt[3]->rotateAxes();
    }
    else
    // frame-like operation. No decimation.
    {
        this->filter (sig, L, H);
        L.rotateAxes(); L.rotateAxes();
        H.rotateAxes(); H.rotateAxes();
        dwt[0]->rotateAxes(); dwt[0]->rotateAxes();
        dwt[1]->rotateAxes(); dwt[1]->rotateAxes();
        dwt[2]->rotateAxes(); dwt[2]->rotateAxes();
        dwt[3]->rotateAxes(); dwt[3]->rotateAxes();
        this->filter (L, *dwt[0], *dwt[1]);   // LL,LH
        this->filter (H, *dwt[2], *dwt[3]);   // HL,HH
        L.rotateAxes();
        H.rotateAxes();
        dwt[0]->rotateAxes();
        dwt[1]->rotateAxes();
        dwt[2]->rotateAxes();
        dwt[3]->rotateAxes();
//        _scaleValues (dwt[0], 0.5);
//        _scaleValues (dwt[1], 0.5);
//        _scaleValues (dwt[2], 0.5);
//        _scaleValues (dwt[3], 0.5);
    }
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
    subsignal.setParent (&tempL);
    subsignal.setSize (L.getSizeX(), L.getSizeY(), L.getSizeZ());
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
    temp.copy(sig);
    L.make (sx, sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32);
    H.make (sx, sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32);
    
    myPrivate->lowPass.filter (temp);
    {
        goSubSignal3D<void> subsignal;
        subsignal.setParent (&temp);
        subsignal.setSize (L.getSizeX(), L.getSizeY(), L.getSizeZ());
        subsignal.setSkip (1,0,0);
        goCopySignal (&subsignal, &L);
    }
    temp.copy(sig);
    myPrivate->highPass.filter (temp);
    {
        goSubSignal3D<void> subsignal;
        subsignal.setParent (&temp);
        subsignal.setSize (H.getSizeX(), H.getSizeY(), H.getSizeZ());
        subsignal.setSkip (1,0,0);
        goCopySignal (&subsignal, &H);
    }
    temp.destroy();
    return true;
}

bool
goDWT3D::filter (goSignal3DBase<void>& sig,
                 goSignal3D<void>& L, 
                 goSignal3D<void>& H)
                          
{
    if (L.getSize() != sig.getSize() || L.getChannelCount() != sig.getChannelCount())
    {
        L.make (sig.getSizeX(), sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32, sig.getChannelCount());
    }if (H.getSize() != sig.getSize() || H.getChannelCount() != sig.getChannelCount())
    {
        H.make (sig.getSizeX(), sig.getSizeY(), sig.getSizeZ(), 16, 16, 16, 32, 32, 32, sig.getChannelCount());
    }
   
    goCopySignal (&sig, &L);
    // L.copy(sig);
    myPrivate->lowPass.filter (L);
    goCopySignal (&sig, &H);
    // H.copy(sig);
    myPrivate->highPass.filter (H);

    return true;
}

bool
goDWT3D::reconstruct (goSignal3DBase<void>& L, goSignal3DBase<void>& H, goSignal3D<void>& target)
{
    if (L.getDataType().getID() != H.getDataType().getID())
    {
        goLog::warning("upsampleFilter(): input data types mismatch.",this);
        return false;
    }
    goSize_t sx = L.getSizeX();
    goSize_t sy = L.getSizeY();
    goSize_t sz = L.getSizeZ();

    target.setDataType (L.getDataType().getID());
    target.make (sx, sy, sz, 16, 16, 16, 32, 32, 32);

    myPrivate->lowPassReverse.filter (L);
    myPrivate->highPassReverse.filter (H);

    _combine (L, H, target);
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
    if (!myPrivate->frameMode)
    {
        return this->dwt (&*parentDWT.getDWT()[0], axes, parentDWT.getDWT()[0]->getDataType().getID());
    }
    else
    {
        return this->dwt (&*parentDWT.getDWT()[0], axes, parentDWT.getDWT()[0]->getDataType().getID());
//        goSubSignal3D<void> subsignal;
//        myPrivate->axes = axes;
//        goIndex_t sx = parentDWT.getDWT()[0].getSizeX();
//        goIndex_t sy = parentDWT.getDWT()[0].getSizeY();
//        goIndex_t sz = parentDWT.getDWT()[0].getSizeZ();
//        if (axes & GO_X)
//            sx = sx <= 1 ? sx : sx >> 1;
//        if (axes & GO_Y)
 //           sy = sy <= 1 ? sy : sy >> 1;
 ////       if (axes & GO_Z)
 //           sz = sz <= 1 ? sz : sz >> 1;
 //       subsignal.setSize (sx, sy, sz);
 //       // FIXME: Fix this for 3D. This only works for 2D.
 //       subsignal.setSkip (1,1,0);
 //       subsignal.setParent (&parentDWT.getDWT()[0]);
 //       return this->dwt (&subsignal, axes, parentDWT.getDWT()[0].getDataType().getID());
    }
}

template <class T>
static void _scaleValues2 (goSignal3DBase<void>& sig, goDouble value)
{
    goSignal3DGenericIterator it (&sig);

    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                *(T*)*it = static_cast<T>(*(T*)*it * value);
                it.incrementX();
            }
            it.incrementY();
        }
        it.incrementZ();
    }
}

static void _scaleValues (goSignal3DBase<void>& sig, goDouble value)
{
    switch (sig.getDataType().getID())
    {
        case   GO_INT8:     _scaleValues2<goInt8>     (sig,value);   break;
        case   GO_UINT8:    _scaleValues2<goUInt8>    (sig,value);   break;
        case   GO_INT16:    _scaleValues2<goInt16>    (sig,value);   break;
        case   GO_UINT16:   _scaleValues2<goUInt16>   (sig,value);   break;
        case   GO_INT32:    _scaleValues2<goInt32>    (sig,value);   break;
        case   GO_UINT32:   _scaleValues2<goUInt32>   (sig,value);   break;
        case   GO_FLOAT:    _scaleValues2<goFloat>    (sig,value);   break;
        case   GO_DOUBLE:   _scaleValues2<goDouble>   (sig,value);   break;
        default: break;
    }
}

bool 
goDWT3D::idwt (goDWT3D& parentDWT)
{
    if (!myPrivate->frameMode)
    {
        // We know this is a signal3d<void>
        return this->idwt (parentDWT.getDWT()[0].get());
    }
    else
    {
        return this->idwt (parentDWT.getDWT()[0].get());
//        goSignal3D<void> tempTarget;
//        this->idwt (&tempTarget);
        // _scaleValues (tempTarget, 2);       // Undo the value scaling in the next stage
//        goSubSignal3D<void> subsignal;
//        subsignal.setSize (this->getDWT()[0].getSizeX(),
//                           this->getDWT()[0].getSizeY(),
//                           this->getDWT()[0].getSizeZ());
        // FIXME: Fix this for 3D. This only works for 2D.
//        subsignal.setSkip (1,1,0);
//        subsignal.setParent (&parentDWT.getDWT()[0]);
//        goFillSignal (&parentDWT.getDWT()[0], 0.0);
//        goCopySignal (&tempTarget, &subsignal);
//        parentDWT.getDWT()[0].rotateAxes();
//        parentDWT.getDWT()[0].rotateAxes();
//        myPrivate->lowPassReverse.filter (parentDWT.getDWT()[0]);
//        parentDWT.getDWT()[0].rotateAxes();
//        myPrivate->lowPassReverse.filter (parentDWT.getDWT()[0]);
//        return true;
    }
}

/** 
 * @brief Inverse dwt.
 * 
 * @note frame mode does not work here. Implement if needed.
 * 
 * @param target Target to hold the result.
 * 
 * @return True if successful, false otherwise.
 */
bool 
goDWT3D::idwt (goSignal3D<void>* target)
{
    if (!target)
    {
        goLog::warning("idwt(): target ist 0.", this);
        return false;
    }
    if (myPrivate->frameMode)
    {
//        myPrivate->lowPass.normalize();
//        myPrivate->highPass.normalize();
//        myPrivate->lowPassReverse.normalize();
//        myPrivate->highPassReverse.normalize();
    }
    goFixedArray<goAutoPtr<goSignal3D<void> > >& dwt = myPrivate->dwt;

    goSignal3D<void> tempH;
    goSignal3D<void> tempL;

    if (!myPrivate->frameMode)
    // Normal DWT operation.
    {
        dwt[2]->rotateAxes(); dwt[2]->rotateAxes();
        dwt[3]->rotateAxes(); dwt[3]->rotateAxes();
        upsampleFilter (*dwt[2], *dwt[3], tempH);
        tempH.rotateAxes();
        dwt[2]->rotateAxes();
        dwt[3]->rotateAxes();

        dwt[0]->rotateAxes(); dwt[0]->rotateAxes();
        dwt[1]->rotateAxes(); dwt[1]->rotateAxes();
        upsampleFilter (*dwt[0], *dwt[1], tempL);
        tempL.rotateAxes();
        dwt[0]->rotateAxes();
        dwt[1]->rotateAxes();
        upsampleFilter (tempL, tempH, *target);
    }
    else
    // frame-like operation. Upsample only LL band.
    {
        dwt[2]->rotateAxes(); dwt[2]->rotateAxes();
        dwt[3]->rotateAxes(); dwt[3]->rotateAxes();
        reconstruct (*dwt[2], *dwt[3], tempH);
        tempH.rotateAxes();   // bring tempH back to XYZ arrangement.
                              // The YZX arrangement stems from the rotation of dwt[...].
        dwt[2]->rotateAxes();
        dwt[3]->rotateAxes();

//        goSize_t sx = dwt[1].getSizeX();
//        goSize_t sy = dwt[1].getSizeY();
//        goSize_t sz = dwt[1].getSizeZ();
//        if (myPrivate->axes & GO_X)
//            sx = sx <= 1 ? sx : sx >> 1;
//        if (myPrivate->axes & GO_Y)
//            sy = sy <= 1 ? sy : sy >> 1;
//        if (myPrivate->axes & GO_Z)
//            sz = sz <= 1 ? sz : sz >> 1;
        dwt[0]->rotateAxes(); dwt[0]->rotateAxes();
        dwt[1]->rotateAxes(); dwt[1]->rotateAxes();
        reconstruct (*dwt[0], *dwt[1], tempL);
        tempL.rotateAxes();
        dwt[0]->rotateAxes();
        dwt[1]->rotateAxes();
        reconstruct (tempL, tempH, *target);
        _scaleValues (*target, (1.0f / 4.0f));
    }
    return true;
}

goFixedArray<goAutoPtr<goSignal3D<void> > >& 
goDWT3D::getDWT ()
{
    return myPrivate->dwt;
}

void
goDWT3D::setFrameMode (bool m)
{
    myPrivate->frameMode = m;
}
