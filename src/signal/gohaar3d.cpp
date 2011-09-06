/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gohaar3d.h>
#include <gosignal3d.h>
#include <gosignal3dgenericiterator.h>
#include <gosubsignal3d.h>
#include <gosignalhelper.h>
#include <golog.h>
#include <godefs.h>
#include <assert.h>

typedef goDouble godwt_t;

static const goDouble TP0 = 1 / sqrt(2);
static const goDouble TP1 = 1 / sqrt(2); // 0.7071;

/*! \todo: Add void type support for goDWT */
template <class T, class targetT>
static inline void
_haarFilterX (goSignal3DBase<void>& signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    goSize_t k;					
    godwt_t  tmp1, tmp2;

    goSignal3DGenericIterator sourceIt (&signal);
    goSignal3DGenericIterator targetIt (&signal_target);

    sourceIt.resetZ();
    targetIt.resetZ();
    while (!sourceIt.endZ())
    {
        sourceIt.resetY();
        targetIt.resetY();
        while (!sourceIt.endY())
        {
            sourceIt.resetX();
            targetIt.resetX();
            for (k = 0; k < (signal.getSizeX() >> 1); ++k)   /* one less than size! */	
            {										
                tmp1 = *(T*)*sourceIt * tp0;			
                tmp2 = *(T*)sourceIt.rightX() * tp1;		
                *(targetT*)*targetIt = (targetT)(tmp1 + tmp2);
                targetIt.incrementX();
                *(targetT*)*targetIt = (targetT)(tmp1 - tmp2);
                targetIt.incrementX();
                sourceIt.incrementX();
                sourceIt.incrementX();
            }
            sourceIt.incrementY();
            targetIt.incrementY();
        }
        sourceIt.incrementZ();
        targetIt.incrementZ();
    }
}

template <class T>
static inline void
haarFilterX2 (goSignal3DBase<void>& signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    switch (signal.getDataType().getID())
    {
        case   GO_UINT8:    _haarFilterX<goUInt8,T>    (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_INT8:     _haarFilterX<goInt8,T>     (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_UINT16:   _haarFilterX<goUInt16,T>   (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_INT16:    _haarFilterX<goInt16,T>    (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_UINT32:   _haarFilterX<goUInt32,T>   (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_INT32:    _haarFilterX<goInt32,T>    (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_FLOAT:    _haarFilterX<goFloat,T>    (signal,   signal_target,   tp0,   tp1);   break;
        case   GO_DOUBLE:   _haarFilterX<goDouble,T>   (signal,   signal_target,   tp0,   tp1);   break;
        default: goLog::warning("haarFilterX(): unknown type."); break;
    }

}

static inline void
haarFilterX (goSignal3DBase<void>& signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    switch (signal_target.getDataType().getID())
    {
        case GO_FLOAT:  haarFilterX2<goFloat>  (signal, signal_target, tp0, tp1); break;
        case GO_DOUBLE: haarFilterX2<goDouble> (signal, signal_target, tp0, tp1); break;
        default: goLog::warning("haarFilterX(): invalid target type."); break;
    }
}

template <class T>
static inline void
haarReverseX2 (goSignal3DBase<void>& dwt_signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    godwt_t f0;
    godwt_t f1;
    f0 = tp0;
    f1 = tp1;
    // f0 = 1.0f / (2 * tp0);
    // f1 = 1.0f / (2 * tp1);
    switch (signal_target.getDataType().getID())
    {
        case   GO_UINT8:    _haarFilterX<T,goUInt8>    (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_INT8:     _haarFilterX<T,goInt8>     (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_UINT16:   _haarFilterX<T,goUInt16>   (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_INT16:    _haarFilterX<T,goInt16>    (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_UINT32:   _haarFilterX<T,goUInt32>   (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_INT32:    _haarFilterX<T,goInt32>    (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_FLOAT:    _haarFilterX<T,goFloat>    (dwt_signal,   signal_target,   f0,   f1);   break;
        case   GO_DOUBLE:   _haarFilterX<T,goDouble>   (dwt_signal,   signal_target,   f0,   f1);   break;
        default: goLog::warning("haarFilterX(): unknown type."); break;
    }
} 
static inline void
haarReverseX (goSignal3DBase<void>& dwt_signal, goSignal3DBase<void>& signal_target, goDouble tp0, goDouble tp1)
{
    switch (dwt_signal.getDataType().getID())
    {
        case GO_FLOAT:  haarReverseX2<goFloat>  (dwt_signal, signal_target, tp0, tp1); break;
        case GO_DOUBLE: haarReverseX2<goDouble> (dwt_signal, signal_target, tp0, tp1); break;
        default: goLog::warning("haarReverseX(): invalid target type."); break;
    }
} 


class goHaar3DPrivate
{
    public:
        goHaar3DPrivate ();
        ~goHaar3DPrivate ();
        
        goSignal3DBase<void>* dwt;
        goSubSignal3D<void>   dwtAccess;
        goIndex_t             haarStages;
        int                   haarAxes;       // or'ed GO_X, GO_Y, GO_Z
};

goHaar3DPrivate::goHaar3DPrivate ()
    : dwt        (0),
      dwtAccess  (),
      haarStages (0),
      haarAxes   (GO_X|GO_Y|GO_Z)
{
}

goHaar3DPrivate::~goHaar3DPrivate ()
{
    if (dwt)
    {
        delete dwt;
        dwt = NULL;
    }
}

goHaar3D::goHaar3D ()
    : goObjectBase (),
      myPrivate    (0)
{
    this->setClassID(GO_HAAR3D);
    myPrivate = new goHaar3DPrivate;
    assert (myPrivate);
}
        
goHaar3D::~goHaar3D ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

bool
goHaar3D::calculateHaar ()
{
    goSize_t sz = myPrivate->dwtAccess.getSizeZ();
    goSize_t sy = myPrivate->dwtAccess.getSizeY();
    goSize_t sx = myPrivate->dwtAccess.getSizeX();
    goSubSignal3D<void>* dwt = &myPrivate->dwtAccess; 
    if (sx > 1 && (myPrivate->haarAxes & GO_X))
    {
        goLog::message("Filtering X",this);
        haarFilterX (*dwt, *dwt, TP0, TP1);
    }
    dwt->rotateAxes(); dwt->rotateAxes();
    if (sy > 1 && (myPrivate->haarAxes & GO_Y))
    {
        goLog::message("Filtering Y",this);
        haarFilterX (*dwt, *dwt, TP0, TP1);
    }
    if (sz > 1 && (myPrivate->haarAxes & GO_Z))
    {
        dwt->rotateAxes(); dwt->rotateAxes();
        goLog::message("Filtering Z",this);
        haarFilterX (*dwt, *dwt, TP0, TP1);
        dwt->rotateAxes();
    }
    dwt->rotateAxes();
    return true;
}

bool
goHaar3D::haar (const goSignal3DBase<void>* sig, int axes, goTypeEnum dwtType)
{
    goSignal3D<void>* newDWT = new goSignal3D<void>;
   
    if (myPrivate->dwt)
    {
        delete myPrivate->dwt;
        myPrivate->dwt = NULL;
    }
    myPrivate->dwt = newDWT;
    
    newDWT->setDataType (dwtType);
    goSize_t sz = sig->getSizeZ();
    goSize_t sy = sig->getSizeY();
    goSize_t sx = sig->getSizeX();

    if (!newDWT->make (sx, sy, sz,
                (sx == 1 ? 1 : 32), (sy == 1 ? 1 : 32), (sz == 1 ? 1 : 32), 
                32, 32, 32, 1))
    {
        goLog::warning("Could not make dwt signal!",this);
        return false;
    }

    if (!goCopySignal (sig,myPrivate->dwt))
    {
        goLog::warning("haar(): goCopySignal() failed.",this);
        return false;
    }
    
    myPrivate->dwtAccess.setSkip   (0,0,0);
    myPrivate->dwtAccess.setParent (myPrivate->dwt);
    myPrivate->dwtAccess.setSize   (sx,sy,sz);
    myPrivate->haarAxes = axes;
    bool ok = calculateHaar();
    if (!ok)
    {
        return false;
    }
    return true;
}

bool
goHaar3D::haar (goHaar3D& parentDWT, int axes)
{
    goSubSignal3D<void>* newDWT = new goSubSignal3D<void>;
    if (!parentDWT.getBand (newDWT, LOW, LOW, LOW))
        return false;

    goSize_t sz = newDWT->getSizeZ();
    goSize_t sy = newDWT->getSizeY();
    goSize_t sx = newDWT->getSizeX();
    
    if (myPrivate->dwt)
    {
        delete myPrivate->dwt;
        myPrivate->dwt = NULL;
    }
    myPrivate->dwt = newDWT;
    
    myPrivate->dwtAccess.setSkip   (0,0,0);
    myPrivate->dwtAccess.setParent (myPrivate->dwt);
    myPrivate->dwtAccess.setSize   (sx,sy,sz);
    myPrivate->haarAxes = axes;
    return calculateHaar();
}

bool 
goHaar3D::unHaar ()
{
    if (!myPrivate->dwt)
    {
        return false;
    }
    goSize_t sz = myPrivate->dwt->getSizeZ();
    goSize_t sy = myPrivate->dwt->getSizeY();
    goSize_t sx = myPrivate->dwt->getSizeX();

    myPrivate->dwtAccess.setSkip        (0,0,0);
    myPrivate->dwtAccess.setParent      (myPrivate->dwt);
    myPrivate->dwtAccess.setSize        (sx,sy,sz);
    
    goSubSignal3D<void>*  dwt = &myPrivate->dwtAccess;
    dwt->rotateAxes();
    if (sz > 1 && (myPrivate->haarAxes & GO_Z))
    {
        goLog::message("Filtering Z",this);
        haarReverseX (*dwt, *dwt, TP0, TP1);
    }
    dwt->rotateAxes();
    if (sy > 1 && (myPrivate->haarAxes & GO_Y))
    {
        goLog::message("Filtering Y",this);
        haarReverseX (*dwt, *dwt, TP0, TP1);
    }
    dwt->rotateAxes();
    if (sx > 1 && (myPrivate->haarAxes & GO_X))
    {
        goLog::message("Filtering X",this);
        haarReverseX (*dwt, *dwt, TP0, TP1);
    }
    return true;
}

bool
goHaar3D::getBand (goSubSignal3D<void>* retBand, int x_band, int y_band, int z_band)
{
    if (!retBand)
    {
        return false;
    }
    if (!this->getTransform())
    {
        return false;
    }
    goSize_t sx = this->getTransform()->getSizeX();
    goSize_t sy = this->getTransform()->getSizeY();
    goSize_t sz = this->getTransform()->getSizeZ();
    retBand->setSize (sx, sy, sz);
    retBand->setParent (this->getTransform());
    retBand->setSkip (0,0,0);
    retBand->shiftRightSize (1);
    retBand->shiftLeftDiff (1);
    retBand->setPosition (x_band, y_band, z_band);
    return true;
}

#if 0
bool 
goHaar3D::haar (goSignal3DBase<void>* sig, goIndex_t stages, int axes)
{
    static bool inplace = false;
    static goIndex_t stageCount = 0;
    if (!sig)
    {
        return false;
    }
    assert (sig);

    goSize_t sz = sig->getSizeZ();
    goSize_t sy = sig->getSizeY();
    goSize_t sx = sig->getSizeX();

    goString msg = "haar(): stageCount == ";
    msg += (int)stageCount;
    msg += ", "; msg += "size: ";
    msg += (int)sx; msg += " ";
    msg += (int)sy; msg += " ";
    msg += (int)sz;
    goLog::message(msg,this);
    
    if (stageCount == 0)
    {
        myPrivate->dwt.destroy();
        myPrivate->dwt.setDataType (GO_DOUBLE);
        if (!myPrivate->dwt.make (sx, sy, sz,
                         32, 32, (sz == 1 ? 1 : 32), 
                         32, 32, 32, 1))
        {
            goLog::warning("Could not make dwt signal!",this);
            return false;
        }
        myPrivate->dwtAccess.setSkip   (0,0,0);
        myPrivate->dwtAccess.setParent (&myPrivate->dwt);
        myPrivate->dwtAccess.setSize   (sx,sy,sz);
        myPrivate->haarAxes = axes;
    }
    // FIXME see unHaar
    goSignal3DBase<void>* src = sig;
    goSubSignal3D<void>* dwt  = &myPrivate->dwtAccess; 
    if (sx > 1 && (axes & GO_X))
    {
        goLog::message("Filtering X",this);
        haarFilterX (*src, *dwt, 0.5, 0.5);
        src = dwt;
    }
    if (!inplace) { sig->rotateAxes(); sig->rotateAxes(); }
    dwt->rotateAxes(); dwt->rotateAxes();
    if (sy > 1 && (axes & GO_Y))
    {
        goLog::message("Filtering Y",this);
        haarFilterX (*src, *dwt, 0.5, 0.5);
        src = dwt;
    }
    if (sz > 1 && (axes & GO_Z))
    {
        if (!inplace) { sig->rotateAxes(); sig->rotateAxes(); }
        dwt->rotateAxes(); dwt->rotateAxes();
        goLog::message("Filtering Z",this);
        haarFilterX (*src, *dwt, 0.5, 0.5);
        if (!inplace) { sig->rotateAxes(); }
        dwt->rotateAxes();
    }
    if (!inplace) { sig->rotateAxes(); }
    dwt->rotateAxes();

    ++stageCount;
    if (stages == stageCount)
    {
        myPrivate->haarStages = stages;
        stageCount = 0;
        inplace = false;
        return true;
    }
    else
    {
        dwt->shiftLeftDiff(1, axes);
        dwt->shiftRightSize(1, axes);
        inplace = true;
        return this->haar (dwt, stages, axes);
    }
    return true;
}

bool 
goHaar3D::unHaar (goSignal3DBase<void>* sig, goIndex_t stage)
{
    static goIndex_t stageCount = 0;
    // FIXME: go on here. reconstruct recursively.
    if (!sig)
    {
        return false;
    }
    assert (sig);
    if (myPrivate->haarStages <= 0)
    {
        return true;
    }
    goSize_t sz = myPrivate->dwt.getSizeZ();
    goSize_t sy = myPrivate->dwt.getSizeY();
    goSize_t sx = myPrivate->dwt.getSizeX();

    // First run
    if (stageCount == 0)
    {
        goSize_t target_sx = myPrivate->dwt.getSizeX();
        goSize_t target_sy = myPrivate->dwt.getSizeY();
        goSize_t target_sz = myPrivate->dwt.getSizeZ();
        if (myPrivate->haarAxes & GO_X)
            target_sx >>= myPrivate->haarStages - stage - 1;
        if (myPrivate->haarAxes & GO_Y)
            target_sy >>= myPrivate->haarStages - stage - 1;
        if (myPrivate->haarAxes & GO_Z)
            target_sz >>= myPrivate->haarStages - stage - 1;
        // FIXME
        if (sig->getSizeX() < target_sx ||
            sig->getSizeY() < target_sy ||
            sig->getSizeZ() < target_sz)
        {
            goLog::warning("unHaar(): target signal size too small.", this);
            return false;
        }
        myPrivate->dwtAccess.setSkip        (0,0,0);
        myPrivate->dwtAccess.setParent      (&myPrivate->dwt);
        myPrivate->dwtAccess.setSize        (sx,sy,sz);
        myPrivate->dwtAccess.shiftLeftDiff  (myPrivate->haarStages - 1, myPrivate->haarAxes);
        myPrivate->dwtAccess.shiftRightSize (myPrivate->haarStages - 1, myPrivate->haarAxes);
    }
    
    goSubSignal3D<void>*  dwt = &myPrivate->dwtAccess;
    goSignal3DBase<void>* src = &dwt;
    sig->rotateAxes();
    dwt.rotateAxes();
    if (sz > 1)
    {
        goLog::message("Filtering Z",this);
        haarReverseX (*src, *sig, 0.5, 0.5);
        src = sig;
    }
    sig->rotateAxes();
    dwt.rotateAxes();
    if (sy > 1)
    {
        goLog::message("Filtering Y",this);
        haarReverseX (*src, *sig, 0.5, 0.5);
        src = sig;
    }
    sig->rotateAxes();
    dwt.rotateAxes();
    if (sx > 1)
    {
        goLog::message("Filtering X",this);
        haarReverseX (*src, *sig, 0.5, 0.5);
    }
    return true;
}
#endif

goSignal3DBase<void>*
goHaar3D::getTransform ()
{
    return myPrivate->dwt;
}

