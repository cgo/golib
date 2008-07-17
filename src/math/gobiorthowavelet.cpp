#include <gobiorthowavelet.h>
#include <gosubsignal3d.h>
#include <gofilter1d.h>
#include <gosignalhelper.h>
#include <gomathsignal.h>
#include <assert.h>

class goBiorthoWaveletPrivate
{
    public:
        goBiorthoWaveletPrivate () {};
        ~goBiorthoWaveletPrivate () {};
};

goBiorthoWavelet::goBiorthoWavelet ()
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassID(GO_BIORTHOWAVELET);
    myPrivate = new goBiorthoWaveletPrivate;
    assert (myPrivate);
}

goBiorthoWavelet::~goBiorthoWavelet ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/**
* @brief Forward transform 1-D signal v to high and low pass signals.
*
* Transforms with bi-orthogonal wavelets using a lifting scheme.
* 
* @param v  Signal to be transformed
* @param highRet  High pass return value
* @param lowRet  Low pass return value
*
* @return True on success, false otherwise.
**/
bool goBiorthoWavelet::transform (goSignal3DBase<void>& v, goSignal3D<void>& highRet, goSignal3D<void>& lowRet)
{
    assert (v.getDataType().getID() == GO_FLOAT ||
            v.getDataType().getID() == GO_DOUBLE);
    assert (v.getSizeY() == 1 && v.getSizeZ() == 1);
    goArray<goFloat> maskLow (2);
    goArray<goFloat> maskHigh (4);
    maskLow[0] = 0.5; maskLow[1] = 0.5;
    maskHigh[0] = -19 / 258.0;
    maskHigh[1] = 81 / 258.0;
    maskHigh[2] = 81 / 258.0;
    maskHigh[3] = -19 / 258.0; //= NOTE: Check if this is correct -- the sum of abs(coeff) is 200.
   
    goSubSignal3D<void> even (&v, v.getSizeX() / 2, 1, 1);
    even.setParent (&v);
    even.setSkip (1, 0, 0);
    even.setPosition (1, 0, 0);
    goSubSignal3D<void> odd (&v, v.getSizeX() - (v.getSizeX() / 2), 1, 1);
    odd.setParent (&v);
    odd.setSkip (1, 0, 0);
    odd.setPosition (0, 0, 0);
    
    even.setObjectName ("even");
    odd.setObjectName ("odd");
    
    //= Copy the two vectors
    goSignal3D<void> temp1;
    temp1.setObjectName ("temp1");
    goSignal3D<void> temp2;
    temp2.setObjectName ("temp2");
    temp1.setDataType (even.getDataType().getID());
    temp2.setDataType (odd.getDataType().getID());
    temp1.make (&even);
    temp2.make (&odd);
    goCopySignal (&even, &temp1);
    goCopySignal (&odd, &temp2);
    goFilter1D filter;
    filter.setMask (maskLow);
    filter.setCenter (1);
    filter.filter (temp1);

    temp2 -= temp1;
    highRet.setDataType (temp2.getDataType().getID());
    highRet.make (&temp2);
    goCopySignal (&temp2, &highRet);
    filter.setMask (maskHigh);
    filter.setCenter (2);
    filter.filter (temp2);
    lowRet.setDataType (even.getDataType().getID());
    lowRet.make (&even);
    goCopySignal (&even, &lowRet);
    lowRet += temp2;
    return true;
}

/**
* @brief Invert a transformed pair of high and low pass signals.
*
* @param hi  High pass. Will be altered during processing.
* @param lo  Low pass. Will be altered during processing.
* @param ret  Reconstructed signal return value.
*
* @return True on success, false otherwise.
**/
bool goBiorthoWavelet::inverse (goSignal3DBase<void>& hi, goSignal3DBase<void>& lo, goSignal3D<void>& ret)
{
    goSignal3D<void> even;
    goSignal3D<void> odd;
    even.setDataType (lo.getDataType().getID());
    odd.setDataType (hi.getDataType().getID());
    even.make (&lo);
    odd.make (&hi);

    goCopySignal (&lo, &even);
    goCopySignal (&hi, &odd);
    
    goArray<goFloat> maskLow (2);
    goArray<goFloat> maskHigh (4);
    maskLow[0] = 0.5; maskLow[1] = 0.5;
    maskHigh[0] = -19 / 258.0;
    maskHigh[1] = 81 / 258.0;
    maskHigh[2] = 81 / 258.0;
    maskHigh[3] = -19 / 258.0; 

    //= Filter
    goFilter1D filter;
    filter.setMask (maskHigh);
    filter.setCenter (2);
    filter.filter (hi);
    even -= hi;
    goCopySignal (&even, &lo);
    
    filter.setMask (maskLow);
    filter.setCenter (1);
    filter.filter (lo);
    odd += lo;
    
    //= Merge
    ret.setDataType (hi.getDataType().getID());
    goSize_t sx = even.getSizeX() + odd.getSizeX();
    goIndex_t border = goMath::min (goIndex_t(sx), goIndex_t(16));
    ret.make (sx, 1, 1, sx, 1, 1, border, 0, 0);
    
    goSubSignal3D<void> evenSub;
    evenSub.setSize (even.getSizeX(), 1, 1);
    evenSub.setSkip (1,0,0);
    evenSub.setParent (&ret);
    evenSub.setPosition (1,0,0);
    goSubSignal3D<void> oddSub;
    oddSub.setSize (odd.getSizeX(), 1, 1);
    oddSub.setSkip (1,0,0);
    oddSub.setParent (&ret);
    oddSub.setPosition (0,0,0);
    
    goCopySignal (&even, &evenSub);
    goCopySignal (&odd, &oddSub);
    return true;
}
