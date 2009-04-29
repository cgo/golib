/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gosignalhelper.h,v 1.1.1.1 2006/04/19 15:27:09 gosch Exp $
 */

#ifndef GOSIGNALHELPER_H
#define GOSIGNALHELPER_H

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOSUBSIGNAL3D_H
# include <gosubsignal3d.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

/*!
 * \addtogroup signal
 * @{
 */
namespace goSignal 
{
    template <class T>
        bool binaryImage (const goMath::Matrix<T>& boundary, goSignal3D<void>& ret, goSize_t width, goSize_t height);
    bool sobel2D (const goSignal3DBase<void>& input, goSignal3DBase<void>& output);
};

bool     goNormalizeSignal    (const goSignal3DBase<void>* sig, goSignal3D<void>* targetSig);
bool     goNormalizeSignal    (goSignal3DBase<void>* sig);
bool     goFindZeroCrossings  (const goSignal3DBase<void>* sig, goArray<goPointf>& pointsRet);
bool     goConvertSignal      (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);
bool     goCopySignal         (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);
bool     goCopySignalChannel  (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);
template <class T> bool goCopySignalArray (const goSignal3DBase<void>* sig, T* targetArray, int* strides = 0);
template <class T> bool goCopySignalArray (const T* array, goSignal3DBase<void>* targetSig, int* strides = 0);
bool     goFillSignal         (goSignal3DBase<void>* sig, goFloat value);
bool     goRGBAtoScalar       (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);
void     goSignalInfoText     (const goSignal3DBase<void>& sig, goString& strRet, bool html = false);
goDouble goSignalMean         (const goSignal3DBase<void>& sig);
void     goSignalMeanVariance (const goSignal3DBase<void>& sig, goDouble& mean, goDouble& variance);
void     goSignalFlipY        (const goSignal3DBase<void>& sig, goSignal3DBase<void>& target);
bool     goSignalCOM          (const goSignal3DBase<void>& sig, goMath::Vectord& comRet);
bool     goSignalMinMaxCoord  (const goSignal3DBase<void>& sig, goDouble thresh, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet);
bool     goSignalMinMaxCoord  (const goSignal3DBase<void>& sig, goAutoPtr<goFunctorBase1<bool, goDouble> > f, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet);
bool     goSignalMinMaxCoordHigher  (const goSignal3DBase<void>& sig, goDouble thresh, goMath::Vector<goSize_t>& minRet, goMath::Vector<goSize_t>& maxRet);
bool     goSignalCrop         (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goDouble thresh);
bool     goSignalCrop         (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goAutoPtr<goFunctorBase1<bool, goDouble> > f);
bool     goSignalCropHigher   (goSignal3DBase<void>& sig, goSubSignal3D<void>& target, goDouble thresh);
bool     goSignalMax          (const goSignal3DBase<void>& sig1, const goSignal3DBase<void>& sig2, goSignal3DBase<void>& target);
bool     goSignalMin          (const goSignal3DBase<void>& sig1, const goSignal3DBase<void>& sig2, goSignal3DBase<void>& target);
/*! @} */
#endif
