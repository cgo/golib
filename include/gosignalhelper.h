/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#ifndef GOSIGNALHELPER_H
#define GOSIGNALHELPER_H

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif

bool goNormalizeSignal   (const goSignal3DBase<void>* sig, goSignal3D<void>* targetSig);
bool goNormalizeSignal   (goSignal3DBase<void>* sig);
bool goFindZeroCrossings (const goSignal3DBase<void>* sig, goArray<goPointf>& pointsRet);
bool goConvertSignal     (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);
bool goRGBAtoScalar      (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig);

#endif
