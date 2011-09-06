/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOBIORTHOWAVELET_H
#define GOBIORTHOWAVELET_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

class goBiorthoWaveletPrivate;

/*!
 * \addtogroup math
 * @{
 */
class goBiorthoWavelet : public goObjectBase
{
    public:
        goBiorthoWavelet ();
        virtual ~goBiorthoWavelet ();

        bool transform (goSignal3DBase<void>& v, goSignal3D<void>& highRet, goSignal3D<void>& lowRet);
        bool inverse   (goSignal3DBase<void>& hi, goSignal3DBase<void>& lo, goSignal3D<void>& ret);

    private:
        goBiorthoWaveletPrivate* myPrivate;
};
/*! @} */
#endif
