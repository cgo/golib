/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal.h>

namespace goSignal
{
    const goSize3D _defaultBlockSize3D (32, 32, 32);
    const goSize3D _defaultBlockSize2D (32, 32, 1);

    const goSize3D& defaultBlockSize3D ()
    {
        return _defaultBlockSize3D;
    }

    const goSize3D& defaultBlockSize2D ()
    {
        return _defaultBlockSize2D;
    }
};
