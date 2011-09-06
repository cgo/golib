/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_HELPER_H
#define GOGUI_HELPER_H

#include <govideocapture.h>
#include <gosignalhelper.h>
#include <gofileio.h>
#include <gotimerobject.h>
#include <golog.h>
#include <gocurve.h>
#include <gofixedarray.h>

namespace goGUI
{
/** @addtogroup gui
 * @{
 */
    bool getFilenameSave (goString& fname, const goString& start, const goString& title = "");
    bool getFilenameOpen (goString& fname, const goString& start, const goString& title = "");
    bool getFoldername   (goString& fname, const goString& start, const goString& title = "");
    bool getFilenames (goFixedArray<goString>& filenames, const goString& start = "", const goString& title = "");

    void about (const goString& text, char* logo[]);

    void warning (const char* text);

/** 
 * @}
 */
};

#endif
