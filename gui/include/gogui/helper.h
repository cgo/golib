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
