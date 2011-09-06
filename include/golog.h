/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOLOG_H
#define GOLOG_H

#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif 
#include <fstream>

/** @addtogroup misc
 * @{
 */
/** 
 * @brief  Logging facility.
 *
 * This class enables you to write messages to a log file (golib.log),
 * which is created every time a golib program is called.
 */
class goLog
{
    public:
//        goLog  ();
//        ~goLog ();

        static void message    (const goString&, const goObjectBase* caller = NULL);
        static void warning    (const goString&, const goObjectBase* caller = NULL);
        static void error      (const goString&, const goObjectBase* caller = NULL);
        static void logFile    (const goString&); 
        static void logConsole ();
        static void on         ();
        static void off        ();


    private:
        static bool          isOn;
        static goString      fileName;
        static std::ofstream outStream;
};
/** @} */
#endif
