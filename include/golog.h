#ifndef GOLOG_H
#define GOLOG_H

#ifndef GOSTRING_H
# include <gostring.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif 
#include <fstream>

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

#endif
