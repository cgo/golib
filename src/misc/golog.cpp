#include <golog.h>
#include <fstream>

bool          goLog::isOn      (true);
goString      goLog::fileName  ("");
std::ofstream goLog::outStream ("golib.log");

void goLog::message (const goString& s, const goObjectBase* caller)
{
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

void goLog::warning (const goString& s, const goObjectBase* caller)
{
    outStream << "WARNING: ";
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

void goLog::error (const goString& s, const goObjectBase* caller)
{
    outStream << "*** ERROR ***: ";
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

void goLog::logFile (const goString& fn)
{
    fileName = fn;
    outStream.close ();
    outStream.open  (fn.toCharPtr(), std::ios::out);
}

void goLog::on ()
{
    isOn = true;
}

void goLog::off ()
{
    isOn = false;
}

void goLog::logConsole ()
{
    // FIXME
}
