#include <golog.h>
#include <fstream>

bool          goLog::isOn      (true);
goString      goLog::fileName  ("");
std::ofstream goLog::outStream ("golib.log");

/** 
 * @brief Write a message to the log.
 * 
 * @param s Message.
 * @param caller Refering object. Can be null.
 */
void goLog::message (const goString& s, const goObjectBase* caller)
{
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

/** 
 * @brief Write a warning message to the log.
 * 
 * @param s Message.
 * @param caller Refering object. Can be null.
 */
void goLog::warning (const goString& s, const goObjectBase* caller)
{
    outStream << "WARNING: ";
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

/** 
 * @brief Write an error message to the log.
 * 
 * @param s Message.
 * @param caller Refering object. Can be null.
 */
void goLog::error (const goString& s, const goObjectBase* caller)
{
    outStream << "*** ERROR ***: ";
    if (caller)
    {
        outStream << caller->getClassName() << " " << caller->getObjectName() << ": ";
    }
    outStream << s.toCharPtr() << std::endl;
}

/** 
 * @brief Close the old logfile and open a new one.
 * 
 * @param fn  Filename.
 */
void goLog::logFile (const goString& fn)
{
    fileName = fn;
    outStream.close ();
    outStream.open  (fn.toCharPtr(), std::ios::out);
}

/** 
 * @brief Turn on logging - no effect yet.
 */
void goLog::on ()
{
    isOn = true;
}

/** 
 * @brief Turn off logging - no effect yet, logging is always on.
 */
void goLog::off ()
{
    isOn = false;
}

void goLog::logConsole ()
{
    // FIXME
}
