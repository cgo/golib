#ifndef GOGNUPLOT_H
#define GOGNUPLOT_H

#include <goobjectbase.h>

class goGnuplotPrivate;

/** 
 * @brief Interface for a gnuplot process.
 *
 * This is a convenient interface for a gnuplot process running in
 * the background. It hides the work to redirect i/o via pipes.
 * Just create an object and call the \c call() method with any gnuplot
 * command.
 *
 * @author Christian Gosch
 */
class goGnuplot : public goObjectBase
{
    public:
        goGnuplot (const char* program_name = 0);
        virtual ~goGnuplot ();

        bool call (const goString& command);
        bool call (const char* command);
        bool setOutput (const char* filename);
        bool setPostscript (const char* filename = 0);
        bool setEPS (const char* filename = 0);
        bool setX11 ();

    private:
        goGnuplotPrivate* myPrivate;
};

#endif
