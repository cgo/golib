#include <gognuplot.h>
#include <goprocess.h>
#include <golog.h>

#include <unistd.h>

class goGnuplotPrivate
{
    public:
        goGnuplotPrivate () 
            : process () 
        {
            pipe1[0] = 0;
            pipe1[1] = 0;
            pipe2[0] = 0;
            pipe2[1] = 0;
        };
        ~goGnuplotPrivate () {};

        goProcess process;
        int pipe1[2];   // 0: input 1: output
        int pipe2[2];
};

goGnuplot::goGnuplot() : goObjectBase(), myPrivate(0)
{
    myPrivate = new goGnuplotPrivate;

#if not defined WIN32
    if (::pipe(myPrivate->pipe1) != 0)
    {
        goLog::warning("goGnuplot: Error opening pipe 1\n");
        // exit(1);
    }
    if (::pipe(myPrivate->pipe2) != 0)
    {
        goLog::warning("goGnuplot: Error opening pipe 2\n");
        // exit(1);
    }
    //= Make the pipes the standard i/o file descriptors of the process.
    myPrivate->process.setInput(myPrivate->pipe1[0]);    // read end of pipe 1
    myPrivate->process.setOutput(myPrivate->pipe2[1]);   // write end of pipe 2

    //goFixedArray<goString> cl (3);
    // cl[0] = "-i";
    // cl[1] = "-c";
    // cl[2] = "\"gnuplot\"";
    // myPrivate->process.run ("bash",cl);
    goFixedArray<goString> cl (0);
    myPrivate->process.run("gnuplot", cl);

    //=
    //= Close the old read/write ends which are now stdin/stdout of the child process.
    //=
    close (myPrivate->pipe1[0]);
    close (myPrivate->pipe2[1]);
#endif
}

goGnuplot::~goGnuplot()
{
#if not defined WIN32
    ::close (myPrivate->pipe1[1]);
    ::close (myPrivate->pipe2[0]);
    myPrivate->process.kill();
#endif
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goGnuplot::call (const goString& command)
{
    return this->call (command.toCharPtr());
}

bool goGnuplot::call (const char* command)
{
    ssize_t written = 0;
    ssize_t total = 0;
    ssize_t totalToWrite = strlen(command) * sizeof(char);
    while (total < totalToWrite)
    {
        written = write(myPrivate->pipe1[1], command + total, totalToWrite - total);
        if (written < 0)
            return false;
        total += written;
    }

    return true;
}

bool goGnuplot::setOutput (const char* filename)
{
    goString cmd = "set output \"";
    cmd += filename;
    cmd += "\"\n";
    return this->call (cmd);
}

bool goGnuplot::setPostscript (const char* filename)
{
    if (filename)
        this->setOutput (filename);
    return this->call ("set terminal postscript colour\n");
}

bool goGnuplot::setEPS (const char* filename)
{
    if (filename)
        this->setOutput (filename);
    return this->call ("set terminal postscript eps colour\n");
}

bool goGnuplot::setX11 ()
{
    return this->call ("set terminal x11\n");
}
