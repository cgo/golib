#include <gopython.h>

class goPythonPrivate
{
    public:
        goPythonPrivate ()
        {
            Py_Initialize (); //= should be a no-op if already initialised.
        };

        ~goPythonPrivate ()
        {
            Py_Finalize ();  //= FIXME Careful: add a reference count here
                             //= before destroying the interpreter.
        };
};

goPython::goPython ()
    : myPrivate (0)
{
    myPrivate = new goPythonPrivate ();
}

goPython::~goPython ()
{
    if (myPrivate)
    {
        delete myPrivate;
    }
    myPrivate = 0;
}

bool goPython::call (const goString& cmd)
{
    PyRun_SimpleString (cmd.toCharPtr());
}

bool goPython::call (const char* cmd)
{
    PyRun_SimpleString (cmd);
}
