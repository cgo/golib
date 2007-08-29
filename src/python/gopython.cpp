#include <gopython.h>

class goPythonPrivate
{
    public:
        goPythonPrivate () : mainMod(0), mainNamespace(0)
        {
            Py_Initialize (); //= should be a no-op if already initialised.
            this->mainMod = PyImport_AddModule("__main__");
            this->mainNamespace = PyModule_GetDict(mainMod);
        };

        ~goPythonPrivate ()
        {
            Py_Finalize ();  //= FIXME Careful: add a reference count here
                             //= before destroying the interpreter.
        };

        PyObject* mainMod;
        PyObject* mainNamespace;
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

PyObject* goPython::run (const goString& cmd)
{
    PyObject* ret = PyRun_String (cmd.toCharPtr(), Py_file_input, myPrivate->mainNamespace, myPrivate->mainNamespace);
    if (!ret)
    {
        PyErr_Print();
    }
}

PyObject* goPython::getObject (const char* name)
{
    PyObject* tempName = PyString_FromString (name);
    PyObject* ret = PyDict_GetItem (myPrivate->mainNamespace, tempName);
    Py_XDECREF(tempName);
    return ret;
}

goFloat goPython::getFloat (const char* name)
{
    PyObject* o = this->getObject (name);
    goFloat ret = 0.0;
    if (PyNumber_Check(o))
    {
        PyObject* f = PyNumber_Float(o);
        ret = PyFloat_AsDouble(f);
        Py_XDECREF(f);
    }
    return ret;
}

goDouble goPython::getDouble (const char* name)
{
    PyObject* o = this->getObject (name);
    goDouble ret = 0.0;
    if (PyNumber_Check(o))
    {
        PyObject* f = PyNumber_Float(o);
        ret = PyFloat_AsDouble(f);
        Py_XDECREF(f);
    }
    return ret;
}

void goPython::set (const char* name, goFloat f)
{
    PyObject* n = PyString_FromString (name);
    PyObject* o = PyFloat_FromDouble (f);
    PyDict_SetItem (myPrivate->mainNamespace, n, o);
    Py_XDECREF(n);
    Py_XDECREF(o);
}

void goPython::set (const char* name, goDouble f)
{
    PyObject* n = PyString_FromString (name);
    PyObject* o = PyFloat_FromDouble (f);
    PyDict_SetItem (myPrivate->mainNamespace, n, o);
    Py_XDECREF(n);
    Py_XDECREF(o);
}
