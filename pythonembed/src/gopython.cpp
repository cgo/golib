/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gopython.h>
#include <golog.h>

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

namespace goPython
{
    goMutex mutex;
    PyObject* mainMod = 0;
    PyObject* mainNamespace = 0;
    bool initialised = false;
}

bool goPython::init ()
{
    goPython::mutex.lock ();
    if (initialised)
    {
        goPython::mutex.unlock ();
        return false;
    }
    
    Py_Initialize (); //= should be a no-op if already initialised.
    goPython::mainMod = PyImport_AddModule("__main__");
    goPython::mainNamespace = PyModule_GetDict(mainMod);
    goPython::initialised = true; 
    goPython::mutex.unlock ();
    return true;
}

bool goPython::final ()
{
    goPython::mutex.lock ();
    if (goPython::initialised)
    {
        Py_Finalize ();  //= FIXME Careful: add a reference count here
        goPython::initialised = false;
    }
    goPython::mutex.unlock ();
    return true;
}

PyObject* goPython::getMainMod ()
{
    return goPython::mainMod;
}

PyObject* goPython::getMainNamespace ()
{
    return goPython::mainNamespace;
}

#if 0
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
#endif

bool goPython::call (const goString& cmd)
{
    PyRun_SimpleString (cmd.toCharPtr());
    return true;
}

bool goPython::call (const char* cmd)
{
    PyRun_SimpleString (cmd);
    return true;
}

bool goPython::runFile (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f)
        return 0;

    if (PyRun_AnyFile (f, filename) == 0)
        return true;

    return false;
}

template <class T>
bool goPython::convert (PyObject* obj, goVector<T>& ret)
{
    if (PyList_Check (obj))
    {
        // Get size without error checking
        goSize_t sz = static_cast<goSize_t> (PyList_GET_SIZE (obj));
        ret.resize (sz); 
        PyObject* element = 0;
        for (goSize_t i = 0; i < sz; ++i)
        {
             element = PyList_GET_ITEM (obj, i);
             if (element)
             {
                 if (PyNumber_Check (element))
                 {
                     ret[i] = PyFloat_AsDouble (PyNumber_Float (element));
                 }
                 else
                 {
                     PyErr_SetString (PyExc_TypeError, "Can only put numeric objects into a goFixedArray");
                     return false;
                 }
             }
        }
        return true;
    }
    return false;
}

template bool goPython::convert<goFloat> (PyObject*, goVector<goFloat>&);

PyObject* goPython::run (const goString& cmd)
{
    PyObject* ret = PyRun_String (cmd.toCharPtr(), Py_file_input, goPython::mainNamespace, goPython::mainNamespace);
    if (!ret)
    {
        PyErr_Print();
    }
    return ret;
}

PyObject* goPython::getObject (const char* name)
{
    PyObject* tempName = PyString_FromString (name);
    PyObject* ret = PyDict_GetItem (goPython::mainNamespace, tempName);
    Py_XDECREF(tempName);
    return ret;
}

goFloat goPython::getFloat (const char* name)
{
    PyObject* o = goPython::getObject (name);
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
    PyObject* o = goPython::getObject (name);
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
    PyDict_SetItem (goPython::mainNamespace, n, o);
    Py_XDECREF(n);
    Py_XDECREF(o);
}

void goPython::set (const char* name, goDouble f)
{
    PyObject* n = PyString_FromString (name);
    PyObject* o = PyFloat_FromDouble (f);
    PyDict_SetItem (goPython::mainNamespace, n, o);
    Py_XDECREF(n);
    Py_XDECREF(o);
}


/** 
 * @brief Set goSignal3DBase<void> pointer.
 * 
 * @param name   Name of the variable.
 * @param o      Pointer to an object.
 * @param own_it If true, python will own that pointer (do not delete from C!). Default: false.
 */
#if 0
void goPython::set (const char* name, goSignal3DBase<void>* o, bool own_it)
{
    swig_type_info* info = SWIG_TypeQuery ("goSignal3DBase < void > *");
    if (!info)
    {
        goLog::error ("goPython::set: SWIG_TypeQuery() failed.");
        return;
    }
    int flags = 0;
    if (own_it)
        flags = SWIG_POINTER_OWN;
    PyObject* obj = SWIG_NewPointerObj (o, info, flags);

    PyObject* n = PyString_FromString (name);
    PyDict_SetItem (goPython::mainNamespace, n, obj);
    Py_XDECREF(n);
    Py_XDECREF(obj);
}
#endif


/** 
 * @brief 
 * 
 * @param name   Name of the variable.
 * @param own_it If true, the C code owns the returned pointer (and must delete it when done). Default: false.
 * 
 * @return The pointer to the object, or 0 if something failed.
 */
# if 0
goVectorf* goPython::getVectorf (const char* name, bool own_it)
{
    const char* type_str = "goVectorf *";
    return getSwigPointer<goVectorf> (name, own_it, type_str, this);
#if 0
    PyObject* o = this->getObject (name);
    if (!o)
        return 0;

    swig_type_info *info = SWIG_TypeQuery ("goVectorf *");
    if (!info)
        return 0;

    goVectorf* ptr = 0;
    int own;
    int flags = 0;
    if (own_it)
        flags = SWIG_POINTER_OWN;
    int ok = SWIG_Python_ConvertPtrAndOwn (o, (void**)&ptr, info, flags, &own);
    if (ok < 0)
        return 0;
    
    return ptr;
#endif
}
#endif

GOPYTHON_GETSET_DEFINITION (goVectorf, goVectorf, Vectorf);
GOPYTHON_GETSET_DEFINITION (goVectord, goVectord, Vectord);
GOPYTHON_GETSET_DEFINITION (goMatrixf, goMatrixf, Matrixf);
GOPYTHON_GETSET_DEFINITION (goMatrixd, goMatrixd, Matrixd);
GOPYTHON_GETSET_DEFINITION (goMatrix<goIndex_t>, goMatrixi, Matrixi);
GOPYTHON_GETSET_DEFINITION (goSignal3DBase<void>, goSignal3DBase < void >, Signal3DBase);
GOPYTHON_GETSET_DEFINITION (goSignal3D<void>, goSignal3D < void >, Signal3D);
GOPYTHON_GETSET_DEFINITION (goString, goString, String);


//goVectord* goPython::getVectord (const char* name, bool own_it)
//{
//    const char* type_str = "goVectord *";
//    return getSwigPointer<goVectord> (name, own_it, type_str, this);
//}

/** 
 * @brief Get all swig type identifier strings from
 * all swig modules.
 * 
 * The identifiers can be used e.g. in SWIG_TypeQuery(),
 * so they are the strings you can use in the GOPYTHON_GETSET_DEFINITION macro,
 * for example.
 *
 * @param ret The list of strings.
 */
void goPython::getAllSwigTypes (goList<goString>& ret)
{
    //= Check all types in all modules:
    swig_module_info* module = SWIG_GetModule ();
    swig_module_info* first_module = module;
    do
    {
        printf ("%d\n", module->size);
        for (size_t i = 0; i < module->size; ++i)
        {
            if (module->types[i]->str)
                ret.append (goString(module->types[i]->str));
            else
                ret.append (goString(module->types[i]->name));
        }
        module = module->next;
    } while (module != first_module);
}
