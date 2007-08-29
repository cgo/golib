#ifndef GOPYTHON_H
#define GOPYTHON_H

#include <Python.h>
#include <gostring.h>

class goPythonPrivate;

class goPython
{
    public:
        goPython ();
        virtual ~goPython ();

        bool call (const goString& cmd);
        bool call (const char* cmd);
        PyObject* run (const goString& cmd);

        PyObject* getObject (const char* name);
        goFloat   getFloat (const char* name);
        goDouble  getDouble (const char* name);
        void      set (const char* name, goFloat value);
        void      set (const char* name, goDouble value);
        
    private:
        goPythonPrivate* myPrivate;
};

#endif
