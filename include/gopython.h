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
        
    private:
        goPythonPrivate* myPrivate;
};

#endif
