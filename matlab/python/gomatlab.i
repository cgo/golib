/* example.i */
%module gomatlab

%{
#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <godwt3d.h>
#include <goarray.h>
#include <gofixedarray.h>
#include <gostring.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <goexception.h>
#include <gomatrix.hpp>
#include <gomatrix.h>
#include <govector.h>
#include <goplot.h>
#include <gohistogram.h>
#include <gopointcloud.h>
#include <gocurve.h>
#include <gognuplot.h>
%}
%import ../../python/golib.i

%{
#include <engine.h>
#include <gomatlab.h>
%}

%include <gomatlab.h>

%extend goMatlab
{
    bool putMatrixf (const goMatrix<goFloat>& m, const char* name)
    {
        return self->putMatrix (m, name);
    };
    bool putMatrixd (const goMatrix<goDouble>& m, const char* name)
    {
        return self->putMatrix (m, name);
    };
    bool getMatrixf (goMatrix<goFloat>& m, const char* name)
    {
        return self->getMatrix (m, name);
    };
    bool getMatrixd (goMatrix<goDouble>& m, const char* name)
    {
        return self->getMatrix (m, name);
    };

    bool getVectorf (goVector<goFloat>& v, const char* name)
    {
        return self->getVector (&v, name);
    };
    bool getVectord (goVector<goDouble>& v, const char* name)
    {
        return self->getVector (&v, name);
    };
    bool putVectorf (const goVector<goFloat>& v, const char* name)
    {
        return self->putVector (&v, name);
    };
    bool putVectord (const goVector<goDouble>& v, const char* name)
    {
        return self->putVector (&v, name);
    };
}
