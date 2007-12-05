/* example.i */
%module gogl

%feature("autodoc","1");
%include "typemaps.i"

%{
#include <gogl/helper.h>
#include <gogl/offfile.h>
#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignal3dref.h>
#include <godwt3d.h>
#include <goarray.h>
#include <gofixedarray.h>
#include <gostring.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <goexception.h>
// #include <gomatrix.hpp>
#include <gomatrix.h>
#include <govector.h>
#include <goplot.h>
#include <gohistogram.h>
#include <gopointcloud.h>
#include <gocurve.h>
#include <gognuplot.h>
#include <gofilter1d.h>
#include <golist.h>
#include <gotimerobject.h>
%}
%import ../../python/golib.i
%include <gogl/helper.h>
%include <gogl/offfile.h>

