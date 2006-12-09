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
%}
%import ../../python/golib.i

%{
#include <engine.h>
#include <gomatlab.h>
%}

%include <gomatlab.h>
