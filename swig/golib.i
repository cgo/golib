/* example.i */
%goops %{ 
   (load-extension "./libgolib_guile.so" "scm_init_golib_guile_module") 
   (primitive-load "./golib_guile-primitive.scm") 
   (primitive-load "./common.scm")
%}
%scheme %{ (load-extension "./libgolib_guile.so" "scm_init_golib_guile_module") %}
// only include the following definition if (my modules foo) cannot
// be loaded automatically
//%goops %{ 
//  (primitive-load "./golib_guile-primitive.scm") 
//  (primitive-load "./common.scm")
//%}

%module golib_guile
%{
#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gostring.h>
#include <gofileio.h>
#include <gosignalhelper.h>
%}

%ignore goSignal3DBase<void>::shiftLeftDiff (int,int);
%ignore goSignal3DBase<void>::shiftRightDiff (int,int);
%ignore goSignal3DBase<void>::shiftLeftSize (int,int);
%ignore goSignal3DBase<void>::shiftRightSize (int,int);

%include <goconfig.h>
%include <gotypes.h>
%include <gosignal3dbase.h>
%include <gosignal3d.h>
%include <goobjectbase.h>
%include <gostring.h>
%include <gofileio.h>
%include <gosignalhelper.h>

%template(gosignal3dbase) goSignal3DBase<void>; 
%template(gosignal3d)     goSignal3D<void>; 

// %rename(readImage) goFileIO::readImage;
