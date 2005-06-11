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
#include <goexception.h>
#include <goarray.h>
%}

%ignore goSignal3DBase<void>::shiftLeftDiff (int,int);
%ignore goSignal3DBase<void>::shiftRightDiff (int,int);
%ignore goSignal3DBase<void>::shiftLeftSize (int,int);
%ignore goSignal3DBase<void>::shiftRightSize (int,int);

%include <goobjectbase.h>
%include <gostring.h>
%include <goconfig.h>
%include <gotypes.h>
%include <gosignal3dbase.h>
%include <gosignal3d.h>
%include <gofileio.h>
%include <gosignalhelper.h>
%include <goarray.h>

%template(goArrayf)        goArray<goFloat>;
//%template(goArrayd)        goArray<goDouble>;
//%template(goArrayi)        goArray<goInt32>;
// Renaming and templates
%rename(make_signal) goSignal3D<void>::make;
%template(goSignal3DBasev) goSignal3DBase<void>; 
%template(goSignal3Dv)     goSignal3D<void>;
%rename(golib_readImage) goFileIO::readImage;

// Exceptions
%exception goFileIO::readImage {
    try {
        $action
    }
    catch (goFileIOException ex) {
        if (ex.code == goFileIOException::NOT_FOUND)
        {
            printf ("readImage: not found.\n");
        }
    }
    catch (goTypeException ex) {
        if (ex.code == goTypeException::WRONG_TYPE)
        {
            printf ("readImage: wrong data type.\n");
        }
        if (ex.code == goTypeException::UNKNOWN_TYPE)
        {
            printf ("readImage: unknown data type.\n");
        }
    }
}
%exception goFileIO::writeImage {
    try {
        $action
    }
    catch (goFileIOException ex) {
        if (ex.code == goFileIOException::NOT_FOUND)
        {
            printf ("readImage: not found.\n");
        }
    }
    catch (goTypeException ex) {
        if (ex.code == goTypeException::WRONG_TYPE)
        {
            printf ("readImage: wrong data type.\n");
        }
        if (ex.code == goTypeException::UNKNOWN_TYPE)
        {
            printf ("readImage: unknown data type.\n");
        }
    }
}
