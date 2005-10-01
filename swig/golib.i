/* example.i */
%scheme %{ (load-extension "libgolib_guile.so" "scm_init_golib_guile_module") %}
%goops %{ 
   (primitive-load-path "golib_guile-primitive.scm") 
   (primitive-load-path "common.scm")
%}

%module golib_guile
%{
#include <goconfig.h>
#include <gotypes.h>
#include <goobjectbase.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <goarray.h>
#include <gostring.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <goexception.h>
#include <goshape.h>
#include <golibguile.h>
%}

%ignore goSignal3DBase<void>::shiftLeftDiff (int,int);
%ignore goSignal3DBase<void>::shiftRightDiff (int,int);
%ignore goSignal3DBase<void>::shiftLeftSize (int,int);
%ignore goSignal3DBase<void>::shiftRightSize (int,int);

// Renaming
// %rename(re_size) goString::resize;
// %rename(re_size) goArray<goFloat>::resize;
%rename(go_sort) goArray::sort;
%rename(make_signal) goSignal3D<void>::make;
//%rename(golib_readImage) goFileIO::readImage;
//%rename(golib_writeImage) goFileIO::writeImage;

%include <goobjectbase.h>
%include <gostring.h>
%include <goconfig.h>
%include <gotypes.h>
%include <gosignal3dbase.h>
%include <gosignal3d.h>
%include <gofileio.h>
%include <gosignalhelper.h>
%include <goarray.h>
%include <goshape.h>
%include <golibguile.h>

// Templates
%template(goArrayf)        goArray<goFloat>;
%template(goArrayd)        goArray<goDouble>;
%template(goArrayi)        goArray<goInt32>;
%template(goSignal3DBasev) goSignal3DBase<void>; 
%template(goSignal3Dv)     goSignal3D<void>;

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
        if (ex.code == goFileIOException::EXISTS)
        {
            printf ("readImage: file exists.\n");
        }
        return false;
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
        return false;
    }
}
