/* example.i */

%module golib
// %include exception.i

%feature("autodoc","1");

%include "typemaps.i"

// Exceptions
%exception goFileIO::readImage {
    try {
        $action
    }
    catch (goFileIOException ex) {
        if (ex.code == goFileIOException::NOT_FOUND)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.readImage(): file not found.");
            return false;
        }
        if (ex.code == goFileIOException::FAILED)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.readImage(): failed to read image.");
            return false;
        }
        return false;
    }
    catch (goTypeException ex) {
        if (ex.code == goTypeException::WRONG_TYPE)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.readImage(): wrong data type.");
            return false;
        }
        if (ex.code == goTypeException::UNKNOWN_TYPE)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.readImage(): unknown data type.");
            return false;
        }
        return false;
    }
}

%exception goFileIO::writeImage {
    try {
        $action
    }
    catch (goFileIOException ex) {
        if (ex.code == goFileIOException::NOT_FOUND)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.writeImage(): file not found.");
            return false;
            // printf ("readImage: not found.\n");
        }
        if (ex.code == goFileIOException::EXISTS)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.writeImage(): file exists.");
            return false;
            // printf ("readImage: file exists.\n");
        }
        return false;
    }
    catch (goTypeException ex) {
        if (ex.code == goTypeException::WRONG_TYPE)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.writeImage(): wrong data type.");
            return false;
            // printf ("readImage: wrong data type.\n");
        }
        if (ex.code == goTypeException::UNKNOWN_TYPE)
        {
            PyErr_SetString(PyExc_IOError, "goFileIO.writeImage(): unknown data type.");
            return false;
            // printf ("readImage: unknown data type.\n");
        }
        return false;
    }
}

%{
#include <goconfig.h>
#include <gotypes.h>
#include <gotype.h>
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
#include <goautoptr.h>
#include <golist.h>
#include <gopointcloud.h>
#include <gocurve.h>
#include <gomath.h>
#include <golu.h>
#include <gosvd.h>
#include <goresample.h>
#include <gorandom.h>
#include <gognuplot.h>
#include <gofilter1d.h>
%}

%ignore goSignal3DBase<void>::shiftLeftDiff (int,int);
%ignore goSignal3DBase<void>::shiftRightDiff (int,int);
%ignore goSignal3DBase<void>::shiftLeftSize (int,int);
%ignore goSignal3DBase<void>::shiftRightSize (int,int);

// Renaming
// %rename(re_size) goString::resize;
// %rename(re_size) goArray<goFloat>::resize;
// %rename("goString-eq") operator==(const goString& str,const goString& s);
// %rename("goString-eq?") bool operator==(const goString& str,const char* s);
//%rename("set") goString::operator=(const char*);
//%rename("to-string") goString::toCharPtr() const;
//%rename("mult") goMatrix<goFloat>::operator*(const goMatrix<goFloat>& other);
//%rename("set") goMatrix<goFloat>::operator=(const goMatrix<goFloat>&);
//%rename(go_sort) goArray::sort;
//%rename(make_signal) goSignal3D<void>::make;
//%rename(golib_readImage) goFileIO::readImage;
//%rename(golib_writeImage) goFileIO::writeImage;

%rename(_print) goMatrix<goFloat>::print;
%rename(_print) goMatrix<goDouble>::print;

%include <goobjectbase.h>
%include <gostring.h>
%include <goconfig.h>
%include <gotypes.h>
%include <gotype.h>
%include <gosignal3dbase.h>
%include <gosignal3d.h>
%include <godwt3d.h>
%include <gofileio.h>
%include <gosignalhelper.h>
%include <goarray.h>
%include <gofixedarray.h>
%include <gomatrix.h>
%include <govector.h>
%include <goplot.h>
%include <gohistogram.h>
%include <goautoptr.h>
%include <gopointcloud.h>
%include <gocurve.h>
%include <golist.h>
%include <gomath.h>
%include <golu.h>
%include <gosvd.h>
%include <goresample.h>
%include <gorandom.h>
%include <gognuplot.h>
%include <gofilter1d.h>

%extend goString
{
    char *__str__ ()
    {
        return self->getPtr();
    }
};
/// Matrix set/get functions -- operator() is not supported in python.
%extend goMatrix<goFloat>
{
    %pythoncode %{
        def setArray (self, A):
            rows = len(A)
            if (rows < 1):
                return
            cols = len(A[0])
            if cols < 1:
                return
            if rows != self.getRows() or cols != self.getColumns():
                self.resize(rows,cols)
            for i in xrange(rows):
                for j in xrange(cols):
                    self.set(i,j,A[i][j])
    %}

    // goFloat __getitem__(int i)
    // {
    //     return (*self)[i];
    // };
    // void __setitem__(int i, goFloat f)
    // {
    //     (*self)[i] = f;
    // };
    char *__str__()
    {
        static goString str;
        str = "";
        // str = "goMatrixf:\n";
        goSize_t r = self->getRows();
        goSize_t c = self->getColumns();
        for (goSize_t i = 0; i < r; ++i)
        {
            for (goSize_t j = 0; j < c; ++j)
            {
                str += (float)(*self)(i,j);
                if (j < c-1)
                    str += " ";
            }
            str += "\n";
        }
        return str.getPtr();
    };

    goMatrixf __pow__ (goFloat e)
    {
        goMatrixf ret (*self);
        ret.power(e);
        return ret;
    };

    void set (goSize_t r, goSize_t c, goFloat value)
    {
        (*self)(r,c) = value;
    }
    goFloat get (goSize_t r, goSize_t c)
    {
        return (*self)(r,c);
    }
};
%extend goMatrix<goDouble>
{
    %pythoncode %{
        def setArray (self, A):
            rows = len(A)
            if (rows < 1):
                return
            cols = len(A[0])
            if cols < 1:
                return
            if rows != self.getRows() or cols != self.getColumns():
                self.resize(rows,cols)
            for i in xrange(rows):
                for j in xrange(cols):
                    self.set(i,j,A[i][j])
    %}
    char *__str__()
    {
        static goString str;
        str = "";
        // str = "goMatrixd:\n";
        goSize_t r = self->getRows();
        goSize_t c = self->getColumns();
        for (goSize_t i = 0; i < r; ++i)
        {
            for (goSize_t j = 0; j < c; ++j)
            {
                str += (float)(*self)(i,j);
                if (j < c-1)
                    str += " ";
            }
            str += "\n";
        }
        return str.getPtr();
    };
    goMatrixd __pow__ (goDouble e)
    {
        goMatrixd ret (*self);
        ret.power(e);
        return ret;
    };

    void set (goSize_t r, goSize_t c, goDouble value)
    {
        (*self)(r,c) = value;
    }
    goDouble get (goSize_t r, goSize_t c)
    {
        return (*self)(r,c);
    }
};
%extend goMatrix<goIndex_t>
{
    %pythoncode %{
        def setArray (self, A):
            rows = len(A)
            if (rows < 1):
                return
            cols = len(A[0])
            if cols < 1:
                return
            if rows != self.getRows() or cols != self.getColumns():
                self.resize(rows,cols)
            for i in xrange(rows):
                for j in xrange(cols):
                    self.set(i,j,A[i][j])
    %}

    char *__str__()
    {
        static goString str;
        str = "";
        // str = "goMatrixi:\n";
        goSize_t r = self->getRows();
        goSize_t c = self->getColumns();
        for (goSize_t i = 0; i < r; ++i)
        {
            for (goSize_t j = 0; j < c; ++j)
            {
                str += (int)(*self)(i,j);
                if (j < c-1)
                    str += " ";
            }
            str += "\n";
        }
        return str.getPtr();
    };
    void set (goSize_t r, goSize_t c, goIndex_t value)
    {
        (*self)(r,c) = value;
    }
    goIndex_t get (goSize_t r, goSize_t c)
    {
        return (*self)(r,c);
    }
};

/// Set/get functions for the goArray
%extend goArray<goFloat>
{
    void set (goIndex_t i, goFloat v)
    {
        (*self)[i] = v;
    }
    goFloat get (goIndex_t i)
    {
        return (*self)[i];
    }
}
%extend goArray<goDouble>
{
    void set (goIndex_t i, goDouble v)
    {
        (*self)[i] = v;
    }
    goDouble get (goIndex_t i)
    {
        return (*self)[i];
    }
}
%extend goArray<goInt32>
{
    void set (goIndex_t i, goInt32 v)
    {
        (*self)[i] = v;
    }
    goInt32 get (goIndex_t i)
    {
        return (*self)[i];
    }
}
%extend goFixedArray<goFloat>
{
    %pythoncode %{
        def setArray (self, A):
            if len(A) != self.getSize():
                self.setSize(len(A))
            for i in xrange(len(A)):
                self[i] = A[i]
    %}
    goFloat __getitem__(int i)
    {
        if (i < 0 || i >= self->getSize())
        {
            PyErr_SetString(PyExc_ValueError, "goVector: range error.");
        }
        return (*self)[i];
    };
    void __setitem__(int i, goFloat f)
    {
        if (i < 0 || i >= self->getSize())
        {
            PyErr_SetString(PyExc_ValueError, "goVector: range error.");
        }
        (*self)[i] = f;
    };
    char *__str__()
    {
        static goString str;
        str = "";
        goSize_t sz = self->getSize();
        for (goSize_t i = 0; i < sz; ++i)
        {
            str += (float)(*self)[i];
            if (i < sz-1)
                str += " ";
        }
        return str.getPtr();
    };
    unsigned int __len__()
    {
        return self->getSize();
    };

    goFloat mean ()
    {
        return goMath::mean<goFixedArray<goFloat>,goFloat> (*self, self->getSize());
    }
    
    void set (goIndex_t i, goFloat v)
    {
        (*self)[i] = v;
    }
    goFloat get (goIndex_t i)
    {
        return (*self)[i];
    }
    void _print ()
    {
        goSize_t sz = (*self).getSize();
        for (goSize_t i = 0; i < sz; ++i)
            printf ("%f ", (*self)[i]);
        printf ("\n");
    }
}
%extend goFixedArray<goDouble>
{
    %pythoncode %{
        def setArray (self, A):
            if len(A) != self.getSize():
                self.setSize(len(A))
            for i in xrange(len(A)):
                self[i] = A[i]
    %}

    goDouble __getitem__(int i)
    {
        return (*self)[i];
    };
    void __setitem__(int i, goDouble f)
    {
        (*self)[i] = f;
    };
    char *__str__()
    {
        static goString str;
        str = "";
        goSize_t sz = self->getSize();
        for (goSize_t i = 0; i < sz; ++i)
        {
            str += (float)(*self)[i];
            if (i < sz-1)
                str += " ";
        }
        return str.getPtr();
    };
    unsigned int __len__()
    {
        return self->getSize();
    };

    goDouble mean ()
    {
        return goMath::mean<goFixedArray<goDouble>,goDouble> (*self, self->getSize());
    }

    void set (goIndex_t i, goDouble v)
    {
        (*self)[i] = v;
    }
    goDouble get (goIndex_t i)
    {
        return (*self)[i];
    }
    void _print ()
    {
        goSize_t sz = (*self).getSize();
        for (goSize_t i = 0; i < sz; ++i)
            printf ("%f ", (*self)[i]);
        printf ("\n");
    }
}
%extend goFixedArray<goSize_t>
{
    %pythoncode %{
        def setArray (self, A):
            if len(A) != self.getSize():
                self.setSize(len(A))
            for i in xrange(len(A)):
                self[i] = A[i]
    %}
    goSize_t __getitem__(int i)
    {
        return (*self)[i];
    };
    void __setitem__(int i, goSize_t f)
    {
        (*self)[i] = f;
    };
    char *__str__()
    {
        static goString str;
        str = "";
        goSize_t sz = self->getSize();
        for (goSize_t i = 0; i < sz; ++i)
        {
            str += (int)(*self)[i];
            if (i < sz-1)
                str += " ";
        }
        return str.getPtr();
    };
    unsigned int __len__()
    {
        return self->getSize();
    };

    void set (goIndex_t i, goSize_t v)
    {
        (*self)[i] = v;
    }
    goSize_t get (goIndex_t i)
    {
        return (*self)[i];
    }
    void _print ()
    {
        goSize_t sz = (*self).getSize();
        for (goSize_t i = 0; i < sz; ++i)
            printf ("%d ", (*self)[i]);
        printf ("\n");
    }
}
%extend goFixedArray<bool>
{
    %pythoncode %{
        def setArray (self, A):
            if len(A) != self.getSize():
                self.setSize(len(A))
            for i in xrange(len(A)):
                self[i] = A[i]
    %}

    bool __getitem__(int i)
    {
        return (*self)[i];
    };
    void __setitem__(int i, bool f)
    {
        (*self)[i] = f;
    };
    char *__str__()
    {
        static goString str;
        str = "";
        goSize_t sz = self->getSize();
        for (goSize_t i = 0; i < sz; ++i)
        {
            str += (*self)[i] ? "True" : "False";
            if (i < sz-1)
                str += " ";
        }
        return str.getPtr();
    };
    unsigned int __len__()
    {
        return self->getSize();
    };

    void set (goIndex_t i, bool v)
    {
        (*self)[i] = v;
    }
    bool get (goIndex_t i)
    {
        return (*self)[i];
    }
    void _print ()
    {
        goSize_t sz = (*self).getSize();
        for (goSize_t i = 0; i < sz; ++i)
            printf ("%s ", (*self)[i] ? "True" : "False");
        printf ("\n");
    }
}
%extend goFixedArray<goAutoPtr<goSignal3D<void> > >
{
    %pythoncode %{
        def setArray (self, A):
            if len(A) != self.getSize():
                self.setSize(len(A))
            for i in xrange(len(A)):
                self[i] = A[i]
    %}

    goAutoPtr<goSignal3D<void> > __getitem__(int i)
    {
        return (*self)[i];
    };
    void __setitem__(int i, goAutoPtr<goSignal3D<void> > f)
    {
        (*self)[i] = f;
    };
    char *__str__()
    {
        static goString str;
        str = "";
        goSize_t sz = self->getSize();
        for (goSize_t i = 0; i < sz; ++i)
        {
            str += "goAutoPtr<goSignal3D>";
            if (i < sz-1)
                str += " ";
        }
        return str.getPtr();
    };
    unsigned int __len__()
    {
        return self->getSize();
    };

    void set (goIndex_t i, goAutoPtr<goSignal3D<void> > v)
    {
        (*self)[i] = v;
    }
    goAutoPtr<goSignal3D<void> > get (goIndex_t i)
    {
        return (*self)[i];
    }
}

%extend goVector<goFloat>
{

    bool copy (goVector<goFloat>& target, goIndex_t startIndex=0, goIndex_t skip=0, goIndex_t lastIndex = -1) const
    {
        return (*self).copy (target, startIndex, skip, lastIndex);
    };
    // FIXME: Add +-*= operators (can I instantiate the members explicitly?)
    goVector<goFloat>& operator+= (const goVector<goFloat>& other)
    {
        return (*self) += other;
    };
    goVector<goFloat>& operator+= (const goVector<goDouble>& other)
    {
        return (*self) += other;
    };
    goVector<goFloat> operator+ (const goVector<goFloat>& other)
    {
        return (*self) + other;
    };
    goVector<goFloat> operator+ (const goVector<goDouble>& other)
    {
        return (*self) + other;
    };
    goVector<goFloat>& operator-= (const goVector<goFloat>& other)
    {
        return (*self) -= other;
    };
    goVector<goFloat>& operator-= (const goVector<goDouble>& other)
    {
        return (*self) -= other;
    };
    goVector<goFloat> operator- (const goVector<goFloat>& other)
    {
        return (*self) - other;
    };
    goVector<goFloat> operator- (const goVector<goDouble>& other)
    {
        return (*self) - other;
    };
    goVector<goFloat> operator* (goFloat n)
    {
        return (*self) * n;
    };
    goVector<goFloat> operator* (goDouble n)
    {
        return (*self) * n;
    };
    goVector<goFloat>& operator*= (const goVector<goFloat>& other)
    {
        (*self) *= other;
        return (*self);
    };
    goVector<goFloat>& operator*= (const goVector<goDouble>& other)
    {
        (*self) *= other;
        return (*self);
    };
}
%extend goVector<goDouble>
{
    bool copy (goVector<goDouble>& target, goIndex_t startIndex=0, goIndex_t skip=0, goIndex_t lastIndex = -1) const
    {
        return (*self).copy (target, startIndex, skip, lastIndex);
    };
    // FIXME: Add +-*= operators (can I instantiate the members explicitly?)
    goVector<goDouble>& operator+= (const goVector<goFloat>& other)
    {
        return (*self) += other;
    };
    goVector<goDouble>& operator+= (const goVector<goDouble>& other)
    {
        return (*self) += other;
    };
    goVector<goDouble> operator+ (const goVector<goFloat>& other)
    {
        return (*self) + other;
    };
    goVector<goDouble> operator+ (const goVector<goDouble>& other)
    {
        return (*self) + other;
    };
    goVector<goDouble>& operator-= (const goVector<goFloat>& other)
    {
        return (*self) -= other;
    };
    goVector<goDouble>& operator-= (const goVector<goDouble>& other)
    {
        return (*self) -= other;
    };
    goVector<goDouble> operator- (const goVector<goFloat>& other)
    {
        return (*self) - other;
    };
    goVector<goDouble> operator- (const goVector<goDouble>& other)
    {
        return (*self) - other;
    };
    goVector<goDouble> operator* (goFloat n)
    {
        return (*self) * n;
    };
    goVector<goDouble> operator* (goDouble n)
    {
        return (*self) * n;
    };
    goVector<goDouble>& operator*= (const goVector<goFloat>& other)
    {
        (*self) *= other;
        return (*self);
    };
    goVector<goDouble>& operator*= (const goVector<goDouble>& other)
    {
        (*self) *= other;
        return (*self);
    };
}

%extend goCurve<goFloat>
{
// No idea why this does not work directly in swig 1.3.31 .. try in next version?
    goDouble get_length ()
    {
        return (*self).getLength();
    }
    bool resample_partial (goDouble start, goDouble end, goSize_t N, goList<goVector<goFloat> >& points)
    {
        return self->resample (start, end, N, points);
    }
}

%extend goSinglePlot
{
     bool add_curve (const goFixedArray<goFloat>& y, const char* title, const char* plotOptions = 0)
     {
         return self->addCurve (y, title, plotOptions);
     };
     bool add_curve (const goFixedArray<goDouble>& y, const char* title, const char* plotOptions = 0)
     {
         return self->addCurve (y, title, plotOptions);
     };
     bool add_curve (const goList<goVector<goFloat> >& points, const char* title, const char* plotOptions = 0)
     {
        goList<goVector<goFloat> >::ConstElement* el = points.getFrontElement();
        goSize_t sz = points.getSize();
        goSize_t szVec = sz;
        if (points.isClosed())
        {
            ++szVec;
        }
        goVectord x(szVec);
        goVectord y(szVec);
        goSize_t i = 0;
        while (i < sz && el)
        {
            x[i] = el->elem[0];
            y[i] = el->elem[1];
            ++i;
            el = el->next;
        }
        if (points.isClosed())
        {
            x[szVec-1] = x[0];
            y[szVec-1] = y[0];
        }
        return self->addCurve (x,y,title,plotOptions);
    };
     bool add_curve (const goList<goVector<goDouble> >& points, const char* title, const char* plotOptions = 0)
     {
        goList<goVector<goDouble> >::ConstElement* el = points.getFrontElement();
        goSize_t sz = points.getSize();
        goSize_t szVec = sz;
        if (points.isClosed())
        {
            ++szVec;
        }
        goVectord x(szVec);
        goVectord y(szVec);
        goSize_t i = 0;
        while (i < sz && el)
        {
            x[i] = el->elem[0];
            y[i] = el->elem[1];
            ++i;
            el = el->next;
        }
        if (points.isClosed())
        {
            x[szVec-1] = x[0];
            y[szVec-1] = y[0];
        }
        return self->addCurve (x,y,title,plotOptions);
    };
}

%extend goCurve<goFloat>
{
    // Why is this needed????  -- python gives error when using resample() directly.
    bool resample_curve (goIndex_t pointCount, goCurve<goFloat>& ret)
    {
        return self->resample (pointCount, ret);
    };
}

%extend goPointCloud<goFloat>
{
    bool getCenterOfMass (goVector<goFloat>& ret)
    {
        return (*self).getCenterOfMass (ret);
    };
}

%extend goPointCloud<goDouble>
{
    bool getCenterOfMass (goVector<goDouble>& ret)
    {
        return (*self).getCenterOfMass (ret);
    };
}

%apply float *OUTPUT {float *phiRet, float *thetaRet, float *radiusRet};
%inline %{
    // Namespaces are not used, but maybe they will in a future swig?
    namespace goMath {
        void euclideanToSphere (const goVectorf& x, float *phiRet, float *thetaRet, float *radiusRet)
        {
            goFloat phi, theta, radius;
            goMath::euclideanToSphere (x, phi, theta, radius);
            *phiRet = phi;
            *thetaRet = theta;
            *radiusRet = radius;
        }
        //goVectorf sphereToEuclidean (goFloat phi, goFloat theta, goFloat radius)
        //{
        //    goVectorf ret(3);
        //    static goVectorf dummy(3);
        //    goMath::sphereToEuclidean (phi, theta, radius, &ret, &dummy);
        //    return ret;
        //}
        //void sphereToEuclidean (goFloat phi, goFloat theta, goFloat radius, goVectorf* posRet, goVectorf* upRet)
        //{
        //    goMath::sphereToEuclidean (phi, theta, radius, posRet, upRet);
        //}
    };
    void goCenterOfMassf (const goMatrix<goFloat>& m, goVector<goFloat>& v)
    {
        goMath::centerOfMass (m, v);
    };
    void goCenterOfMassd (const goMatrix<goDouble>& m, goVector<goDouble>& v)
    {
        goMath::centerOfMass (m, v);
    };
%}

%pythoncode %{
        def integrate(vector):
            if isinstance(vector, goVectorf):
                return goIntegratef (vector)
            elif isinstance(vector, goVectord):
                return goIntegrated (vector)
            else:
                raise TypeError 

        def integrateSimpson(vector):
            if isinstance(vector, goVectorf):
                return goIntegrateSimpsonf (vector)
            elif isinstance(vector, goVectord):
                return goIntegrateSimpsond (vector)
            else:
                raise TypeError 

        def centerOfMass(matrix):
            if isinstance(matrix, goMatrixf):
                com = goVectorf()
                goCenterOfMassf (matrix, com)
                return com
            if isinstance(matrix, goMatrixd):
                com = goVectord()
                goCenterOfMassd (matrix, com)
                return com

        def translate(matrix, vec):
            if isinstance(matrix, goMatrixf):
                goTranslatef (matrix, vec)
            if isinstance(matrix, goMatrixd):
                goTranslated (matrix, vec)

        def barycentricToEuclidean (simplexMatrix, baryCoord):
            if isinstance(simplexMatrix, goMatrixf):
                ret = goVectorf(2)
                bc = goVectorf(3)
                bc.setArray (baryCoord)
                goBarycentricToEuclideanf (simplexMatrix, bc, ret)
                return ret
            elif isinstance(simplexMatrix, goMatrixd):
                ret = goVectord(2)
                bc = goVectord(3)
                bc.setArray (baryCoord)
                goBarycentricToEuclideand (simplexMatrix, bc, ret)
                return ret
            else:
                raise TypeError

        def euclideanToBarycentric (simplexMatrix, coord):
            if isinstance(simplexMatrix, goMatrixf):
                ret = goVectorf(2)
                bc = goVectorf(2)
                bc.setArray (coord)
                goEuclideanToBarycentricf (simplexMatrix, bc, ret)
                return ret
            elif isinstance(simplexMatrix, goMatrixd):
                ret = goVectord(2)
                bc = goVectord(2)
                bc.setArray (coord)
                goEuclideanToBarycentricd (simplexMatrix, bc, ret)
                return ret
            else:
                raise TypeError

        def sphereToEuclidean (phi, theta, radius):
            posRet = goVectord()
            upRet  = goVectord()
            goSphereToEuclideand (phi, theta, radius, posRet, upRet)
            return posRet, upRet

        def planeLineCut (planeNormal, planePoint, lineDirection, linePoint):
            if isinstance(planeNormal,goVectorf):
                ret = goVectorf()
                goPlaneLineCutf (planeNormal, planePoint, lineDirection, linePoint, ret)
                return ret
            elif isinstance(planeNormal,goVectord):
                ret = goVectord()
                goPlaneLineCutd (planeNormal, planePoint, lineDirection, linePoint, ret)
                return ret
            else:
                n0,n1,p0,p1 = goVectord(),goVectord(),goVectord(),goVectord()
                n0.setArray (planeNormal)
                n1.setArray (lineDirection)
                p0.setArray (planePoint)
                p1.setArray (linePoint)
                temp = goVectord()
                goPlaneLineCutd (n0, p0, n1, p1, temp)
                ret = []
                for i in xrange(len(temp)):
                    ret.append(temp[i])
                return ret
%}

// Auto pointers
%template(goAutoPtrSignal3D) goAutoPtr<goSignal3D<void> >;

// Templates

%template(affineMatchf) goMath::affineMatch<goFloat>;
%template(goResampleLinearf) goResampleLinear<goFloat>;
%template(goResampleLineard) goResampleLinear<goDouble>;

%template(goResampleVectorLinearf) goMath::resampleLinear<goFloat>;
%template(goResampleVectorLineard) goMath::resampleLinear<goDouble>;

%template(goResampleCubicf) goMath::resampleCubic<goFloat>;
%template(goResampleCubicd) goMath::resampleCubic<goDouble>;


%template(goTranslatef) goMath::translate<goFloat>;
%template(goTranslated) goMath::translate<goDouble>;

%template(goSignal3DBasev) goSignal3DBase<void>; 
%template(goSignal3Dv)     goSignal3D<void>;

%template(goFixedArrayf)   goFixedArray<goFloat>;
%template(goFixedArrayd)   goFixedArray<goDouble>;
%template(goFixedArraySize_t)   goFixedArray<goSize_t>;
%template(goFixedArrayBool)   goFixedArray<bool>;
%template(goFixedArraySignal3D)   goFixedArray<goAutoPtr<goSignal3D<void> > >;

%template(goVectorf)       goVector<goFloat>;
%template(goVectord)       goVector<goDouble>;
%template(goArrayf)        goArray<goFloat>;
%template(goArrayd)        goArray<goDouble>;
%template(goArrayi)        goArray<goInt32>;
%template(goMatrixf)       goMatrix<goFloat>;
%template(goMatrixd)       goMatrix<goDouble>;
%template(goMatrixi)       goMatrix<goIndex_t>;
%template(goMatrixMultf)   goMatrixMult<goFloat>;
%template(goMatrixMultd)   goMatrixMult<goDouble>;
%template(goMatrixVectorMultf)   goMatrixVectorMult<goFloat>;
%template(goMatrixVectorMultd)   goMatrixVectorMult<goDouble>;

%template(goHistogramf)    goHistogram<goFloat>;
%template(goHistogramd)    goHistogram<goDouble>;

%template(goListgoVectorf)  goList<goVector<goFloat> >;
%template(goListgoVectord)  goList<goVector<goDouble> >;
%template(goListgoMatrixf)  goList<goMatrixf>;
%template(goListgoMatrixd)  goList<goMatrixd>;
%template(goPointCloudf)   goPointCloud<goFloat>;
%template(goPointCloudd)   goPointCloud<goDouble>;
%template(goCurvef)        goCurve<goFloat>;
%template(goCurved)        goCurve<goDouble>;
%template(goListgoCurvef)  goList<goCurve<goFloat> >;
%template(goListgoCurved)  goList<goCurve<goDouble> >;
%template(goSVDf)          goMath::SVD<goFloat>;
%template(goThinSVDf)      goMath::ThinSVD<goFloat>;
%template(goSVDd)          goMath::SVD<goDouble>;
%template(goThinSVDd)      goMath::ThinSVD<goDouble>;
%template(goLUf)           goMath::LU<goFloat>;
%template(goLUd)           goMath::LU<goDouble>;

%template(goSinf)          goMath::sin<goFloat>;
%template(goSind)          goMath::sin<goDouble>;
%template(goCosf)          goMath::cos<goFloat>;
%template(goCosd)          goMath::cos<goDouble>;
%template(goAsinf)         goMath::asin<goFloat>;
%template(goAsind)         goMath::asin<goDouble>;
%template(goAcosf)         goMath::acos<goFloat>;
%template(goAcosd)         goMath::acos<goDouble>;
%template(goExpf)          goMath::exp<goFloat>;
%template(goExpd)          goMath::exp<goDouble>;
%template(goLogf)          goMath::log<goFloat>;
%template(goLogd)          goMath::log<goDouble>;

%template(goPlaneLineCutf) goMath::planeLineCut<goFloat>;
%template(goPlaneLineCutd) goMath::planeLineCut<goDouble>;

%template(goBarycentricToEuclideanf) goMath::barycentricToEuclidean<goFloat>;
%template(goBarycentricToEuclideand) goMath::barycentricToEuclidean<goDouble>;

%template(goEuclideanToBarycentricf) goMath::euclideanToBarycentric<goFloat>;
%template(goEuclideanToBarycentricd) goMath::euclideanToBarycentric<goDouble>;

%template(goSphereToEuclideanf) goMath::sphereToEuclidean<goFloat>;
%template(goSphereToEuclideand) goMath::sphereToEuclidean<goDouble>;

%template(goIntegratef)    goMath::integrate<goFloat>;
%template(goIntegrated)    goMath::integrate<goDouble>;
%template(goIntegrateSimpsonf)    goMath::integrateSimpson<goFloat>;
%template(goIntegrateSimpsond)    goMath::integrateSimpson<goDouble>;

%template(goPlotf)         goPlot::plot<goFloat>;
%template(goPlotd)         goPlot::plot<goDouble>;
%template(goPlot3Df)       goPlot::plot3D<goFloat>;
%template(goPlot3Dd)       goPlot::plot3D<goDouble>;

%template(writeBinaryMatrixf) goFileIO::writeBinaryMatrix<goFloat>;
%template(writeBinaryMatrixd) goFileIO::writeBinaryMatrix<goDouble>;