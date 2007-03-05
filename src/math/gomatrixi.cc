#include <gomatrix.h>
#include <gomatrix.hpp>
#include <gocomplex.h>
#include <gomath.h>		// MAX()
#ifndef GOEIGENVALUE_H
# include <goeigenvalue.h>
#endif
#include <iostream>

template <>
goComplexf goMatrix<goComplexf>::norm () const
{
    goLog::error("goMatrix::norm not implemented for complex types.");
    return goComplexf(0.0f,0.0f);
}

//template <>
//goComplexd goMatrix<goComplexd>::norm () const
//{
//    goLog::error("goMatrix::norm not implemented for complex types.");
//    return goComplexd(0.0f,0.0f);
//}

template <>
bool goMatrix<goComplexf>::writeASCII (FILE* f) const
{
    goLog::error ("read/write for goMatrix not implemented for complex types.");
    return false;
};
template <>
bool goMatrix<goComplexf>::readASCII (FILE* f)
{
    goLog::error ("read/write for goMatrix not implemented for complex types.");
    return false;
};

template <>
bool goMatrix<goFloat>::invert ()
{
    //= Factorise A P = L U
    goSize_t M = this->getColumns();
    if (M != this->getRows())
    {
        goLog::warning ("goMatrix::invert(): tried to invert non-quadratic matrix.");
        return false;
    }
    int* P = new int [M * M];
    if (clapack_sgetrf (CblasRowMajor, M, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
    {
        delete[] P;
        return false;
    }
    if (clapack_sgetri (CblasRowMajor, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
    {
        delete[] P;
        return false;
    }
    delete[] P;
    return true;
}

template <>
bool goMatrix<goDouble>::invert ()
{
    //= Factorise A P = L U
    goSize_t M = this->getColumns();
    if (M != this->getRows())
    {
        goLog::warning ("goMatrix::invert(): tried to invert non-quadratic matrix.");
        return false;
    }
    int* P = new int [M * M];
    if (clapack_dgetrf (CblasRowMajor, M, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
    {
        delete[] P;
        return false;
    }
    if (clapack_dgetri (CblasRowMajor, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
    {
        delete[] P;
        return false;
    }
    delete[] P;
    return true;
}

template <class T>
bool goMatrix<T>::invert ()
{
    goLog::error ("goMatrix::invert() not implemented for types other than goFloat and goDouble.");
    return false;
}

template <>
void goMatrix<goFloat>::power (goFloat scalar)
{
    goMatrixPower<goFloat> (*this, scalar);
}

template <>
void goMatrix<goDouble>::power (goDouble scalar)
{
    goMatrixPower<goDouble> (*this, scalar);
}

template <class T>
void goMatrix<T>::power (T)
{
    goLog::warning ("goMatrix::power() not defined for this type.");
}

template<>
void goMatrixPower (goMatrix<goComplexf>& A, goComplexf scalar)
{
    goLog::error ("goMatrixPower() not implemented for complex.");
}

template<>
void goMatrixPower (goMatrix<goComplexd>& A, goComplexd scalar)
{
    goLog::error ("goMatrixPower() not implemented for complex.");
}

template<class T>
void goMatrixPower (goMatrix<T>& A, T scalar)
{
    if (A.getRows() != A.getColumns())
    {
        goLog::warning ("goMatrixPower(): A is not quadratic.");
        return;
    }
    goMath::Eigenvalue<T> eig (A);
    const goMatrix<T>& V = eig.getV();
    goMatrix<T> Vtemp = V;
    goVector<T>& d = eig.getRealEigenvalues();
    //= V * D.^scalar * V'
    goSize_t n = d.getSize();
    for (goSize_t i = 0; i < n; ++i)
        d[i] = T(::pow (d[i],scalar));
    n = Vtemp.getRows ();
    goVector<T> refV;
    for (goSize_t i = 0; i < n; ++i)
    {
        Vtemp.refRow (i,refV);
        refV *= d;  //= Element-wise multiplication
    }
    goMatrixMult<T> (T(1), Vtemp, false, V, true, T(0), A);
}

template<>
void goMatrixMult<goFloat> (goFloat alpha, const goMatrix<goFloat>& A, bool transA, 
                                           const goMatrix<goFloat>& B, bool transB, 
                            goFloat beta, goMatrix<goFloat>& C)
{
    goSize_t M = transA ? A.getColumns() : A.getRows();
    goSize_t N = transB ? B.getRows() : B.getColumns();
    goSize_t K = transA ? A.getRows() : A.getColumns();
    if (C.getRows() != M || C.getColumns() != N)
    {
        C.resize (M,N);
        C.fill (0.0f);
    }
    cblas_sgemm (CblasRowMajor, 
                 transA ? CblasTrans : CblasNoTrans, 
                 transB ? CblasTrans : CblasNoTrans,
                 M, N, K, alpha,
                 A.getData(), A.getLeadingDimension(),
                 B.getData(), B.getLeadingDimension(),
                 beta, C.getData(), C.getLeadingDimension());
}

template<>
void goMatrixMult<goDouble> (goDouble alpha, const goMatrix<goDouble>& A, bool transA, 
                                           const goMatrix<goDouble>& B, bool transB, 
                            goDouble beta, goMatrix<goDouble>& C)
{
    goSize_t M = transA ? A.getColumns() : A.getRows();
    goSize_t N = transB ? B.getRows() : B.getColumns();
    goSize_t K = transA ? A.getRows() : A.getColumns();
    if (C.getRows() != M || C.getColumns() != N)
    {
        C.resize (M,N);
        C.fill (0.0);
    }
    cblas_dgemm (CblasRowMajor, 
                 transA ? CblasTrans : CblasNoTrans, 
                 transB ? CblasTrans : CblasNoTrans,
                 M, N, K, alpha,
                 A.getData(), A.getLeadingDimension(),
                 B.getData(), B.getLeadingDimension(),
                 beta, C.getData(), C.getLeadingDimension());
}

template<>
bool goMatrixVectorMult<goFloat> (goFloat alpha, const goMatrix<goFloat>& A, bool transA,
                                  const goVector<goFloat>& x, goFloat beta, goVector<goFloat>& y)
{
    goSize_t M = 0;
    goSize_t N = 0;
    if (transA)
    {
        M = A.getColumns();
        N = A.getRows();
    }
    else
    {
        M = A.getRows();
        N = A.getColumns();
    }

    if (x.getSize() != N)
    {
        return false;
    }
    if (y.getSize() != M)
    {
        y.resize (M);
        y.fill (0.0f);
    }
    
    //= M,N parameters are here rows(A), columns(A) no matter what transA is.
    cblas_sgemv (CblasRowMajor,
                 transA ? CblasTrans : CblasNoTrans, A.getRows(), A.getColumns(),
                 alpha, A.getPtr(), A.getLeadingDimension(), x.getPtr(), x.getStride(), beta, y.getPtr(), y.getStride());

    return true;
}

template<>
bool goMatrixVectorMult<goDouble> (goDouble alpha, const goMatrix<goDouble>& A, bool transA,
                                   const goVector<goDouble>& x, goDouble beta, goVector<goDouble>& y)
{
    goSize_t M = 0;
    goSize_t N = 0;
    if (transA)
    {
        M = A.getColumns();
        N = A.getRows();
    }
    else
    {
        M = A.getRows();
        N = A.getColumns();
    }

    if (x.getSize() != N)
    {
        return false;
    }
    if (y.getSize() != M)
    {
        y.resize (M);
        y.fill (0.0f);
    }
    
    cblas_dgemv (CblasRowMajor,
                 transA ? CblasTrans : CblasNoTrans, A.getRows(), A.getColumns(),
                 alpha, A.getPtr(), A.getLeadingDimension(), x.getPtr(), x.getStride(), beta, y.getPtr(), y.getStride());

    return true;
}

/* Instantiation */
template void  goMatrixPower<goFloat> (goMatrix<goFloat>& A, goFloat);
template class goMatrix<goDouble>;
template class goMatrix<goFloat>;
template class goMatrix<goIndex_t>;
template class goMatrix<goSize_t>;
// template class goMatrix<goInt32>;
template class goMatrix<goComplexf>;
