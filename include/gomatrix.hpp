#ifndef GOMATRIX_I
#define GOMATRIX_I

#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <golog.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

extern "C" 
{
 #include <cblas.h>
}

template<>
goMatrix<goDouble> goMatrix<goDouble>::operator* (const goMatrix<goDouble>& other) const
{
    goSize_t M = this->getRows();
    goSize_t N = other.getColumns();
    goSize_t K = this->getColumns();
    goMatrix<goDouble> C (this->getRows(), other.getColumns());
    C.fill (0.0);
    cblas_dgemm (CblasRowMajor, 
                 CblasNoTrans, 
                 CblasNoTrans,
                 M, N, K, 1.0,
                 this->getData(), this->getLeadingDimension(),
                 other.getData(), other.getLeadingDimension(),
                 0.0, C.getData(), C.getLeadingDimension());
    return C;
};
template<>
goMatrix<goFloat> goMatrix<goFloat>::operator* (const goMatrix<goFloat>& other) const
{
    goSize_t M = this->getRows();
    goSize_t N = other.getColumns();
    goSize_t K = this->getColumns();
    goMatrix<goFloat> C (this->getRows(), other.getColumns());
    C.fill (0.0);
    cblas_sgemm (CblasRowMajor, 
                 CblasNoTrans, 
                 CblasNoTrans,
                 M, N, K, 1.0,
                 this->getData(), this->getLeadingDimension(),
                 other.getData(), other.getLeadingDimension(),
                 0.0, C.getData(), C.getLeadingDimension());
    return C;
};

template<>
goMatrix<goFloat>& goMatrix<goFloat>::operator*= (const goMatrix<goFloat>& other)
{
    goSize_t M = this->getRows();
    goSize_t N = other.getColumns();
    goSize_t K = this->getColumns();
    goMatrix<goFloat> C (this->getRows(), other.getColumns());
    C.fill (0.0);
    cblas_sgemm (CblasRowMajor, 
                 CblasNoTrans, 
                 CblasNoTrans,
                 M, N, K, 1.0,
                 this->getData(), this->getLeadingDimension(),
                 other.getData(), other.getLeadingDimension(),
                 0.0, C.getData(), C.getLeadingDimension());
    *this = C;             
    return *this;
}

template<>
goMatrix<goDouble>& goMatrix<goDouble>::operator*= (const goMatrix<goDouble>& other)
{
    goSize_t M = this->getRows();
    goSize_t N = other.getColumns();
    goSize_t K = this->getColumns();
    goMatrix<goDouble> C (this->getRows(), other.getColumns());
    C.fill (0.0);
    cblas_dgemm (CblasRowMajor, 
                 CblasNoTrans, 
                 CblasNoTrans,
                 M, N, K, 1.0,
                 this->getData(), this->getLeadingDimension(),
                 other.getData(), other.getLeadingDimension(),
                 0.0, C.getData(), C.getLeadingDimension());
    *this = C;             
    return *this;
}

template<>
goVector<goFloat> goMatrix<goFloat>::operator* (const goVector<goFloat>& v) const
{
    assert (v.getSize() == this->getColumns());
    goVector<goFloat> y (v.getSize());
    y.fill (0.0f);
    cblas_sgemv (CblasRowMajor, CblasNoTrans, 
                 this->getRows(), this->getColumns(), 
                 1.0, this->matrix, this->getLeadingDimension(), 
                 v.getPtr(), v.getStride(), 
                 0.0f, y.getPtr(), y.getStride());
    return y;
}


template<>
goVector<goDouble> goMatrix<goDouble>::operator* (const goVector<goDouble>& v) const
{
    assert (v.getSize() == this->getColumns());
    goVector<goDouble> y (v.getSize());
    y.fill (0.0f);
    cblas_dgemv (CblasRowMajor, CblasNoTrans, 
                 this->getRows(), this->getColumns(), 
                 1.0, this->matrix, this->getLeadingDimension(), 
                 v.getPtr(), v.getStride(), 
                 0.0f, y.getPtr(), y.getStride());
    return y;
}

#if 0
template<>
goVector<goDouble> goMatrix<goDouble>::operator* (const goVector<goFloat>& v) const
{
    assert (v.getSize() == this->getColumns());
    goVector<goDouble> y (v.getSize());
    y.fill (0.0f);
    matrixVectorMult (*this, v, y);
    return y;
}
#endif

template <class Tm, class Tv, class Tr>
static void matrixVectorMult (const goMatrix<Tm>& m, const goVector<Tv>& v, goVector<Tr>& r)
{
    goSize_t N = m.getRows();
    goSize_t M = m.getColumns();
    goSize_t i;
    goSize_t j;
    if (r.getSize() != N)
    {
        r.setSize (N);
    }
    for (i = 0; i < N; ++i)
    {
        r[i] = Tr(0);
        for (j = 0; j < M; ++j)
        {
            r[i] += m(i,j) * v[j];
        }
    }
}
#if 1
template<class T>
goVector<T> goMatrix<T>::operator* (const goVector<T>& v) const
{
    assert (v.getSize() == this->getColumns());
    goVector<T> y (v.getSize());
    y.fill (T(0));
    matrixVectorMult (*this, v, y);
    return y;
}
#endif


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

#endif
