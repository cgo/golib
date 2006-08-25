#ifndef GOMATRIX_HPP
#define GOMATRIX_HPP

#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <golog.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOLOG_H
# include <golog.h>
#endif

extern "C" 
{
 #include <cblas.h>
}

/*!
* @param y Number of rows.
* @param x Number of columns.
*/
template <class T>
goMatrix<T>::goMatrix (goSize_t rows, goSize_t cols)
    : externalData (false), matrix (0), rows (rows), columns (cols), leadingDimension (cols)
{
    this->matrix = new T[rows * cols];
}

/** 
* @brief Copy constructor.
* 
* @param other Other matrix. Will be deep copied.
*/
template <class T>
goMatrix<T>::goMatrix (const goMatrix<T>& other)
    : externalData (false), matrix (0), rows (0), columns (0), leadingDimension (0)
{
    *this = other;
}

/** 
* @brief Constructor for using external data.
* 
* @param data Pointer to the external data.
* @param r Rows
* @param c Columns
*/
template <class T>
goMatrix<T>::goMatrix (T* data, goSize_t r, goSize_t c, goSize_t leadingDim)
    : externalData (true), matrix (data), rows (r), columns (c), leadingDimension (leadingDim)
{
    if (leadingDim == 0)
    {
        leadingDimension = columns;
    }
}

template <class T>
goMatrix<T>::~goMatrix ()
{
    if (!this->externalData && this->matrix)
    {
        delete[] this->matrix;
        this->matrix = 0;
        this->rows = 0;
        this->columns = 0;
    }
}

/** 
 * @brief Resize (re-allocate) the matrix.
 * 
 * Memory is newly allocated, so all former data is lost.
 *
 * @param rows Rows.
 * @param cols Columns.
 * 
 * @return  True if successful, false otherwise. Currently always true.
 */
template <class T>
bool goMatrix<T>::resize (goSize_t rows, goSize_t cols)
{
    if (!this->externalData && this->matrix)
    {
        delete[] this->matrix;
    }
    this->matrix = new T[rows * cols];
    this->rows = rows;
    this->columns = cols;
    this->leadingDimension = cols;
    this->externalData = false;
    return true;
}

/** 
 * @brief Set external data.
 * 
 * @param data Pointer to the external data.
 * @param r    Rows 
 * @param c    Columns
 * 
 * @return True.
 */
template <class T>
bool goMatrix<T>::setData (T* data, goSize_t r, goSize_t c, goSize_t leadingDim)
{
    if (this->matrix && !this->externalData)
    {
        delete[] this->matrix;
    }
    this->externalData = true;
    this->matrix = data;
    this->rows = r;
    this->columns = c;
    if (leadingDim == 0)
    {
        this->leadingDimension = c;
    }
    else
    {
        this->leadingDimension = leadingDim;
    }
    return true; 
}

/* 
 * @brief Set external data.
 * 
 * Please forgive me this. I needed a way to reference const data ...
 *
 * @param data Pointer to the external data.
 * @param r    Rows 
 * @param c    Columns
 * 
 * @return True.
 */
template <class T>
bool goMatrix<T>::setData (const T* data, goSize_t r, goSize_t c, goSize_t leadingDim) const
{
    //= FORGIVE ME PLEASE PLEASE PLEASE PLEASE ....
    if (this->matrix && !this->externalData)
    {
        delete[] const_cast<T*>(this->matrix);
    }

    const_cast<goMatrix<T>*>(this)->externalData = true;
    const_cast<goMatrix<T>*>(this)->matrix = const_cast<T*>(data);
    const_cast<goMatrix<T>*>(this)->rows = r;
    const_cast<goMatrix<T>*>(this)->columns = c;
    if (leadingDim == 0)
    {
        const_cast<goMatrix<T>*>(this)->leadingDimension = c;
    }
    else
    {
        const_cast<goMatrix<T>*>(this)->leadingDimension = leadingDim;
    }
    return true; 
}

/** 
 * @brief Get a transposed copy of this matrix.
 *
 * @param trans Contains the transpose after the method returned.
 */
template <class T>
void goMatrix<T>::getTranspose (goMatrix<T>& trans)
{
    if (trans.getColumns() != this->getRows() || trans.getRows() != this->getColumns())
    {
        trans.resize (this->getColumns(), this->getRows());
    }
    goSize_t i;
    goSize_t j;
    goSize_t M = this->getRows();
    goSize_t N = this->getColumns();
    for (j = 0; j < N; ++j)
    {
        for (i = 0; i < M; ++i)
        {
            trans(j,i) = (*this)(i,j);
        }
    }
}

/** 
 * @brief Flip in row or column direction.
 * 
 * @param dim If 0, flips rows, otherwise flips columns.
 */
template <class T>
void goMatrix<T>::flip (goSize_t dim)
{
    if (dim == 0)
    {
        //= Flip rows.
        goSize_t r = this->getRows();
        goSize_t c = this->getColumns();
        goSize_t i,i2,j;
        T temp;
        i = 0;
        i2 = r - 1;
        while (i < i2)
        {
            for (j = 0; j < c; ++j)
            {
                temp = (*this)(i,j);
                (*this)(i,j) = (*this)(i2, j);
                (*this)(i2, j) = temp;
            }
            ++i;
            --i2;
        }
    }
    else 
    {
        //= Flip columns.
        goSize_t r = this->getRows();
        goSize_t c = this->getColumns();
        goSize_t i,j,j2;
        T temp;
        for (i = 0; i < r; ++i)
        {
            j = 0;
            j2 = c - 1;
            while (j < j2)
            {
                temp = (*this)(i, j);
                (*this)(i, j) = (*this)(i, j2);
                (*this)(i, j2) = temp;
                ++j;
                --j2;
            }
        }
    }
}

/** 
 * @brief Transpose the data.
 * This is very slow and should be used scarcely.
 * For Multiplication with transposition, use goMatrixMult().
 */
template <class T>
void goMatrix<T>::transpose ()
{
    goMatrix<T> temp;
    this->getTranspose(temp);
    *this = temp;
}

template <class T>
bool goMatrix<T>::operator== (const goMatrix<T>& other) const
{
    goSize_t R = this->getRows();
    goSize_t C = this->getColumns();
    if (R != other.getRows() || C != other.getColumns())
    {
        return false;
    }
    goSize_t i,j;
    for (i = 0; i < R; ++i)
    {
        for (j = 0; j < C; ++j)
        {
            if ((*this)(i,j) != other(i,j))
            {
                return false;
            }
        }
    }
    return true;
}

template <class T>
bool goMatrix<T>::operator!= (const goMatrix<T>& other) const
{
    return !(*this == other);
}

/*
 * @brief Convenience method for in-line use in calculations.
 * 
 * THIS DOES NOT WORK AS SOON AS SOMETHING LIKE THIS APPEARS: goMatrixf M4 = M1(1,1,3,3) * M1(2,0,4,3);
 * THEREFORE IT IS DUMPED AND LEFT HERE AS WARNING FOR THE FUTURE.
 */
//template <class T>
//const goMatrix<T>& goMatrix<T>::operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2) const
//{
//    static const goMatrix<T> retRef;
//    retRef.setData (&(*this)(i1,j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
//    return retRef;
//}

/** 
 * @brief Copy sub-matrix from i1,j1 to i2,j2 into target.
 *
 * For referencing, use the setData() method.
 */
template <class T>
void goMatrix<T>::operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2, goMatrix<T>& target) const
{
    assert (i1 <= i2 && j1 <= j2);
    const goMatrix<T> temp;
    temp.setData (&(*this)(i1, j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
    target = temp;
}

/** 
 * @brief Copy source to sub-matrix from i1,j1 to i2,j2 in this matrix.
 *
 * @note Dimensions must agree and are assert()ed. Check logfile for warnings.
 */
template <class T>
void goMatrix<T>::operator () (const goMatrix<T>& source, goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2)
{
    assert (source.getRows() == i2-i1+1);
    assert (source.getColumns() == j2-j1+1);
    if (source.getRows() != i2-i1+1 || source.getColumns() != j2-j1+1)
    {
        goLog::warning ("goMatrix::operator(): source dimensions do not agree with sub-matrix. Not copying.");
        return;
    }
    goMatrix<T> temp (&(*this)(i1, j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
    temp = source;
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

template <class T>
goMatrix<T> goMatrix<T>::operator* (const goMatrix<T>& other) const
{
    if (this->getColumns() != other.getRows())
    {
        goLog::warning ("goMatrix::operator*: Matrix dimensions do not match.");
        return goMatrix<T> (1,1);
    }
    goMatrix<T> retval (getRows(), other.getColumns());
    goSize_t x, y;
    goSize_t columns = retval.getColumns();
    goVector<T> row;
    goVector<T> column;
    for (y = 0; y < retval.getRows(); ++y) 
    {
        for (x = 0; x < columns; ++x) 
        {
            this->refRow (x, row);
            other.refColumn (y, column);
            retval (y,x) = row * column;      
        }
    } 
    return retval;
}

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

/** 
 * @brief this = this * other
 * @note Uses CLBLAS for goFloat and goDouble types.
 * @see goMatrixMult()
 * @param other A Matrix.
 * 
 * @return Reference to this.
 */
template <class T>
goMatrix<T>& goMatrix<T>::operator*= (const goMatrix<T>& other)
{
    // Slow and reliable (and quick to hack).
    *this = *this * other;
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

  /** 
  * @brief Element-wise addition.
  * 
  * @param other 
  * 
  * @return this = this + other
  */
template <class T>
goMatrix<T>& goMatrix<T>::operator+= (const goMatrix<T>& other)
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    const goSize_t R = this->getRows();
    const goSize_t C = this->getColumns();
    if (R != other.getRows() || C != other.getColumns())
    {
        return *this;
    }
    goSize_t i;
    goSize_t j;
    for (i = 0; i < R; ++i)
    {
        for (j = 0; j < C; ++j)
        {
            (*this)(i,j) += other(i,j);
        }
    }
    return *this;
}

//= Quite slow, quick hack.
template <class T>
goMatrix<T> goMatrix<T>::operator+ (const goMatrix<T>& other) const
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    goMatrix<T> C (this->getRows(), this->getColumns());
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            C(i,j) = (*this)(i,j) + other(i,j);
        }
    }
    return C;
}

//= Quite slow, quick hack.
template <class T>
goMatrix<T> goMatrix<T>::operator-  (const goMatrix<T>& other) const
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    goMatrix<T> C (this->getRows(), this->getColumns());
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            C(i,j) = (*this)(i,j) - other(i,j);
        }
    }
    return C;
}

/** 
 * @brief Element-wise subtraction.
 * 
 * @param other 
 * 
 * @return this = this - other
 */
template <class T>
goMatrix<T>& goMatrix<T>::operator-= (const goMatrix<T>& other)
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            (*this)(i,j) -= other(i,j);
        }
    }
    return *this;
}

/** 
 * @brief Deep copy operator.
 * 
 * @param other Other matrix.
 * 
 * @return Reference to this matrix.
 */
template <class T>
goMatrix<T>& goMatrix<T>::operator= (const goMatrix<T>& other)
{
    if (this->rows != other.getRows() || this->columns != other.getColumns())
    {
        this->resize (other.getRows(), other.getColumns());
    }
    goSize_t C = this->getColumns();
    goSize_t R = this->getRows();
    goSize_t i,j;
    for (i = 0; i < R; ++i)
    {
        for (j = 0; j < C; ++j)
        {
            (*this)(i,j) = other(i,j);
        }
    }
    // memcpy (this->matrix, other.getData(), this->rows * this->columns * sizeof(T));
    return *this;
}

/** 
 * @brief Element-wise multiplication.
 * 
 * *this .* other
 *
 * @param other Other matrix.
 * 
 * @return True
 */
template <class T>
bool goMatrix<T>::multiplyElements (const goMatrix<T>& other)
{
    assert (this->getRows() == other.getRows() && this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    for (j = 0; j < this->getColumns(); ++j)
    {
        for (i = 0; i < this->getRows(); ++i)
        {
            (*this)(i,j) *= other(i,j);
        }
    }
    return true;
}

/** 
 * @brief Calculate the Frobenius norm \f$ \left( \sum_i\sum_j |a_{i,j}|^2 \right)^{\frac{1}{2}} = \mathrm{trace} (A A^T) = \sum_i \sigma_i^2 \f$
 * where \f$ \sigma_i \f$ is the \f$ i\f$'th singular value.
 *
 * @return The Frobenius norm of this matrix.
 */
template <class T>
T goMatrix<T>::norm () const
{
    goSize_t sz = this->getRows() * this->getColumns();
    goSize_t i;
    double retValue = 0.0;
    T* mptr = this->matrix;
    for (i = 0; i < sz; ++i)
    {
        retValue += *mptr * *mptr;
        ++mptr;
    }
    return static_cast<T>(sqrt(retValue));
}

/** 
 * @brief Load identity matrix.
 */
template <class T>
void goMatrix<T>::setIdentity()
{
    this->fill(T(0));
    goSize_t n = goMath::min(this->getRows(), this->getColumns());
    goSize_t i;
    for (i = 0; i < n; ++i)
    {
        (*this)(i,i) = T(1);
    }
}

/** 
 * @brief Fill matrix with a value v.
 * 
 * @param v Value to fill with.
 */
template <class T>
void goMatrix<T>::fill(T v)
{
    goSize_t sz = this->getRows() * this->getColumns();
    goSize_t i;
    T* mptr = this->matrix;
    for (i = 0; i < sz; ++i)
    {
        *mptr = v;
        ++mptr;
    }
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
  /** 
  * @brief Matrix vector multiplication.
  * @note Uses CBLAS for goFloat and goDouble types.
  * @param v A vector.
  * 
  * @return this * v
  */
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
