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

/*!
* @param y Number of rows.
* @param x Number of columns.
*/
template <class T>
goMatrix<T>::goMatrix (goSize_t rows, goSize_t cols)
    : externalData (false), matrix (0), rows (rows), columns (cols)
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
    : externalData (false), matrix (0), rows (0), columns (0)
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
goMatrix<T>::goMatrix (T* data, goSize_t r, goSize_t c)
    : externalData (true), matrix (data), rows (r), columns (c)
{
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
bool goMatrix<T>::setData (T* data, goSize_t r, goSize_t c)
{
    if (this->matrix && !this->externalData)
    {
        delete[] this->matrix;
    }
    this->externalData = true;
    this->matrix = data;
    this->rows = r;
    this->columns = c;
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
