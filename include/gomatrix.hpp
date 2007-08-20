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
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif

extern "C" 
{
 #include <cblas.h>
 #include <clapack.h>
}

/*!
* @param y Number of rows.
* @param x Number of columns.
*/
template <class T>
goMatrix<T>::goMatrix (goSize_t rows, goSize_t cols)
    : externalData (false), matrix (0), rows (rows), columns (cols), leadingDimension (cols),
      rowMajor (true)
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
    : externalData (false), matrix (0), rows (0), columns (0), leadingDimension (0), rowMajor (other.getRowMajor())
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
goMatrix<T>::goMatrix (T* data, goSize_t r, goSize_t c, goSize_t leadingDim, bool row_major)
    : externalData (true), matrix (data), rows (r), columns (c), leadingDimension (leadingDim),
      rowMajor (row_major)
{
    if (leadingDim == 0)
    {
        if (row_major)
            leadingDimension = columns;
        else
            leadingDimension = rows;
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
    if (this->rowMajor)
    {
        this->leadingDimension = cols;
    }
    else
    {
        this->leadingDimension = rows;
    }
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
        if (this->rowMajor)
        {
            this->leadingDimension = c;
        }
        else
        {
            this->leadingDimension = r;
        }
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
        if (this->rowMajor)
        {
            const_cast<goMatrix<T>*>(this)->leadingDimension = c;
        }
        else
        {
            const_cast<goMatrix<T>*>(this)->leadingDimension = r;
        }
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
void goMatrix<T>::getTranspose (goMatrix<T>& trans) const
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
 * @brief Shift (cyclically permute) rows.
 * 
 * Shifts all rows "down" a given offset.
 * The offset can be positive or negative
 *
 * @param offset Offset
 * @param ret    On return, contains the shifted matrix.
 */
template <class T>
void goMatrix<T>::shiftRows (goIndex_t offset, goMatrix<T>& ret) const
{
    if (ret.getRows() != this->getRows() || ret.getColumns() != this->getColumns())
    {
        ret.resize (this->getRows(), this->getColumns());
    }

    goSize_t sz = this->getRows();
    goSize_t newIndex = (offset >= 0) ? (offset) : (sz + offset);
    const goVector<T> ref(0);
    for (goSize_t oldIndex = 0; oldIndex < sz; ++oldIndex, ++newIndex)
    {
        if (newIndex >= sz)
            newIndex = 0;
        this->refRow (oldIndex, ref);
        ret.setRow (newIndex, ref);
    }
}

/** 
 * @brief Shift (cyclically permute) columns.
 * 
 * Shifts all columns "down" a given offset.
 * The offset can be positive or negative
 *
 * @param offset Offset
 * @param ret    On return, contains the shifted matrix.
 */
template <class T>
void goMatrix<T>::shiftColumns (goIndex_t offset, goMatrix<T>& ret) const
{
    if (ret.getRows() != this->getRows() || ret.getColumns() != this->getColumns())
    {
        ret.resize (this->getRows(), this->getColumns());
    }

    goSize_t sz = this->getColumns();
    goSize_t newIndex = (offset >= 0) ? (offset) : (sz + offset);
    const goVector<T> ref(0);
    for (goSize_t oldIndex = 0; oldIndex < sz; ++oldIndex, ++newIndex)
    {
        if (newIndex >= sz)
            newIndex = 0;
        this->refColumn (oldIndex, ref);
        ret.setColumn (newIndex, ref);
    }
}

/** 
 * @brief Copies sub-matrix from this matrix to target matrix.
 *
 * Copies the given sub-matrix from this to target starting in target at 0,0.
 * 
 * @param startRow 
 * @param startCol 
 * @param endRow 
 * @param endCol 
 * @param target 
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMatrix<T>::copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goMatrix<T>& target) const
{
    return this->copy (startRow, startCol, endRow, endCol, 0, 0, target);
}

/** 
 * @brief Copy rectangular sub-matrix to another matrix.
 * 
 * Copies from this matrix the sub-matrix (startRow...endRow , startCol...endCol) to the sub-matrix in target starting at target_row,target_col.
 *
 * @param startRow 
 * @param startCol 
 * @param endRow 
 * @param endCol 
 * @param target_row 
 * @param target_col 
 * @param target 
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMatrix<T>::copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goSize_t target_row, goSize_t target_col, goMatrix<T>& target) const
{
    goSize_t num_rows = endRow - startRow + 1;
    goSize_t num_cols = endCol - startCol + 1;
    if (target.getRows() - target_row < num_rows || target.getColumns() - target_col < num_cols)
    {
        goLog::warning ("goMatrix::copy(): target too small. Not copying.");
        return false;
    }
    goMatrix<T> refM(0,0);
    this->ref (startRow, startCol, num_rows, num_cols, refM);
    goMatrix<T> refTarget(0,0);
    target.ref (target_row, target_col, num_rows, num_cols, refTarget);
    refTarget = refM;

    return true;
}

/** 
 * @brief Copies this matrix to target matrix. 
 * 
 * Target is not resized, this matrix is copied to target starting at 0,0.
 * If target is too small, false is returned.
 *
 * @param target 
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMatrix<T>::copy (goMatrix<T>& target) const
{
    return this->copy (0, 0, this->getRows() - 1, this->getColumns() - 1, 0, 0, target);
}


/** 
 * @brief Transpose the data.
 * This is very slow and should be used scarcely.
 * For Multiplication with transposition, use goMatrixMult().
 */
template <class T>
void goMatrix<T>::transpose ()
{
    if (this->externalData)
    {
        goLog::warning ("goMatrix::transpose() called on a matrix with external data. The matrix will not be a reference to the external data any more -- is this what you want?");
    }
    goMatrix<T> temp;
    this->getTranspose(temp);
    if (this->externalData)
    {
        this->resize (0, 0);
    }
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
 * @brief References sub-matrix from i1,j1 to i2,j2 into target.
 *
 * To make clear it is referencing, use the ref() method.
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
    assert (source.getRows() == (goSize_t)(i2-i1+1));
    assert (source.getColumns() == (goSize_t)(j2-j1+1));
    if (source.getRows() != static_cast<goSize_t>(i2-i1+1) || source.getColumns() != static_cast<goSize_t>(j2-j1+1))
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
    goVector<goFloat> y (this->getRows());
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
    goVector<goDouble> y (this->getRows());
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
 * If the size of this matrix does not match, it will be resized (ref()'ed matrices
 * will no longer be references!). If the size matches, nothing is changed and
 * data are just copied.
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
 * @brief Calculate the Frobenius norm \f$ \left( \sum_i\sum_j |a_{i,j}|^2 \right)^{\frac{1}{2}} = \left(\mathrm{trace} (A A^T)\right)^\frac{1}{2} = \left( \sum_i \sigma_i^2 \right)^\frac{1}{2} \f$
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
    T* row = this->matrix;
    for (goSize_t i = 0; i < this->rows; ++i)
    {
        T* mptr = row;
        for (goSize_t j = 0; j < this->columns; ++j, ++mptr)
        {
            *mptr = v;
        }
        row += this->leadingDimension;
    }
}

template<class T>
bool goMatrix<T>::writeASCII (FILE* f) const
{
    if (!f)
    {
        return false;
    }

    goSize_t R = this->getRows();
    goSize_t C = this->getColumns();
    goSize_t r, c;
    fprintf (f, "goMatrix\n");
    fprintf (f, "size %d %d\n", (int)R, (int)C);
    goDouble d;
    for (r = 0; r < R; ++r)
    {
        for (c = 0; c < C; ++c)
        {
            d = (goDouble)(*this)(r,c);
            fprintf (f, "%lf ", d);
        }
        fprintf (f, "\n");
    }
    return true;
}

template<class T>
bool goMatrix<T>::writeASCII (const char* fname) const
{
    if (!fname)
    {
        return false;
    }
    FILE* f = fopen (fname,"w");
    if (!f)
    {
        return false;
    }
    bool ok = this->writeASCII (f);
    fclose (f);
    return ok;
}

template<class T>
bool goMatrix<T>::readASCII (FILE* f)
{
    if (!f)
    {
        return false;
    }

    goString s = "";
    
    if (!goFileIO::readASCIILine (f, s))
    {
        return false;
    }

    if (s != "goMatrix")
    {
        goString msg = "goMatrix::readASCII: expected goMatrix, got ";
        msg += s.toCharPtr ();
        goLog::warning (msg);
        return false;
    }

    if (!goFileIO::readASCIILine (f,s))
    {
        return false;
    }

    goList<goString> words;
    s.getWords (words);

    if (words.getSize() != 3 || words.getFrontElement()->elem != "size")
    {
        goString msg = "goMatrix::readASCII: expected size x y, got ";
        msg += s.toCharPtr ();
        goLog::warning (msg);
        return false;
    }

    goSize_t R = 0;
    goSize_t C = 0;

    R = words.getFrontElement()->next->elem.toInt();
    C = words.getFrontElement()->next->next->elem.toInt();

    this->resize (R,C);
    goSize_t r, c;
    goDouble d = 0.0;
    for (r = 0; r < R; ++r)
    {
        for (c = 0; c < C; ++c)
        {
            fscanf (f, "%lf ", &d);
            (*this)(r,c) = T(d);
        }
        fscanf (f, "\n");
    }

    return true;
}

template<class T>
bool goMatrix<T>::readASCII (const char* fname) 
{
    if (!fname)
    {
        return false;
    }
    FILE* f = fopen (fname,"r");
    if (!f)
    {
        return false;
    }
    bool ok = this->readASCII (f);
    fclose (f);
    return ok;
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
  * @return (*this) * v
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

#endif
