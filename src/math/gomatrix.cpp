#include <gomatrix.h>
// #include <gomatrix.hpp>
#include <gocomplex.h>
#include <gomath.h>		// MAX()
#ifndef GOEIGENVALUE_H
# include <goeigenvalue.h>
#endif
#include <iostream>

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

template<class T>
const bool goMath::Matrix<T>::rowMajor = true;

/*!
* @param y Number of rows.
* @param x Number of columns.
*/
template <class T>
goMath::Matrix<T>::Matrix (goSize_t r, goSize_t c)
    : externalData (false), matrix (0), rows (0), columns (0), leadingDimension (c)
{
  if (true != this->resize (r, c))
    {
      this->resize (0, 0);
    }
}

/** 
* @brief Copy constructor.
* 
* @param other Other matrix. Will be deep copied.
*/
template <class T>
goMath::Matrix<T>::Matrix (const goMath::Matrix<T>& other)
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
goMath::Matrix<T>::Matrix (T* data, goSize_t r, goSize_t c, goSize_t leadingDim)
    : externalData (true), matrix (data), rows (r), columns (c), leadingDimension (leadingDim)
{
    if (leadingDim == 0)
    {
        if (goMath::Matrix<T>::rowMajor)
            leadingDimension = columns;
        else
            leadingDimension = rows;
    }
}

template <class T>
goMath::Matrix<T>::~Matrix ()
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
 * @brief Get an iterator to the first row.
 * 
 * @return Vector iterator to the first row.
 */
template <class T>
typename goMath::Matrix<T>::vector_iterator       goMath::Matrix<T>::rowBegin () { return vector_iterator (this->getPtr(), this->getColumns(), 1, this->getLeadingDimension()); }

/** 
 * @brief Get an iterator to the first row.
 * 
 * @return Vector iterator to the first row.
 */
template <class T>
typename goMath::Matrix<T>::const_vector_iterator       goMath::Matrix<T>::rowBegin () const { return const_vector_iterator (this->getPtr(), this->getColumns(), 1, this->getLeadingDimension()); }

/** 
 * @brief Get an iterator pointing after the last row.
 * 
 * @return Iterator pointing after the last row.
 */
template <class T>
typename goMath::Matrix<T>::vector_iterator       goMath::Matrix<T>::rowEnd () { return vector_iterator (this->getPtr() + this->getLeadingDimension() * this->getRows(), this->getColumns(), 1, this->getLeadingDimension()); }

/** 
 * @brief Get an iterator pointing after the last row.
 * 
 * @return Iterator pointing after the last row.
 */
template <class T>
typename goMath::Matrix<T>::const_vector_iterator       goMath::Matrix<T>::rowEnd () const { return const_vector_iterator (this->getPtr() + this->getLeadingDimension() * this->getRows(), this->getColumns(), 1, this->getLeadingDimension()); }

/** 
 * @brief Get an iterator to the first column.
 * 
 * @return Vector iterator to the first column.
 */
template <class T>
typename goMath::Matrix<T>::vector_iterator       goMath::Matrix<T>::colBegin () { return vector_iterator (this->getPtr(), this->getRows(), this->getLeadingDimension(), 1); }

/** 
 * @brief Get an iterator to the first column.
 * 
 * @return Vector iterator to the first column.
 */
template <class T>
typename goMath::Matrix<T>::const_vector_iterator       goMath::Matrix<T>::colBegin () const { return const_vector_iterator (this->getPtr(), this->getRows(), this->getLeadingDimension(), 1); }

/** 
 * @brief Get an iterator pointing after the last column.
 * 
 * @return Iterator pointing after the last column.
 */
template <class T>
typename goMath::Matrix<T>::vector_iterator       goMath::Matrix<T>::colEnd () { return vector_iterator (this->getPtr() + this->getColumns(), this->getRows(), this->getLeadingDimension(), 1); }

/** 
 * @brief Get an iterator pointing after the last column.
 * 
 * @return Iterator pointing after the last column.
 */
template <class T>
typename goMath::Matrix<T>::const_vector_iterator       goMath::Matrix<T>::colEnd () const { return const_vector_iterator (this->getPtr() + this->getColumns(), this->getRows(), this->getLeadingDimension(), 1); }
#if 0
template <class T>
typename goMath::Matrix<T>::const_row_iterator goMath::Matrix<T>::rowBegin () const { return const_row_iterator (*this, 0); }
template <class T>
typename goMath::Matrix<T>::const_row_iterator goMath::Matrix<T>::rowEnd () const { return const_row_iterator (*this, this->getRows()); } 
template <class T>
typename goMath::Matrix<T>::col_iterator       goMath::Matrix<T>::colBegin () { return col_iterator (*this, 0); }
template <class T>
typename goMath::Matrix<T>::col_iterator       goMath::Matrix<T>::colEnd () { return col_iterator (*this, this->getColumns()); }
template <class T>
typename goMath::Matrix<T>::const_col_iterator goMath::Matrix<T>::colBegin () const { return const_col_iterator (*this, 0); }
template <class T>
typename goMath::Matrix<T>::const_col_iterator goMath::Matrix<T>::colEnd () const { return const_col_iterator (*this, this->getColumns()); }
#endif

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
bool goMath::Matrix<T>::resize (goSize_t rows, goSize_t cols)
{
  if (!this->externalData && this->matrix)
  {
    delete[] this->matrix;
    this->matrix = 0;
  }
  if (rows * cols == 0)
  {
    this->matrix = 0;
  }
  else 
  {
    // Check if the target size fits in our memory space at all:
    int bits = ilogb (rows) + ilogb (cols) + ilogb (sizeof(T));
    if (bits > (sizeof(std::size_t) << 3))
      {
	this->matrix  = 0;
	this->columns = 0;
	this->rows    = 0;
	return false;
      }
    this->matrix = new (std::nothrow) T[rows * cols];
    if (0 == this->matrix)
      {
	this->rows	  = 0;
	this->columns = 0;
	return false;
      }
  }
  
  this->rows = rows;
  this->columns = cols;
  if (goMath::Matrix<T>::rowMajor)
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
 * @brief Reshape this matrix as long as the new shape results in the same
 total number of elements.

 If the number rows*cols is the same as this->getRows()*this->getColumns(),
 the matrix is reshaped; the data are not touched in any way.
 Nothing is reallocated.

 * @param rows Number of rows of the new shape
 * @param cols Number of columns of the new shape
 * 
 * @return True on success, false otherwise.
 */
template <class T>
bool goMath::Matrix<T>::reshape (goSize_t rows, goSize_t cols)
{
  if (rows * cols != this->getRows() * this->getColumns())
  {
    return false;
  }

  this->rows = rows;
  this->columns = cols;
  if (goMath::Matrix<T>::rowMajor)
  {
    this->leadingDimension = cols;
  }
  else
  {
    this->leadingDimension = rows;
  }

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
bool goMath::Matrix<T>::setData (T* data, goSize_t r, goSize_t c, goSize_t leadingDim)
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
        if (goMath::Matrix<T>::rowMajor)
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
bool goMath::Matrix<T>::setData (const T* data, goSize_t r, goSize_t c, goSize_t leadingDim) const
{
    //= FORGIVE ME PLEASE PLEASE PLEASE PLEASE ....
    if (this->matrix && !this->externalData)
    {
        delete[] const_cast<T*>(this->matrix);
    }

    const_cast<goMath::Matrix<T>*>(this)->externalData = true;
    const_cast<goMath::Matrix<T>*>(this)->matrix = const_cast<T*>(data);
    const_cast<goMath::Matrix<T>*>(this)->rows = r;
    const_cast<goMath::Matrix<T>*>(this)->columns = c;
    if (leadingDim == 0)
    {
        if (goMath::Matrix<T>::rowMajor)
        {
            const_cast<goMath::Matrix<T>*>(this)->leadingDimension = c;
        }
        else
        {
            const_cast<goMath::Matrix<T>*>(this)->leadingDimension = r;
        }
    }
    else
    {
        const_cast<goMath::Matrix<T>*>(this)->leadingDimension = leadingDim;
    }
    return true; 
}

/** 
 * @brief Get a transposed copy of this matrix.
 *
 * @param trans Contains the transpose after the method returned.
 */
template <class T>
void goMath::Matrix<T>::getTranspose (goMath::Matrix<T>& trans) const
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

template <class T>
goMath::Matrix<T> goMath::Matrix<T>::getTranspose () const
{
    goMath::Matrix<T> trans;
    this->getTranspose (trans);
    return trans;
}

/** 
 * @brief Flip in row or column direction.
 * 
 * @param dim If 0, flips rows, otherwise flips columns.
 */
template <class T>
void goMath::Matrix<T>::flip (goSize_t dim)
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
void goMath::Matrix<T>::shiftRows (goIndex_t offset, goMath::Matrix<T>& ret) const
{
    if (ret.getRows() != this->getRows() || ret.getColumns() != this->getColumns())
    {
        ret.resize (this->getRows(), this->getColumns());
    }

    goSize_t sz = this->getRows();
    goSize_t newIndex = (offset >= 0) ? (offset) : (sz + offset);
    const goMath::Vector<T> ref(0);
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
void goMath::Matrix<T>::shiftColumns (goIndex_t offset, goMath::Matrix<T>& ret) const
{
    if (ret.getRows() != this->getRows() || ret.getColumns() != this->getColumns())
    {
        ret.resize (this->getRows(), this->getColumns());
    }

    goSize_t sz = this->getColumns();
    goSize_t newIndex = (offset >= 0) ? (offset) : (sz + offset);
    const goMath::Vector<T> ref(0);
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
 * @param trans    Transpose while copying (target = transpose(this))
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::Matrix<T>::copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goMath::Matrix<T>& target, bool trans) const
{
    return this->copy (startRow, startCol, endRow, endCol, 0, 0, target, trans);
}

/** 
 * @brief Copy rectangular sub-matrix to another matrix.
 * 
 * Copies from this matrix the sub-matrix (startRow...endRow , startCol...endCol) to the sub-matrix in target starting at target_row,target_col.
 *
 * Does not do resizing. If the target is too small, nothing is copied.
 *
 * @param startRow Starting row in this matrix.
 * @param startCol Starting column in this matrix.
 * @param endRow   End row in this matrix (inclusive)
 * @param endCol   End column in this matrix (inclusive)
 * @param target_row Starting row in target
 * @param target_col Starting column in target
 * @param target     Target matrix
 * @param trans    Transpose while copying (target = transpose(this))
 *
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::Matrix<T>::copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goSize_t target_row, goSize_t target_col, goMath::Matrix<T>& target, bool trans) const
{
    goSize_t num_rows = endRow - startRow + 1;
    goSize_t num_cols = endCol - startCol + 1;

    if (!trans)
    {
        if (target.getRows() - target_row < num_rows || target.getColumns() - target_col < num_cols)
        {
            goLog::warning ("goMath::Matrix::copy(): target too small. Not copying.");
            return false;
        }

        goMath::Matrix<T> refM(0,0);
        this->ref (startRow, startCol, num_rows, num_cols, refM);
        goMath::Matrix<T> refTarget(0,0);
        target.ref (target_row, target_col, num_rows, num_cols, refTarget);
        refTarget = refM;
    }
    else
        //= Copy transposed directly
    {
        if (target.getRows() - target_row < num_cols || target.getColumns() - target_col < num_rows)
        {
            goLog::warning ("goMath::Matrix::copy(): target too small. Not copying.");
            return false;
        }
        
        ptrdiff_t ld_source = this->getLeadingDimension ();
        ptrdiff_t ld_target = target.getLeadingDimension ();
        const T* source_p = this->getPtr() + startRow * ld_source + startCol;
        T* target_p = target.getPtr() + target_row * ld_target + target_col;
        for (goSize_t i = 0; i < num_cols; ++i)
        {
            const T* source_p_col = source_p;
            T* target_p_row = target_p;
            for (goSize_t j = 0; j < num_rows; ++j)
            {
                *target_p_row = *source_p_col;
                ++target_p_row;
                source_p_col += ld_source;
            }
            ++source_p;
            target_p += ld_target;
        }
    }

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
bool goMath::Matrix<T>::copy (goMath::Matrix<T>& target) const
{
    return this->copy (0, 0, this->getRows() - 1, this->getColumns() - 1, 0, 0, target);
}


/** 
 * @brief Transpose the data.
 * This is very slow and should be used scarcely.
 * For Multiplication with transposition, use goMath::MatrixMult().
 */
template <class T>
void goMath::Matrix<T>::transpose ()
{
    goMath::Matrix<T> temp;
    this->getTranspose(temp);

    if (true != this->reshape (this->getColumns(), this->getRows()))
    {
      goLog::error ("Matrix: Reshaping for transpose() did not work.");
      return;
    }

    *this = temp;
}

template <class T>
bool goMath::Matrix<T>::operator== (const goMath::Matrix<T>& other) const
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
bool goMath::Matrix<T>::operator!= (const goMath::Matrix<T>& other) const
{
    return !(*this == other);
}

/** 
 * @brief Sum over all elements.
 * 
 * @return Sum over all elements.
 */
template <class T>
T goMath::Matrix<T>::sum () const
{
    T s = T(0);
    goSize_t r = this->getRows();
    goSize_t c = this->getColumns();
    for (goSize_t i = 0; i < r; ++i)
    {
        for (goSize_t j = 0; j < c; ++j)
        {
            s += (*this)(i,j);
        }
    }
    return s;
}

/** 
 * @brief Sum over all columns / all rows.
 * 
 * @param dimension If 0, sums over all columns and the result is a row vector.
 * Else, sums over all rows and the result is a column vector.
 * @param ret Result.
 */
template <class T>
void goMath::Matrix<T>::sum (int dimension, goMath::Matrix<T>& ret) const
{
    if (dimension == 0)
    {
        goSize_t r = this->getRows();
        goSize_t c = this->getColumns();
        ret.resize (1, c);
        ret.fill (T(0));
        for (goSize_t i = 0; i < r; ++i)
        {
            for (goSize_t j = 0; j < c; ++j)
            {
                ret(0,j) += (*this)(i,j);
            }
        }
    }
    else
    {
        goSize_t r = this->getRows();
        goSize_t c = this->getColumns();
        ret.resize (r, 1);
        ret.fill (T(0));
        for (goSize_t i = 0; i < r; ++i)
        {
            T temp = T(0);
            for (goSize_t j = 0; j < c; ++j)
            {
                temp += (*this)(i,j);
            }
            ret(i,0) = temp;
        }
    }
}

template <class T>
void goMath::Matrix<T>::sum (int dimension, goMath::Vector<T>& ret) const
{
    goMath::Matrix<T> M;
    this->sum (dimension, M);
    if (dimension == 0)
    {
        M.copyRow (0, ret);
    }
    else
    {
        M.copyColumn (0, ret);
    }
}

/*
 * @brief Convenience method for in-line use in calculations.
 * 
 * THIS DOES NOT WORK AS SOON AS SOMETHING LIKE THIS APPEARS: goMath::Matrixf M4 = M1(1,1,3,3) * M1(2,0,4,3);
 * THEREFORE IT IS DUMPED AND LEFT HERE AS WARNING FOR THE FUTURE.
 */
//template <class T>
//const goMath::Matrix<T>& goMath::Matrix<T>::operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2) const
//{
//    static const goMath::Matrix<T> retRef;
//    retRef.setData (&(*this)(i1,j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
//    return retRef;
//}

/** 
 * @brief References sub-matrix from i1,j1 to i2,j2 into target.
 *
 * To make clear it is referencing, use the ref() method.
 */
template <class T>
void goMath::Matrix<T>::operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2, goMath::Matrix<T>& target) const
{
    assert (i1 <= i2 && j1 <= j2);
    const goMath::Matrix<T> temp;
    temp.setData (&(*this)(i1, j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
    target = temp;
}

/** 
 * @brief Copy source to sub-matrix from i1,j1 to i2,j2 in this matrix.
 *
 * @note Dimensions must agree and are assert()ed. Check logfile for warnings.
 */
template <class T>
void goMath::Matrix<T>::operator () (const goMath::Matrix<T>& source, goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2)
{
    assert (source.getRows() == (goSize_t)(i2-i1+1));
    assert (source.getColumns() == (goSize_t)(j2-j1+1));
    if (source.getRows() != static_cast<goSize_t>(i2-i1+1) || source.getColumns() != static_cast<goSize_t>(j2-j1+1))
    {
        goLog::warning ("goMath::Matrix::operator(): source dimensions do not agree with sub-matrix. Not copying.");
        return;
    }
    goMath::Matrix<T> temp (&(*this)(i1, j1), i2-i1+1, j2-j1+1, this->getLeadingDimension());
    temp = source;
}

namespace goMath {
    template<>
        goMath::Matrix<goDouble> Matrix<goDouble>::operator* (const goMath::Matrix<goDouble>& other) const
        {
            goSize_t M = this->getRows();
            goSize_t N = other.getColumns();
            goSize_t K = this->getColumns();
            goMath::Matrix<goDouble> C (this->getRows(), other.getColumns());
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
        goMath::Matrix<goFloat> Matrix<goFloat>::operator* (const goMath::Matrix<goFloat>& other) const
        {
            goSize_t M = this->getRows();
            goSize_t N = other.getColumns();
            goSize_t K = this->getColumns();
            goMath::Matrix<goFloat> C (this->getRows(), other.getColumns());
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
        Matrix<goFloat>& Matrix<goFloat>::operator*= (const Matrix<goFloat>& other)
        {
            goSize_t M = this->getRows();
            goSize_t N = other.getColumns();
            goSize_t K = this->getColumns();
            Matrix<goFloat> C (this->getRows(), other.getColumns());
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
        Matrix<goDouble>& Matrix<goDouble>::operator*= (const Matrix<goDouble>& other)
        {
            goSize_t M = this->getRows();
            goSize_t N = other.getColumns();
            goSize_t K = this->getColumns();
            Matrix<goDouble> C (this->getRows(), other.getColumns());
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
        goMath::Vector<goFloat> Matrix<goFloat>::operator* (const goMath::Vector<goFloat>& v) const
        {
            assert (v.getSize() == this->getColumns());
            goMath::Vector<goFloat> y (this->getRows());
            y.fill (0.0f);
            cblas_sgemv (CblasRowMajor, CblasNoTrans, 
                    this->getRows(), this->getColumns(), 
                    1.0, this->matrix, this->getLeadingDimension(), 
                    v.getPtr(), v.getStride(), 
                    0.0f, y.getPtr(), y.getStride());
            return y;
        }


    template<>
        goMath::Vector<goDouble> Matrix<goDouble>::operator* (const goMath::Vector<goDouble>& v) const
        {
            assert (v.getSize() == this->getColumns());
            goMath::Vector<goDouble> y (this->getRows());
            y.fill (0.0f);
            cblas_dgemv (CblasRowMajor, CblasNoTrans, 
                    this->getRows(), this->getColumns(), 
                    1.0, this->matrix, this->getLeadingDimension(), 
                    v.getPtr(), v.getStride(), 
                    0.0f, y.getPtr(), y.getStride());
            return y;
        }

};

template <class T>
goMath::Matrix<T> goMath::Matrix<T>::operator* (const goMath::Matrix<T>& other) const
{
    if (this->getColumns() != other.getRows())
    {
        goLog::warning ("goMath::Matrix::operator*: Matrix dimensions do not match.");
        return goMath::Matrix<T> (1,1);
    }
    goMath::Matrix<T> retval (getRows(), other.getColumns());
    goSize_t x, y;
    goSize_t columns = retval.getColumns();
    goMath::Vector<T> row;
    goMath::Vector<T> column;
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

/** 
 * @brief this = this * other
 * @note Uses CLBLAS for goFloat and goDouble types.
 * @see goMath::MatrixMult()
 * @param other A Matrix.
 * 
 * @return Reference to this.
 */
template <class T>
goMath::Matrix<T>& goMath::Matrix<T>::operator*= (const goMath::Matrix<T>& other)
{
    // Slow and reliable (and quick to hack).
    *this = *this * other;
    return *this;
}

  /** 
  * @brief Element-wise addition.
  * 
  * @param other 
  * 
  * @return this = this + other
  */
template <class T>
goMath::Matrix<T>& goMath::Matrix<T>::operator+= (const goMath::Matrix<T>& other)
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

template <class T>
goMath::Matrix<T>& goMath::Matrix<T>::operator+= (T scalar)
{
    goSize_t i;
    goSize_t j;
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            (*this)(i,j) += scalar;
        }
    }
    return *this;
}

template <class T>
goMath::Matrix<T>& goMath::Matrix<T>::operator-= (T scalar)
{
    goSize_t i;
    goSize_t j;
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            (*this)(i,j) -= scalar;
        }
    }
    return *this;
}

//= Quite slow, quick hack.
template <class T>
goMath::Matrix<T> goMath::Matrix<T>::operator+ (const goMath::Matrix<T>& other) const
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    goMath::Matrix<T> C (this->getRows(), this->getColumns());
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
goMath::Matrix<T> goMath::Matrix<T>::operator-  (const goMath::Matrix<T>& other) const
{
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    goMath::Matrix<T> C (this->getRows(), this->getColumns());
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
goMath::Matrix<T>& goMath::Matrix<T>::operator-= (const goMath::Matrix<T>& other)
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
goMath::Matrix<T>& goMath::Matrix<T>::operator= (const goMath::Matrix<T>& other)
{
    if (this->rows != other.getRows() || this->columns != other.getColumns())
    {
      if (true != this->reshape (other.getRows(), other.getColumns()))
      {
	this->resize (other.getRows(), other.getColumns());
      }
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
bool goMath::Matrix<T>::multiplyElements (const goMath::Matrix<T>& other)
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

//template <class T>
//T goMath::Matrix<T>::norm () const
//{
//    goSize_t sz = this->getRows() * this->getColumns();
//    goSize_t i;
//    double retValue = 0.0;
//    T* mptr = this->matrix;
//    for (i = 0; i < sz; ++i)
//    {
//        retValue += *mptr * *mptr;
//        ++mptr;
//    }
//    return static_cast<T>(sqrt(retValue));
//}

/** 
 * @brief Calculate the Frobenius norm \f$ \left( \sum_i\sum_j |a_{i,j}|^2 \right)^{\frac{1}{2}} = \left(\mathrm{trace} (A A^T)\right)^\frac{1}{2} = \left( \sum_i \sigma_i^2 \right)^\frac{1}{2} \f$
 * where \f$ \sigma_i \f$ is the \f$ i\f$'th singular value.
 *
 * @return The Frobenius norm of this matrix.
 */
template <class T>
T goMath::Matrix<T>::norm () const
{
    goSize_t i,j;
    double retValue = 0.0;
    goSize_t R = this->getRows();
    goSize_t C = this->getColumns();
    for (i = 0; i < R; ++i)
    {
        T temp = T(0);
        for (j = 0; j < C; ++j)
        {
            temp = (*this)(i,j);
            retValue += temp * temp;
        }
    }
//    goSize_t sz = this->getRows() * this->getColumns();
//    T* mptr = this->matrix;
//    for (i = 0; i < sz; ++i)
//    {
//        retValue += *mptr * *mptr;
//        ++mptr;
//    }
    return static_cast<T>(sqrt(retValue));
}

//    goSize_t i,j;
//    goComplexf retValue (0.0,0.0);
//    goSize_t R = goMath::min<goSize_t> (this->getRows(),this->getColumns());
//    for (i = 0; i < R; ++i)
//    {
//        retValue = retValue + (*this)(i,i);
//    }
//    goSize_t sz = this->getRows() * this->getColumns();
//    T* mptr = this->matrix;
//    for (i = 0; i < sz; ++i)
//    {
//        retValue += *mptr * *mptr;
//        ++mptr;
//    }
//    return static_cast<T>(sqrt(retValue));
//}

/** 
 * @brief Calculates the trace \f$ tr(A) = \sum_i A_{i,i} \f$.
 *
 * Works also for non-square matrices; in that case, the loop runs
 * to the smaller of the two dimensions.
 *
 * @return The trace of the matrix.
 */
template <class T>
T goMath::Matrix<T>::trace () const
{
    goSize_t i;
    double retValue = 0.0;
    goSize_t R = goMath::min<goSize_t> (this->getRows(),this->getColumns());
    for (i = 0; i < R; ++i)
    {
        retValue += (*this)(i,i);
    }
//    goSize_t sz = this->getRows() * this->getColumns();
//    T* mptr = this->matrix;
//    for (i = 0; i < sz; ++i)
//    {
//        retValue += *mptr * *mptr;
//        ++mptr;
//    }
    return static_cast<T>(retValue);
}

/** 
 * @brief Load identity matrix.
 */
template <class T>
void goMath::Matrix<T>::setIdentity()
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
void goMath::Matrix<T>::fill(T v)
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
bool goMath::Matrix<T>::writeASCII (FILE* f) const
{
    if (!f)
    {
        return false;
    }

    goSize_t R = this->getRows();
    goSize_t C = this->getColumns();
    goSize_t r, c;
    fprintf (f, "goMath::Matrix\n");
    fprintf (f, "size %d %d\n", (int)R, (int)C);
    goDouble d;
    for (r = 0; r < R; ++r)
    {
        for (c = 0; c < C; ++c)
        {
            d = (goDouble)(*this)(r,c);
            fprintf (f, "%.20lf ", d);
        }
        fprintf (f, "\n");
    }
    return true;
}

template<class T>
bool goMath::Matrix<T>::writeASCII (const char* fname) const
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
bool goMath::Matrix<T>::readASCII (FILE* f)
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

    if (s != "goMath::Matrix" && s != "goMatrix")
    {
        ::rewind (f);
        if (!this->readASCIISimple (f))
        {
            goString msg = "goMath::Matrix::readASCII: expected goMath::Matrix or goMatrix, got ";
            msg += s.toCharPtr ();
            msg += " and readASCIISimple also failed.";
            goLog::warning (msg);
            return false;
        }
        return true;
    }

    if (!goFileIO::readASCIILine (f,s))
    {
        return false;
    }

    goList<goString> words;
    s.getWords (words);

    if (words.getSize() != 3 || words.getFrontElement()->elem != "size")
    {
        goString msg = "goMath::Matrix::readASCII: expected size x y, got ";
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

template <class T>
bool goMath::Matrix<T>::readASCIISimple (FILE* f)
{
    goList<goString> words;
    goString line;
    goFileIO::readASCIILine (f, line);
    line.getWords (words);
    while (words.getFront()[0] == '#' && !::feof (f))
    {
        words.erase ();
        line = "";
        goFileIO::readASCIILine (f, line);
        line.getWords (words);
    }

    goSize_t columns = words.getSize ();
    goSize_t rows = 1;

    goSize_t chunkSize = goMath::max<goSize_t> (256, columns);
    goArray<goDouble> buffer (chunkSize);

    do
    {
        if ((goSize_t)words.getSize() != columns)
        {
            //goString s = "goMath::Matrix::readASCIISimple(): wrong column count in row ";
            //s += (int) rows;
            //s += " -- stopping reading.";
            //goLog::warning (s);
            //return false;
            break;
        }
        if (rows * columns >= (goSize_t)buffer.getSize())
        {
            buffer.resize (buffer.getSize() + chunkSize);
        }

        goSize_t start = (rows - 1) * columns;
        goSize_t i = start;
        goList<goString>::iterator iter = words.begin ();
        while (iter != words.end())
        {
            buffer[i] = iter->toDouble ();
            ++i;
            ++iter;
        }

        if (::feof (f))
            break;
        
        words.erase ();
        line = "";
        goFileIO::readASCIILine (f, line);
        line.getWords (words);
        if ((goSize_t)words.getSize() != columns)
            break;

        ++rows;
    } while (true);

    this->resize (rows, columns);
    goSize_t i = 0;
    for (goSize_t r = 0; r < rows; ++r)
    {
        for (goSize_t c = 0; c < columns; ++c, ++i)
        {
            (*this)(r, c) = static_cast<T>(buffer[i]);
        }
    }

    return true;
}

template<class T>
bool goMath::Matrix<T>::readASCII (const char* fname) 
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
goMath::Vector<goDouble> goMath::Matrix<goDouble>::operator* (const goMath::Vector<goFloat>& v) const
{
    assert (v.getSize() == this->getColumns());
    goMath::Vector<goDouble> y (v.getSize());
    y.fill (0.0f);
    matrixVectorMult (*this, v, y);
    return y;
}
#endif

template <class Tm, class Tv, class Tr>
static void matrixVectorMult (const goMath::Matrix<Tm>& m, const goMath::Vector<Tv>& v, goMath::Vector<Tr>& r)
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
goMath::Vector<T> goMath::Matrix<T>::operator* (const goMath::Vector<T>& v) const
{
    assert (v.getSize() == this->getColumns());
    goMath::Vector<T> y (v.getSize());
    y.fill (T(0));
    matrixVectorMult (*this, v, y);
    return y;
}
#endif


template <class T>
bool goMath::Matrix<T>::invert ()
{
    goLog::error ("goMath::Matrix::invert() not implemented for types other than goFloat and goDouble.");
    return false;
}


template <class T>
void goMath::Matrix<T>::power (T)
{
    goLog::warning ("goMatrix::power() not defined for this type.");
}

template<class T>
void goMath::matrixPower (goMath::Matrix<T>& A, T scalar)
{
    if (A.getRows() != A.getColumns())
    {
        goLog::warning ("goMatrixPower(): A is not quadratic.");
        return;
    }
    goMath::Eigenvalue<T> eig (A);
    const goMath::Matrix<T>& V = eig.getV();
    goMath::Matrix<T> Vtemp = V;
    goMath::Vector<T>& d = eig.getRealEigenvalues();
    //= V * D.^scalar * V'
    goSize_t n = d.getSize();
    for (goSize_t i = 0; i < n; ++i)
        d[i] = T(::pow (d[i],scalar));
    n = Vtemp.getRows ();
    goMath::Vector<T> refV;
    for (goSize_t i = 0; i < n; ++i)
    {
        Vtemp.refRow (i,refV);
        refV *= d;  //= Element-wise multiplication
    }
    goMath::matrixMult<T> (T(1), Vtemp, false, V, true, T(0), A);
}

namespace goMath {
    template<>
        void matrixPower (goMath::Matrix<goComplexf>& A, goComplexf scalar)
        {
            goLog::error ("goMath::MatrixPower() not implemented for complex.");
        }

    template<>
        void matrixPower (goMath::Matrix<goComplexd>& A, goComplexd scalar)
        {
            goLog::error ("goMath::MatrixPower() not implemented for complex.");
        }
    template <>
        goComplexf Matrix<goComplexf>::trace () const
        {
            goLog::error ("goMath::Matrix::trace() not implemented for complex.");
            return goComplexf (0.0,0.0);
        }
    template <>
        void Matrix<goFloat>::power (goFloat scalar)
        {
            matrixPower<goFloat> (*this, scalar);
        }

    template <>
        void Matrix<goDouble>::power (goDouble scalar)
        {
            matrixPower<goDouble> (*this, scalar);
        }
    template <>
        goComplexf Matrix<goComplexf>::norm () const
        {
            goLog::error("goMath::Matrix::norm not implemented for complex types.");
            return goComplexf(0.0f,0.0f);
        }

    //template <>
    //goComplexd goMath::Matrix<goComplexd>::norm () const
    //{
    //    goLog::error("goMath::Matrix::norm not implemented for complex types.");
    //    return goComplexd(0.0f,0.0f);
    //}

    template <>
        bool Matrix<goComplexf>::writeASCII (FILE* f) const
        {
            goLog::error ("read/write for goMath::Matrix not implemented for complex types.");
            return false;
        };
    template <>
        bool Matrix<goComplexf>::readASCII (FILE* f)
        {
            goLog::error ("read/write for goMath::Matrix not implemented for complex types.");
            return false;
        };

    template <>
        bool Matrix<goFloat>::invert ()
        {
            //= Factorise A P = L U
            goSize_t M = this->getColumns();
            if (M != this->getRows())
            {
                goLog::warning ("goMath::Matrix::invert(): tried to invert non-quadratic matrix.");
                return false;
            }
            int* P = new int [M];
            if (clapack_sgetrf (CblasRowMajor, M, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
            {
                delete[] P;
                P = 0;
                return false;
            }
            if (clapack_sgetri (CblasRowMajor, M, this->getPtr(), this->getLeadingDimension(), P) != 0)
            {
                delete[] P;
                P = 0;
                return false;
            }
            delete[] P;
            P = 0;
            return true;
        }

    template <>
        bool Matrix<goDouble>::invert ()
        {
            //= Factorise A P = L U
            goSize_t M = this->getColumns();
            if (M != this->getRows())
            {
                goLog::warning ("goMath::Matrix::invert(): tried to invert non-quadratic matrix.");
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


    template<>
        void matrixMult<goFloat> (goFloat alpha, const goMath::Matrix<goFloat>& A, bool transA, 
                const goMath::Matrix<goFloat>& B, bool transB, 
                goFloat beta, goMath::Matrix<goFloat>& C)
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
        void matrixMult<goDouble> (goDouble alpha, const goMath::Matrix<goDouble>& A, bool transA, 
                const goMath::Matrix<goDouble>& B, bool transB, 
                goDouble beta, goMath::Matrix<goDouble>& C)
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
        bool matrixVectorMult<goFloat> (goFloat alpha, const goMath::Matrix<goFloat>& A, bool transA,
                const goMath::Vector<goFloat>& x, goFloat beta, goMath::Vector<goFloat>& y)
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
        bool matrixVectorMult<goDouble> (goDouble alpha, const goMath::Matrix<goDouble>& A, bool transA,
                const goMath::Vector<goDouble>& x, goDouble beta, goMath::Vector<goDouble>& y)
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
};
/* Instantiation */
template void  goMath::matrixPower<goFloat> (goMath::Matrix<goFloat>& A, goFloat);
template class goMath::Matrix<goDouble>;
template class goMath::Matrix<goFloat>;
// template class goMath::Matrix<goIndex_t>;
// template class goMath::Matrix<goSize_t>;
// template class goMath::Matrix<goInt32>;
template class goMath::Matrix<goComplexf>;

template class goMath::Matrix<goInt8>;
template class goMath::Matrix<goUInt8>;
template class goMath::Matrix<goInt16>;
template class goMath::Matrix<goUInt16>;
template class goMath::Matrix<goInt32>;
template class goMath::Matrix<goUInt32>;
#ifdef HAVE_INT64
template class goMath::Matrix<goInt64>;
template class goMath::Matrix<goUInt64>;
#endif
