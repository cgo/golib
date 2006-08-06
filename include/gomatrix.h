#ifndef GOMATRIX_H
#define GOMATRIX_H


#include <gotypes.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

template <class T> class goVector;

/** \addtogroup math 
 * @{ */

/*!
 * \brief Matrix class.
 *
 * This class uses linear data storage, row major, i.e. rows are contigious 
 * in memory. It is designed to be compatible with
 * CBLAS operations. Leading dimension in CBLAS is here the
 * number of columns -- get it via getLeadingDimension().
 *
 * There is no support for [i][j] indexing anymore. Use (i,j) instead.
 * 
 * If you need to instantiate the template for a new type,
 * include gomatrix.hpp and know about the implications.
 *
 * \author Christian Gosch
 */
template <class T>
class goMatrix 
{
 public:
  /*!
   * @param y Number of rows.
   * @param x Number of columns.
   */
  goMatrix (goSize_t rows = 4, goSize_t cols = 4)
    : externalData (false), matrix (0), rows (rows), columns (cols)
  {
    this->matrix = new T[rows * cols];
  };

  /** 
  * @brief Copy constructor.
  * 
  * @param other Other matrix. Will be deep copied.
  */
  goMatrix (const goMatrix<T>& other)
    : externalData (false), matrix (0), rows (0), columns (0)
  {
    *this = other;
  };

  /** 
  * @brief Constructor for using external data.
  * 
  * @param data Pointer to the external data.
  * @param r Rows
  * @param c Columns
  */
  goMatrix (T* data, goSize_t r, goSize_t c)
    : externalData (true), matrix (data), rows (r), columns (c)
  {
  };

  virtual ~goMatrix ()
  {
    if (!this->externalData && this->matrix)
    {
        delete[] this->matrix;
        this->matrix = 0;
        this->rows = 0;
        this->columns = 0;
    }
  };
  
  /** 
  * @brief Set external data.
  * 
  * @param data Pointer to the external data.
  * @param r    Rows 
  * @param c    Columns
  * 
  * @return True.
  */
  bool setData   (T* data, goSize_t r, goSize_t c)
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
  };

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
  bool resize (goSize_t rows, goSize_t cols)
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
  };

  /** 
  * @brief Transpose the data.
  * This is very slow and should be used scarcely.
  * For Multiplication with transposition, use goMatrixMult().
  */
  void transpose ()
  {
    goMatrix<T> temp;
    this->getTranspose(temp);
    *this = temp;
  };

  /** 
  * @brief Get a transposed copy of this matrix.
  *
  * @param trans Contains the transpose after the method returned.
  */
  void getTranspose (goMatrix<T>& trans)
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
  };

  /** 
  * @brief Deep copy operator.
  * 
  * @param other Other matrix.
  * 
  * @return Reference to this matrix.
  */
  goMatrix<T>& operator= (const goMatrix<T>& other)
  {
    if (this->rows != other.getRows() || this->columns != other.getColumns())
    {
        this->resize (other.getRows(), other.getColumns());
    }
    memcpy (this->matrix, other.getData(), this->rows * this->columns * sizeof(T));
    return *this;
  };

  /** 
  * @brief Get data pointer.
  * @return Pointer to the matrix array.
  */
  T*       getData   () { return this->matrix; };
  /** 
  * @brief Get data pointer.
  * @return Const pointer to the matrix array.
  */
  const T* getData   () const { return this->matrix; };
  
  /** 
  * @brief Get number of columns.
  * 
  * @return Number of columns.
  */
  goSize_t   getColumns () const 
  { 
    return this->columns;
  };
  /** 
  * @brief Get number of rows.
  * 
  * @return Number of rows.
  */
  goSize_t   getRows () const 
  { 
    return this->rows;
  };

  /** 
  * @brief Get the leading dimension.
  * In row major order, this is the number of columns.
  * This can be directly used as LDX parameter in CBLAS routines.<br>
  *  From http://www.inf.bv.tum.de/~heisserer/softwarelab04/index.html<br>
  *    "Note that for cblas-functions the leading dimension (for 2D arrays in C-fashion, i.e. row major order) 
  *     is the number of columns of the matrix (not the rows as in Fortran).
  *     The leading dimension is the number of entries in memory that separate the e.g. 
  *     first elements of rows in c-fashion storage 
  *     (row major order, i.e. elements of one row are contiguous in memory).
  *     As Fortran stores in column major order the leading dimension is the number of rows."
  * 
  * @return Leading dimension.
  */
  goSize_t getLeadingDimension () const
  {
    return this->columns;
  };
 
  // TNT compatibility methods BEGIN
  inline int        dim1 () const { return this->getRows(); };
  inline int        dim2 () const { return this->getColumns(); };
  inline const goMatrix<T>& copy () const { return *this;};  // NOTE: Makes a deep copy here
                                                             // and a reference in TNT
  // TNT compatibility methods END  
 
  /** 
  * @brief Makes a vector reference a row from this matrix.
  * 
  * @note The data are not copied.
  *
  * @param row Row to reference
  * @param v   Vector that holds the reference after the method returns.
  */
  void              refRow    (goSize_t row, goVector<T>& v)
  {
    v.setData (&this->matrix[row * this->getColumns()], this->getColumns(), 1);
  };

  /** 
   * @brief Const reference to row.
   * 
   * @note Argument <b>v will be changed</b> even though it is const.
   * This is just to provide a consistent way to reference rows and columns in const matrices.
   * It may be bad style from one point of view, on the other hand it enables referencing
   * without further hassle.
   * 
   * @param row Row index.
   * @param v   WILL BE CHANGED and contains the reference.
   */
  void              refRow    (goSize_t row, const goVector<T>& v) const
  {
    const_cast<goVector<T>&>(v).setData (&const_cast<T*>(this->matrix)[row * this->getColumns()], this->getColumns(), 1);
  };
  /** 
  * @brief Makes a vector reference a column from this matrix.
  * 
  * @note The data are not copied.
  *
  * @param column Column to reference
  * @param v   Vector that holds the reference after the method returns.
  */
  void              refColumn (goSize_t column, goVector<T>& v)
  {
    v.setData (&this->matrix[column], this->getRows(), this->getColumns());
  };
  /** 
   * @brief Const reference to column.
   * 
   * @note Argument <b>v will be changed</b> even though it is const.
   * This is just to provide a consistent way to reference rows and columns in const matrices.
   * It may be bad style from one point of view, on the other hand it enables referencing
   * without further hassle.
   * 
   * @param column Column index.
   * @param v   WILL BE CHANGED and contains the reference.
   */
  void              refColumn    (goSize_t column, const goVector<T>& v) const
  {
    const_cast<goVector<T>&>(v).setData (&const_cast<T*>(this->matrix)[column], this->getRows(), this->getColumns());
  };

  T&                operator() (goIndex_t i, goIndex_t j)
  {
    return this->matrix[i * this->columns + j];
  };
  const T&          operator() (goIndex_t i, goIndex_t j) const
  {
    return this->matrix[i * this->columns + j];
  };

  T&       operator[] (goSize_t index) { return this->matrix[index]; };
  const T& operator[] (goSize_t index) const { return this->matrix[index]; };
  inline goMatrix<T>		operator*  (const goMatrix<T>& other);

  //= Quite slow, quick hack.
  goMatrix<T>		operator-  (const goMatrix<T>& other)
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
  };

  //= Quite slow, quick hack.
  goMatrix<T>		operator+  (const goMatrix<T>& other)
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
  };

  inline goMatrix<T>&		operator*= (const goMatrix<T>& other);

  /** 
  * @brief Element-wise addition.
  * 
  * @param other 
  * 
  * @return this = this + other
  */
  goMatrix<T>&		operator+= (const goMatrix<T>& other)
  {
    assert (this->getRows() == other.getRows());
    assert (this->getColumns() == other.getColumns());
    goSize_t i;
    goSize_t j;
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            (*this)(i,j) += other(i,j);
        }
    }
    return *this;
  };

  /** 
  * @brief Element-wise subtraction.
  * 
  * @param other 
  * 
  * @return this = this - other
  */
  goMatrix<T>&		operator-= (const goMatrix<T>& other)
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
  };

  /** 
  * @brief Matrix vector multiplication.
  * 
  * @param v 
  * 
  * @return this * v
  */
  inline goVector<T>       operator*  (const goVector<T>& v);

  /** 
  * @brief Multiplication by a scalar.
  * 
  * @param scalar 
  * 
  * @return this .* scalar.
  */
  goMatrix<T>&		operator*= (T scalar)
  {
    goSize_t i;
    goSize_t j;
    for (i = 0; i < this->getRows(); ++i)
    {
        for (j = 0; j < this->getColumns(); ++j)
        {
            (*this)(i,j) *= scalar;
        }
    }
    return *this;
  };

  /** 
  * @brief Division by a scalar.
  * 
  * @param scalar 
  * 
  * @return this ./ scalar.
  */
  goMatrix<T>&		operator/= (T scalar)
  {
    if (scalar != T(0))
    {
        *this *= T(1) / scalar;
    }
    return *this;
  };

  /** 
  * @brief Element-wise multiplication.
  * 
  * *this .* other
  *
  * @param other Other matrix.
  * 
  * @return True
  */
  bool              multiplyElements (const goMatrix<T>& other)
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
  };

  /** 
  * @brief Calculate the Frobenius norm \f$ \left( \sum_i\sum_j |a_{i,j}|^2 \right)^{\frac{1}{2}} = \mathrm{trace} (A A^T) = \sum_i \sigma_i^2 \f$
  * where \f$ \sigma_i \f$ is the \f$ i\f$'th singular value.
  *
  * @return The Frobenius norm of this matrix.
  */
  T norm () const
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
  };

  /** 
  * @brief Load identity matrix.
  */
  void setUnity()
  {
    this->setIdentity();
  };
  /** 
  * @brief Load identity matrix.
  */
  void setIdentity()
  {
	this->fill(T(0));
	goSize_t n = goMath::min(this->getRows(), this->getColumns());
	goSize_t i;
	for (i = 0; i < n; ++i)
	{
		(*this)(i,i) = T(1);
	}
  };
  /** 
  * @brief Fill matrix with a value v.
  * 
  * @param v Value to fill with.
  */
  void fill(T v)
  {
    goSize_t sz = this->getRows() * this->getColumns();
    goSize_t i;
    T* mptr = this->matrix;
    for (i = 0; i < sz; ++i)
    {
        *mptr = v;
        ++mptr;
    }
  };

  void print()
    {
        goSize_t i;
        goSize_t j;
        for (i = 0; i < this->getRows(); i++)
        {
            for (j = 0; j < this->getColumns(); j++)
            {
                std::cout << (*this)(i,j) << " ";
            }
            std::cout << "\n";
        }
        std::cout << std::endl;
    };

 protected:
  bool               externalData;
  T*                 matrix;
  goSize_t           rows;
  goSize_t           columns;
};

/** 
* @brief Calculate \f$C = \alpha \cdot A \cdot B + \beta \cdot C \f$.
* 
* A and B can optionally be used as transpose.
* This function uses CBLAS functions.
*
* This function is implemented for goFloat and goDouble,
* using cblas_[s|d]gemm functions from CBLAS.
*
* @param alpha    Multiplicative factor for A
* @param A        Matrix A
* @param transA   If true, A will be used transposed.
* @param B        Matrix B
* @param transB   If true, B will be used transposed.
* @param beta     Multiplicative factor for C
* @param C        Matrix C, also holds the result.
*/
template<class T>
void goMatrixMult (T alpha, const goMatrix<T>& A, bool transA, 
                            const goMatrix<T>& B, bool transB, 
                   T beta, goMatrix<T>& C);

typedef goMatrix<goDouble> goMatrixd;
typedef goMatrix<goFloat>  goMatrixf;
/** @} */

#endif /* __GOMATRIX_H */
