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
  goMatrix (goSize_t rows = 4, goSize_t cols = 4);

  /** 
  * @brief Copy constructor.
  * 
  * @param other Other matrix. Will be deep copied.
  */
  goMatrix (const goMatrix<T>& other);

  /** 
  * @brief Constructor for using external data.
  * 
  * @param data Pointer to the external data.
  * @param r Rows
  * @param c Columns
  * @param leadingDim leading dimension -- if 0, is set to c.
  * @param rowMajor Leave at true (default) -- not implemented yet (and might never be).
  */
  goMatrix (T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0, bool rowMajor = true);

  virtual ~goMatrix ();
  
  bool setData (T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0);
  bool setData (const T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0) const;

  bool resize (goSize_t rows, goSize_t cols);
  
  template <class To>
  bool resize (const goMatrix<To>& o)
  {
      return this->resize (o.getRows(),o.getColumns());
  };

  void transpose ();
  void getTranspose (goMatrix<T>& trans) const;

  bool invert ();

  void power (T scalar);

  void flip (goSize_t dim = 0);

  //= FIXME: column major is almost not implemented yet. It may never be.
  bool getRowMajor () const { return this->rowMajor; };

  goMatrix<T>& operator= (const goMatrix<T>& other);

  bool operator== (const goMatrix<T>& other) const;
  bool operator!= (const goMatrix<T>& other) const;

  /** 
  * @brief Get data pointer.
  * @return Pointer to the matrix array.
  */
  inline T*       getData   () { return this->matrix; };
  /** 
  * @brief Get data pointer.
  * @return Const pointer to the matrix array.
  */
  inline const T* getData   () const { return this->matrix; };
 
  /** 
   * @brief Get data pointer. Same as getData().
   */
  inline T*       getPtr    () { return this->matrix; };
  /** 
   * @brief Get data pointer. Same as getData().
   */
  inline const T* getPtr    () const { return this->matrix; };

  /** 
  * @brief Get number of columns.
  * 
  * @return Number of columns.
  */
  inline goSize_t   getColumns () const 
  { 
    return this->columns;
  };
  /** 
  * @brief Get number of rows.
  * 
  * @return Number of rows.
  */
  inline goSize_t   getRows () const 
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
  inline goSize_t getLeadingDimension () const
  {
    return this->leadingDimension;
  };

  inline T sum () const
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
  };

  /** 
   * @brief Sum over all columns / all rows.
   * 
   * @param dimension If 0, sums over all columns and the result is a row vector.
   * Else, sums over all rows and the result is a column vector.
   * @param ret Result.
   */
  template <class To>
  inline void sum (int dimension, goMatrix<To>& ret) const
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
  };
  template <class To>
  inline void sum (int dimension, goVector<To>& ret) const
  {
      goMatrix<To> M;
      this->sum (dimension, M);
      if (dimension == 0)
      {
          M.copyRow (0, ret);
      }
      else
      {
          M.copyColumn (0, ret);
      }
  };

  // TNT compatibility methods BEGIN
  inline int        dim1 () const { return this->getRows(); };
  inline int        dim2 () const { return this->getColumns(); };
  inline const goMatrix<T>& copy () const { return *this;};  // NOTE: Makes a deep copy here
                                                             // and a reference in TNT
  // TNT compatibility methods END  

  /** 
   * @brief Make a reference to sub-matrix.
   * 
   * refMatrix will be initialised to refer to the sub-matrix starting at
   * (startRow,startColumn) and extending for num_rows and num_cols rows and columns,
   * respectively.
   *
   * @note No bound checks are done.
   *
   * @param startRow     Start element.
   * @param startColumn  Start element.
   * @param num_rows     Number of rows.
   * @param num_cols     Number of columns.
   * @param refMatrix    Refers to the defined sub-matrix on return.
   */
  inline void ref (goSize_t startRow, goSize_t startColumn, 
                   goSize_t num_rows, goSize_t num_cols, goMatrix<T>& refMatrix)
  {
      refMatrix.setData (&(*this)(startRow,startColumn), num_rows, num_cols, this->getLeadingDimension());
  };

  bool copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goMatrix<T>& target);
  bool copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goSize_t target_row, goSize_t target_col, goMatrix<T>& target);
  bool copy (goMatrix<T>& target);

  /** 
   * @brief Make a reference to sub-matrix.
   * 
   * refMatrix will be initialised to refer to the sub-matrix starting at
   * (startRow,startColumn) and extending for num_rows and num_cols rows and columns,
   * respectively.
   *
   * @note No bound checks are done.
   *
   * @param startRow     Start element.
   * @param startColumn  Start element.
   * @param num_rows     Number of rows.
   * @param num_cols     Number of columns.
   * @param refMatrix    Refers to the defined sub-matrix on return.
   */
  inline void ref (goSize_t startRow, goSize_t startColumn, 
                   goSize_t num_rows, goSize_t num_cols, const goMatrix<T>& refMatrix) const
  {
      const_cast<goMatrix<T>&>(refMatrix).setData (const_cast<T*>(&(*this)(startRow,startColumn)), num_rows, num_cols, this->getLeadingDimension());
  };

  /** 
  * @brief Makes a vector reference a row from this matrix.
  * 
  * @note The data are not copied.
  *
  * @param row Row to reference
  * @param v   Vector that holds the reference after the method returns.
  */
  inline void              refRow    (goSize_t row, goVector<T>& v)
  {
    v.setData (&(*this)(row,0), this->getColumns(), 1);
  };
  /** 
   * @brief Reference to a sub row.
   * 
   * @param row    Start element row
   * @param column Start element column
   * @param length length of vector
   * @param v      Vector that contains the reference after the method call.
   */
  inline void              refRow    (goSize_t row, goSize_t column, goSize_t length, goVector<T>& v)
  {
    v.setData (&(*this)(row,column), length, 1);
  };

  inline void setRow (goSize_t row, const goVector<T>& v)
  {
      goSize_t sz = this->getColumns();
      assert (v.getSize() >= sz);
      for (goSize_t i = 0; i < sz; ++i)
      {
          (*this)(row, i) = v[i];
      }
  };

  inline void setColumn (goSize_t col, const goVector<T>& v)
  {
      goSize_t sz = this->getRows();
      assert (v.getSize() >= sz);
      for (goSize_t i = 0; i < sz; ++i)
      {
          (*this)(i, col) = v[i];
      }
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
  inline void              refRow    (goSize_t row, const goVector<T>& v) const
  {
    const_cast<goVector<T>&>(v).setData (const_cast<T*>(&(*this)(row,0)), this->getColumns(), 1);
  };
  /** 
   * @brief Reference to a sub row.
   * 
   * @param row    Start element row
   * @param column Start element column
   * @param length length of vector
   * @param v      Vector that contains the reference after the method call.
   */
  inline void              refRow    (goSize_t row, goSize_t column, goSize_t length, const goVector<T>& v) const
  {
    const_cast<goVector<T>&>(v).setData (const_cast<T*>(&(*this)(row,column)), length, 1);
  };

  /** 
  * @brief Makes a vector reference a column from this matrix.
  * 
  * @note The data are not copied.
  *
  * @param column Column to reference
  * @param v   Vector that holds the reference after the method returns.
  */
  inline void              refColumn (goSize_t column, goVector<T>& v)
  {
      v.setData (&(*this)(0,column), this->getRows(), this->getLeadingDimension());
  };
  /** 
   * @brief Reference to a sub column.
   * 
   * @param row    Start element row
   * @param column Start element column
   * @param length length of vector
   * @param v      Vector that contains the reference after the method call.
   */
  inline void              refColumn (goSize_t row, goSize_t column, goSize_t length, goVector<T>& v)
  {
      v.setData (&(*this)(row,column), length, this->getLeadingDimension());
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
  inline void              refColumn    (goSize_t column, const goVector<T>& v) const
  {
      const_cast<goVector<T>&>(v).setData (const_cast<T*>(&(*this)(0,column)), this->getRows(), this->getLeadingDimension());
  };
  /** 
   * @brief Reference to a sub column.
   * 
   * @param row    Start element row
   * @param column Start element column
   * @param length length of vector
   * @param v      Vector that contains the reference after the method call.
   */
  inline void              refColumn    (goSize_t row, goSize_t column, goSize_t length, const goVector<T>& v) const
  {
      const_cast<goVector<T>&>(v).setData (const_cast<T*>(&(*this)(row,column)), length, this->getLeadingDimension());
  };

  /** 
   * @brief Copies a row to vector vRet.
   *
   * vRet is resized if it vRet.getSize() != this->getColumns().
   * 
   * @param row   Row to copy.
   * @param vRet  Vector.
   */
  template <class To>
   inline void              copyRow      (goSize_t row, goVector<To>& vRet) const
    {
        if (vRet.getSize() != this->getColumns())
        {
            vRet.resize (this->getColumns());
        }
        goSize_t sz = vRet.getSize();
        goSize_t i;
        for (i = 0; i < sz; ++i)
        {
            vRet[i] = (*this)(row, i);
        }
    };

  /** 
   * @brief Copied a column to vector vRet.
   *
   * vRet is resized if it vRet.getSize() != this->getRows().
   * 
   * @param col   Column to copy.
   * @param vRet  Vector.
   */
  template <class To>
    inline void              copyColumn      (goSize_t col, goVector<To>& vRet) const
    {
        if (vRet.getSize() != this->getRows())
        {
            vRet.resize (this->getRows());
        }
        goSize_t sz = vRet.getSize();
        goSize_t i;
        for (i = 0; i < sz; ++i)
        {
            vRet[i] = (*this)(i, col);
        }
    };

  inline T&                operator() (goIndex_t i, goIndex_t j)
  {
    assert (i >= 0 && i < static_cast<goIndex_t>(this->rows));
    assert (j >= 0 && j < static_cast<goIndex_t>(this->columns));
    return this->matrix[i * this->leadingDimension + j];
  };
  inline const T&          operator() (goIndex_t i, goIndex_t j) const
  {
    assert (i >= 0 && i < static_cast<goIndex_t>(this->rows));
    assert (j >= 0 && j < static_cast<goIndex_t>(this->columns));
    return this->matrix[i * this->leadingDimension + j];
  };

  void operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2, goMatrix<T>& target) const;
  void operator () (const goMatrix<T>& source, goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2);

  //= Handle these with care. They use the plain pointer, no leading dimension.
  inline T&       operator[] (goSize_t index) { return this->matrix[index]; };
  inline const T& operator[] (goSize_t index) const { return this->matrix[index]; };


  /** 
   * @brief Matrix multiplication.
   * @note Uses CBLAS for goFloat and goDouble types.
   * @see goMatrixMult()
   *
   * @param other A matrix.
   * 
   * @return this * other
   */
  goMatrix<T>		operator*  (const goMatrix<T>& other) const;

  goMatrix<T>		operator-  (const goMatrix<T>& other) const;

  goMatrix<T>		operator+  (const goMatrix<T>& other) const;

  goMatrix<T>&		operator*= (const goMatrix<T>& other);
  goMatrix<T>&		operator+= (const goMatrix<T>& other);
  goMatrix<T>&		operator-= (const goMatrix<T>& other);

  goVector<T>       operator*  (const goVector<T>& v) const;
//  goVector<T>       operator*  (const goVector<goDouble>& v) const;

  /** 
  * @brief Multiplication by a scalar.
  * 
  * @param scalar A scalar.
  * 
  * @return this .* scalar.
  */
  inline goMatrix<T>&		operator*= (T scalar)
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

  inline goMatrix<T> operator* (T scalar)
  {
      goMatrix<T> temp = *this;
      temp *= scalar;
      return temp;
  };

  /** 
  * @brief Division by a scalar.
  * 
  * @param scalar 
  * 
  * @return this ./ scalar.
  */
  inline goMatrix<T>&		operator/= (T scalar)
  {
    if (scalar != T(0))
    {
        *this *= T(1) / scalar;
    }
    return *this;
  };

  bool multiplyElements (const goMatrix<T>& other);

  T norm () const;

  /** 
  * @brief Load identity matrix.
  */
  inline void setUnity()
  {
    this->setIdentity();
  };

  void setIdentity();

  void fill(T v);

  inline void print() const
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

  bool writeASCII (FILE* f) const;
  bool writeASCII (const char* fname) const;
  bool readASCII  (FILE* f);
  bool readASCII  (const char* f);

 protected:
  bool               externalData;
  T*                 matrix;
  goSize_t           rows;
  goSize_t           columns;
  goSize_t           leadingDimension;
  bool               rowMajor;
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

template<class T>
void goMatrixPower (goMatrix<T>& A, T scalar);

/**
 * @brief calculate \f$ y = \alpha A x + \beta y \f$.
 *
 * Sizes of A and x are checked. If y has mismatching size, it is resized and
 * initialised with 0 before the operation.
 *
 * Parameters are named as in the formula above.
 *
 * Uses cblas_<>gemv().
 *
 * @todo TEST THIS FUNCTION.
 *
 * @param alpha Scalar factor
 * @param A     Matrix A
 * @param transA If true, A is used as transposed.
 * @param x      Vector x
 * @param beta   Scalar factor (for y)
 * @param y      Vector y, also holds the result.
 *
 * @return true if successful, false otherwise.
 */
template<class T>
bool goMatrixVectorMult (T alpha, const goMatrix<T>& A, bool transA,
                         const goVector<T>& x, T beta, goVector<T>& y);

typedef goMatrix<goDouble> goMatrixd;
typedef goMatrix<goFloat>  goMatrixf;
/** @} */

#endif /* __GOMATRIX_H */
