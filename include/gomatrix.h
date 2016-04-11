/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOMATRIX_H
#define GOMATRIX_H

#include <gotypes.h>
//#include <gosignal3dbase.h>
//#include <gosignal3d.h>
//#include <gosubsignal3d.h>
//#include <gosignalmacros.h>
#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

// Needed for LAPACK_ROW_MAJOR.
#include <golib_clapack.h>

// template <class T> class goMath::Vector;

namespace goMath {
    /** \addtogroup mathla
     * @{ */

    template <class T> class VectorIterator;
    template <class T> class ConstVectorIterator;

    /*! @todo FIXME: These are assumed to be row major e.g. by the row/column iterators returned by goMath::Matrix.
        Make these selectable object-wise.
     */
    static int const defaultMatrixOrder = LAPACK_ROW_MAJOR;
    static bool const rowMajor          = defaultMatrixOrder == LAPACK_ROW_MAJOR;

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
     * \author Christian Gosch
     */
    template <class T>
        class Matrix 
        {
            public:
                static const bool rowMajor;
                typedef T value_type;
                typedef VectorIterator<T>      vector_iterator;
                typedef ConstVectorIterator<T> const_vector_iterator;

                /*!
                 * @param y Number of rows.
                 * @param x Number of columns.
                 */
                Matrix (goSize_t rows = 4, goSize_t cols = 4);

                /** 
                 * @brief Copy constructor.
                 * 
                 * @param other Other matrix. Will be deep copied.
                 */
                Matrix (const Matrix<T>& other);

                /** 
                 * @brief Constructor for using external data.
                 * 
                 * @param data Pointer to the external data.
                 * @param r Rows
                 * @param c Columns
                 * @param leadingDim leading dimension -- if 0, is set to c.
                 */
                Matrix (T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0);

                virtual ~Matrix ();

                vector_iterator       rowBegin ();
                vector_iterator       rowEnd ();
                const_vector_iterator rowBegin () const;
                const_vector_iterator rowEnd () const;
                vector_iterator       colBegin ();
                vector_iterator       colEnd ();
                const_vector_iterator colBegin () const;
                const_vector_iterator colEnd () const;

                bool setData (T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0);
                bool setData (const T* data, goSize_t r, goSize_t c, goSize_t leadingDim = 0) const;

                bool resize (goSize_t rows, goSize_t cols);
		bool reshape (goSize_t rows, goSize_t cols);

                template <class To>
                    bool resize (const Matrix<To>& o)
                    {
                        return this->resize (o.getRows(),o.getColumns());
                    }

                bool transpose ();
                bool getTranspose (Matrix<T>& trans) const;
                Matrix<T> getTranspose () const;

                bool invert ();

                void power (T scalar);

                void flip (goSize_t dim = 0);
                void shiftRows (goIndex_t offset, Matrix<T>& ret) const;
                void shiftColumns (goIndex_t offset, Matrix<T>& ret) const;

                //= FIXME: column major is almost not implemented yet. It may never be.
                // bool getRowMajor () const { return this->rowMajor; };

                Matrix<T>& operator= (const Matrix<T>& other);

                bool operator== (const Matrix<T>& other) const;
                bool operator!= (const Matrix<T>& other) const;

                /** 
                 * @brief Get data pointer.
                 * @return Pointer to the matrix array.
                 */
                inline T*       getData   () { return this->matrix; }
                /** 
                 * @brief Get data pointer.
                 * @return Const pointer to the matrix array.
                 */
                inline const T* getData   () const { return this->matrix; }

                /** 
                 * @brief Get data pointer. Same as getData().
                 */
                inline T*       getPtr    () { return this->matrix; }
                /** 
                 * @brief Get data pointer. Same as getData().
                 */
                inline const T* getPtr    () const { return this->matrix; }

                /** 
                 * @brief Get number of columns.
                 * 
                 * @return Number of columns.
                 */
                inline goSize_t   getColumns () const 
                { 
                    return this->columns;
                }
                /** 
                 * @brief Get number of rows.
                 * 
                 * @return Number of rows.
                 */
                inline goSize_t   getRows () const 
                { 
                    return this->rows;
                }

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
                }

                T sum () const;
                void sum (int dimension, Matrix<T>& ret) const;
                void sum (int dimension, goMath::Vector<T>& ret) const;

                // TNT compatibility methods BEGIN
                inline int        dim1 () const { return this->getRows(); }
                inline int        dim2 () const { return this->getColumns(); }
                inline const Matrix<T>& copy () const { return *this;}  // NOTE: Makes a deep copy here
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
                        goSize_t num_rows, goSize_t num_cols, Matrix<T>& refMatrix)
                {
                    refMatrix.setData (&(*this)(startRow,startColumn), num_rows, num_cols, this->getLeadingDimension());
                }

                bool copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, Matrix<T>& target, bool trans = false) const;
                bool copy (goSize_t startRow, goSize_t startCol, goSize_t endRow, goSize_t endCol, goSize_t target_row, goSize_t target_col, Matrix<T>& target, bool trans = false) const;
                bool copy (Matrix<T>& target) const;

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
                        goSize_t num_rows, goSize_t num_cols, const Matrix<T>& refMatrix) const
                {
                    const_cast<Matrix<T>&>(refMatrix).setData (const_cast<T*>(&(*this)(startRow,startColumn)), num_rows, num_cols, this->getLeadingDimension());
                }

                /** 
                 * @brief Makes a vector reference a row from this matrix.
                 * 
                 * @note The data are not copied.
                 *
                 * @param row Row to reference
                 * @param v   Vector that holds the reference after the method returns.
                 */
                inline void              refRow    (goSize_t row, goMath::Vector<T>& v)
                {
                    v.setData (&(*this)(row,0), this->getColumns(), 1);
                }
                /** 
                 * @brief Reference to a sub row.
                 * 
                 * @param row    Start element row
                 * @param column Start element column
                 * @param length length of vector
                 * @param v      Vector that contains the reference after the method call.
                 */
                inline void              refRow    (goSize_t row, goSize_t column, goSize_t length, goMath::Vector<T>& v)
                {
                    v.setData (&(*this)(row,column), length, 1);
                }

                inline void setRow (goSize_t row, const goMath::Vector<T>& v)
                {
                    goSize_t sz = this->getColumns();
                    assert (v.getSize() >= sz);
                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        (*this)(row, i) = v[i];
                    }
                }

                inline void setColumn (goSize_t col, const goMath::Vector<T>& v)
                {
                    goSize_t sz = this->getRows();
                    assert (v.getSize() >= sz);
                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        (*this)(i, col) = v[i];
                    }
                }

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
                inline void              refRow    (goSize_t row, const goMath::Vector<T>& v) const
                {
                    const_cast<goMath::Vector<T>&>(v).setData (const_cast<T*>(&(*this)(row,0)), this->getColumns(), 1);
                }
                /** 
                 * @brief Reference to a sub row.
                 * 
                 * @param row    Start element row
                 * @param column Start element column
                 * @param length length of vector
                 * @param v      Vector that contains the reference after the method call.
                 */
                inline void              refRow    (goSize_t row, goSize_t column, goSize_t length, const goMath::Vector<T>& v) const
                {
                    const_cast<goMath::Vector<T>&>(v).setData (const_cast<T*>(&(*this)(row,column)), length, 1);
                }

                /** 
                 * @brief Makes a vector reference a column from this matrix.
                 * 
                 * @note The data are not copied.
                 *
                 * @param column Column to reference
                 * @param v   Vector that holds the reference after the method returns.
                 */
                inline void              refColumn (goSize_t column, goMath::Vector<T>& v)
                {
                    v.setData (&(*this)(0,column), this->getRows(), this->getLeadingDimension());
                }
                /** 
                 * @brief Reference to a sub column.
                 * 
                 * @param row    Start element row
                 * @param column Start element column
                 * @param length length of vector
                 * @param v      Vector that contains the reference after the method call.
                 */
                inline void              refColumn (goSize_t row, goSize_t column, goSize_t length, goMath::Vector<T>& v)
                {
                    v.setData (&(*this)(row,column), length, this->getLeadingDimension());
                }

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
                inline void              refColumn    (goSize_t column, const goMath::Vector<T>& v) const
                {
                    const_cast<goMath::Vector<T>&>(v).setData (const_cast<T*>(&(*this)(0,column)), this->getRows(), this->getLeadingDimension());
                }
                /** 
                 * @brief Reference to a sub column.
                 * 
                 * @param row    Start element row
                 * @param column Start element column
                 * @param length length of vector
                 * @param v      Vector that contains the reference after the method call.
                 */
                inline void              refColumn    (goSize_t row, goSize_t column, goSize_t length, const goMath::Vector<T>& v) const
                {
                    const_cast<goMath::Vector<T>&>(v).setData (const_cast<T*>(&(*this)(row,column)), length, this->getLeadingDimension());
                }

                /** 
                 * @brief Copies a row to vector vRet.
                 *
                 * vRet is resized if it vRet.getSize() != this->getColumns().
                 * 
                 * @param row   Row to copy.
                 * @param vRet  Vector.
                 */
                template <class To>
                    inline void              copyRow      (goSize_t row, goMath::Vector<To>& vRet) const
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
                    }

                /** 
                 * @brief Copied a column to vector vRet.
                 *
                 * vRet is resized if it vRet.getSize() != this->getRows().
                 * 
                 * @param col   Column to copy.
                 * @param vRet  Vector.
                 */
                template <class To>
                    inline void              copyColumn      (goSize_t col, goMath::Vector<To>& vRet) const
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
                    }

                void swapRows (goIndex_t i1, goIndex_t i2)
                {
                    goMath::Vector<T> ref1, ref2;
                    this->refRow (i1, ref1);
                    this->refRow (i2, ref2);
                    goSize_t sz = ref1.getSize();
                    T temp = T(0);
                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        temp = ref1[i];
                        ref1[i] = ref2[i];
                        ref2[i] = temp;
                    }
                }

                void swapColumns (goIndex_t i1, goIndex_t i2)
                {
                    goMath::Vector<T> ref1, ref2;
                    this->refColumn (i1, ref1);
                    this->refColumn (i2, ref2);
                    goSize_t sz = ref1.getSize();
                    T temp = T(0);
                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        temp = ref1[i];
                        ref1[i] = ref2[i];
                        ref2[i] = temp;
                    }
                }

                inline T&                operator() (goIndex_t i, goIndex_t j)
                {
                    assert (i >= 0 && i < static_cast<goIndex_t>(this->rows));
                    assert (j >= 0 && j < static_cast<goIndex_t>(this->columns));
                    return this->matrix[i * this->leadingDimension + j];
                }
                inline const T&          operator() (goIndex_t i, goIndex_t j) const
                {
                    assert (i >= 0 && i < static_cast<goIndex_t>(this->rows));
                    assert (j >= 0 && j < static_cast<goIndex_t>(this->columns));
                    return this->matrix[i * this->leadingDimension + j];
                }

                void operator () (goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2, Matrix<T>& target) const;
                void operator () (const Matrix<T>& source, goIndex_t i1, goIndex_t j1, goIndex_t i2, goIndex_t j2);

                //= Handle these with care. They use the plain pointer, no leading dimension.
                inline T&       operator[] (goSize_t index) { return this->matrix[index]; }
                inline const T& operator[] (goSize_t index) const { return this->matrix[index]; }


                /** 
                 * @brief Matrix multiplication.
                 * @note Uses CBLAS for goFloat and goDouble types.
                 * @see MatrixMult()
                 *
                 * @param other A matrix.
                 * 
                 * @return this * other
                 */
                Matrix<T>		operator*  (const Matrix<T>& other) const;

                Matrix<T>		operator-  (const Matrix<T>& other) const;

                Matrix<T>		operator+  (const Matrix<T>& other) const;

                Matrix<T>&		operator*= (const Matrix<T>& other);
                Matrix<T>&		operator+= (const Matrix<T>& other);
                Matrix<T>&		operator+= (T scalar);
                Matrix<T>&		operator-= (const Matrix<T>& other);
                Matrix<T>&		operator-= (T scalar);

                goMath::Vector<T>       operator*  (const goMath::Vector<T>& v) const;
                //  goMath::Vector<T>       operator*  (const goMath::Vector<goDouble>& v) const;

                /** 
                 * @brief Multiplication by a scalar.
                 * 
                 * @param scalar A scalar.
                 * 
                 * @return this .* scalar.
                 */
                inline Matrix<T>&		operator*= (T scalar)
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
                }

                inline Matrix<T> operator* (T scalar)
                {
                    Matrix<T> temp = *this;
                    temp *= scalar;
                    return temp;
                }

                /** 
                 * @brief Division by a scalar.
                 * 
                 * @param scalar 
                 * 
                 * @return this ./ scalar.
                 */
                inline Matrix<T>&		operator/= (T scalar)
                {
                    if (scalar != T(0))
                    {
                        *this *= T(1) / scalar;
                    }
                    return *this;
                }

                bool multiplyElements (const Matrix<T>& other);

                T norm () const;
                T trace () const;

                /** 
                 * @brief Load identity matrix.
                 */
                inline void setUnity()
                {
                    this->setIdentity();
                }

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
                }

                bool writeASCII (FILE* f) const;
                bool writeASCII (const char* fname) const;
                bool readASCII  (FILE* f);
                bool readASCIISimple  (FILE* f);
                bool readASCII  (const char* f);

            protected:
                bool               externalData;
                T*                 matrix;
                goSize_t           rows;
                goSize_t           columns;
                goSize_t           leadingDimension;
        };

  
    /** 
     * @brief Vector iterator for multi-dimensional array data.
     *
     * This class is used as goMath::Matrix::vector_iterator.
     * It can be used to iterate over a number of vectors stored e.g.
     * in a matrix.
     *
     * @param T Data type of the data.
     */
    template <class T>
        class VectorIterator
        {
            public:
                /** 
                 * @brief Construct an iterator over data starting at \c ptr,
                 * of length \c len, using \c stride to get from one element to the next,
                 * and \c increment to get from one vector to the next.
                 * 
                 * @param ptr Start pointer.
                 * @param len Length of a vector.
                 * @param stride Stride within the vector (number of elements to add to the pointer 
                 * in order to get to the next element).
                 * @param increment Increment to get from one vector to the next, in elements.
                 */
                VectorIterator (T* ptr, goSize_t len, goPtrdiff_t stride, goPtrdiff_t increment)
                    : myRef (ptr, len, stride),
                      myLen (len),
                      myIncrement (increment)
                {
                }

                /** 
                 * @brief Copy constructor.
                 *
                 * @param other Iterator to be copied.
                 */
                VectorIterator (const VectorIterator<T>& other)
                    : myRef (0),
                      myLen (0),
                      myIncrement (0)
                {
                    *this = other;
                }

                /** 
                 * @brief Copy operator.
                 * 
                 * @param other Iterator to be copied.
                 * 
                 * @return this.
                 */
                VectorIterator<T>& operator= (const VectorIterator<T>& other)
                {
                    printf ("VectIt operator=\n");
                    (*const_cast<VectorIterator<T>*>(&other))->ref (myRef);
                    myLen = other.len ();
                    myIncrement = other.increment ();
                    return *this;
                }

                /** 
                 * @brief Length of a vector.
                 * 
                 * @return Length of a vector.
                 */
                goSize_t len () const { return myLen; }
                /** 
                 * @brief Increment to add to a pointer to get to the next vector.
                 * 
                 * @return Increment to add to a pointer to get to the next vector, in elements.
                 */
                goPtrdiff_t increment () const { return myIncrement; }

                /** 
                 * @brief Increment the iterator (move to the next vector).
                 * 
                 * @return this.
                 */
                VectorIterator<T>& operator++ ()
                {
                    myRef.setPtr (myRef.getPtr() + myIncrement);
                    return *this;
                }

                /** 
                 * @brief Decrement the iterator (move to the previous vector).
                 * 
                 * @return this.
                 */
                VectorIterator<T>& operator-- ()
                {
                    myRef.setPtr (myRef.getPtr() - myIncrement);
                    return *this;
                }

                /** 
                 * @brief Dereference operator.
                 * 
                 * @return The current vector.
                 */
                Vector<T>& operator* () { return myRef; }
                /** 
                 * @brief Pointer operator.
                 * 
                 * @return Pointer to the current vector.
                 */
                Vector<T>* operator-> () { return &myRef; }
                const Vector<T>& operator* () const { return myRef; }
                const Vector<T>* operator-> () const { return &myRef; }

                /** 
                 * @brief Compares the actual pointers.
                 * 
                 * @param other Iterator to compare with.
                 * 
                 * @return True if the pointers are equal, false otherwise.
                 */
                bool operator== (const VectorIterator<T>& other)
                {
                    return other->getPtr() == myRef.getPtr();
                }

                /** 
                 * @brief Compares the actual pointers.
                 * 
                 * @param other Iterator to compare with.
                 * 
                 * @return True if the pointers are unequal, false otherwise.
                 */
                bool operator!= (const VectorIterator<T>& other)
                {
                    return other->getPtr() != myRef.getPtr();
                }

            private:
                Vector<T>   myRef;
                goSize_t    myLen;
                goPtrdiff_t myIncrement;
        };

    /** 
     * @brief Cons iterator over vectors.
     * @see VectorIterator
     */
    template <class T>
        class ConstVectorIterator
        {
            public:
                ConstVectorIterator (const T* ptr, goSize_t len, goPtrdiff_t stride, goPtrdiff_t increment)
                    : myRef (const_cast<T*> (ptr), len, stride),
                      myLen (len),
                      myIncrement (increment)
                {
                }

                ConstVectorIterator (const ConstVectorIterator<T>& other)
                    : myRef (0),
                      myLen (0),
                      myIncrement (0)
                {
                    *this = other;
                }

                ConstVectorIterator<T>& operator= (const ConstVectorIterator<T>& other)
                {
                    //= This language can be so ugly ...
                    const_cast<Vector<T>*> (const_cast<ConstVectorIterator<T>*>(&other)->operator->())->ref (myRef);
                    myLen = other.len ();
                    myIncrement = other.increment ();
                    return *this;
                }

                goSize_t len () const { return myLen; }
                goPtrdiff_t increment () const { return myIncrement; }

                ConstVectorIterator<T>& operator++ ()
                {
                    myRef.setPtr (myRef.getPtr() + myIncrement);
                    return *this;
                }

                ConstVectorIterator<T>& operator-- ()
                {
                    myRef.setPtr (myRef.getPtr() - myIncrement);
                    return *this;
                }

                const Vector<T>& operator* () { return myRef; }
                const Vector<T>* operator-> () { return &myRef; }
                const Vector<T>& operator* () const { return myRef; }
                const Vector<T>* operator-> () const { return &myRef; }

                bool operator== (const ConstVectorIterator<T>& other)
                {
                    return other->getPtr() == myRef.getPtr();
                }

                bool operator!= (const ConstVectorIterator<T>& other)
                {
                    return other->getPtr() != myRef.getPtr();
                }

            private:
                Vector<T>   myRef;
                goSize_t    myLen;
                goPtrdiff_t myIncrement;
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
        void matrixMult (T alpha, const goMath::Matrix<T>& A, bool transA, 
                const goMath::Matrix<T>& B, bool transB, 
                T beta, goMath::Matrix<T>& C);

    template<class T>
        void matrixPower (goMath::Matrix<T>& A, T scalar);

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
        bool matrixVectorMult (T alpha, const goMath::Matrix<T>& A, bool transA,
                const goMath::Vector<T>& x, T beta, goMath::Vector<T>& y);

    typedef goMath::Matrix<goDouble> Matrixd;
    typedef goMath::Matrix<goFloat>  Matrixf;
    /** @} */
};

#ifndef goMatrix
# define goMatrix goMath::Matrix
#endif
typedef goMatrix<goFloat> goMatrixf;
typedef goMatrix<goDouble> goMatrixd;
#endif /* __GOMATRIX_H */
