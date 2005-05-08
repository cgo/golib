#ifndef GOSPARSEMATRIX_H
#define GOSPARSEMATRIX_H

#define HAVE_MATLAB

#ifdef HAVE_MATLAB
# include "mex.h"
#endif
#include <assert.h>

class goSparseFEMPrivate;

/**
 * @brief Basic sparse matrix class with Matlab mxArray support.
 *
 * Provides facilities to fill and read a sparse matrix without the 
 * pain in the @$#! caused by Matlab's C interface.
 *
 * However, there are a few things to remember:
 * - NEVER fill in two values at the same coordinate. 
 *   The data you fill in 
 *   <b>may not contain doubles</b>.
 * - If you don't like this/need something more forgiving,
 *   please extend this class. Please do not
 *   add double-checks in fillNext() since I fear this will slow it down
 *   too much.
 *
 * It is not too well tested but worked so far for me.
 * There are some more or less limited multiplication functions for 
 * goArray and other goSparseMatrix objects. Extend as needed.
 * 
 * To fill a mxArray with 3 values, you could do:
 *
 * \code
 *      #include "gosparsematrix.h"
 *      ...
 *      mxArray* myArray;            // myArray must exist and be allocated in the right size.
 *      ...
 *      goSparseMatrix m (100,100);  // Say myArray is 100x100.
 *      m.fillBegin (3);             // Fill in 3 values
 *      m.fillNext  (1,2,6.0);
 *      m.fillNext  (5,2,4.0);
 *      m.fillNext  (4,5,4.0);
 *      m.fillEnd   ();              // Done.
 *      m.getMatlabSparse (myArray);
 *      // myArray should now contain the above values.
 *      ...
 * \endcode
 * 
 * @todo Make a "get all nonzero elements" method.
 *
 * @author Christian Gosch
 **/
class goSparseMatrix
{
    public:
        // inline goSparseMatrix  ();
        // inline goSparseMatrix  (mxArray* array);
        inline goSparseMatrix  (int rows = 0, int cols = 0);
        inline ~goSparseMatrix ();

        inline void setSize (int rows, int cols);
//        inline double operator () (int row, int column);
        inline void set (int row, int col, double value);
        
#ifdef HAVE_MATLAB
        inline bool getMatlabSparse (mxArray* target);
#endif
        
//        inline int  getColElementCount (int column);
        inline int  getElementCount    () const; 

        inline bool fillBegin (int elementCount);
        inline int  fillNext  (int row, int col, double value);
        inline void fillEnd   ();

        inline bool matrixVectorMult (goArray<goDouble>& ret, const goArray<goDouble>& v);
        inline bool vectorMatrixMult (goArray<goDouble>& ret, const goArray<goDouble>& v);
        inline bool matrixVectorMult (goSparseMatrix&    ret, const goArray<goDouble>& v);
        inline bool vectorMatrixMult (goSparseMatrix&    ret, const goArray<goDouble>& v);

        inline goSparseMatrix& operator*= (goDouble scalar);
        
        /// Returns the row of the element at elementIndex
        inline goIndex_t row    (goIndex_t elementIndex) const
            {
                return mRowIndex[elementIndex];
            };
        /// Returns the row of the element at elementIndex
        inline goIndex_t column (goIndex_t elementIndex) const
            {
                return mColIndex[elementIndex];
            };
        /// Returns the value of the element at elementIndex
        inline goDouble value (goIndex_t elementIndex) const
            {
                return mValues[elementIndex];
            };
        /// Returns the number of rows
        inline int getRowCount () const
            {
                return mRows;
            };
        /// Returns the number of columns
        inline int getColumnCount () const
            {
                return mCols;
            };
        
        inline void sortRows    ();
        inline void sortColumns ();
        inline bool appendRow   (const goSparseMatrix& r);
            
#ifdef HAVE_MATLAB        
    private:
        inline bool matlabFillBegin (int elementCount);
        inline int  matlabFillNext  (int row, int col, double value);
        inline void matlabFillEnd   ();
#endif

    private:
        goArray<goIndex_t> mColIndex;
        goArray<goIndex_t> mRowIndex;
        goArray<goDouble>  mValues;

        int      mMaxElementCount;
        double   mDefaultValue;
        int      mRows;
        int      mCols;
        int      mElementCount;               // Number of set elements by the fill*() methods.
        
        // For the fill routines
        int      mFillElementCount;
        int      mFillCurrentIndex;
#ifdef HAVE_MATLAB
        int*     mMatlabColFirstIndex;        // mCols + 1            , Jc from mxArray
        int*     mMatlabRowIndex;             // max. mMaxElementCount, Ir from mxArray
        double*  mMatlabData;                 // max. mMaxElementCount, Pr from mxArray
        // For the matlab fill routines
        int      mMatlabFillElementCount;
        int      mMatlabFillCurrentColumn;
        int      mMatlabFillCurrentIndex;
#endif
};

#if 0
goSparseMatrix::goSparseMatrix *mxArray* array)
    :  mArray               (array),
      mColFirstIndex       (),
      mRowIndex            (),
      mData                (),
      mMaxElementCount     (0),
      mDefaultValue        (0.0),
      mRows                (0),
      mCols                (0),
      mFillElementCount    (0),
      mFillCurrentColumn   (0),
      mFillCurrentIndex    (0)
{
    if (array)
    {
        mColFirstIndex   = mxGetJc (array);
        mRowIndex        = mxGetIr (array);
        mData            = mxGetPr (array);
        mMaxElementCount = mxGetNzmax (array);
        mRows            = mxGetM  (array);
        mCols            = mxGetN  (array);
    }
}
#endif

goSparseMatrix::goSparseMatrix (int rows, int cols)
    : 
      mColIndex            (),
      mRowIndex            (),
      mValues              (),
      mDefaultValue        (0.0),
      mRows                (0),
      mCols                (0),
      mElementCount        (0),
      mFillElementCount    (0),
      mFillCurrentIndex    (0)
#ifdef HAVE_MATLAB
      ,mMatlabColFirstIndex       (0),
       mMatlabRowIndex            (0),
       mMatlabData                (0),
       mMatlabFillElementCount    (0),
       mMatlabFillCurrentColumn   (0),
       mMatlabFillCurrentIndex    (0)
#endif
{
    this->setSize (rows, cols);
}

goSparseMatrix::~goSparseMatrix ()
{
}

void goSparseMatrix::setSize (int rows, int cols)
{
    mRows = rows;
    mCols = cols;
}

#if 0
/**
 * @brief Get a value from the matrix.
 *
 * @param row  Row
 * @param col  Column
 *
 * @return Value at (row,col)
 * @note Untested.
 **/
double goSparseMatrix::operator () (int row, int col)
{
    int nnz = getColElementCount(col);
    if (nnz <= 0)
    {
        return mDefaultValue;
    }
    int firstIndex = mColFirstIndex[col];
    int lastIndex  = mColFirstIndex[col+1]-1;
    do
    {
        if (mRowIndex[firstIndex] == row)
        {
            return mData[firstIndex];
        }
        ++firstIndex;
    }
    while (firstIndex <= lastIndex);
    return mDefaultValue;
}
#endif

/**
 * @brief Not implemented.
 *
 * A general set function would require some reallocating and copying of
 * arrays. I had no use for that so far, but please implement it <b>AND GIVE IT BACK TO ME</b>
 * if you need it.
 *
 * @param row  
 * @param col  
 * @param v  
 **/
void goSparseMatrix::set (int row, int col, double v)
{
    printf ("WARNING: set() method not implemented. Sparse wrapper is read only.\n");
}

/**
* @brief Sorts in ascending column order.
* 
* Sorts all elements so that they appear in ascending column order in the internal arrays.
**/
void goSparseMatrix::sortColumns ()
{
    // Sort in ascending column order
    goArray<int> indexArray (mRowIndex.getSize());
    int i;
    for (i = 0; i < indexArray.getSize(); ++i)
    {
        indexArray[i] = i;
    }
    mColIndex.sort (indexArray);
    goArray<goIndex_t> sortedRows (mRowIndex.getSize());
    goArray<goDouble>  sortedValues (mValues.getSize());

    goIndex_t sz = sortedRows.getSize();
    for (i = 0; i < sz; ++i)
    {
        sortedRows[i]   = mRowIndex[indexArray[i]];
        sortedValues[i] = mValues[indexArray[i]];
    }
    mRowIndex = sortedRows;
    mValues   = sortedValues;
}

/**
* @brief Sorts in ascending row order.
* 
* Sorts all elements so that they appear in ascending row order in the internal arrays.
**/
void goSparseMatrix::sortRows ()
{
    // Sort in ascending row order
    goArray<int> indexArray (mRowIndex.getSize());
    int i;
    for (i = 0; i < indexArray.getSize(); ++i)
    {
        indexArray[i] = i;
    }
    mRowIndex.sort (indexArray);
    goArray<goIndex_t> sortedCols (mColIndex.getSize());
    goArray<goDouble>  sortedValues (mValues.getSize());

    goIndex_t sz = sortedCols.getSize();
    for (i = 0; i < sz; ++i)
    {
        sortedCols[i]   = mColIndex[indexArray[i]];
        sortedValues[i] = mValues[indexArray[i]];
    }
    mColIndex = sortedCols;
    mValues   = sortedValues;
}

/**
* @brief  Fill target with values from this.
*
* If HAVE_MATLAB is defined at compile time,
* this method can be used to fill a Matlab sparse
* mxArray with the values from this object.
* target must be of the same size as this 
* and the maximum number of nonzero elements
* in target must be sufficient (mxGetNzmax() Matlab function).
* target must be sparse (created with mxCreateSparse()).
* 
* @param target  The mxArray to fill.
*
* @return True if successful, false otherwise.
**/
#ifdef HAVE_MATLAB
bool goSparseMatrix::getMatlabSparse (mxArray* target)
{
    // Be conservative about the size of the target matrix. 
    // It must match exactly and be sparse.
    if (!mxIsSparse(target))
    {
        return false;
    }
   
    if (mxGetM(target) != mRows)
    {
        return false;
    }
    if (mxGetN(target) != mCols)
    {
        return false;
    }
    if (mxGetNzmax(target) < mElementCount)
    {
        return false;
    }
    
    mMatlabColFirstIndex = mxGetJc(target);        // mCols + 1            , Jc from mxArray
    mMatlabRowIndex      = mxGetIr(target);        // max. mMaxElementCount, Ir from mxArray
    mMatlabData          = mxGetPr(target);        // max. mMaxElementCount, Pr from mxArray

    // Sort in ascending column order
    goArray<int> indexArray (mRowIndex.getSize());
    int i;
    for (i = 0; i < indexArray.getSize(); ++i)
    {
        indexArray[i] = i;
    }
    goArray<goIndex_t> sortedCols (mColIndex);
    sortedCols.sort (indexArray);
    goArray<goIndex_t> sortedRows (mRowIndex.getSize());
    goArray<goDouble>  sortedValues (mValues.getSize());

    goIndex_t sz = sortedRows.getSize();
    for (i = 0; i < sz; ++i)
    {
        sortedRows[i]   = mRowIndex[indexArray[i]];
        sortedValues[i] = mValues[indexArray[i]];
    }
    
    sz = this->getElementCount();
    if (!this->matlabFillBegin (sz))
    {
        printf("goSparseMatrix::getMatlabSparse(): cannot initialise matrix fill.\n");
        mMatlabColFirstIndex = 0;
        mMatlabRowIndex      = 0;
        mMatlabData          = 0;
        return false;
    }
    for (i = 0; i < sz; ++i)
    {
        assert (sortedRows[i] >= 0);
        assert (sortedCols[i] >= 0);
        assert (sortedRows[i] < mRows);
        assert (sortedCols[i] < mCols);
        this->matlabFillNext (sortedRows[i], sortedCols[i], sortedValues[i]);
    }
    this->matlabFillEnd();
    mMatlabColFirstIndex = 0;
    mMatlabRowIndex      = 0;
    mMatlabData          = 0;
    return true;

}
#endif

#if 0
/**
 * @brief Get the number of nonzero elements in a specific column.
 *
 * @param column  The column.
 *
 * @return Number of nonzero elements in column <code>column</code>.
 **/
int goSparseMatrix::getColElementCount (int column)
{
    return mColFirstIndex[column+1] - mColFirstIndex[column];
}
#endif

/**
 * @brief Get the total number of nonzero elements.
 *
 * nnz() in Matlab.
 *
 * @return Number of nonzero elements.
 **/
int goSparseMatrix::getElementCount () const
{
    return mElementCount;
    // return mColFirstIndex[mCols];
}

/**
* @brief Begin filling elementCount elements in this matrix.
*
* The actual amount of elements filled in can be lower than elementCount.
* Do not forget to call fillEnd() when done filling in values.
* 
* @param elementCount  Maximum number of elements to be filled in.
*
* @return  True if successful, false otherwise.
**/
bool goSparseMatrix::fillBegin (int elementCount)
{
    mFillElementCount = elementCount;
    mFillCurrentIndex = 0;
    mRowIndex.resize (elementCount);
    mColIndex.resize (elementCount);
    mValues.resize   (elementCount);
    return true;
}

/**
* @brief Fills the next element in this matrix.
*
* @param row   Row of the element
* @param col   Column of the element
* @param value Value of the element
*
* @return  Number of elements that can still be filled in after this call.
*          If zero, all subsequent calls to fillNext() will not change the matrix.
**/
int goSparseMatrix::fillNext (int row, int col, double value)
{
    if (mFillElementCount <= 0)
    {
        return 0;
    }
    if (row < 0 || col < 0)
    {
        return mFillElementCount;
    }
    assert (mColIndex.getSize() == mRowIndex.getSize());
    assert (mColIndex.getSize() == mValues.getSize());
    assert (mFillCurrentIndex < mColIndex.getSize());
    assert (mFillCurrentIndex >= 0);
    
    mRowIndex[mFillCurrentIndex] = row;
    mColIndex[mFillCurrentIndex] = col;
    mValues  [mFillCurrentIndex] = value;
    ++mFillCurrentIndex;
    --mFillElementCount;
    return mFillElementCount;
}

/**
* @brief Ends filling the matrix.
*
* Ends filling and resizes the internal arrays according to the number of elements
* filled in.
**/
void goSparseMatrix::fillEnd ()
{
    mElementCount     = mFillCurrentIndex;
    mFillElementCount = 0;
    mFillCurrentIndex = 0;
    // If we stopped before filling the whole arrays, resize them.
    // Resizing to a smaller value should not touch the rest of the array.
    if (mElementCount < mRowIndex.getSize())
    {
        mRowIndex.resize (mElementCount);
        mColIndex.resize (mElementCount);
        mValues.resize   (mElementCount);
    }
}

/**
* @brief  (this) * v
*
* @param ret  
* @param v  
*
* @return 
**/
bool goSparseMatrix::matrixVectorMult (goArray<goDouble>& ret, const goArray<goDouble>& v)
{
    if (mCols != v.getSize())
    {
        return false;
    }
    goIndex_t elemCount = this->getElementCount();
    ret.resize (mRows);
    ret.fill (0.0);
    for (goIndex_t i = 0; i < elemCount; ++i)
    {
        ret[this->row(i)] += this->value(i) * v[this->column(i)];
    } 
    return true;
}

/**
* @brief (this) * v
*
* @param ret  
* @param v  
*
* @return 
**/
bool goSparseMatrix::matrixVectorMult (goSparseMatrix& ret, const goArray<goDouble>& v)
{
    if (mCols != v.getSize())
    {
        return false;
    }
    goIndex_t elemCount = this->getElementCount();

    //= Sort in ascending row order
    this->sortRows();
    ret.setSize (mRows,1);
    if (elemCount == 0)
    {
        return true;
    }
    //= Put exactly as many elements in ret as there are rows with nonzero elements in (this)
    ret.fillBegin (this->row(elemCount-1) + 1);
    goIndex_t rowIndex = this->row(0);
    goDouble  value    = 0.0;
    for (goIndex_t i = 0; i < elemCount; ++i)
    {
        if (rowIndex == this->row(i))
        {
            value += this->value(i) * v[this->column(i)];
        }
        else
        {
            if (ret.fillNext (rowIndex,0,value) == 0)
            {
                printf ("**************** SOMETHING TERRIBLE HAPPENED! *******************");
            }
            rowIndex = this->row(i);
            value = this->value(i) * v[this->column(i)];
        }
    }
    //= Fill in the last value
    ret.fillNext (rowIndex,0,value);
    ret.fillEnd ();
    return true;
}

/**
* @brief v' * (this)
*
* @param ret  
* @param v  
*
* @return 
**/
bool goSparseMatrix::vectorMatrixMult (goSparseMatrix& ret, const goArray<goDouble>& v)
{
    if (mRows != v.getSize())
    {
        return false;
    }
    goIndex_t elemCount = this->getElementCount();

    //= Sort in ascending row order
    this->sortColumns();
    ret.setSize (1,mCols);
    if (elemCount == 0)
    {
        return true;
    }
    //= Put exactly as many elements in ret as there are rows with nonzero elements in (this)
    ret.fillBegin (this->column(elemCount-1) + 1);
    goIndex_t colIndex = this->column(0);
    goDouble  value    = 0.0;
    for (goIndex_t i = 0; i < elemCount; ++i)
    {
        if (colIndex == this->column(i))
        {
            value += this->value(i) * v[this->row(i)];
        }
        else
        {
            if (ret.fillNext (0,colIndex,value) == 0)
            {
                printf ("**************** SOMETHING TERRIBLE HAPPENED! *******************");
            }
            colIndex = this->column(i);
            value = this->value(i) * v[this->row(i)];
        }
    }
    //= Fill in the last value
    ret.fillNext (0,colIndex,value);
    ret.fillEnd ();
    return true;
}

goSparseMatrix& goSparseMatrix::operator*= (goDouble scalar)
{
    goIndex_t i;
    goIndex_t sz = mValues.getSize();
    goDouble* ptr = mValues.getPtr();
    for (i = 0; i < sz; ++i)
    {
        *ptr *= scalar;
        ++ptr;
    }
    return *this;
}

/**
* @brief Appends the row matrix r to this matrix.
*
* The result is [(*this)' r']'
*
* @param r  Must be of size (1, this->getColumnCount())
*
* @return True if successful, false otherwise.
**/
bool goSparseMatrix::appendRow (const goSparseMatrix& r)
{
    //= Only accept row-vectors
    if (r.getColumnCount() != mCols || r.getRowCount() != 1)
    {
        return false;
    }
    goIndex_t elemCount = r.getElementCount();
    this->setSize (mRows + 1, mCols);
    goIndex_t i = mElementCount;
    mElementCount = this->getElementCount() + elemCount;
    mColIndex.resize (mElementCount);
    mRowIndex.resize (mElementCount);
    mValues.resize   (mElementCount);
    goIndex_t j;
    goIndex_t rowIndex = mRows - 1;
    for (j = 0; i < mElementCount; ++i,++j)
    {
        mColIndex[i] = r.column(j);
        mRowIndex[i] = rowIndex;
        mValues  [i] = r.value(j);
    }
    return true;
}

/**
* @brief v' * (this)
*
* @param ret  
* @param v  
*
* @return 
**/
bool goSparseMatrix::vectorMatrixMult (goArray<goDouble>& ret, const goArray<goDouble>& v)
{
    if (mRows != v.getSize())
    {
        return false;
    }
    goIndex_t elemCount = this->getElementCount();
    ret.resize (mCols);
    ret.fill (0.0);
    for (goIndex_t i = 0; i < elemCount; ++i)
    {
        ret[this->column(i)] += this->value(i) * v[this->row(i)];
    } 
    return true;
}

/**
 * @brief Begin filling in ascending column order.
 *
 * Initiates a filling cycle. All elements <b>must</b> be written
 * in ascending column order. If your data is not in that
 * order, consider sorting it beforehand.
 * 
 * @param elementCount  Max. number of elements that will be written.
 *
 * @return True if successful, false otherwise.
 **/
bool goSparseMatrix::matlabFillBegin (int elementCount)
{
//    if (elementCount > mMaxElementCount)
//    {
//        return false;
//    }
    mMatlabFillCurrentColumn = 0;
    mMatlabFillElementCount  = elementCount;
    mMatlabFillCurrentIndex  = 0;
    int i;
    for (i = 0; i <= mCols; ++i)
    {
        mMatlabColFirstIndex[i] = 0;
    }
    return true;
}

/**
 * @brief Put a value within a fill cycle.
 *
 * @see fillBegin()
 * 
 * @param row    Row
 * @param col    Column
 * @param value  Value to put
 *
 * @return The number of elements left to put. 
 *  No more elements will be written to the matrix if the return value is 0.
 **/
int goSparseMatrix::matlabFillNext (int row, int col, double value)
{
    if (mMatlabFillElementCount <= 0)
    {
        return 0;
    }
    if (col < mMatlabFillCurrentColumn)
    {
        printf ("goSparseMatrix::matlabFillNext(): Must be filled in ascending column order!!!");
        return false;
    }
    if (col > mMatlabFillCurrentColumn)
    {
        int emptyColumns = col - mMatlabFillCurrentColumn - 1;
        int i;
        ++mMatlabFillCurrentColumn;
        for (i = 0; i < emptyColumns; ++i)
        {
            ++mMatlabFillCurrentColumn;
            mMatlabColFirstIndex [mMatlabFillCurrentColumn] = mMatlabFillCurrentIndex;
        }
    }
    mMatlabRowIndex      [mMatlabFillCurrentIndex]    = row;
    mMatlabColFirstIndex [mMatlabFillCurrentColumn+1] = mMatlabFillCurrentIndex + 1;
    mMatlabData          [mMatlabFillCurrentIndex]    = value;
    ++mMatlabFillCurrentIndex;
    --mMatlabFillElementCount;
    return mMatlabFillElementCount;
}

/**
 * @brief Ends a fill cycle.
 *
 * @note This <b>must</b> be called after the last element is written with
 *       fillNext(). That does not necessarily have to be the number of
 *       elements given by fillBegin(), but may be less.
 **/
void goSparseMatrix::matlabFillEnd ()
{
    // if (mFillElementCount == 0)
    {
        // Fill up the remaining mColFirstIndex array:
        int i;
        for (i = mMatlabFillCurrentColumn + 2; i <= mCols; ++i)
        {
            mMatlabColFirstIndex [i] = mMatlabFillCurrentIndex;
        }
    }
}

// =========================================

#endif
