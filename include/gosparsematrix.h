#ifndef GOSPARSEMATRIX_H
#define GOSPARSEMATRIX_H

#ifdef HAVE_MATLAB
# include "mex.h"
#endif
#ifndef GOLOG_H
# include <golog.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#include <assert.h>
#ifndef GOHASHTABLE_H
# include <gohashtable.h>
#endif
#ifndef GOINDEXPAIR_H
# include <goindexpair.h>
#endif
#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOSORT_H
# include <gosort.h>
#endif

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
 *      if (goGetMatlabSparse (myArray, m))
 *          printf ("All is well!\n");
 *      
 *      // myArray should now contain the above values.
 *      ...
 * \endcode
 * 
 * @todo Make a "get all nonzero elements" method.
 * @todo TEST ADD / MULT / SUBTRACT METHODS THOROUGHLY!
 * @bug See todo, do tests.
 * @author Christian Gosch
 **/
class goSparseMatrix
{
    public:

        //= Sort types
        enum
        {
            UNSORTED,
            ROW_WISE,
            COLUMN_WISE
        };

        // inline goSparseMatrix  ();
        // inline goSparseMatrix  (mxArray* array);
        inline goSparseMatrix  (int rows = 0, int cols = 0);
        inline ~goSparseMatrix ();

        inline void init    ();
        inline void setSize (int rows, int cols);
//        inline double operator () (int row, int column);
        inline void set (int row, int col, double value);
        
//        inline int  getColElementCount (int column);
        inline int  getElementCount    () const; 
        inline int  getSortType        () const;
        
        inline bool fillBegin (int elementCount);
        inline int  fillNext  (int row, int col, double value);
        inline void fillEnd   (int sortType = ROW_WISE);

        inline bool matrixVectorMult (goArray<goDouble>&  ret, const goArray<goDouble>&  v);
        template <class Tv>
        inline bool matrixVectorMult     (goVector<Tv>&       ret, const goVector<Tv>&       v);
        inline bool vectorMatrixMult     (goArray<goDouble>&  ret, const goArray<goDouble>&  v);
        inline bool matrixVectorMult     (goSparseMatrix&     ret, const goArray<goDouble>&  v);
        inline bool vectorMatrixMult     (goSparseMatrix&     ret, const goArray<goDouble>&  v);
        inline bool matrixMatrixMult     (goSparseMatrix&     ret, goSparseMatrix& m);
        inline bool matrixMatrixAdd      (goSparseMatrix&     ret, goSparseMatrix& m);
        inline bool matrixMatrixSubtract (goSparseMatrix&     ret, goSparseMatrix& m);

        inline goSparseMatrix& operator*= (goDouble scalar);
        inline goSparseMatrix operator* (const goSparseMatrix& other) const;

        template <class Tv>
        inline goVector<Tv> operator* (const goVector<Tv>& v) const;
        inline goSparseMatrix operator* (goDouble s) const;
        inline goSparseMatrix operator+ (const goSparseMatrix& m) const;
        inline goSparseMatrix operator- (const goSparseMatrix& m) const;
        
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
        /// For row-sorted matrices
        inline const goArray<goIndex_t>& getRowStart () const
        {
            return mRowStartIndex;
        };
        /// For column-sorted matrices
        inline const goArray<goIndex_t>& getColStart () const
        {
            return mColStartIndex;
        };
        
        inline void sortRows     (bool sort_columns = false);
        inline void sortColumns  (bool sort_rows = false);
        inline void findRows     (); 
        inline void findColumns  (); 
        inline bool appendRow    (const goSparseMatrix& r);
        inline bool appendRows   (const goSparseMatrix& m);
        
        inline void transpose    ();

        inline goArray<goIndex_t>&       getColIndex() { return mColIndex; };
        inline const goArray<goIndex_t>& getColIndex() const { return mColIndex; };
        inline goArray<goIndex_t>&       getRowIndex() { return mRowIndex; };
        inline const goArray<goIndex_t>& getRowIndex() const { return mRowIndex; };
        inline goArray<goDouble>&        getValues() { return mValues; };
        inline const goArray<goDouble>&  getValues() const { return mValues; };


    protected:
        inline void setSortType (int t) { mSortType = t; };
        
// #ifdef HAVE_MATLAB        
//    private:
//        inline bool matlabFillBegin (int elementCount);
//        inline int  matlabFillNext  (int row, int col, double value);
//        inline void matlabFillEnd   ();
// #endif

    private:
        goArray<goIndex_t> mColIndex;
        goArray<goIndex_t> mRowIndex;
        goArray<goDouble>  mValues;
        
        goArray<goIndex_t> mRowStartIndex;
        goArray<goIndex_t> mColStartIndex;
        
        int      mMaxElementCount;
        double   mDefaultValue;
        int      mRows;
        int      mCols;
        int      mElementCount;               // Number of set elements by the fill*() methods.
        
        // For the fill routines
        int      mFillElementCount;
        int      mFillCurrentIndex;

        int      mSortType;

// #ifdef HAVE_MATLAB
//        int*     mMatlabColFirstIndex;        // mCols + 1            , Jc from mxArray
//        int*     mMatlabRowIndex;             // max. mMaxElementCount, Ir from mxArray
//        double*  mMatlabData;                 // max. mMaxElementCount, Pr from mxArray
//        // For the matlab fill routines
//        int      mMatlabFillElementCount;
//        int      mMatlabFillCurrentColumn;
//        int      mMatlabFillCurrentIndex;
// #endif
        
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
      mRowStartIndex       (),
      mColStartIndex       (),
      mMaxElementCount     (0),
      mDefaultValue        (0.0),
      mRows                (0),
      mCols                (0),
      mElementCount        (0),
      mFillElementCount    (0),
      mFillCurrentIndex    (0),
      mSortType            (UNSORTED)
// #ifdef HAVE_MATLAB
//      ,mMatlabColFirstIndex       (0),
//       mMatlabRowIndex            (0),
//       mMatlabData                (0),
//       mMatlabFillElementCount    (0),
//       mMatlabFillCurrentColumn   (0),
//       mMatlabFillCurrentIndex    (0)
// #endif
{
    this->setSize (rows, cols);
}

goSparseMatrix::~goSparseMatrix ()
{
}

void goSparseMatrix::init ()
{
    mColIndex.resize(0);
    mRowIndex.resize(0);
    mValues.resize(0);
    mElementCount     = 0;
    mMaxElementCount  = 0;
    mFillCurrentIndex = 0;
    mFillElementCount = 0;
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

/** --------------------------------------------------------------------------
 * @brief Sort array2 and array3 following to (sorted) array1
 * 
 * @param array1 
 * @param array2 
 * @param array3 
 * @param indexArray 
 * @param size 
 ----------------------------------------------------------------------------*/
template<class T, class T2>
static void sortHierarchy (T* array1, T* array2, T2* array3, goSize_t size)
{
    T v;
    goSize_t i = 0;
    while (i < size)
    {
        v = array1[i];
        goSize_t startIndex = i;
        while (i < size && array1[i] == v)
            ++i;
        goQuickSort (startIndex, i-1, array2, array3);
    }
}

/**
* @brief Sorts in ascending column order.
* 
* Sorts all elements so that they appear in ascending column order in the internal arrays.
**/
void goSparseMatrix::sortColumns (bool sort_rows)
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
    if (sort_rows)
        sortHierarchy (mColIndex.getPtr(), sortedRows.getPtr(), sortedValues.getPtr(), sortedValues.getSize());
    mRowIndex = sortedRows;
    mValues   = sortedValues;
    this->findColumns();
    this->mSortType = goSparseMatrix::COLUMN_WISE;
}

#if 1
void goSparseMatrix::findRows ()
{
    mRowStartIndex.resize(0);
    mColStartIndex.resize(0);
    //= Matrix elements must be sorted row-first.
    goIndex_t* rowIndex = this->getRowIndex().getPtr();
    goIndex_t startIndex = 0;
    goIndex_t size = this->getElementCount();
    goIndex_t i = 0;
    while (i < size)
    {
        startIndex = rowIndex[i];
        mRowStartIndex += i;
        while (i < size && rowIndex[i] == startIndex)
            ++i;
    }
    mRowStartIndex += i;
}

void goSparseMatrix::findColumns ()
{
    mColStartIndex.resize (0);
    mRowStartIndex.resize(0);
    //= Matrix elements must be sorted row-first.
    goIndex_t* colIndex = this->getColIndex().getPtr();
    goIndex_t startIndex = 0;
    goIndex_t size = this->getElementCount();
    goIndex_t i = 0;
    while (i < size)
    {
        startIndex = colIndex[i];
        mColStartIndex += i;
        while (i < size && colIndex[i] == startIndex)
            ++i;
    }
    mColStartIndex += i;
}
#endif

/**
* @brief Sorts in ascending row order.
* 
* Sorts all elements so that they appear in ascending row order in the internal arrays.
**/
void goSparseMatrix::sortRows (bool sort_columns)
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
    if (sort_columns)
        sortHierarchy (mRowIndex.getPtr(), sortedCols.getPtr(), sortedValues.getPtr(), sortedValues.getSize());
    mColIndex = sortedCols;
    mValues   = sortedValues;
    this->findRows();
    this->mSortType = goSparseMatrix::ROW_WISE;
}

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

int goSparseMatrix::getSortType () const
{
    return mSortType;
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
void goSparseMatrix::fillEnd (int sortMethod)
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
    switch (sortMethod)
    {
        case goSparseMatrix::ROW_WISE: this->sortRows(true); break;
        case goSparseMatrix::COLUMN_WISE: this->sortColumns(true); break;
        default: break;
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
* @brief  (this) * v
*
* @param ret  
* @param v  
*
* @return 
**/
template <class Tv>
bool goSparseMatrix::matrixVectorMult (goVector<Tv>& ret, const goVector<Tv>& v)
{
    // printf ("Matrix vector mult %dx%d * %d\n", this->getRowCount(), this->getColumnCount(), v.getSize());
    if (mCols != (int)v.getSize())
    {
        printf ("Wrong size!\n");
        return false;
    }
    goIndex_t elemCount = this->getElementCount();
    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    ret.setSize (mRows);
    ret.fill (0.0);
    goIndex_t i = 0;
    goDouble value = 0.0;
    goIndex_t row = 0;
    goIndex_t* rowIndex = this->getRowIndex().getPtr();
    goIndex_t* colIndex = this->getColIndex().getPtr();
    goDouble*  values   = this->getValues().getPtr();
    while (i < elemCount)
    {
        row = *rowIndex;
        // row = this->row(i);
        value = 0.0;
        while (i < elemCount && row == *rowIndex)
        {
            value += *values * v[*colIndex];
            ++i;
            ++rowIndex;
            ++colIndex;
            ++values;
        }
        ret[row] = value;
    }
//    for (goIndex_t i = 0; i < elemCount; ++i)
//    {
//        ret[this->row(i)] += this->value(i) * v[this->column(i)];
//    } 
    // printf ("Done!\n");
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
    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    ret.setSize (mRows,1);
    if (elemCount == 0)
    {
        return true;
    }
    //= Put exactly as many elements in ret as there are rows with nonzero elements in (this)
    ret.fillBegin (this->getRowStart().getSize()-1);
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
                goLog::error("goSparseMatrix::matrixVectorMult(): **************** SOMETHING TERRIBLE HAPPENED! *******************");
            }
            rowIndex = this->row(i);
            value = this->value(i) * v[this->column(i)];
        }
    }
    //= Fill in the last value
    ret.fillNext (rowIndex,0,value);
    ret.fillEnd (UNSORTED);
    ret.setSortType (ROW_WISE);
    ret.findRows();
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
    if (this->mSortType != goSparseMatrix::COLUMN_WISE)
    {
        this->sortColumns(true);
    }
    ret.setSize (1,mCols);
    if (elemCount == 0)
    {
        return true;
    }
    //= Put exactly as many elements in ret as there are columns with nonzero elements in (this)
    ret.fillBegin (this->getColStart().getSize()-1);
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
    ret.fillEnd (UNSORTED);
    ret.setSortType (COLUMN_WISE);
    ret.findColumns();
    return true;
}

//= Multiply one line with another line (e.g. row * column)
template <class T>
static inline T multSparse (goIndex_t* startIndex1, goIndex_t elemCount1,
                            goIndex_t* startIndex2, goIndex_t elemCount2,
                            T* values1, T* values2)
{
    T value = T(0);
    while (elemCount1 > 0 && elemCount2 > 0)
    {
        if (*startIndex1 == *startIndex2)
        {
            value += *values1 * *values2;
            ++startIndex1;
            ++startIndex2;
            ++values1;
            ++values2;
            --elemCount1;
            --elemCount2;
        }
        else
        {
            if (*startIndex1 < *startIndex2)
            {
                ++startIndex1;
                ++values1;
                --elemCount1;
            }
            else
            {
                ++startIndex2;
                ++values2;
                --elemCount2;
            }
        }
    }
    return value;
}

bool goSparseMatrix::matrixMatrixMult (goSparseMatrix& ret, goSparseMatrix& m) 
{
    assert (this->getColumnCount() == m.getRowCount());

    if (this->getColumnCount() != m.getRowCount())
        return false;
    
    goArray<goIndex_t> retRows;
    goArray<goIndex_t> retCols;
    goArray<goDouble>  retValues;
    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    if (m.getSortType() != goSparseMatrix::COLUMN_WISE)
    {
        m.sortColumns(true);
    }
    
    //printf ("After sorting:\n");
    //for (goIndex_t i = 0; i < this->getElementCount(); ++i)
    //{
    //    printf ("(%d,%d) == %f\n", this->row(i), this->column(i), this->value(i));
   // }
    //printf("\n");
    //for (goIndex_t i = 0; i < m.getElementCount(); ++i)
   // {
   //     printf ("(%d,%d) == %f\n", m.row(i), m.column(i), m.value(i));
   // }
   // printf("\n");
    
    
    goIndex_t thisSize = this->getElementCount();
    goIndex_t otherSize = m.getElementCount();
    goIndexPair indexPair;
    //= For each non-zero column in matrix m
    goIndex_t j = 0;
    goIndex_t i = 0;
    goIndex_t j2 = 0;
    goIndex_t i2 = 0;
    goIndex_t currentRow = this->row(i);
    goIndex_t currentCol = m.column(j);
    goDouble  value      = 0.0;
    goArray<goIndex_t>* thisCols   = &this->getColIndex();
    goArray<goIndex_t>* otherRows  = &m.getRowIndex();
    goArray<goDouble>* thisValues  = &this->getValues();
    goArray<goDouble>* otherValues = &m.getValues();

    const goArray<goIndex_t>* rowStart = &this->getRowStart();
    const goArray<goIndex_t>* colStart = &m.getColStart();
    goIndex_t rowCount = rowStart->getSize() - 1;
    goIndex_t colCount = colStart->getSize() - 1;
    goIndex_t colStartIndex = 0;
    goIndex_t elemCountRow = 0;
    goIndex_t elemCountCol = 0;
    for (i = 0; i < rowCount; ++i)
    {
        goIndex_t* col1 = &(*thisCols)[(*rowStart)[i]];
        elemCountRow = (*rowStart)[i+1] - (*rowStart)[i];
        goDouble*  valueStart = &(*thisValues)[(*rowStart)[i]];
        for (j = 0; j < colCount; ++j)
        {
            colStartIndex = (*colStart)[j];
            elemCountCol = (*colStart)[j+1] - colStartIndex;
            value = multSparse (col1, elemCountRow,
                                &(*otherRows)[colStartIndex],
                                elemCountCol,
                                valueStart,
                                &(*otherValues)[colStartIndex]);
            if (value != 0.0)
            {
                retRows += this->row((*rowStart)[i]);
                retCols += m.column((*colStart)[j]);
                retValues += value;
            }
        }
    }
    ret.setSize (this->getRowCount(), m.getColumnCount());
    ret.fillBegin (retRows.getSize());
    {
        goIndex_t size = retRows.getSize();
        for (i = 0; i < size; ++i)
        {
            ret.fillNext (retRows[i], retCols[i], retValues[i]);
        }
    }
    //= The result should already be sorted row-wise.
    ret.fillEnd(goSparseMatrix::UNSORTED);
    ret.setSortType(goSparseMatrix::ROW_WISE);
    ret.findRows();
    return true;
}

bool goSparseMatrix::matrixMatrixAdd (goSparseMatrix& ret, goSparseMatrix& m)
{
    assert (this->getRowCount() == m.getRowCount() &&
            this->getColumnCount() == m.getColumnCount());

    if (this->getRowCount() != m.getRowCount() || 
        this->getColumnCount() != m.getColumnCount())
    {
        return false;
    }
    goArray<goIndex_t> retRows;
    goArray<goIndex_t> retCols;
    goArray<goDouble>  retValues;
    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    if (m.getSortType() != goSparseMatrix::ROW_WISE)
    {
        m.sortRows(true);
    }
    
    //printf ("After sorting:\n");
    //for (goIndex_t i = 0; i < this->getElementCount(); ++i)
    //{
    //    printf ("(%d,%d) == %f\n", this->row(i), this->column(i), this->value(i));
   // }
    //printf("\n");
    //for (goIndex_t i = 0; i < m.getElementCount(); ++i)
   // {
   //     printf ("(%d,%d) == %f\n", m.row(i), m.column(i), m.value(i));
   // }
   // printf("\n");
    
    
    goIndex_t thisSize = this->getElementCount();
    goIndex_t otherSize = m.getElementCount();
    //= For each non-zero column in matrix m
    goIndex_t j = 0;
    goIndex_t i = 0;
    goDouble  value = 0.0;
    goArray<goIndex_t>* thisCols   = &this->getColIndex();
    goArray<goIndex_t>* otherRows  = &m.getRowIndex();
    goArray<goDouble>* thisValues  = &this->getValues();
    goArray<goDouble>* otherValues = &m.getValues();

    const goArray<goIndex_t>* rowStart = &this->getRowStart();
    const goArray<goIndex_t>* otherRowStart = &m.getRowStart();
    goIndex_t rowCount = rowStart->getSize() - 1;
    goIndex_t otherRowCount = otherRowStart->getSize() - 1;
    i = 0;
    j = 0;
    goIndex_t thisElemCount = this->getElementCount();
    goIndex_t otherElemCount = m.getElementCount();
    while (i < thisElemCount && j < otherElemCount)
    {
        if (this->row(i) == m.row(j))
        {
            if (this->column(i) < m.column(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            else
            {
                if (this->column(i) > m.column(j))
                {
                    retRows += m.row(j);
                    retCols += m.column(j);
                    retValues += m.value(j);
                    ++j;
                }
                else
                {
                    retRows += m.row(j);
                    retCols += m.column(j);
                    retValues += m.value(j) + this->value(i);
                    ++j;
                    ++i;
                }
            }
        }
        else
        {
            if (this->row(i) < m.row(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            else
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += m.value(j);
                ++j;
            }
        }
    }
    while (i < thisElemCount)
    {
        retRows += this->row(i);
        retCols += this->column(i);
        retValues += this->value(i);
        ++i;
    }
    while (j < otherElemCount)
    {
        retRows += m.row(j);
        retCols += m.column(j);
        retValues += m.value(j);
        ++j;
    }

    ret.setSize (this->getRowCount(), this->getColumnCount());
    ret.fillBegin (retRows.getSize());
    {
        goIndex_t size = retRows.getSize();
        for (i = 0; i < size; ++i)
        {
            ret.fillNext (retRows[i], retCols[i], retValues[i]);
        }
    }
    //= The result should already be sorted row-wise.
    ret.fillEnd(goSparseMatrix::UNSORTED);
    ret.setSortType(goSparseMatrix::ROW_WISE);
    ret.findRows();
    return true;
}

bool goSparseMatrix::matrixMatrixSubtract (goSparseMatrix& ret, goSparseMatrix& m)
{
    assert (this->getRowCount() == m.getRowCount() &&
            this->getColumnCount() == m.getColumnCount());

    if (this->getRowCount() != m.getRowCount() || 
        this->getColumnCount() != m.getColumnCount())
    {
        return false;
    }
    goArray<goIndex_t> retRows;
    goArray<goIndex_t> retCols;
    goArray<goDouble>  retValues;
    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    if (m.getSortType() != goSparseMatrix::ROW_WISE)
    {
        m.sortRows(true);
    }
    
    //printf ("After sorting:\n");
    //for (goIndex_t i = 0; i < this->getElementCount(); ++i)
    //{
    //    printf ("(%d,%d) == %f\n", this->row(i), this->column(i), this->value(i));
   // }
    //printf("\n");
    //for (goIndex_t i = 0; i < m.getElementCount(); ++i)
   // {
   //     printf ("(%d,%d) == %f\n", m.row(i), m.column(i), m.value(i));
   // }
   // printf("\n");
    
    
    goIndex_t thisSize = this->getElementCount();
    goIndex_t otherSize = m.getElementCount();
    //= For each non-zero column in matrix m
    goIndex_t j = 0;
    goIndex_t i = 0;
    goDouble  value = 0.0;
    goArray<goIndex_t>* thisCols   = &this->getColIndex();
    goArray<goIndex_t>* otherRows  = &m.getRowIndex();
    goArray<goDouble>* thisValues  = &this->getValues();
    goArray<goDouble>* otherValues = &m.getValues();

    const goArray<goIndex_t>* rowStart = &this->getRowStart();
    const goArray<goIndex_t>* otherRowStart = &m.getRowStart();
    goIndex_t rowCount = rowStart->getSize() - 1;
    goIndex_t otherRowCount = otherRowStart->getSize() - 1;
    i = 0;
    j = 0;
    goIndex_t thisElemCount = this->getElementCount();
    goIndex_t otherElemCount = m.getElementCount();
    while (i < thisElemCount && j < otherElemCount)
    {
        if (this->row(i) == m.row(j))
        {
            if (this->column(i) < m.column(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            else
            {
                if (this->column(i) > m.column(j))
                {
                    retRows += m.row(j);
                    retCols += m.column(j);
                    retValues += -m.value(j);
                    ++j;
                }
                else
                {
                    retRows += m.row(j);
                    retCols += m.column(j);
                    retValues += this->value(i) - m.value(j);
                    ++j;
                    ++i;
                }
            }
        }
        else
        {
            if (this->row(i) < m.row(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            else
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += -m.value(j);
                ++j;
            }
        }
    }
    while (i < thisElemCount)
    {
        retRows += this->row(i);
        retCols += this->column(i);
        retValues += this->value(i);
        ++i;
    }
    while (j < otherElemCount)
    {
        retRows += m.row(j);
        retCols += m.column(j);
        retValues += -m.value(j);
        ++j;
    }

    ret.setSize (this->getRowCount(), this->getColumnCount());
    ret.fillBegin (retRows.getSize());
    {
        goIndex_t size = retRows.getSize();
        for (i = 0; i < size; ++i)
        {
            ret.fillNext (retRows[i], retCols[i], retValues[i]);
        }
    }
    //= The result should already be sorted row-wise.
    ret.fillEnd(goSparseMatrix::UNSORTED);
    ret.setSortType(goSparseMatrix::ROW_WISE);
    ret.findRows();
    return true;
}

// FIXME
# if 0
bool goSparseMatrix::matrixMatrixAdd (goSparseMatrix& ret, goSparseMatrix& m)
{
    assert (this->getColumnCount() == m.getColumnCount() && this->getRowCount() == m.getRowCount());

    if (this->getColumnCount() != m.getColumnCount() || this->getRowCount() != m.getRowCount())
        return false;
    
    // goSize_t maxElements = goMath::max (this->getElementCount(), m.getElementCount());

    goIndex_t thisElementCount  = this->getElementCount();
    goIndex_t otherElementCount = m.getElementCount();
    goArray<goIndex_t> retRows;
    goArray<goIndex_t> retCols;
    goArray<goDouble>  retValues;

    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    if (m.getSortType() != goSparseMatrix::ROW_WISE)
    {
        m.sortRows(true);
    }
    
    goIndex_t i = 0;
    goIndex_t j = 0;

    while (i < thisElementCount && j < otherElementCount)
    {
        while (i < thisElementCount && this->row(i) < m.row(j))
        {
            retRows += this->row(i);
            retCols += this->column(i);
            retValues += this->value(i);
            ++i;
        }
        while (j < otherElementCount && this->row(i) > m.row(j))
        {
            retRows += m.row(j);
            retCols += m.column(j);
            retValues += m.value(j);
            ++j;
        }
        if (i < thisElementCount && j < otherElementCount && this->row(i) == m.row(j))
        {
            while (i < thisElementCount && this->column(i) < m.column(j) && this->row(i) == m.row(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            while (j < otherElementCount && this->column(i) > m.column(j) && this->row(i) == m.row(j))
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += m.value(j);
                ++j;
            }
            if (i < thisElementCount && j < otherElementCount && this->column(i) == m.column(j) && this->row(i) == m.row(j))
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += m.value(j) + this->value(i);
                ++i;
                ++j;
            }
        }
        if (i >= thisElementCount)
        {
            while (j < otherElementCount)
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += m.value(j);
                ++j;
            }
        }
        if (j >= otherElementCount)
        {
            while (i < thisElementCount)
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
        }
    }
    
    ret.setSize (this->getRowCount(), this->getColumnCount());
    ret.fillBegin (retRows.getSize());
    {
        goIndex_t size = retRows.getSize();
        for (i = 0; i < size; ++i)
        {
            ret.fillNext (retRows[i], retCols[i], retValues[i]);
        }
    }
    ret.fillEnd (UNSORTED);
    ret.setSortType (ROW_WISE);
    ret.findRows();
    return true;
}
#endif

#if 0
bool goSparseMatrix::matrixMatrixSubtract (goSparseMatrix& ret, goSparseMatrix& m)
{
    assert (this->getColumnCount() == m.getColumnCount() && this->getRowCount() == m.getRowCount());

    if (this->getColumnCount() != m.getColumnCount() || this->getRowCount() != m.getRowCount())
        return false;
    
    // goSize_t maxElements = goMath::max (this->getElementCount(), m.getElementCount());

    goIndex_t thisElementCount  = this->getElementCount();
    goIndex_t otherElementCount = m.getElementCount();
    goArray<goIndex_t> retRows;
    goArray<goIndex_t> retCols;
    goArray<goDouble>  retValues;

    if (this->mSortType != goSparseMatrix::ROW_WISE)
    {
        this->sortRows(true);
    }
    if (m.getSortType() != goSparseMatrix::ROW_WISE)
    {
        m.sortRows(true);
    }
    
    goIndex_t i = 0;
    goIndex_t j = 0;

    while (i < thisElementCount && j < otherElementCount)
    {
        while (i < thisElementCount && this->row(i) < m.row(j))
        {
            retRows += this->row(i);
            retCols += this->column(i);
            retValues += this->value(i);
            ++i;
        }
        while (j < otherElementCount && this->row(i) > m.row(j))
        {
            retRows += m.row(j);
            retCols += m.column(j);
            retValues += -m.value(j);
            ++j;
        }
        if (i < thisElementCount && j < otherElementCount && this->row(i) == m.row(j))
        {
            while (i < thisElementCount && this->column(i) < m.column(j) && this->row(i) == m.row(j))
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
            while (j < otherElementCount && this->column(i) > m.column(j) && this->row(i) == m.row(j))
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += -m.value(j);
                ++j;
            }
            if (i < thisElementCount && j < otherElementCount && this->column(i) == m.column(j) && this->row(i) == m.row(j))
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += this->value(i) - m.value(j);
                ++i;
                ++j;
            }
        }
        if (i >= thisElementCount)
        {
            while (j < otherElementCount)
            {
                retRows += m.row(j);
                retCols += m.column(j);
                retValues += -m.value(j);
                ++j;
            }
        }
        if (j >= otherElementCount)
        {
            while (i < thisElementCount)
            {
                retRows += this->row(i);
                retCols += this->column(i);
                retValues += this->value(i);
                ++i;
            }
        }
    }
    
    ret.setSize (this->getRowCount(), this->getColumnCount());
    ret.fillBegin (retRows.getSize());
    {
        goIndex_t size = retRows.getSize();
        for (i = 0; i < size; ++i)
        {
            ret.fillNext (retRows[i], retCols[i], retValues[i]);
        }
    }
    ret.fillEnd(goSparseMatrix::UNSORTED);
    ret.setSortType (goSparseMatrix::ROW_WISE);
    ret.findRows();
    return true;
}
#endif

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

goSparseMatrix goSparseMatrix::operator* (goDouble scalar) const
{
    goSparseMatrix ret;
    ret = *this;
    goIndex_t i;
    goIndex_t sz = ret.getElementCount();
    goDouble* ptr = ret.getValues().getPtr();
    for (i = 0; i < sz; ++i)
    {
        *ptr *= scalar;
        ++ptr;
    }
    return ret;
}

goSparseMatrix goSparseMatrix::operator* (const goSparseMatrix& other) const
{
    goSparseMatrix ret;
    goSparseMatrix temp = *this;
    goSparseMatrix temp2 = other;
    //= Hack to avoid the const for the *Mult() methods. Using operator* should be avoided in speed-relevant code anyway
    //= because of the copy operations.
    temp.matrixMatrixMult (ret, temp2);
    return ret;
}

template <class Tv>
goVector<Tv> goSparseMatrix::operator* (const goVector<Tv>& v) const
{
    goVector<Tv> ret;
    goSparseMatrix temp = *this;
    //= Hack to avoid the const for the *Mult() methods. Using operator* should be avoided in speed-relevant code anyway
    //= because of the copy operations.
    temp.matrixVectorMult (ret, v);
    return ret;
}

goSparseMatrix goSparseMatrix::operator+ (const goSparseMatrix& other) const
{
    goSparseMatrix ret;
    goSparseMatrix temp = *this;
    goSparseMatrix temp2 = other;
    //= Hack to avoid the const for the *Mult() methods. Using operator* should be avoided in speed-relevant code anyway
    //= because of the copy operations.
    temp.matrixMatrixAdd(ret, temp2);
    return ret;
}

goSparseMatrix goSparseMatrix::operator- (const goSparseMatrix& other) const
{
    goSparseMatrix ret;
    goSparseMatrix temp = *this;
    goSparseMatrix temp2 = other;
    //= Hack to avoid the const for the *Mult() methods. Using operator* should be avoided in speed-relevant code anyway
    //= because of the copy operations.
    temp.matrixMatrixSubtract(ret, temp2);
    return ret;
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
    goIndex_t j = 0;
    goIndex_t rowIndex = mRows - 1;
    for (j = 0; j < elemCount; ++i,++j)
    {
        mColIndex[i] = r.column(j);
        mRowIndex[i] = rowIndex;
        mValues  [i] = r.value(j);
    }
    return true;
}

/**
 * @bug After appending rows, findRows() should be called when this matrix is
 *      row-sorted. This needs to be fixed or it will lead to
 *      misunderstandings and malfunctioning code/hard to find errors.
 */
bool goSparseMatrix::appendRows (const goSparseMatrix& m)
{
    //= Only accept same column count
    if (m.getColumnCount() != mCols)
    {
        return false;
    }

    goIndex_t elemCount = m.getElementCount();
    goIndex_t rowBase = mRows;
    this->setSize (mRows + m.getRowCount(), mCols);
    goIndex_t i = mElementCount;
    mElementCount = this->getElementCount() + elemCount;
    mColIndex.resize (mElementCount);
    mRowIndex.resize (mElementCount);
    mValues.resize   (mElementCount);
    goIndex_t j;
    // goIndex_t rowIndex = rowBase; // mRows - m.getRowCount();
    for (j = 0; j < elemCount; ++i,++j)
    {
        mColIndex[i] = m.column(j);
        mRowIndex[i] = m.row(j) + rowBase;
        mValues  [i] = m.value(j);
    }
    return true;
}

void goSparseMatrix::transpose ()
{
    goArray<goIndex_t> temp = mRowIndex;
    mRowIndex = mColIndex;
    mColIndex = temp;
    goIndex_t temp2 = mRows;
    mRows = mCols;
    mCols = temp2;
    if (this->mSortType == goSparseMatrix::ROW_WISE)
    {
        this->mSortType = goSparseMatrix::COLUMN_WISE;
        this->mColStartIndex = this->mRowStartIndex;
        this->mRowStartIndex.resize(0);
    }
    else
    {
        if (this->mSortType == goSparseMatrix::COLUMN_WISE)
        {
            this->mSortType = goSparseMatrix::ROW_WISE;
            this->mRowStartIndex = this->mColStartIndex;
            this->mColStartIndex.resize(0);
        }
    }
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

// =========================================

#ifdef HAVE_MATLAB
/*!
* If HAVE_MATLAB is defined at compile time,
* this method can be used to fill a Matlab sparse
* mxArray with the values from a goSparseMatrix object.
* target must be of the same size as source object
* and the maximum number of nonzero elements
* in target must be sufficient (mxGetNzmax() Matlab function).
* target must be sparse (created with mxCreateSparse()).
*/
static bool goGetMatlabSparse (mxArray* target, goSparseMatrix& matrix)
{
    int*     mMatlabColFirstIndex = 0;        // mCols + 1            , Jc from mxArray
    int*     mMatlabRowIndex = 0;             // max. mMaxElementCount, Ir from mxArray
    double*  mMatlabData = 0;                 // max. mMaxElementCount, Pr from mxArray
    // For the matlab fill routines
    int      mMatlabFillElementCount = 0;
    int      mMatlabFillCurrentColumn = 0;
    int      mMatlabFillCurrentIndex = 0;
    
    int mRows = matrix.getRowCount();
    int mCols = matrix.getColumnCount();
    int mElementCount = matrix.getElementCount();
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

    goArray<goIndex_t>* mRowIndex = &matrix.getRowIndex();
    goArray<goIndex_t>* mColIndex = &matrix.getColIndex();
    goArray<goDouble>*  mValues   = &matrix.getValues();
    
    
    // Sort in ascending column order
    goArray<int> indexArray (mRowIndex->getSize());
    int i;
    for (i = 0; i < indexArray.getSize(); ++i)
    {
        indexArray[i] = i;
    }
    goArray<goIndex_t> sortedCols (*mColIndex);
    sortedCols.sort (indexArray);
    goArray<goIndex_t> sortedRows (mRowIndex->getSize());
    goArray<goDouble>  sortedValues (mValues->getSize());

    goIndex_t sz = sortedRows.getSize();
    for (i = 0; i < sz; ++i)
    {
        sortedRows[i]   = (*mRowIndex)[indexArray[i]];
        sortedValues[i] = (*mValues)[indexArray[i]];
    }
    
    sz = mElementCount;
    
    //= fillBegin
    mMatlabFillCurrentColumn = 0;
    mMatlabFillElementCount  = mElementCount;
    mMatlabFillCurrentIndex  = 0;
    
    for (i = 0; i <= mCols; ++i)
    {
        mMatlabColFirstIndex[i] = 0;
    }
    
    int row = 0;
    int col = 0;
    double value = 0.0; 
    for (i = 0; i < sz; ++i)
    {
        assert (sortedRows[i] >= 0);
        assert (sortedCols[i] >= 0);
        assert (sortedRows[i] < mRows);
        assert (sortedCols[i] < mCols);

        //= fillNext
        if (mMatlabFillElementCount <= 0)
        {
            break;
        }
        col = sortedCols[i];
        row = sortedRows[i];
        value = sortedValues[i];
        if (col < mMatlabFillCurrentColumn)
        {
            printf ("goSparseMatrix::matlabFillNext(): Must be filled in ascending column order!!!");
            return false;
        }
        if (col > mMatlabFillCurrentColumn)
        {
            int emptyColumns = col - mMatlabFillCurrentColumn - 1;
            int j;
            ++mMatlabFillCurrentColumn;
            for (j = 0; j < emptyColumns; ++j)
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
        //= End fillNext
        // this->matlabFillNext (sortedRows[i], sortedCols[i], sortedValues[i]);
    }
    //= fillEnd
    
    for (i = mMatlabFillCurrentColumn + 2; i <= mCols; ++i)
    {
        mMatlabColFirstIndex [i] = mMatlabFillCurrentIndex;
    }
    // this->matlabFillEnd();
    return true;
}
#endif
#endif
