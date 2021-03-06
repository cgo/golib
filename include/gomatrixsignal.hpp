#ifndef GOMATRIXSIGNAL_I
#define GOMATRIXSIGNAL_I

#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <golog.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

template <class T>
bool
goMatrixSignal<T>::initializeRows ()
{
    assert (this->matrix);
    if (this->rows)
    {
        delete[] this->rows;
        this->rows = NULL;
    }
    if (this->rowVectors)
    {
        delete[] this->rowVectors;
        this->rowVectors = NULL;
    }
    this->rows       = new goSubSignal3D<T> [this->getRows()];
    this->rowVectors = new goRowVector<T>   [this->getRows()];
    if (!this->rows || !this->rowVectors)
    {
        assert (false);
        return false;
    }
    goSize_t i;
    goSize3D rowSize (this->getColumns(), 1, 1);
    for (i = 0; i < this->getRows(); ++i)
    {
        this->rows[i].setSize        (rowSize);
        this->rows[i].setParent      (this->matrix);
        this->rows[i].setPosition    (0, i, 0);
        this->rowVectors[i].setData  (&this->rows[i]);
    }
    return true;
}

template <class T>
T& goMatrixSignal<T>::operator() (goIndex_t i, goIndex_t j)
{
    return this->rowVectors[i][j];
}

template <class T>
const T& goMatrixSignal<T>::operator() (goIndex_t i, goIndex_t j) const 
{
    return this->rowVectors[i][j];
}

template <class T>
goRowVector<T>&
goMatrixSignal<T>::operator [] (goSize_t row)
{
    assert (this->rowVectors);
    return this->rowVectors [row];
}

template <class T>
const goRowVector<T>&
goMatrixSignal<T>::operator [] (goSize_t row) const
{
    assert (this->rowVectors);
    return this->rowVectors [row];
}

//===================================================================

template <class T> 
goMatrixSignal<T>::goMatrixSignal (goSignal3DBase<T>* data)
    :
    linearStorage (false),
    externalData  (true),
    matrix        (0),
    rows          (NULL),
    rowVectors    (NULL)
{
    this->setData (data);
}

template <class T>
goMatrixSignal<T>::goMatrixSignal (goSize_t rows, goSize_t cols, bool linear)
    :
    linearStorage (linear),
    externalData  (false),
    matrix        (NULL),
    rows          (NULL),
    rowVectors    (NULL)
{
    if (linear)
    {
        matrix = (goSignal3DBase<T>*) new goSignal3D<T> (cols, rows, 1, cols, rows, 1, 0, 0, 0, 1);
    }
    else
    {
        matrix = (goSignal3DBase<T>*) new goSignal3D<T> (cols, rows, 1);
    }
    initializeRows ();
}

template<class T>
goMatrixSignal<T>::goMatrixSignal(const goMatrixSignal<T>& other)
    :
    linearStorage (other.isLinear()),
    externalData (false),
    matrix       (NULL),
    rows         (NULL),
    rowVectors   (NULL)
{
    assert (other.matrix != NULL);
    if (this->linearStorage)
    {
        matrix = (goSignal3DBase<T>*) new goSignal3D<T> (other.getColumns(), other.getRows(), 1, other.getColumns(), other.getRows(), 1, 0, 0, 0, 1);
    }
    else
    {
        matrix = (goSignal3DBase<T>*) new goSignal3D<T> (other.getColumns(), other.getRows(), 1);
    }
    if (other.getSizeX() == getSizeX() &&
        other.getSizeY() == getSizeY())
    {
        // std::cout << "Copy constructor...\n";
        GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    }
    initializeRows ();
}

template <class T>
goMatrixSignal<T>::~goMatrixSignal () 
{
    if (!externalData && matrix)
    {
        delete matrix;
        matrix = NULL;
    }
    if (this->rows)
    {
        delete [] this->rows;
        this->rows = NULL;
    }
    if (this->rowVectors)
    {
        delete [] this->rowVectors;
        this->rowVectors = NULL;
    }
}


template <class T> 
goSize_t
goMatrixSignal<T>::getColumns() const
{
    return getSizeX();
}

template <class T>  
goSize_t
goMatrixSignal<T>::getRows() const
{
    return getSizeY();
}


// TNT compatibility methods BEGIN

template <class T>
int
goMatrixSignal<T>::dim1 () const
{
    return (int)getRows ();
}

template <class T>
int
goMatrixSignal<T>::dim2 () const
{
    return (int)getColumns ();
}

template <class T>
const goMatrixSignal<T>&
goMatrixSignal<T>::copy () const
{
    return *this;
}

// TNT compatibility methods END  

template <class T>
T&
goMatrixSignal<T>::elem (goSize_t i, goSize_t j)
{
    return *(T*)matrix->getPtr (j,i,0);
}

template <class T>
goSize_t
goMatrixSignal<T>::getSizeX() const
{
    if (matrix)
    {
        return matrix->getSizeX();
    }
    return 0;
}

template <class T>
goSize_t
goMatrixSignal<T>::getSizeY() const
{
    if (matrix)
    {
        return matrix->getSizeY();
    }
    return 0;
}

//===================================================================


template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator= (const goMatrixSignal<T>& other) 
{
    assert (matrix);
    if (other.getSizeX() != getSizeX() ||
        other.getSizeY() != getSizeY())
    {
        this->resize (other.getRows(), other.getColumns());
    }
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    initializeRows ();
    return *this;
}

template <class T>
goMatrixSignal<T>
goMatrixSignal<T>::operator* (const goMatrixSignal<T>& other) 
{
  if (getColumns() != other.getRows())
  {
      goLog::warning ("goMatrixSignal::operator*: Matrix dimensions do not match.");
      return goMatrixSignal<T> (1,1);
  }
  goMatrixSignal<T> retval (getRows(), other.getColumns());
  goSize_t x, y, x2;
  goSize_t columns = retval.getColumns();
  goSize_t thisColumns = this->getColumns();
  for (y = 0; y < retval.getRows(); ++y) 
  {
    for (x = 0; x < columns; ++x) 
    {
      T* p  = this->matrix->getPtr (0, y, 0);
      T* po = other.matrix->getPtr (x, 0, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getYDiff();
      T value = (T)0; 
      for (x2 = 0; x2 < thisColumns; ++x2)
      {
          value += *p * *po;
          p += *pDiff;
          po += *poDiff;
          ++pDiff;
          ++poDiff;
      }
      retval.elem(y,x) = value;      
    }
  } 
  return retval;
}

// FIXME
template <class T>
goMatrixSignal<T>
goMatrixSignal<T>::operator- (const goMatrixSignal<T>& other) {
  goMatrixSignal<T> retval(getRows(), getColumns());
  goSize_t x, y;
  goSize_t columns = this->getColumns();
  for (y = 0; y < retval.getRows(); y++) 
  {
      T* p  = this->matrix->getPtr  (0, y, 0);
      T* po = other.matrix->getPtr  (0, y, 0);
      T* pr = retval.matrix->getPtr (0, y, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getXDiff();
      goPtrdiff_t* prDiff = retval.matrix->getXDiff();
      for (x = 0; x < columns; ++x)
      {
          *pr = *p - *po;
          p += *pDiff; po += *poDiff; pr += *prDiff;
          ++prDiff;
          ++poDiff;
          ++pDiff;
      }
  } 
  return retval;
}

template <class T>
goMatrixSignal<T>
goMatrixSignal<T>::operator+ (const goMatrixSignal<T>& other) {
  goMatrixSignal<T> retval(getRows(), getColumns());
  goSize_t x, y;
  goSize_t columns = this->getColumns();
  for (y = 0; y < retval.getRows(); y++) 
  {
      T* p  = this->matrix->getPtr  (0, y, 0);
      T* po = other.matrix->getPtr  (0, y, 0);
      T* pr = retval.matrix->getPtr (0, y, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getXDiff();
      goPtrdiff_t* prDiff = retval.matrix->getXDiff();
      for (x = 0; x < columns; ++x)
      {
          *pr = *p + *po;
          p += *pDiff; po += *poDiff; pr += *prDiff;
          ++prDiff;
          ++poDiff;
          ++pDiff;
      }
  } 
  return retval;
}

// FIXME
// This is a quick hack, very slow
template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator*= (const goMatrixSignal<T>& other) 
{
   goMatrixSignal<T> m = *this * other;
   *this = m;
   return *this;
}

template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator+= (const goMatrixSignal<T>& other) 
{
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr += *__ptr_target, (*this->matrix), (*other.matrix), T, T);
    return *this;
}

template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator-= (const goMatrixSignal<T>& other) {
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr += *__ptr_target, (*this->matrix), (*other.matrix), T, T);
    return *this;
}

template <class T>
goVector<T> 
goMatrixSignal<T>::operator* (const goVector<T>& v)
{
    goSize_t sz = v.getSize();
    if (getColumns() != sz)
    {
        goLog::warning ("goMatrixSignal::operator*: Matrix dimensions do not match vector.");
        return goVector<T> (1,1);
    }
    goSize_t retSz = this->getRows();
    goVector<T> retval (retSz);
    goSize_t x, y;
    // goSize_t columns = retval.getColumns();
    for (y = 0; y < retSz; ++y) 
    {
        T* p  = this->matrix->getPtr (0, y, 0);
        goPtrdiff_t* pDiff = this->matrix->getXDiff();
        const T* pv = v.getPtr ();
        T value = (T)0; 
        for (x = 0; x < sz; ++x) 
        {
            value += *p * *pv;
            p += *pDiff;
            ++pv;
            ++pDiff;
        }
        retval[y] = value;
    } 
    return retval;
}

template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator*= (T scalar) 
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr *= scalar, (*matrix), T);
    return *this;
}

template <class T>
goMatrixSignal<T>&
goMatrixSignal<T>::operator/= (T scalar) {
    GO_SIGNAL3D_EACHELEMENT (*__ptr /= scalar, (*matrix), T);
    return *this;
}

/** 
 * @brief Element-wise multiplication: this(i,j) = this(i,j) * other(i,j).
 * 
 * @param other Other matrix.
 * 
 * @return True if successful, false otherwise, e.g. when dimensions do not match (check logfile).
 */
template <class T>
bool goMatrixSignal<T>::multiplyElements (const goMatrixSignal<T>& other)
{
    if (this->getRows() != other.getRows() || this->getColumns() != other.getColumns())
    {
        goLog::warning ("goMatrixSignal::multiplyElements(): dimensions mismatch.\n");
        return false;
    }
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr_target *= *__ptr, (*other.getData()), (*this->getData()), const T, T);
    return true;
}

template <class T>
void
goMatrixSignal<T>::transpose() 
{
    matrix->swapXY();
    initializeRows ();
}

template<class T>
void
goMatrixSignal<T>::setIdentity ()
{
	fill(0);
	goSize_t n = MAX(matrix->getSizeX(),matrix->getSizeY());
	goSize_t i;
	for (i = 0; i < n; ++i)
	{
		(*this)[i][i] = (T)1;
	}
}

/** 
 * @brief L2 norm.
 *
 * @return \f$ \sqrt{tr(M^\top \cdot M)}\f$
 */
template<class T>
T goMatrixSignal<T>::norm () const
{
    T retValue = T(0);
    GO_SIGNAL3D_EACHELEMENT (retValue += *__ptr * *__ptr, (*this->matrix), T);
    return T(sqrt(retValue));
}


template<class T>
void
goMatrixSignal<T>::setUnity ()
{
    this->setIdentity ();
}

template<class T>
void
goMatrixSignal<T>::fill(T v)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = v, (*matrix), T);
}

template<class T>
void
goMatrixSignal<T>::print()
{
	unsigned int i,j;
    goSize_t sizeX = matrix->getSizeX();
    goSize_t sizeY = matrix->getSizeY();
	for (i = 0; i < sizeY; i++)
	{
		for (j = 0; j < sizeX; j++)
		{
            std::cout << *(T*)matrix->getPtr(j,i,0) << " ";
		}
        std::cout << "\n";
	}
    std::cout << std::endl;
}

template <class T>
bool goMatrixSignal<T>::resize (goSize_t rows, goSize_t columns)
{
    assert (this->matrix);
    if (this->externalData)
    {
        // Will not resize external data 
        return false;
    }
    this->matrix->destroy ();
    if (this->linearStorage)
    {
        ((goSignal3D<T>*)this->matrix)->make (columns, rows, 1, columns, rows, 1, 0, 0, 0, 1);
    }
    else
    {
        ((goSignal3D<T>*)this->matrix)->make (columns, rows, 1);
    }
    return initializeRows ();
}

template <class T>
bool goMatrixSignal<T>::setData (goSignal3DBase<T>* data)
{
    if (!this->externalData && this->matrix)
    {
        delete this->matrix;
        this->matrix = NULL;
    }
    this->matrix = data;
    this->externalData = true;
    if (data->getBlockSizeX() == data->getSizeX() && data->getBlockSizeY() == data->getSizeY() && data->getBlockSizeZ() == data->getSizeY())
    {
        this->linearStorage = true;
    }
    else
    {
        this->linearStorage = false;  
    }
    return initializeRows ();
}

#endif
