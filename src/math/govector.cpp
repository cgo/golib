#include <govector.h>
#include <gomatrix.h>
#include <golog.h>
#if 1
extern "C"
{
#include <cblas.h>
}

template <class T>
goVector<T>::goVector () 
    : goFixedArray<T> (1,0,0) 
{
}

template <class T>
goVector<T>::goVector (goSize_t s, goIndex_t leftBorder, goIndex_t rightBorder) 
    : goFixedArray<T> (s,leftBorder,rightBorder) 
{
}

template <class T>
goVector<T>::~goVector ()
{
}

//= Was inlined.
#if 0
template <class T>
goVector<T> goVector<T>::operator- (const goVector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goVector<T> ret (this->getSize());
    goIndex_t max = this->getSize();
    T* retArray = ret.getPtr();
    const T* array = this->getPtr();
    goIndex_t stride = this->getStride();
    const T* otherArray = other.getPtr();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *retArray = *array - *otherArray;
        array += stride;
        otherArray += otherStride;
        ++retArray;
    }
    return ret;
};
#endif

template<> 
goFloat goVector<goFloat>::operator* (const goVector<goFloat>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_sdot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template<> 
goDouble goVector<goDouble>::operator* (const goVector<goDouble>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_ddot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template <class T>
goVector<T>& goVector<T>::operator*= (const goMatrix<T>& m)
{
    if (m.getColumns() != this->getSize())
    {
        goLog::warning ("goVector::operator*= (goMatrix): Matrix has wrong column count.");
        return *this;
    }
    *this = m * *this;
    return *this;
}

template <>
goVector<goComplexf>& goVector<goComplexf>::operator*= (const goMatrix<goComplexf>&)
{
    goLog::warning ("goVector::operator*= (goMatrix): Not implemented for complex types.");
    return *this;
}

//template <>
//goVector<goComplexd>& goVector<goComplexd>::operator*= (const goMatrix<goComplexd>&)
//{
//    goLog::warning ("goVector::operator*= (goMatrix): Not implemented for complex types.");
//    return *this;
//}

/**
 * @brief Outer product.
 * @note Untested.
 * @return 
 **/
template <class T>
void goVector<T>::outerProduct (const goVector<T>& other, goMatrix<T>& ret) const
{
    goVectorOuter<T> (T(1), *this, other, ret);
};

template <class T>
T goVector<T>::operator* (const goVector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    T ret = T(0);
    goIndex_t max = this->getSize();
    const T* array = this->getPtr();
    const T* otherArray = other.getPtr();
    goIndex_t stride = this->getStride();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        ret += *array * *otherArray;
        array += stride;
        otherArray += otherStride;
    }
    return ret;
};
#endif

/**
 * @brief Calculates conj(this) * this.
 *
 * @return The return value is of the form r+i*0, meaning the imaginary part is 0.
 **/
template<> goComplexf goVector<goComplexf>::square () const
{
    const goComplexf* array = this->getPtr();
    goFloat sum = 0.0;
    goIndex_t size = this->getSize();
    for (goIndex_t i = 0; i < size; ++i, ++array)
    {
        sum += array->re() * array->re() + array->im() * array->im();
    }
    return goComplexf (sum, 0.0f);
}

template <class T>
inline T goVector<T>::square () const
{
    return *this * *this;
}

template<> inline goComplexf goVector<goComplexf>::abs () const
{
    return goComplexf(sqrt(this->square().re()), 0.0f);
}

template<> inline goComplexd goVector<goComplexd>::abs () const
{
    return goComplexd(sqrt(this->square().re()), 0.0);
}

template<class T> T goVector<T>::abs () const
{
    return this->norm2 ();
}

template<> goFloat goVector<goFloat>::norm2 () const
{
    return cblas_snrm2 (this->getSize(), this->getPtr(), this->getStride());
}

template<> goDouble goVector<goDouble>::norm2 () const
{
    return cblas_dnrm2 (this->getSize(), this->getPtr(), this->getStride());
}

template<> goFloat goVector<goFloat>::norm1 () const
{
    return cblas_sasum (this->getSize(), this->getPtr(), this->getStride());
}

template<> goDouble goVector<goDouble>::norm1 () const
{
    return cblas_dasum (this->getSize(), this->getPtr(), this->getStride());
}

template<> goComplexf goVector<goComplexf>::norm2 () const
{
    return this->abs();
}

template<> goComplexd goVector<goComplexd>::norm2 () const
{
    return this->abs();
}

template<> goComplexf goVector<goComplexf>::norm1 () const
{
    goLog::error ("goVector::norm1() not implemented for complex types.");
    return goComplexf(0.0f,0.0f);
}

template<> goComplexd goVector<goComplexd>::norm1 () const
{
    goLog::error ("goVector::norm1() not implemented for complex types.");
    return goComplexd(0.0,0.0);
}

template<class T> T goVector<T>::norm2 () const
{
    return static_cast<T>(sqrt(static_cast<goDouble>(*this * *this)));
}

template<class T> T goVector<T>::norm1 () const
{
    goLog::error ("goVector::norm1() not implemented for this type (integer?).");
    return T(0);
}

template <class T>
void goVector<T>::fillRange (const T& start, const T& step, const T& end)
{
    goSize_t i = 0;
    for (T v = start; v < end; v += step, ++i)
    {
        (*this)[i] = v;
    }
}

// =====================================

template <>
bool goVectorAdd<goFloat> (goFloat alpha, const goVector<goFloat>& x, goVector<goFloat>& y)
{
    assert (x.getSize() == y.getSize());
    goSize_t N = x.getSize();
    if (y.getSize() != N)
    {
        return false;
    }
    cblas_saxpy (N, alpha, x.getPtr(), x.getStride(), y.getPtr(), y.getStride());
    return true;
}

template <>
bool goVectorAdd<goDouble> (goDouble alpha, const goVector<goDouble>& x, goVector<goDouble>& y)
{
    assert (x.getSize() == y.getSize());
    goSize_t N = x.getSize();
    if (y.getSize() != N)
    {
        return false;
    }
    cblas_daxpy (N, alpha, x.getPtr(), x.getStride(), y.getPtr(), y.getStride());
    return true;
}

template <>
void goVectorOuter<goFloat> (goFloat alpha, const goVector<goFloat>& x, const goVector<goFloat>& y, goMatrix<goFloat>& ret)
{
    goSize_t M = x.getSize();
    goSize_t N = y.getSize();

    if (ret.getRows() != M || ret.getColumns() != N)
    {
        ret.resize (M,N);
        ret.fill (0.0f);
    }

    cblas_sger (CblasRowMajor, M, N, alpha, x.getPtr(), x.getStride(), y.getPtr(), y.getStride(), ret.getPtr(), ret.getLeadingDimension());
}

template <>
void goVectorOuter<goDouble> (goDouble alpha, const goVector<goDouble>& x, const goVector<goDouble>& y, goMatrix<goDouble>& ret)
{
    goSize_t M = x.getSize();
    goSize_t N = y.getSize();

    if (ret.getRows() != M || ret.getColumns() != N)
    {
        ret.resize (M,N);
        ret.fill (0.0);
    }

    cblas_dger (CblasRowMajor, M, N, alpha, x.getPtr(), x.getStride(), y.getPtr(), y.getStride(), ret.getPtr(), ret.getLeadingDimension());
}

template <class T>
void goVectorOuter (T alpha, const goVector<T>& x, const goVector<T>& y, goMatrix<T>& ret)
{
    goSize_t M = x.getSize();
    goSize_t N = y.getSize();

    if (ret.getRows() != M || ret.getColumns() != N)
    {
        ret.resize (M,N);
    }

    T tempy;
    goIndex_t stridex = x.getStride();
    for (goSize_t i = 0; i < N; ++i)
    {
        tempy = y[i];
        const T* array = x.getPtr();
        //= NOTE: To account for complex numbers, we run through the whole matrix.
        //= --> FIXME!
        for (goSize_t j = 0; j < M; ++j, array += stridex)
        {
            ret(j,i) += alpha * tempy * *array;
        }
    }
}

template class goVector<goFloat>;
template class goVector<goDouble>;
template class goVector<goComplexf>;
// template class goVector<goComplexd>;
template class goVector<goIndex_t>;
template class goVector<goSize_t>;
