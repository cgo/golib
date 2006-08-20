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
template <>
goVector<goComplexd>& goVector<goComplexd>::operator*= (const goMatrix<goComplexd>&)
{
    goLog::warning ("goVector::operator*= (goMatrix): Not implemented for complex types.");
    return *this;
}

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

template class goVector<goFloat>;
template class goVector<goDouble>;
template class goVector<goComplexf>;
template class goVector<goComplexd>;
template class goVector<goIndex_t>;
template class goVector<goSize_t>;
