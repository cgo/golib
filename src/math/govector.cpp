#include <govector.h>
#include <gomatrix.h>
#include <golog.h>
#include <gotypes.h>
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif
#if 1
extern "C"
{
#include <cblas.h>
}

template <class T>
goVector<T>::goVector () 
    : goFixedArray<T> (1,0,0) 
{
    this->fill (T(0));
}

template <class T>
goVector<T>::goVector (goSize_t s, goIndex_t leftBorder, goIndex_t rightBorder) 
    : goFixedArray<T> (s,leftBorder,rightBorder) 
{
    this->fill (T(0));
}

template <class T>
goVector<T>::~goVector ()
{
}

template <class T>
goVector<T>::goVector (const goFixedArray<T>& other)
{
    *this = other;
}

template<>
goVector<goFloat>& goVector<goFloat>::operator= (const goFixedArray<goFloat>& other)
{
    goSize_t N = other.getSize();
    if (this->getSize() != N)
    {
        this->resize (N);
    }
    cblas_scopy (N, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template<>
goVector<goDouble>& goVector<goDouble>::operator= (const goFixedArray<goDouble>& other)
{
    goSize_t N = other.getSize();
    if (this->getSize() != N)
    {
        this->resize (N);
    }
    cblas_dcopy (N, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator= (const goFixedArray<T>& other)
{
    if (other.getStride() != 1)
    {
        goFixedArray<T>::operator= (other);
        return *this;
    }
    if (this->getSize() == other.getSize() && this->getStride() == 1)
    {
        memcpy (this->getPtr(), other.getPtr(), this->getSize() * sizeof(T));
        return *this;
    }
    this->resize (other.getSize());
    memcpy (this->getPtr(), other.getPtr(), this->getSize() * sizeof(T));
    return *this;
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
void goVector<goFloat>::print (const char* formatstring) const
{
    goSize_t sz = this->getSize();

    for (goSize_t i = 0; i < sz; ++i)
    {
        printf (formatstring, (*this)[i]);
    }
    printf ("\n");
}

template<>
void goVector<goDouble>::print (const char* formatstring) const
{
    goSize_t sz = this->getSize();

    for (goSize_t i = 0; i < sz; ++i)
    {
        printf (formatstring, (*this)[i]);
    }
    printf ("\n");
}

template <class T>
void goVector<T>::print (const char*) const
{
    printf ("goVector<T>::_print() not defined for this type.\n");
}

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
    goSize_t sz = this->getSize();
    for (T v = start; v < end && i < sz; v += step, ++i)
    {
        (*this)[i] = v;
    }
}

template <class T>
bool goVector<T>::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f)
        return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}

template <class T>
bool goVector<T>::readASCII (FILE* file)
{
    if (!file)
        return false;

    goString line = "";
    goFileIO::readASCIILine (file, line);
    if (line != "goVector")
    {
        goString s = "goVector::readASCII(): expected goVector, got ";
        s += line;
        goLog::warning (s);
        return false;
    }
    line = "";
    goFileIO::readASCIILine (file, line);
    goList<goString> words;
    line.getWords (words);
    if (words.getSize() != 2 || words.getFront() != "size")
    {
        goString s = "goVector::readASCII(): expected size, got ";
        s += line;
        goLog::warning (s);
        return false;
    }
    goSize_t sz = 0;
    sz = words(1)->elem.toInt();
    this->resize (sz);
    goSize_t i = 0;
    while (!::feof(file) && i < sz)
    {
        line = "";
        goFileIO::readASCIILine (file, line);
        (*this)[i] = T(line.toDouble());
        ++i;
    }
    return true;
}

template <class T>
bool goVector<T>::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f)
        return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

template <class T>
bool goVector<T>::writeASCII (FILE* file) const
{
    if (!file)
    {
        return false;
    }
    goFileIO::writeASCII (file, "goVector\n");
    goString s = "size ";
    s += (int)this->getSize();
    s += "\n";
    goFileIO::writeASCII (file, s);
    goSize_t sz = this->getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        fprintf (file, "%.20lf\n", (double)(*this)[i]);
        //s = "";
        //s += (double)(*this)[i];
        //s += "\n";
        //goFileIO::writeASCII (file, s);
    }
    return true;
}

template <>
bool goVector<goComplexf>::writeASCII (FILE*) const
{
    goLog::error ("goVector::writeASCII() not implemented for goComplexf");
    return false;
}

template <>
bool goVector<goComplexf>::readASCII (FILE*)
{
    goLog::error ("goVector::readASCII() not implemented for goComplexf");
    return false;
}

template <>
goComplexf goVector<goComplexf>::min () const
{
    goLog::error ("goVector::min() not implemented for goComplexf");
    return false;
}

template <>
goComplexf goVector<goComplexf>::max () const
{
    goLog::error ("goVector::max() not implemented for goComplexf");
    return false;
}

template <class T>
T goVector<T>::min () const
{
    return goMath::min<T> (*this);
}

template <class T>
T goVector<T>::max () const
{
    return goMath::max<T> (*this);
}

template <class T>
goVector<T>& goVector<T>::operator/= (T s)
{
    return this->operator*= (T(1)/s);
}

template <>
goVector<goFloat>& goVector<goFloat>::operator*= (goFloat n)
{
    cblas_sscal (this->getSize(), n, this->getPtr(), this->getStride());
    return *this;
}

template <>
goVector<goDouble>& goVector<goDouble>::operator*= (goDouble n)
{
    cblas_dscal (this->getSize(), n, this->getPtr(), this->getStride());
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator*= (T n)
{
    goIndex_t max = this->getSize();
    T* array = this->getPtr();
    goIndex_t stride = this->getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *array = static_cast<T>(*array * n);
        // *array *= n;
        array += stride;
    }
    return *this;
}

template <>
goVector<goFloat>& goVector<goFloat>::operator-= (const goVector<goFloat>& other)
{
    cblas_saxpy (this->getSize(), -1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template <>
goVector<goDouble>& goVector<goDouble>::operator-= (const goVector<goDouble>& other)
{
    cblas_daxpy (this->getSize(), -1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator-= (const goVector<T>& other)
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goIndex_t max = this->getSize();
    T* array = this->getPtr();
    const T* otherArray = other.getPtr();
    goIndex_t stride = this->getStride();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *array -= *otherArray;
        array += stride;
        otherArray += otherStride;
    }
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator-= (T s)
{
    goIndex_t max = this->getSize();
    T* array = this->getPtr();
    goIndex_t stride = this->getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *array -= s;
        array += stride;
    }
    return *this;
};

template <>
goVector<goFloat>& goVector<goFloat>::operator+= (const goVector<goFloat>& other)
{
    cblas_saxpy (this->getSize(), 1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template <>
goVector<goDouble>& goVector<goDouble>::operator+= (const goVector<goDouble>& other)
{
    cblas_daxpy (this->getSize(), 1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator+= (const goVector<T>& other)
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goIndex_t max = this->getSize();
    T* array = this->getPtr();
    const T* otherArray = other.getPtr();
    goIndex_t stride = this->getStride();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *array += *otherArray;
        array += stride;
        otherArray += otherStride;
    }
    return *this;
}

template <class T>
goVector<T>& goVector<T>::operator+= (T s)
{
    goIndex_t max = this->getSize();
    T* array = this->getPtr();
    goIndex_t stride = this->getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *array += s;
        array += stride;
    }
    return *this;
}

template<>
goVector<goFloat> goVector<goFloat>::operator- (const goVector<goFloat>& other) const
{
    goVector<goFloat> ret (*this);
    ret -= other;
    return ret;
}

template<>
goVector<goDouble> goVector<goDouble>::operator- (const goVector<goDouble>& other) const
{
    goVector<goDouble> ret (*this);
    ret -= other;
    return ret;
}

template<>
goVector<goFloat> goVector<goFloat>::operator+ (const goVector<goFloat>& other) const
{
    goVector<goFloat> ret (*this);
    ret += other;
    return ret;
}

template<>
goVector<goDouble> goVector<goDouble>::operator+ (const goVector<goDouble>& other) const
{
    goVector<goDouble> ret (*this);
    ret += other;
    return ret;
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
    const T* otherArray = other.getPtr();
    goIndex_t stride = this->getStride();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *retArray = *array - *otherArray;
        array += stride;
        otherArray += otherStride;
        ++retArray;
    }
    return ret;
}

template<class T>
goVector<T> goVector<T>::operator+ (const goVector<T>& other) const
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
    const T* otherArray = other.getPtr();
    goIndex_t stride = this->getStride();
    goIndex_t otherStride = other.getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *retArray = *array + *otherArray;
        array += stride;
        otherArray += otherStride;
        ++retArray;
    }
    return ret;
}

template<class T>
goVector<T> goVector<T>::operator* (T n) const
{
    goVector<T> ret (*this);
    ret *= n;
    return ret;
}

template<class T>
goVector<T> goVector<T>::operator/ (T n) const
{
    goVector<T> ret (*this);
    ret *= T(1)/n;
    return ret;
}



#if 0
template<class T>
goVector<T> goVector<T>::operator* (T n) const
{
    goVector<T> ret (this->getSize());
    goIndex_t max = this->getSize();
    const T* array = this->getPtr();
    T* retArray = ret.getPtr();
    goIndex_t stride = this->getStride();
    for (goIndex_t i = 0; i < max; ++i)
    {
        *retArray = static_cast<T>(*array * n);
        array += stride;
        ++retArray;
    }
    return ret;
}
#endif


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
