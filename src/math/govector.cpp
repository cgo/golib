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
goMath::Vector<T>::Vector () 
    : goFixedArray<T> (1,0,0) 
{
    this->fill (T(0));
}

template <class T>
goMath::Vector<T>::Vector (goSize_t s, goIndex_t leftBorder, goIndex_t rightBorder) 
    : goFixedArray<T> (s,leftBorder,rightBorder) 
{
    this->fill (T(0));
}

template <class T>
goMath::Vector<T>::~Vector ()
{
}

template <class T>
goMath::Vector<T>::Vector (const goFixedArray<T>& other)
{
    *this = other;
}

namespace goMath {
    template<>
        goMath::Vector<goFloat>& Vector<goFloat>::operator= (const goFixedArray<goFloat>& other)
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
        goMath::Vector<goDouble>& Vector<goDouble>::operator= (const goFixedArray<goDouble>& other)
        {
            goSize_t N = other.getSize();
            if (this->getSize() != N)
            {
                this->resize (N);
            }
            cblas_dcopy (N, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
            return *this;
        }
}

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator= (const goFixedArray<T>& other)
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
goMath::Vector<T> goMath::Vector<T>::operator- (const goMath::Vector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goMath::Vector<T> ret (this->getSize());
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

namespace goMath {
    template<>
        void Vector<goFloat>::print (const char* formatstring) const
        {
            goSize_t sz = this->getSize();

            for (goSize_t i = 0; i < sz; ++i)
            {
                printf (formatstring, (*this)[i]);
            }
            printf ("\n");
        }

    template<>
        void Vector<goDouble>::print (const char* formatstring) const
        {
            goSize_t sz = this->getSize();

            for (goSize_t i = 0; i < sz; ++i)
            {
                printf (formatstring, (*this)[i]);
            }
            printf ("\n");
        }
}

template <class T>
void goMath::Vector<T>::print (const char*) const
{
    printf ("goMath::Vector<T>::_print() not defined for this type.\n");
}

namespace goMath {
    template<> 
        goFloat Vector<goFloat>::operator* (const goMath::Vector<goFloat>& other) const
        {
            assert (other.getSize() == this->getSize());
            return cblas_sdot (this->getSize(), 
                    this->getPtr(), this->getStride(),
                    other.getPtr(),
                    other.getStride());
        }

    template<> 
        goDouble Vector<goDouble>::operator* (const goMath::Vector<goDouble>& other) const
        {
            assert (other.getSize() == this->getSize());
            return cblas_ddot (this->getSize(), 
                    this->getPtr(), this->getStride(),
                    other.getPtr(),
                    other.getStride());
        }

    template <>
        goMath::Vector<goComplexf>& goMath::Vector<goComplexf>::operator*= (const goMath::Matrix<goComplexf>&)
        {
            goLog::warning ("goMath::Vector::operator*= (goMath::Matrix): Not implemented for complex types.");
            return *this;
        }
}

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator*= (const goMath::Matrix<T>& m)
{
    if (m.getColumns() != this->getSize())
    {
        goLog::warning ("goMath::Vector::operator*= (goMath::Matrix): Matrix has wrong column count.");
        return *this;
    }
    *this = m * *this;
    return *this;
}


//template <>
//goMath::Vector<goComplexd>& goMath::Vector<goComplexd>::operator*= (const goMath::Matrix<goComplexd>&)
//{
//    goLog::warning ("goMath::Vector::operator*= (goMath::Matrix): Not implemented for complex types.");
//    return *this;
//}

/**
 * @brief Outer product.
 * @note Untested.
 * @return 
 **/
template <class T>
void goMath::Vector<T>::outerProduct (const goMath::Vector<T>& other, goMath::Matrix<T>& ret) const
{
    goMath::vectorOuter<T> (T(1), *this, other, ret);
};

template <class T>
T goMath::Vector<T>::operator* (const goMath::Vector<T>& other) const
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

namespace goMath {
    /**
     * @brief Calculates conj(this) * this.
     *
     * @return The return value is of the form r+i*0, meaning the imaginary part is 0.
     **/
    template<> goComplexf Vector<goComplexf>::square () const
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
}

template <class T>
inline T goMath::Vector<T>::square () const
{
    return *this * *this;
}

template<class T> T goMath::Vector<T>::abs () const
{
    return this->norm2 ();
}

namespace goMath {
    template<> inline goComplexf Vector<goComplexf>::abs () const
    {
        return goComplexf(sqrt(this->square().re()), 0.0f);
    }

    template<> inline goComplexd Vector<goComplexd>::abs () const
    {
        return goComplexd(sqrt(this->square().re()), 0.0);
    }


    template<> goFloat Vector<goFloat>::norm2 () const
    {
        return cblas_snrm2 (this->getSize(), this->getPtr(), this->getStride());
    }

    template<> goDouble Vector<goDouble>::norm2 () const
    {
        return cblas_dnrm2 (this->getSize(), this->getPtr(), this->getStride());
    }

    template<> goFloat Vector<goFloat>::norm1 () const
    {
        return cblas_sasum (this->getSize(), this->getPtr(), this->getStride());
    }

    template<> goDouble Vector<goDouble>::norm1 () const
    {
        return cblas_dasum (this->getSize(), this->getPtr(), this->getStride());
    }

    template<> goComplexf Vector<goComplexf>::norm2 () const
    {
        return this->abs();
    }

    template<> goComplexd Vector<goComplexd>::norm2 () const
    {
        return this->abs();
    }

    template<> goComplexf Vector<goComplexf>::norm1 () const
    {
        goLog::error ("goMath::Vector::norm1() not implemented for complex types.");
        return goComplexf(0.0f,0.0f);
    }

    template<> goComplexd Vector<goComplexd>::norm1 () const
    {
        goLog::error ("goMath::Vector::norm1() not implemented for complex types.");
        return goComplexd(0.0,0.0);
    }
}

template<class T> T goMath::Vector<T>::norm2 () const
{
    return static_cast<T>(sqrt(static_cast<goDouble>(*this * *this)));
}

template<class T> T goMath::Vector<T>::norm1 () const
{
    goLog::error ("goMath::Vector::norm1() not implemented for this type (integer?).");
    return T(0);
}

template <class T>
void goMath::Vector<T>::fillRange (const T& start, const T& step, const T& end)
{
    goSize_t i = 0;
    goSize_t sz = this->getSize();
    for (T v = start; v < end && i < sz; v += step, ++i)
    {
        (*this)[i] = v;
    }
}

template <class T>
bool goMath::Vector<T>::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f)
        return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}

template <class T>
bool goMath::Vector<T>::readASCII (FILE* file)
{
    if (!file)
        return false;

    goString line = "";
    goFileIO::readASCIILine (file, line);
    if (line != "goMath::Vector")
    {
        goString s = "goMath::Vector::readASCII(): expected goMath::Vector, got ";
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
        goString s = "goMath::Vector::readASCII(): expected size, got ";
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
bool goMath::Vector<T>::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f)
        return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

template <class T>
bool goMath::Vector<T>::writeASCII (FILE* file) const
{
    if (!file)
    {
        return false;
    }
    goFileIO::writeASCII (file, "goMath::Vector\n");
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

namespace goMath {
    template <>
        bool Vector<goComplexf>::writeASCII (FILE*) const
        {
            goLog::error ("goMath::Vector::writeASCII() not implemented for goComplexf");
            return false;
        }

    template <>
        bool Vector<goComplexf>::readASCII (FILE*)
        {
            goLog::error ("goMath::Vector::readASCII() not implemented for goComplexf");
            return false;
        }

    template <>
        goComplexf Vector<goComplexf>::min () const
        {
            goLog::error ("goMath::Vector::min() not implemented for goComplexf");
            return false;
        }

    template <>
        goComplexf Vector<goComplexf>::max () const
        {
            goLog::error ("goMath::Vector::max() not implemented for goComplexf");
            return false;
        }

    template <>
        goMath::Vector<goFloat>& Vector<goFloat>::operator*= (goFloat n)
        {
            cblas_sscal (this->getSize(), n, this->getPtr(), this->getStride());
            return *this;
        }

    template <>
        goMath::Vector<goDouble>& Vector<goDouble>::operator*= (goDouble n)
        {
            cblas_dscal (this->getSize(), n, this->getPtr(), this->getStride());
            return *this;
        }

    template <>
        goMath::Vector<goFloat>& goMath::Vector<goFloat>::operator-= (const goMath::Vector<goFloat>& other)
        {
            cblas_saxpy (this->getSize(), -1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
            return *this;
        }

    template <>
        goMath::Vector<goDouble>& goMath::Vector<goDouble>::operator-= (const goMath::Vector<goDouble>& other)
        {
            cblas_daxpy (this->getSize(), -1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
            return *this;
        }

    template <>
        goMath::Vector<goFloat>& Vector<goFloat>::operator+= (const goMath::Vector<goFloat>& other)
        {
            cblas_saxpy (this->getSize(), 1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
            return *this;
        }

    template <>
        goMath::Vector<goDouble>& Vector<goDouble>::operator+= (const goMath::Vector<goDouble>& other)
        {
            cblas_daxpy (this->getSize(), 1.0f, other.getPtr(), other.getStride(), this->getPtr(), this->getStride());
            return *this;
        }
}

template <class T>
T goMath::Vector<T>::min () const
{
    return goMath::min<T> (*this);
}

template <class T>
T goMath::Vector<T>::max () const
{
    return goMath::max<T> (*this);
}

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator/= (T s)
{
    return this->operator*= (T(1)/s);
}

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator*= (T n)
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

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator-= (const goMath::Vector<T>& other)
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
goMath::Vector<T>& goMath::Vector<T>::operator-= (T s)
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

template <class T>
goMath::Vector<T>& goMath::Vector<T>::operator+= (const goMath::Vector<T>& other)
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
goMath::Vector<T>& goMath::Vector<T>::operator+= (T s)
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

namespace goMath {
    template<>
        goMath::Vector<goFloat> Vector<goFloat>::operator- (const goMath::Vector<goFloat>& other) const
        {
            goMath::Vector<goFloat> ret (*this);
            ret -= other;
            return ret;
        }

    template<>
        goMath::Vector<goDouble> Vector<goDouble>::operator- (const goMath::Vector<goDouble>& other) const
        {
            goMath::Vector<goDouble> ret (*this);
            ret -= other;
            return ret;
        }

    template<>
        goMath::Vector<goFloat> Vector<goFloat>::operator+ (const goMath::Vector<goFloat>& other) const
        {
            goMath::Vector<goFloat> ret (*this);
            ret += other;
            return ret;
        }

    template<>
        goMath::Vector<goDouble> Vector<goDouble>::operator+ (const goMath::Vector<goDouble>& other) const
        {
            goMath::Vector<goDouble> ret (*this);
            ret += other;
            return ret;
        }
}

template <class T>
goMath::Vector<T> goMath::Vector<T>::operator- (const goMath::Vector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goMath::Vector<T> ret (this->getSize());
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
goMath::Vector<T> goMath::Vector<T>::operator+ (const goMath::Vector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    goMath::Vector<T> ret (this->getSize());
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
goMath::Vector<T> goMath::Vector<T>::operator* (T n) const
{
    goMath::Vector<T> ret (*this);
    ret *= n;
    return ret;
}

template<class T>
goMath::Vector<T> goMath::Vector<T>::operator/ (T n) const
{
    goMath::Vector<T> ret (*this);
    ret *= T(1)/n;
    return ret;
}



#if 0
template<class T>
goMath::Vector<T> goMath::Vector<T>::operator* (T n) const
{
    goMath::Vector<T> ret (this->getSize());
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

namespace goMath {
    template <>
        bool vectorAdd<goFloat> (goFloat alpha, const goMath::Vector<goFloat>& x, goMath::Vector<goFloat>& y)
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
        bool vectorAdd<goDouble> (goDouble alpha, const goMath::Vector<goDouble>& x, goMath::Vector<goDouble>& y)
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
        void vectorOuter<goFloat> (goFloat alpha, const goMath::Vector<goFloat>& x, const goMath::Vector<goFloat>& y, goMath::Matrix<goFloat>& ret)
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
        void vectorOuter<goDouble> (goDouble alpha, const goMath::Vector<goDouble>& x, const goMath::Vector<goDouble>& y, goMath::Matrix<goDouble>& ret)
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
}

template <class T>
void goMath::vectorOuter (T alpha, const goMath::Vector<T>& x, const goMath::Vector<T>& y, goMath::Matrix<T>& ret)
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


template class goMath::Vector<goFloat>;
template class goMath::Vector<goDouble>;
template class goMath::Vector<goComplexf>;
// template class goMath::Vector<goComplexd>;
template class goMath::Vector<goIndex_t>;
template class goMath::Vector<goSize_t>;
