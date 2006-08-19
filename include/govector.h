#ifndef GOVECTOR_H
#define GOVECTOR_H

#include <goconfig.h>
#include <goexception.h>
#include <gofixedarray.h>
#ifndef GOLOG_H
# include <golog.h>
#endif
#ifndef GOCOMPLEX_H
# include <gocomplex.h>
#endif
extern "C"
{
#include <cblas.h>
}


template <class T> class goMatrix;

/**
 * @addtogroup math
 * @{
 */
/** 
 * @brief General vector class.
 *
 * Use this instead of goNVector, which should be deprecated soon.
 * This may not be optimal yet, but it will be improved when I have
 * more need for the class.
 *
 * @todo Document members.
 * 
 * @author Christian Gosch
 */
template<class T>
class goVector : public goFixedArray<T>
{
    public:
        goVector ();
        goVector (goSize_t s, goIndex_t leftBorder = 0, goIndex_t rightBorder = 0);
        goVector (const goFixedArray<T>& o) : goFixedArray<T> (o) {};
        // template <class To> goVector (const goFixedArray<To>& o) : goFixedArray<T> (o) {};
        virtual ~goVector ();

        void resize (goSize_t s)
        {
            this->setSize (s,this->getLeftBorder(),this->getRightBorder());
        }
        
        // template <class To>
        goVector<T> operator- (const goVector<T>& other) const;

        template <class To>
        goVector<T> operator+ (const goVector<To>& other) const
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
            const To* otherArray = other.getPtr();
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
        };

        /**
         * @brief Inner product.
         *
         * @note Uses CBLAS.
         *
         * @return The inner product this * other.
         **/
        T operator* (const goVector<T>& other) const;

        // template <class ScalarType>
        goVector<T> operator* (goFloat n) const
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
        };
        goVector<T> operator* (goDouble n) const
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
        };

        goVector<T>& operator*= (goFloat n)
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
        };
        goVector<T>& operator*= (goDouble n)
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
        };

        template <class To>
        goVector<T>& operator-= (const goVector<To>& other)
        {
        #ifdef GO_USE_EXCEPTIONS
            if (this->getSize() != other.getSize())
            {
                throw goMathException (goMathException::SIZE_MISMATCH);
            }
        #endif
            goIndex_t max = this->getSize();
            T* array = this->getPtr();
            const To* otherArray = other.getPtr();
            goIndex_t stride = this->getStride();
            goIndex_t otherStride = other.getStride();
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array -= *otherArray;
                array += stride;
                otherArray += otherStride;
            }
            return *this;
        };

        template <class To>
        goVector<T>& operator+= (const goVector<To>& other)
        {
        #ifdef GO_USE_EXCEPTIONS
            if (this->getSize() != other.getSize())
            {
                throw goMathException (goMathException::SIZE_MISMATCH);
            }
        #endif
            goIndex_t max = this->getSize();
            T* array = this->getPtr();
            const To* otherArray = other.getPtr();
            goIndex_t stride = this->getStride();
            goIndex_t otherStride = other.getStride();
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array += *otherArray;
                array += stride;
                otherArray += otherStride;
            }
            return *this;
        };

        //= Element-wise multiplication.
        template <class To>
        goVector<T>& operator*= (const goVector<To>& other)
        {
        #ifdef GO_USE_EXCEPTIONS
            if (this->getSize() != other.getSize())
            {
                throw goMathException (goMathException::SIZE_MISMATCH);
            }
        #endif
            goIndex_t max = this->getSize();
            T* array = this->getPtr();
            const To* otherArray = other.getPtr();
            goIndex_t stride = this->getStride();
            goIndex_t otherStride = other.getStride();
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array *= *otherArray;
                array += stride;
                otherArray += otherStride;
            }
            return *this;
        };


        /** 
         * @brief this = m * this
         * 
         * @param m Matrix
         * 
         * @return this = m * this
         */
        goVector<T>& operator*= (const goMatrix<T>& m);

        /**
         * @brief Outer product.
         * @note Untested.
         * @return 
         **/
        template <class To>
        bool outerProduct (const goVector<To>& other, goMatrix<T>& ret) const
        {
            if (this->getSize() != other.getSize())
                return false;
            goIndex_t sz = (goIndex_t)this->getSize();
            if (!ret.resize (sz, sz))
                return false;
            T temp;
            To tempi;
            goIndex_t stride = this->getStride();
            for (goIndex_t i = 0; i < sz; ++i)
            {
                tempi = other[i];
                const T* array = this->getPtr();
                //= NOTE: To account for complex numbers, we run through the whole matrix.
                //= --> FIXME!
                for (goIndex_t j = 0; j < sz; ++j, array += stride)
                {
                    temp = tempi * *array;
                    ret(j,i) = temp;
                }
            }
            return true;
        };

        template <class To>
        bool copy (goVector<To>& target, goIndex_t startIndex, goIndex_t skip, goIndex_t lastIndex = -1) const
        {
            assert (static_cast<goIndex_t>(this->getSize()) > startIndex);
            assert (skip >= 0);
            if (lastIndex < 0)
            {
                lastIndex = static_cast<goIndex_t>(this->getSize() - 1);
            }
            goSize_t sz = (lastIndex + 1 - startIndex + skip) / (skip + 1);
            if (target.getSize() < sz)
            {
                target.setSize(sz);
            }
            goIndex_t i = 0;
            goIndex_t szi = static_cast<goIndex_t>(sz);
            goIndex_t ithis = startIndex;
            for (i = 0; i < szi; ++i)
            {
                target[i] = (*this)[ithis];
                ithis += skip + 1;
            }
            return true;
        };

        template <class To>
        bool copy (To* target, goIndex_t startIndex, goIndex_t skip, goIndex_t lastIndex = -1) const
        {
            assert (this->getSize() > startIndex);
            assert (skip >= 0);
            if (lastIndex < 0)
            {
                lastIndex = static_cast<goIndex_t>(this->getSize() - 1);
            }
            goSize_t sz = (lastIndex + 1 - startIndex + skip) / (skip + 1);
            goIndex_t i = 0;
            goIndex_t szi = static_cast<goIndex_t>(sz);
            goIndex_t ithis = startIndex;
            for (i = 0; i < szi; ++i)
            {
                target[i] = (*this)[ithis];
                ithis += skip + 1;
            }
            return true;
        };


        template <class To, class To2>
        bool cat (const goVector<To>& other, goVector<To2>& target) const
        {
            goSize_t sz1 = this->getSize();
            goSize_t sz2 = other.getSize();
            goSize_t targetSize = sz1+sz2;
            if (target.getSize() != targetSize)
            {
                target.setSize(targetSize);
            }
            goSize_t i = 0;
            goSize_t j;
            for (j = 0; j < sz1; ++j,++i)
            {
                target[i] = (*this)[j];
            }
            for (j = 0; j < sz2; ++j,++i)
            {
                target[i] = other[j];
            }
            return true;
        };

        inline T square () const;
        inline T conjInnerProduct (const goVector<T>&) const;

        inline T abs () const;

        T sum (goIndex_t start = 0, goIndex_t end = -1) const
        {
            if (end == -1)
            {
                end = this->getSize() - 1;
            }
            goIndex_t i;
            T retValue = T(0);
            for (i = start; i <= end; ++i)
            {
                retValue += (*this)[i];
            }
            return retValue;
        };
};


// inline goComplexf goVector<goComplexf>::square () const;

template<class T> inline T goVector<T>::abs () const
{
    return static_cast<T>(sqrt(this->square()));
};


/**
 * @brief Calculates conj(this) * this.
 *
 * @return The return value is of the form r+i*0, meaning the imaginary part is 0.
 **/
template<> inline goComplexf goVector<goComplexf>::square () const
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

template<> inline goComplexf goVector<goComplexf>::conjInnerProduct (const goVector<goComplexf>& v) const
{
    assert (this->getSize() == v.getSize());
    const goComplexf* array = this->getPtr();
    const goComplexf* other = v.getPtr();
    goComplexf sum (0,0);
    goIndex_t size = this->getSize();
    for (goIndex_t i = 0; i < size; ++i, ++array, ++other)
    {
        sum += array->conj() * *other;
    }
    return sum;
}

template<> inline goComplexf goVector<goComplexf>::abs () const
{
    return goComplexf(sqrt(this->square().re()), 0.0f);
};

template<> inline goComplexd goVector<goComplexd>::abs () const
{
    return goComplexd(sqrt(this->square().re()), 0.0);
};


template <class T>
inline T goVector<T>::conjInnerProduct (const goVector<T>& v) const
{
    return *this * v;
}

template <class T>
inline T goVector<T>::square () const
{
    return *this * *this;
}

#if 0
extern "C"
{
#include <cblas.h>
}

template <class T>
inline goVector<T> goVector<T>::operator- (const goVector<T>& other) const
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
inline goFloat goVector<goFloat>::operator* (const goVector<goFloat>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_sdot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template<> 
inline goDouble goVector<goDouble>::operator* (const goVector<goDouble>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_ddot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template <class T>
inline goVector<T>& goVector<T>::operator*= (const goMatrix<T>& m)
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
inline goVector<goComplexf>& goVector<goComplexf>::operator*= (const goMatrix<goComplexf>&)
{
    goLog::warning ("goVector::operator*= (goMatrix): Not implemented for complex types.");
    return *this;
}
template <>
inline goVector<goComplexd>& goVector<goComplexd>::operator*= (const goMatrix<goComplexd>&)
{
    goLog::warning ("goVector::operator*= (goMatrix): Not implemented for complex types.");
    return *this;
}

template <class T>
inline T goVector<T>::operator* (const goVector<T>& other) const
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

/** @} */

typedef goVector<goFloat>  goVectorf;
typedef goVector<goDouble> goVectord;

#endif
