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
        goVector (T* ptr, goSize_t size, goIndex_t stride = 1) : goFixedArray<T> (ptr, size, stride) {};
        goVector (const goFixedArray<T>& o);
        // goVector (const goFixedArray<T>& o) : goFixedArray<T> (o) {};
        // template <class To> goVector (const goFixedArray<To>& o) : goFixedArray<T> (o) {};
        virtual ~goVector ();

        goVector<T>& operator= (const goFixedArray<T>& other);

        void resize (goSize_t s)
        {
            this->setSize (s,this->getLeftBorder(),this->getRightBorder());
        }

        void print (const char* formatstring = "%f\n") const;

#if 0
        template <class To>
        goVector<T> operator- (const goVector<To>& other) const
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
                *retArray = *array - *otherArray;
                array += stride;
                otherArray += otherStride;
                ++retArray;
            }
            return ret;
        };

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
#endif
        // goVector<T> operator* (T scalar) const;

#if 0
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
#endif

        /**
         * @brief Quite inefficient vector-matrix multiplication.
         * 
         * If possible, avoid this or use only matrices in your programs
         * if you can.
         *
         * @TODO This is inefficient, but may be frequently needed. Solve this issue!
         *
         * @return this * M
         */
        goVector<T> operator* (const goMatrix<T>& M) const
        {
            const goMatrix<T> this_M (0,0);
            this_M.setData (&(*this)[0], this->getSize(), 1, this->getStride());
            goVector<T> ret;
            goMatrix<T> temp;
            temp = this_M * M;
            ret.resize (temp.getRows());
            temp.copyColumn (0, ret);
            return ret;
        };

#if 0
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
#endif
        goVector<T> cross (const goVector<T>& other) const
        {
            goVector<T> v(3);
            this->cross (other, v);
            return v;
        };

        bool cross (const goVector<T>& other, goVector<T>& ret) const
        {
            if (other.getSize() != 3 || this->getSize() != 3)
            {
                goLog::warning ("goVector::cross(): vectors must be size 3.");
                return false;
            }
            if (ret.getSize() < 3)
            {
                ret.resize(3);
            }

            T x = (*this)[0];
            T y = (*this)[1];
            T z = (*this)[2];
            T ox = other[0];
            T oy = other[1];
            T oz = other[2];
            ret[0] = y * oz - z * oy;
            ret[1] = z * ox - oz * x;
            ret[2] = x * oy - ox * y;
            return true;
        };

        /**
         * @brief Inner product.
         *
         * @note Uses CBLAS.
         *
         * @return The inner product this * other.
         **/
        T operator* (const goVector<T>& other) const;
        goVector<T>& operator*= (T scalar);
        goVector<T>& operator/= (T scalar);
        goVector<T>& operator-= (T s);
        goVector<T>& operator+= (T s);
        goVector<T>& operator+= (const goVector<T>& other);
        goVector<T>& operator-= (const goVector<T>& other);
        goVector<T> operator- (const goVector<T>& other) const;
        goVector<T> operator+ (const goVector<T>& other) const;
        goVector<T> operator* (T scalar) const;
        goVector<T> operator/ (T scalar) const;
#if 0
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

        goVector<T>& operator-= (T s)
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

        goVector<T>& operator+= (T s)
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
        };
#endif

        //= Element-wise (Hadamard) multiplication.
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
        //= Element-wise division.
        template <class To>
        goVector<T>& operator/= (const goVector<To>& other)
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
                *array /= *otherArray;
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

        void outerProduct (const goVector<T>& other, goMatrix<T>& ret) const;

        template <class To>
        bool copy (goVector<To>& target, goIndex_t startIndex = 0, goIndex_t skip = 0, goIndex_t lastIndex = -1) const
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
        bool copy (To* target, goIndex_t startIndex = 0, goIndex_t skip = 0, goIndex_t lastIndex = -1) const
        {
            assert (this->getSize() > (goSize_t)startIndex);
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

        T square () const;
        inline T conjInnerProduct (const goVector<T>&) const;

        /**
         * @brief Deprecated. Calls norm2() for scalar real types,
         * special implementation for goComplex types.
         *
         * Use norm2() instead.
         */
        T abs () const;

        /**
         * @brief 2-Norm of this vector.
         *
         * Uses cblas_<>nrm2().
         * Calls special abs() implementation for complex.
         * 
         * @todo Use cblas for complex as well. Use cblas internal cblas storage then.
         * @return 2-norm of this vector.
         */
        T norm2 () const;
        /**
         * @brief 1-Norm of this vector: The sum of absolute values.
         *
         * Uses cblas_<>asum().
         * Not implemented for goComplex types.
         * 
         * @todo Use cblas for complex as well. Use cblas internal cblas storage then.
         * @return 2-norm of this vector.
         */
        T norm1 () const;

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

        T min () const;
        T max () const;

        void fillRange (const T& start, const T& step, const T& end);

        bool readASCII  (const char* filename);
        bool readASCII  (FILE* file);
        bool writeASCII (const char* filename) const;
        bool writeASCII (FILE* file) const;
};


// inline goComplexf goVector<goComplexf>::square () const;


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


template <class T>
inline T goVector<T>::conjInnerProduct (const goVector<T>& v) const
{
    return *this * v;
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

/**
 * @brief y = alpha * x + y
 *
 * @note Implemented for goDouble and goFloat
 *
 * Uses cblas_<>axpy functions.
 * Sizes of x and y must match and are checked for.
 * 
 * @param alpha Scalar factor
 * @param x     Vector x
 * @param y     Vector y
 *
 * @return True if successful, false otherwise.
 */
template <class T>
bool goVectorAdd (T alpha, const goVector<T>& x, goVector<T>& y);


/** 
 * @brief Outer vector product \f$ A = A + \alpha \cdot x \cdot y^\top \f$
 * 
 * @note Implemented for goDouble and goFloat using cblas_<>ger(). Other types 
 * are directly implemented and therefore slower in most implementations.
 * 
 * @param alpha Scalar factor
 * @param x     Vector x of size M
 * @param y     Vector y of size N
 * @param ret   Return matrix A, of size MxN. Resized and initialised to 0 if it does not have the right size.
 */
template <class T>
void goVectorOuter (T alpha, const goVector<T>& x, const goVector<T>& y, goMatrix<T>& ret);
/** @} */

typedef goVector<goFloat>  goVectorf;
typedef goVector<goDouble> goVectord;
typedef goVector<goInt32>  goVectori;

#endif
