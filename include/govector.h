/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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


namespace goMath {

    template <class T> class Matrix;

/**
 * @addtogroup mathla
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
class Vector : public goFixedArray<T>
{
    public:
        typedef T value_type;
    public:
        Vector ();
        explicit Vector (goSize_t s, goSize_t reserve = 0, goSize_t resize_overhead = 0);
        Vector (T* ptr, goSize_t size, goIndex_t stride = 1) : goFixedArray<T> (ptr, size, stride) {};
        Vector (const goFixedArray<T>& o);
        // Vector (const goFixedArray<T>& o) : goFixedArray<T> (o) {};
        // template <class To> Vector (const goFixedArray<To>& o) : goFixedArray<T> (o) {};
        virtual ~Vector ();

        Vector<T>& operator= (const goFixedArray<T>& other);

        //= resize now done in goFixedArray.
        //void resize (goSize_t s)
        //{
        //    this->setSize (s);
        //}

        void print (const char* formatstring = "%f\n") const;

#if 0
        template <class To>
        Vector<T> operator- (const Vector<To>& other) const
        {
        #ifdef GO_USE_EXCEPTIONS
            if (this->getSize() != other.getSize())
            {
                throw goMathException (goMathException::SIZE_MISMATCH);
            }
        #endif
            Vector<T> ret (this->getSize());
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
        Vector<T> operator+ (const Vector<To>& other) const
        {
        #ifdef GO_USE_EXCEPTIONS
            if (this->getSize() != other.getSize())
            {
                throw goMathException (goMathException::SIZE_MISMATCH);
            }
        #endif
            Vector<T> ret (this->getSize());
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
        // Vector<T> operator* (T scalar) const;

#if 0
        // template <class ScalarType>
        Vector<T> operator* (goFloat n) const
        {
            Vector<T> ret (this->getSize());
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
        Vector<T> operator* (goDouble n) const
        {
            Vector<T> ret (this->getSize());
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
        Vector<T> operator* (const goMath::Matrix<T>& M) const
        {
            const goMath::Matrix<T> this_M (0,0);
            this_M.setData (&(*this)[0], this->getSize(), 1, this->getStride());
            Vector<T> ret;
            goMath::Matrix<T> temp;
            temp = this_M * M;
            ret.resize (temp.getRows());
            temp.copyColumn (0, ret);
            return ret;
        };

#if 0
        Vector<T>& operator*= (goFloat n)
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
        Vector<T>& operator*= (goDouble n)
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
        Vector<T> cross (const Vector<T>& other) const
        {
            Vector<T> v(3);
            this->cross (other, v);
            return v;
        };

        bool cross (const Vector<T>& other, Vector<T>& ret) const
        {
            if (other.getSize() != 3 || this->getSize() != 3)
            {
	      // goLog::warning ("Vector::cross(): vectors must be size 3.");
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
        T operator* (const Vector<T>& other) const;
        Vector<T>& operator*= (T scalar);
        Vector<T>& operator/= (T scalar);
        Vector<T>& operator-= (T s);
        Vector<T>& operator+= (T s);
        Vector<T>& operator+= (const Vector<T>& other);
        Vector<T>& operator-= (const Vector<T>& other);
        Vector<T> operator- (const Vector<T>& other) const;
        Vector<T> operator+ (const Vector<T>& other) const;
        Vector<T> operator* (T scalar) const;
        Vector<T> operator/ (T scalar) const;
#if 0
        template <class To>
        Vector<T>& operator-= (const Vector<To>& other)
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

        Vector<T>& operator-= (T s)
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
        Vector<T>& operator+= (const Vector<To>& other)
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

        Vector<T>& operator+= (T s)
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
        Vector<T>& operator*= (const Vector<To>& other)
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
        Vector<T>& operator/= (const Vector<To>& other)
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
        Vector<T>& operator*= (const goMath::Matrix<T>& m);

        void outerProduct (const Vector<T>& other, goMath::Matrix<T>& ret) const;

        template <class To>
        bool copy (Vector<To>& target, goIndex_t startIndex = 0, goIndex_t skip = 0, goIndex_t lastIndex = -1) const
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
        }

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
        bool cat (const Vector<To>& other, Vector<To2>& target) const
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
        inline T conjInnerProduct (const Vector<T>&) const;

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


// inline goComplexf Vector<goComplexf>::square () const;


template<> inline goComplexf Vector<goComplexf>::conjInnerProduct (const Vector<goComplexf>& v) const
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
inline T Vector<T>::conjInnerProduct (const Vector<T>& v) const
{
    return *this * v;
}

#if 0
extern "C"
{
#include <cblas.h>
}

template <class T>
inline Vector<T> Vector<T>::operator- (const Vector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    Vector<T> ret (this->getSize());
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
inline goFloat Vector<goFloat>::operator* (const Vector<goFloat>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_sdot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template<> 
inline goDouble Vector<goDouble>::operator* (const Vector<goDouble>& other) const
{
    assert (other.getSize() == this->getSize());
    return cblas_ddot (this->getSize(), 
                       this->getPtr(), this->getStride(),
                       other.getPtr(),
                       other.getStride());
}

template <class T>
inline Vector<T>& Vector<T>::operator*= (const goMath::Matrix<T>& m)
{
    if (m.getColumns() != this->getSize())
    {
        goLog::warning ("Vector::operator*= (goMath::Matrix): Matrix has wrong column count.");
        return *this;
    }
    *this = m * *this;
    return *this;
}

template <>
inline Vector<goComplexf>& Vector<goComplexf>::operator*= (const goMath::Matrix<goComplexf>&)
{
    goLog::warning ("Vector::operator*= (goMath::Matrix): Not implemented for complex types.");
    return *this;
}
template <>
inline Vector<goComplexd>& Vector<goComplexd>::operator*= (const goMath::Matrix<goComplexd>&)
{
    goLog::warning ("Vector::operator*= (goMath::Matrix): Not implemented for complex types.");
    return *this;
}

template <class T>
inline T Vector<T>::operator* (const Vector<T>& other) const
{
#ifdef GO_USE_EXCEPTIONS
    if (this->getSize() != other.getSize())
    {
        throw goMathException (goMathException::SIZE_MISMATCH);
    }
#endif
    T		ret	    = T(0);
    goIndex_t	max	    = this->getSize();
    const T*	array	    = this->getPtr();
    const T*	otherArray  = other.getPtr();
    goIndex_t	stride	    = this->getStride();
    goIndex_t	otherStride = other.getStride();
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
bool vectorAdd (T alpha, const Vector<T>& x, Vector<T>& y);


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
void vectorOuter (T alpha, const Vector<T>& x, const Vector<T>& y, goMath::Matrix<T>& ret);

typedef goMath::Vector<goFloat>  Vectorf;
typedef goMath::Vector<goDouble> Vectord;
typedef goMath::Vector<goInt32>  Vectori;
/** @} */
};

typedef goMath::Vector<goFloat>  goVectorf;
typedef goMath::Vector<goDouble> goVectord;
typedef goMath::Vector<goInt32>  goVectori;

#ifndef goVector
# define goVector goMath::Vector
#endif

#endif
