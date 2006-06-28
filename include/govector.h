#ifndef GOVECTOR_H
#define GOVECTOR_H

#include <goconfig.h>
#include <goexception.h>
#include <gofixedarray.h>
#ifndef GOCOMPLEX_H
# include <gocomplex.h>
#endif
#include <gomatrix.h>

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
        goVector () : goFixedArray<T> (1,0,0) {};
        goVector (goSize_t s, goIndex_t leftBorder = 0, goIndex_t rightBorder = 0) 
            : goFixedArray<T> (s,leftBorder,rightBorder) {};
        goVector (const goFixedArray<T>& o) : goFixedArray<T> (o) {};
        virtual ~goVector () {};

        void resize (goSize_t s)
        {
            this->setSize (s,this->getLeftBorder(),this->getRightBorder());
        }
        
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                *retArray = *array - *otherArray;
                ++array;
                ++otherArray;
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                *retArray = *array + *otherArray;
                ++array;
                ++otherArray;
                ++retArray;
            }
            return ret;
        };

        /**
         * @brief Inner product.
         *
         * @return The inner product this * other.
         **/
        template <class To>
        T operator* (const goVector<To>& other) const
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                ret += *array * *otherArray;
                ++array;
                ++otherArray;
            }
            return ret;
        };

        template <class ScalarType>
        goVector<T> operator* (ScalarType n) const
        {
            goVector<T> ret (this->getSize());
            goIndex_t max = this->getSize();
            const T* array = this->getPtr();
            T* retArray = ret.getPtr();
            for (goIndex_t i = 0; i < max; ++i)
            {
                *retArray = *array * n;
                ++array;
                ++retArray;
            }
            return ret;
        };

        template <class ScalarType>
        goVector<T>& operator*= (ScalarType n)
        {
            goIndex_t max = this->getSize();
            T* array = this->getPtr();
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array *= n;
                ++array;
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array -= *otherArray;
                ++array;
                ++otherArray;
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array += *otherArray;
                ++array;
                ++otherArray;
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
            for (goIndex_t i = 0; i < max; ++i)
            {
                *array *= *otherArray;
                ++array;
                ++otherArray;
            }
            return *this;
        };

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
            for (goIndex_t i = 0; i < sz; ++i)
            {
                tempi = other[i];
                const T* array = this->getPtr();
                //= NOTE: To account for complex numbers, we run through the whole matrix.
                //= --> FIXME!
                for (goIndex_t j = 0; j < sz; ++j, ++array)
                {
                    temp = tempi * *array;
                    ret[j][i] = temp;
                }
            }
            return true;
        };

        template <class To>
        bool copy (goVector<To>& target, goIndex_t startIndex, goIndex_t skip) const
        {
            assert (static_cast<goIndex_t>(this->getSize()) > startIndex);
            assert (skip >= 0);
            goSize_t sz = (this->getSize() - startIndex + skip) / (skip + 1);
            if (target.getSize() != sz)
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
        bool copy (To* target, goIndex_t startIndex, goIndex_t skip) const
        {
            assert (this->getSize() > startIndex);
            assert (skip >= 0);
            goSize_t sz = (this->getSize() - startIndex + skip) / (skip + 1);
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

        T abs () const
        {
            return sqrt(this->square());
        };
};

// inline goComplexf goVector<goComplexf>::square () const;

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

typedef goVector<goFloat>  goVectorf;
typedef goVector<goDouble> goVectord;

#endif
