#ifndef __GO_COMPLEX_H__
#define __GO_COMPLEX_H__

#include <iostream>
#include <gotypes.h>

/*!
 * \addtogroup math
 * @{
 */
/**
 * @brief Complex number class.
 **/
template <class T>
class goComplex 
{
    public:
        goComplex  () : real (T(0)), ima (T(0)) {}
        goComplex  (T r, T i = T(0)) : real (r), ima (i) {}
        goComplex  (const goComplex<T>& other) { *this = other; }
        ~goComplex () {}

        inline T&	re () { return real; }
        inline T&	im () { return ima; }
        inline const T&	re () const { return real; }
        inline const T&	im () const { return ima; }

        operator T () { return this->abs(); }

        goComplex<T>	operator*  (const goComplex<T>& other) const
        {
            goComplex<T> retval;

            retval.re() = (real * other.re()) - (ima * other.im());
            retval.im() = (real * other.im()) + (ima * other.re());

            return retval;
        }

        goComplex<T>&	operator*= (const goComplex<T>& other) 
        {
            T tmpreal;
            T tmpima;

            tmpreal = (real * other.re()) - (ima * other.im());
            tmpima  = (real * other.im()) + (ima * other.re());

            real = tmpreal;
            ima  = tmpima;
            return *this;

        }

        goComplex<T>	operator+  (const goComplex<T>& other) const
        {
            goComplex<T> retval;

            retval.re() = real + other.re();
            retval.im() = ima  + other.im();

            return retval;
        }

        goComplex<T>&	operator+= (const goComplex<T>& other)
        {
            real += other.re();
            ima  += other.im();
            return *this;
        }

        goComplex<T>	operator-  (const goComplex<T>& other) const
        {
            goComplex<T> retval;

            retval.re() = real - other.re();
            retval.im() = ima  - other.im();

            return retval;
        }

        goComplex<T>&	operator-= (const goComplex<T>& other)
        {
            real -= other.re();
            ima  -= other.im();

            return *this;
        }

        goComplex<T>	operator/  (const goComplex<T>& other) const
        {
            goComplex<T> retval;
            T tmp;

            tmp = (other.re()*other.re()) + (other.im()*other.im());
            retval.re() = ( (real * other.re()) + (ima * other.im()) ) / tmp; 
            retval.im() = ( (other.re() * ima) - (real * other.im()) ) / tmp;

            return retval;
        }

        goComplex<T>&	operator/= (const goComplex<T>& other)
        {
            T tmpreal;
            T tmpima;
            T tmp;

            tmp		= (other.re()*other.re()) + (other.im()*other.im());
            tmpreal	= ( (real * other.re()) + (ima * other.im()) ) / tmp; 
            tmpima	= ( (other.re() * ima) - (real * other.im()) ) / tmp;

            real = tmpreal;
            ima  = tmpima;
            return *this;
        }

        goComplex<T>&	operator=  (const goComplex<T>& other)
        {
            real = other.re();
            ima  = other.im();
            return *this;
        }

        bool		operator== (const goComplex<T>& other) const
        {
            if ( (other.re() == real) && (other.im() == ima) ) 
            {
                return true;
            }
            return false;
        }

        bool		operator!= (const goComplex<T>& other) const
        {
            if ( (other.re() != real) || (other.im() != ima) ) 
            {
                return true;
            }
            return false;
        }

        bool		operator>  (const goComplex<T>& other) const
        {
            if (other.abs() < this->abs()) {
                return true;
            }
            return false;
        }

        bool		operator<  (const goComplex<T>& other) const
        {
            if (other.abs() > this->abs()) {
                return true;
            }
            return false;
        }

        /// make conjugate complex number
        void	conjugate ()
        {
            ima = -ima;
        }

        goComplex<T> conj () const
        {
            return goComplex<T>(this->re(),-this->im());
        }

        /**
         * Calculate the argument (phase angle) of the complex number.
         * @returns Argument of this complex number.
         */
        goDouble  arg () const
        {
            goDouble argValue = 0.0;
            if (real != 0) 
            { 
                if (real > 0) 
                {
                    if (ima >= 0)
                        argValue = ( (T)atan (ima / real) ); 
                    else argValue = ( M_PI + M_PI_2 - (T)atan (ima / real) );
                } 
                else 
                {
                    if (ima >= 0)
                        argValue = ( M_PI_2 - (T)atan (ima / real) );
                    else 
                        argValue = ( M_PI + (T)atan (ima / real) );
                }

            } 
            else 
            {
                argValue = (T)(M_PI_2);
            }
            return argValue;
        }

        /**
         * Calculate the absolute value of the complex number.
         * @returns Absolute value of this complex number
         */
        goDouble  abs () const
        {
            return sqrt ( (double) (ima*ima + real*real) );
        }

    protected:
        T real;
        T ima;
};
/*! @} */
///
std::ostream& operator<< (std::ostream& o, const class goComplex<goDouble>& c);
std::ostream& operator<< (std::ostream& o, const class goComplex<goFloat>& c);

typedef goComplex<goFloat>  goComplexf;
typedef goComplex<goDouble> goComplexd;

#endif 
