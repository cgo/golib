#ifndef GOPOINT_H
#define GOPOINT_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#include <go4vector.h>

template <class T>
class goPoint : public go4Vector<T>
{
    public:
        /** 
         * @brief Constructor.
         * 
         * @note Note that the 4th entry in a new vector is initialised
         * to 1. This is to support work with homogeneous coordinates,
         * which is the main reason for the existence of this class.
         * 
         * @param xf x coordinate. Default 0.0.
         * @param yf y coordinate. Default 0.0.
         * @param zf z coordinate. Default 0.0.
         * @param wf w or t coordinate. Default 0.0.
         * @param v  Value (misc.) for this point. This is not used for calculations 
         * with matrices, i.e. it is not interpreted as coordinate value. Default 0.0.
         */
        goPoint (T xf = T(0.0), T yf = T(0.0), T zf = T(0.0), T wf = T(0.0), goDouble v = 0.0);
        template <class To>
        goPoint (const goPoint<To>& other)
            : go4Vector<T> (other)
        {
            this->value = other.value;
        };
        template <class To>
        goPoint (const go4Vector<To>& other)
            : go4Vector<T> (other)
        {
        };
        virtual ~goPoint ();

        //goDouble abs () const;
        bool operator<  (const goPoint<T>& other) const;
        bool operator>  (const goPoint<T>& other) const;
        bool operator== (const goPoint<T>& other) const;
        bool operator!= (const goPoint<T>& other) const;
        goPoint<T> operator- (const goPoint<T>& other) const;
        goPoint<T> operator+ (const goPoint<T>& other) const;
        void       operator-= (const goPoint<T>& other);
        void       operator+= (const goPoint<T>& other);
        template <class To>
        goPoint<T>& operator= (const goPoint<To>& other)
        {
            go4Vector<T>::operator= (other);
            this->value = other.value;
            return *this;
        };
            
        // void       operator*= (T);
        
        //T x;
        //T y;
        //T z;
        //T w;
        goDouble value;
};

typedef goPoint<goFloat> goPointf;
typedef goPoint<goDouble> goPointd;

#endif
