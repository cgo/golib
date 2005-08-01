#ifndef GOPOINT_H
#define GOPOINT_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GO4VECTOR_H
# include <go4vector.h>
#endif

template <class T>
class goPoint : public go4Vector<T>
{
    public:
        goPoint (T xf = T(0.0), T yf = T(0.0), T zf = T(0.0), T wf = T(0.0), goDouble v = 0.0);
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
