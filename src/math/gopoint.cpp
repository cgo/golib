#include <gopoint.h>
#include <gotypes.h>

template <class T>
goPoint<T>::goPoint (T xf, T yf, T zf, T wf, goDouble v)
    : go4Vector<T> (xf, yf, zf, wf),
      value (v)
{
}

template <class T>
goPoint<T>::~goPoint ()
{
}

#if 0
template <class T>
goDouble
goPoint<T>::abs () const
{
    return sqrt(x*x + y*y + z*z + w*w);
}
#endif

template <class T>
bool
goPoint<T>::operator< (const goPoint<T>& other) const
{
    return (this->abs() < other.abs());
}

template <class T>
bool
goPoint<T>::operator> (const goPoint<T>& other) const
{
    return (this->abs() > other.abs());
}

#if 1
/**
 * @brief Comparison of the coordinates
 *
 * @param other  goPoint of the same type
 *
 * @note Only the coordinates are compared, NOT the value.
 * @return True if the coordinates of this point are equal to those
 *         of other.
 **/
template <class T>
bool
goPoint<T>::operator== (const goPoint<T>& other) const
{
    return (this->x == other.x &&
            this->y == other.y &&
            this->z == other.z &&
            this->w == other.w);
}

template <class T>
bool
goPoint<T>::operator!= (const goPoint<T>& other) const
{
    return !this->operator== (other);
}

template <class T>
goPoint<T>
goPoint<T>::operator- (const goPoint<T>& other) const
{
    goPoint<T> ret (this->x - other.x,
                    this->y - other.y,
                    this->z - other.z,
                    this->w - other.w);
    return ret;
}

template <class T>
goPoint<T>
goPoint<T>::operator+ (const goPoint<T>& other) const
{
    goPoint<T> ret (this->x + other.x,
                    this->y + other.y,
                    this->z + other.z,
                    this->w + other.w);
    return ret;
}

template <class T>
void goPoint<T>::operator-= (const goPoint<T>& other)
{
    this->x -= other.x;
    this->y -= other.y;
    this->z -= other.z;
    this->w -= other.w;
}

template <class T>
void goPoint<T>::operator+= (const goPoint<T>& other)
{
    this->x += other.x;
    this->y += other.y;
    this->z += other.z;
    this->w += other.w;
}

//template <class T>
//void goPoint<T>::operator*= (T scalar)
//{
//    this->x *= scalar;
//    this->y *= scalar;
//    this->z *= scalar;
//    this->w *= scalar;
//}
#endif
template class goPoint<goFloat>;
template class goPoint<goDouble>;
