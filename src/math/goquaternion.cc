#include <goquaternion.h>

template <class T>
goQuaternion<T>::goQuaternion()
{
	v.x = 0; v.y = 0; v.z = 0;
	scalar = 0;
}

template <class T>
goQuaternion<T>::~goQuaternion()
{
}

template <class T>
void
goQuaternion<T>::operator= (goQuaternion<T> other)
{
	this->scalar = other.scalar;
	this->v      = other.v;
}

template <class T>
goQuaternion<T>
goQuaternion<T>::operator* (goQuaternion<T> other)
{
	goQuaternion<T> retval;
	go3Vector<T> V;
	V = v;
	V.cross (other.v);
	retval.scalar = (scalar * other.scalar - (v * other.v));
	retval.v      = other.v * scalar + v * other.scalar + V;
    return retval;
}

template <class T>
goQuaternion<T>
goQuaternion<T>::operator+ (goQuaternion<T> other)
{
	goQuaternion<T> retval;
	retval.scalar = scalar + other.scalar;
	retval.v      = v + other.v;
	return retval;
}

template <class T>
void
goQuaternion<T>::conjugate ()
{
	v *= -1;
}

template class goQuaternion<goFloat>;
