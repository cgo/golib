#include <goquaternion.h>

/**
 * @brief Constructor.
 **/
template <class T>
goQuaternion<T>::goQuaternion()
{
	v.x = 0; v.y = 0; v.z = 0;
	scalar = 0;
}

/**
 * @brief Destructor.
 **/
template <class T>
goQuaternion<T>::~goQuaternion()
{
}

/**
 * @brief Deep copy.
 *
 * @param other  Object to be copied.
 **/
template <class T>
void
goQuaternion<T>::operator= (goQuaternion<T> other)
{
	this->scalar = other.scalar;
	this->v      = other.v;
}

/**
 * @brief Quaternion multiplication.
 *
 * Calculates this * other.
 * 
 * @param other  Right-hand operand
 *
 * @return this * other
 **/
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

/**
 * @brief Quaternion addition.
 *
 * @param other  Right-hand operator
 *
 * @return this + other
 **/
template <class T>
goQuaternion<T>
goQuaternion<T>::operator+ (goQuaternion<T> other)
{
	goQuaternion<T> retval;
	retval.scalar = scalar + other.scalar;
	retval.v      = v + other.v;
	return retval;
}

/**
 * @brief Conjugate this quaternion.
 **/
template <class T>
void
goQuaternion<T>::conjugate ()
{
	v *= -1;
}

template class goQuaternion<goFloat>;
template class goQuaternion<goDouble>;
