/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goquaternion.h>

/**
 * @brief Constructor.
 **/
template <class T>
goQuaternion<T>::goQuaternion()
{
	v.x = 0.0f; v.y = 0.0f; v.z = 0.0f;
	scalar = 1.0f;
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
goQuaternion<T>::operator= (const goQuaternion<T>& other)
{
	this->scalar = other.scalar;
	this->v      = other.v;
}

template <class T>
void
goQuaternion<T>::setRotation (T alpha, const goVector<T>& axis)
{
    T temp = alpha * 0.5f;
    this->scalar = cos (temp);
    this->v.x = axis[0];
    this->v.y = axis[1];
    this->v.z = axis[2];
    this->v *= sin (temp);
}

template <class T>
void
goQuaternion<T>::setRotation (T alpha, const go3Vector<T>& axis)
{
    T temp = alpha * 0.5f;
    this->scalar = cos (temp);
    this->v = axis;
    this->v *= sin (temp);
}

template <class T>
T goQuaternion<T>::getRotationAngle () const
{
    return 2.0f * acos (this->scalar);
}

/** 
 * @brief Column-wise.
 * 
 * @param matrix44 
 * 
 * @see http://www.cprogramming.com/tutorial/3d/quaternions.html
 */
template <class T>
void goQuaternion<T>::getRotationMatrix (T* matrix44) const
{
    matrix44[0] = 1.0f - 2.0f * (v.y*v.y + v.z*v.z);
    matrix44[1] = 2.0f * (v.x*v.y + scalar*v.z);
    matrix44[2] = 2.0f * (v.x*v.z - scalar*v.y);
    matrix44[3] = 0.0f;
    matrix44[4] = 2.0f * (v.x*v.y - scalar*v.z);
    matrix44[5] = 1.0f - 2.0f * (v.x*v.x + v.z*v.z);
    matrix44[6] = 2.0f * (v.y*v.z - scalar*v.z);
    matrix44[7] = 0.0f;
    matrix44[8] = 2.0f * (v.x*v.z + scalar*v.y);
    matrix44[9] = 2.0f * (v.y*v.z - scalar*v.z);
    matrix44[10] = 1.0f - 2.0f * (v.x*v.x + v.y*v.y);
    matrix44[11] = 0.0f;
    matrix44[12] = 0.0f;
    matrix44[13] = 0.0f;
    matrix44[14] = 0.0f;
    matrix44[15] = 1.0f;
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
goQuaternion<T>::operator* (const goQuaternion<T>& other) const
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
goQuaternion<T>::operator+ (const goQuaternion<T>& other) const
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
