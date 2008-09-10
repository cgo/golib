#ifndef GOQUATERNION
#define GOQUATERNION


#include <gotypes.h>
#include <go3vector.h>
#include <goconfig.h>
#include <govector.h>

/*!
 * \addtogroup math
 * @{
 */
/**
 * @brief Quaternion class.
 **/
template <class T>
class goQuaternion
{
	public:
	goQuaternion();
	~goQuaternion();


	void operator= (const goQuaternion<T>& other);

    void setRotation (T angle, const goMath::Vector<T>& axis);
    void setRotation (T angle, const go3Vector<T>& axis);
    T getRotationAngle () const;
    void getRotationMatrix (T* matrix44) const;

	goQuaternion<T> operator* (const goQuaternion<T>& other) const;
	goQuaternion<T> operator+ (const goQuaternion<T>& other) const;
	void conjugate();

	go3Vector<T> v;
	T			 scalar;
};
/*! @} */

#endif
