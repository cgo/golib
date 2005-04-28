#ifndef GOQUATERNION
#define GOQUATERNION


#include <gotypes.h>
#include <go3vector.h>
#include <goconfig.h>

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


	void operator= (goQuaternion<T> other);
	goQuaternion<T> operator* (goQuaternion<T> other);
	goQuaternion<T> operator+ (goQuaternion<T> other);
	void conjugate();

	go3Vector<T> v;
	T			 scalar;
};
/*! @} */

#endif
