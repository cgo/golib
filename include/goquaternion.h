#ifndef GOQUATERNION
#define GOQUATERNION


#include <gotypes.h>
#include <go3vector.h>
#include <config.h>

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

#endif
