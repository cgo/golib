#ifndef __GONVECTOR_H__
#define __GONVECTOR_H__

#include <gotypes.h>
#include <gocomplex.h>
#include <goarray.h>
#include <iostream>

/**
 * This implements a vector of arbitrary length.
 * @author Christian Gosch
 */
template<class T>
class goNVector : public goArray<T> {
 public:
  /**
   * Constructor.
   */
  goNVector  (goUInt32 n);
  goNVector  ();

	/*!
	 * @attention The internal pointer gets deleted ALWAYS when the
	 * destructor is called, even if you set it by setVectorPtr().
	 * Therefore, be very careful with the setVectorPtr() method.
	 */
  ~goNVector ();

  /**
   * Sets internal vector pointer to ptr. This is useful if you don't
   * want to deep-copy your data for performance reasons.
   * @attention See the destructor for important notes.
   * @see ~goNVector()
   */
  inline void  setVectorPtr (T* ptr);

  /**
   * Sets size of the vector in elements.
   * This should only be used together with setVectorPtr ().
   */
  void 	   setSize (int sz) { arraySize = sz; absValid = false;}

  /**
   * Adds two NVectors.
   */
  inline goNVector<T>& 	operator+= (goNVector<T>& other);

  /**
   * Subtracts two NVectors.
   */
  inline goNVector<T>& 	operator-= (goNVector<T>& other);

  /**
   * Calculates the absolute value of the vector.
   * DOES ONLY WORK WITH DOUBLE TYPE YET !
   */
  inline T		abs ();
 protected:
  bool	   delete_vector;	//tells the destructor whether to delete the vector
  T	   absValue;
  bool	   absValid;
};

  /*
   * This still is no real solution.
   * It's a quick hack to output vectors for debugging.
   */
//ostream&       operator<< (ostream& ostr, goNVector<T>& vec);
std::ostream&       operator<< (std::ostream& ostr, goNVector<goComplex<goDouble> >& vec);


#endif /* __GONVECTOR_H__ */















