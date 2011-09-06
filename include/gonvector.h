/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef __GONVECTOR_H__
#define __GONVECTOR_H__

#include <gotypes.h>
#include <gocomplex.h>
#include <goarray.h>
#include <iostream>

/* Throw this code away. */

/*
 * \addtogroup math
 * @{
 */
/*
 * \brief Somewhat deprecated vector class.
 * @note DEPRECATED -- do not use.
 * This implements a vector of arbitrary length.
 * Do not use this for maths, use goMatrix(n,1) instead.
 * @author Christian Gosch
 */
template<class T>
class goNVector : public goArray<T> {
 public:
  /*
   * Constructor.
   */
  goNVector  (goUInt32 n);
  goNVector  ();

	/*
	 * @attention The internal pointer gets deleted ALWAYS when the
	 * destructor is called, even if you set it by setVectorPtr().
	 * Therefore, be very careful with the setVectorPtr() method.
	 */
  ~goNVector ();

  /*
   * Sets internal vector pointer to ptr. This is useful if you don't
   * want to deep-copy your data for performance reasons.
   * @attention See the destructor for important notes.
   * @see ~goNVector()
   */
  inline void  setVectorPtr (T* ptr);

  /*
   * Sets size of the vector in elements.
   * This should only be used together with setVectorPtr ().
   */
  void 	   setSize (int sz) { this->resize(sz); absValid = false;}

  /*
   * Adds two NVectors.
   */
  inline goNVector<T>& 	operator+= (goNVector<T>& other);

  /*
   * Subtracts two NVectors.
   */
  inline goNVector<T>& 	operator-= (goNVector<T>& other);

  /*
   * Calculates the absolute value of the vector.
   * DOES ONLY WORK WITH DOUBLE TYPE YET !
   */
  inline T		abs ();
 protected:
  bool	   delete_vector;	//tells the destructor whether to delete the vector
  T	   absValue;
  bool	   absValid;
};
/* æ} */

  /*
   * This still is no real solution.
   * It's a quick hack to output vectors for debugging.
   */
//ostream&       operator<< (ostream& ostr, goNVector<T>& vec);
std::ostream&       operator<< (std::ostream& ostr, goNVector<goComplex<goDouble> >& vec);


#endif /* __GONVECTOR_H__ */















