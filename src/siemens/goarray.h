/* --*- C++ -*-- */
#ifndef __GOARRAY_HH__
#define __GOARRAY_HH__

#include <gotypes.h>
#include <iostream.h>

/*!
 * Resizeable array. Modified version for tempgolib, 2001 work placement with
 * Siemens.
 * @author Christian Gosch
 */

template< class T >
class
goArray {
 public:
  ///
  goArray();
  ///
  goArray(const goArray<T>& other);
  ///
  ~goArray();
  
  ///
  goIndex_t resize (goIndex_t size); 

  void sort ();
  void quickSort (goIndex_t, goIndex_t);
  /*!
   * Sort this array and sort indexArray in the same order.
   * Good for keeping track of the original indices.
   */
  void sort (goArray<goSize_t>& indexArray);
  void sort (goArray<void*>& indexArrays);
  void quickSort (goIndex_t, goIndex_t, goArray<goSize_t>& indexArray);
  void quickSort (goIndex_t, goIndex_t, goArray<void*>& indexArrays);
  
  ///
  inline goIndex_t size () {return arraySize;}
  ///
  inline goIndex_t getSize () {return arraySize;}
  ///
  inline goIndex_t getSize () const {return arraySize;}
  ///
  T*	   getPtr () { return Array; }
  T*	   getPtr () const { return Array; }
  inline T&      operator[] (goIndex_t idx) { return Array[idx]; }
  inline T&      operator[] (goIndex_t idx) const { return Array[idx]; }
  ///
  goArray<T>& operator=  (goArray<T>& other);
  ///
  // goArray<T>& operator=  (const goArray<T>& other);
  /// Resizes the array and adds {\bf item} at the end of it.
  // goArray<T>& operator+= (T& item);
  goArray<T>& operator+= (T item);

  /// 
  bool operator== (goArray<T>& other);
  /// 
  bool operator!= (goArray<T>& other);
  
 protected:
  T* Array;
  goIndex_t arraySize;
};

ostream& operator<< (ostream& o, goArray<goInt32>& a);
ostream& operator<< (ostream& o, goArray<goUInt32>& a);

#endif




