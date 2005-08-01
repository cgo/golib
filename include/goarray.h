/* --*- C++ -*-- */
#ifndef GOARRAY_H
#define GOARRAY_H

#include <gotypes.h>

/*!
 * \addtogroup data
 * @{
 */
/*!
 * Resizeable array. 
 * @author Christian Gosch
 */
template< class T >
class
goArray {
 public:
  ///
  goArray();
  ///
  goArray(goSize_t s);
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
  void sort (goArray<int>& indexArray);
  void sort (goArray<goSize_t>& indexArray);
  void sort (goArray<void*>& indexArrays);
  void quickSort (goIndex_t, goIndex_t, goArray<int>& indexArray);
  void quickSort (goIndex_t, goIndex_t, goArray<goSize_t>& indexArray);
  void quickSort (goIndex_t, goIndex_t, goArray<void*>& indexArrays);

    /*!
     * Fills the array <strong>bytewise</strong> with byte value f (fast)
     * @param f Byte value to fill the array with
     */
  void byteFill (int f);
  /*!
   * Fills the array with the given value.
   * @param value Value the array is to be filled with.
   */
  void fill (T value);

    /*!
     * Removes entry at given index. All following entries are moved to the "left".
     * @attention This method should be used with caution, since it can be very slow for large arrays.
     * @param index The index of the entry to be removed.
     */
    void remove (goIndex_t index);
  
  ///
  inline goIndex_t getSize () const {return arraySize;};
  inline goIndex_t dim     () const {return arraySize;};
  ///
  inline T*	      getPtr () { return Array; };
  inline const T* getPtr () const { return Array; };
  inline T&       operator[] (goIndex_t idx) { return Array[idx]; };
  inline const T& operator[] (goIndex_t idx) const { return Array[idx]; };
  ///
  goArray<T>& operator=  (const goArray<T>& other);
  /// Resizes the array and adds {\bf item} at the end of it.
  // goArray<T>& operator+= (T& item);
  goArray<T>& operator+= (T item);

  /// 
  bool operator== (const goArray<T>& other);
  /// 
  bool operator!= (const goArray<T>& other);
  
 protected:
  T*        Array;
  goIndex_t arraySize;
};
/*! @} */
#endif
