/* --*- C++ -*-- */


#ifndef __GOHEAP_H__
#define __GOHEAP_H__

#include <goarray.h>


/** 
 * Heap implementation .
 */
template <class T, class KEY_T>
class goHeap {
 public:
  ///
  goHeap ();
  ///
  goHeap (int sz);
  ///
  ~goHeap();

  ///
  bool set(int pos,T item, KEY_T key);
  ///
  bool set(int length,int pos,const T* items, const KEY_T* keys);
  ///
  T get(int pos);
  KEY_T getKey (int pos);

  ///
  void heapify(void);
  ///
  void buildHeap(void);
  ///
  bool sift(int pos);
  ///
  bool resize(int sz);
  ///
  int  getSize(void) { return size;}

 protected:
  ///
  goArray<KEY_T> 	tree;
  ///
  goArray<T>		valueTree; 
  ///
  int 		size;
  ///
  T*         	dummy;
};


#endif
