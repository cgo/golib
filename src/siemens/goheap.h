/* --*- C++ -*-- */


#ifndef __GOHEAP_H__
#define __GOHEAP_H__

#include <goarray.h>

/** 
 * Heap implementation .
 */
template <class T>
class goHeap {
 public:
  ///
  goHeap ();
  ///
  goHeap (int sz);
  ///
  ~goHeap();

  ///
  bool set(int pos,const T& item);
  ///
  bool set(int length,int pos,const T* items);
  ///
  const T& get(int pos);

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
  goArray<T> 	tree;
  ///
  int 		size;
  ///
  T*         	dummy;
};

#endif
