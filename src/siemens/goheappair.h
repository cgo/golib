/* --*- C++ -*-- */


#ifndef __GOHEAPPAIR_H__
#define __GOHEAPPAIR_H__

#include <goarray.h>

/** 
 * Heap implementation .
 */
template <class T, class T2>
class goHeapPair {
 public:
  ///
  goHeapPair ();
  ///
  goHeapPair (int sz);
  ///
  ~goHeapPair();

  ///
  bool set(int pos,const T& item, const T2& item2);
  ///
  bool set(int length,int pos,const T* items, const T2* items2);
  ///
  const T& getFirst(int pos);
  const T2& getSecond(int pos);

  ///
  void heapify(void);
  ///
  void buildHeap(int pos = 0);
  ///
  bool sift(int pos);
  ///
  bool resize(int sz);
  ///
  int  getSize(void) { return size;}

 protected:
  ///
  goArray<T> 	tree;
  goArray<T2>	tree2;
  ///
  int 		size;
  ///
  T*         	dummy;
  T2*		dummy2;
};

#endif
