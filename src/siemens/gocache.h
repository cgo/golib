#ifndef GOCACHE_H
#define GOCACHE_H

#include <gotypes.h>
#include <goarray.h>

template<class T>
class goCache {
 public:
  goCache () {
    setSize (128);
    queries = 0;
    hits = 0;
    youthCounter = 0;
  }
  virtual ~goCache () {
    goSize_t i;
    for (i = 0; i < (goSize_t)array.getSize(); i++) {
      if (array[i]) {
	((T*)array[i])->destroy();
	delete (T*)array[i];
      }
    }
    cout << "Queries: " << queries << ", hits: " << hits << ", rate: " << hits / (float)queries << endl;
  }

  /*!
   * Must be a power of two!
   */
  void setSize (goSize_t s) 
    { 
      size = s; modVal = s - 1; 
      count.resize(s);
      array.resize(s);
      keyArray.resize(s);
      goSize_t i;
      for (i = 0; i < s; i++) 
	{
	  array[i] = 0;
	  keyArray[i] = 0;
	  count[i] = 0;
	}
    }
  inline void add (goSize_t index, goSize_t key, T* object) 
    {
      goSize_t p = 0;
      goSize_t i;
      for (i = 0; i < size; i++) 
	{
	  if ( (count[p] == 0) )
	    break;
	  if ( (count[p] > count[i]) )
	    p = i;
	}
      if (array[p]) 
	{
	  T* tempPtr = (T*)array[p];
	  tempPtr->destroy();
	  delete tempPtr;
	}
      array[p] = (void*)object;
      keyArray[p] = key;
      count[p] = youthCounter;
    }

  inline T* get (goSize_t index, goSize_t key) 
    {
      goSize_t i;
      queries++;
      for (i = 0; i < size; i++)
	{
	  if (keyArray[i] == key)
	    break;
	}
      // cout << "hitrate: " << hits / (float)queries << endl;
      if (i < size) 
	{
	  hits++;
	  count[i] = youthCounter++;
	  return (T*)array[i];
	}
      return NULL;
    }

 protected:
  goSize_t size;
  goSize_t modVal;
  goSize_t queries;
  goSize_t hits;
  goArray<void*> array;
  goArray<goSize_t> keyArray;

  goArray<goSize_t> count;
  goSize_t youthCounter;
};


#endif
