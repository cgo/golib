#ifndef GOHASHTABLE_H
#define GOHASHTABLE_H

#include <gotypes.h>
#include <goarray.h>

template <class I, class O>
class goHashEntry {
 public:
  goHashEntry& operator = (goHashEntry& other);
  I	key;
  O	value;
};

// typedef goList<goHashEntry<goUInt64, void*> >	entryList;

/**
 * Implementation of a simple hash table.
 * You always have to check fail() before doing anything to the result of an operator[] call,
 * if you don't, the code is likely to crash. If you do, it should be safe.
 * The table index is calculated out of the last lb(modValue+1) bits of the key.
 */
template <class I, class O>
class goHashTable {
 public:
  ///
  goHashTable (goUInt32 modval = 511);
  ///
  ~goHashTable ();

  /// should always be something like (2^n-1).
  void	setModValue (goUInt32 i) { modValue = i; }
  /*!
   * Returns the value for in.
   * If in is not found, a dummy is returned. The dummy is always 0.
   */
  O&	operator[] (I in);
  ///
  void	add (I key, O value);
  /// NOT YET IMPLEMENTED
  // void	remove (I key, O value);
  
  ///
  bool	fail ();

  void sort ();

 protected:
  ///
  bool		lastFailed;
  ///
  goUInt32	modValue;
  /// pointers to goArray<void*> --> to goHashEntry<I,O>
  goArray<void*>	table;
  ///
  O		dummy;
};


#endif
