#ifndef GOHASHTABLE_H
#define GOHASHTABLE_H

#include <gotypes.h>
#include <goarray.h>
#include <golist.h>

template <class I, class O>
class goHashEntry {
 public:
    virtual goHashEntry<I,O>& operator = (goHashEntry<I,O>& other);
  I	key;
  O	value;
};

// typedef goList<goHashEntry<goUInt64, void*> >	entryList;

/**
 * Implementation of a simple hash table.
 * You always have to check fail() before doing anything to the result of an operator[] call,
 * if you don't, the code is likely to crash. If you do, it should be safe.
 * The table index is calculated out of the last lb(modValue+1) bits of the key.
 * @author Christian Gosch
 * @note This class is considered as not tested hard enough. If you find bugs,
 * please contact the author.
 * @note I didn't say this is fast, did I?
 * @todo I need testing!!! Class was changed together with goList 21.8.2001.
 * Optimizations are possible.
 */
template <class I, class O>
class goHashTable {
 public:
  ///
  goHashTable ();
  /*!
   * @todo Problem: Hashtable für Zeiger. Daten müssen per Hand gelöscht werden. 
   */
  virtual ~goHashTable ();

  /*!
   * Sets the hash mod value. Only call this once, before you do anything else with the
   * hash table object. If you don't, or if you call it more than once, unexpected behaviour is
   * the result.
   * should always be something like (2^n-1).
   * @attention All previous entries are lost when calling this method. Call only once!
   * @param i Mod value.
   */
  void	setModValue (goUInt32);

  ///
  goUInt32 getModValue () { return modValue; }
  ///
  O&	operator[] (I in);
  ///
  // Removed for game2
  // virtual void	add (I key, O value);
  virtual void  add (I key, const O& value);
  /*!
   * @note Only superficially tested.
   * @return The value for the given key.
   */ 
  virtual O	remove (I key);
  
  ///
  bool	fail ();

 protected:
  void addEntry (I table, void* entry);
  /// Erases all lists and resizes the table to zero.
  void eraseLists();
  ///
  bool		lastFailed;
  ///
  goUInt32	modValue;
  ///
    // goArray<goList<goHashEntry<I,O> > >	*table;
  goArray<void*> theTable;
  ///
  O		dummy;
};

#endif
