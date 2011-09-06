/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSPECIALARRAYS_H
#define GOSPECIALARRAYS_H

#include <string.h>
#include <golog.h>

typedef goUInt32 goNibbleArray_t;
typedef goUInt32 goBitArray_t;

/*!
 * Array addressing single bits
 * @author Christian Gosch
 * @date 6.8.2001
 * @see goPresenceManager
 */
class goBitArray
{
 public:
  /*!
   * @param bitsize Size in bits of the array
   */
    goBitArray(goSize_t bitsize) {
		wordLength = sizeof(goBitArray_t) << 3;
		goSize_t temp = wordLength;
		wordLengthLog = 0;
		while (temp > 0) 
		{
		    temp >>= 1;
		    wordLengthLog++;
		}
		wordLengthLog--;
		// cout << "wordLength = " << wordLength << endl;
		// cout << "wordLengthLog = " << wordLengthLog << endl;
		array.resize( (bitsize >> wordLengthLog) + 1 );
   	 }
     ~goBitArray() {
		array.resize(0);
   	 }

    /*!
     * @param n Index of a bit in the array
     * @return 0 if bit n is not set, 1 if bit n is set
     */
    inline int operator[] (goSize_t n);
    /*!
     * @param n Index of a bit to be set
     */
    inline void set (goSize_t n);
    /*!
     * @param n Index of a bit to be unset (set to zero)
     */
    inline void unSet (goSize_t n);
    /*!
     * Clears the whole array.
     */
    inline void clear();

	/*!
	 * @return The array of goBitArray_t containing the words that form the bit array.
	 */ 
	inline goArray<goBitArray_t>& getArray ();

	inline void ANDNOT (goBitArray& other);
 private:
    goArray<goBitArray_t> array;
    goSize_t wordLength;
    goSize_t wordLengthLog;
};

inline
void
goBitArray::ANDNOT (goBitArray& other)
{
	if (other.getArray().getSize() != array.getSize())
	{
		goLog::warning("goBitArray::ANDNOT()","differing array sizes");
		return;
	}
	goBitArray_t *p  = array.getPtr();
	goBitArray_t *p2 = other.getArray().getPtr();
	goSize_t i;
	for (i = 0; i < (goSize_t)array.getSize(); i++)
	{
		*p = *p & (~*p2);    // *p = *p AND (NOT *p2);
		p++; p2++;
	}
}

inline
goArray<goBitArray_t>& goBitArray::getArray()
{
	return array;
}

inline
int
goBitArray::operator[] (goSize_t n)
{
    goSize_t index = n >> wordLengthLog;
    int retVal = 1 & (array[index] >> (n & (wordLength - 1)));
    return retVal;
}

inline
void
goBitArray::set (goSize_t n)
{
    goSize_t index = n >> wordLengthLog;
    goBitArray_t v = 1 << (n & (wordLength - 1));
    array[index] |= v;
}

inline
void
goBitArray::unSet (goSize_t n)
{
    goSize_t index = n >> wordLengthLog;
    goBitArray_t v = ~(1 << (n & (wordLength - 1)));
    array[index] &= v;
}

inline
void
goBitArray::clear()
{
  goIndex_t i;
  for (i = 0; i < array.getSize(); i++)
    {
      array[i] = 0;
    }
}

/*!
 * Provides an array of nibbles (4-bit units).
 * @attention THERE IS A BUG IN THIS CLASS; IT IS  A T  L E A S T  NOT THREADSAFE!
 * @author Christian Gosch
 * @date 6.8.2001
 * @see goResolutionManager
 */
class
goNibbleArray
{
 public:
  /*!
   * @param numberOfNibbles Size of the array in nibbles
   */
  goNibbleArray(goSize_t numberOfNibbles) {
    nibblesPerWord = sizeof(goNibbleArray_t) << 1;
    goSize_t temp = nibblesPerWord;
    nibblesPerWordLog = 0;
    while (temp > 0) 
      {
		temp >>= 1;
		nibblesPerWordLog++;
      }
    nibblesPerWordLog--;
    array.resize(numberOfNibbles >> nibblesPerWordLog);
    cout << "nibblesPerWord = " << nibblesPerWord << endl;
    cout << "nibblesPerWordLog = " << nibblesPerWordLog << endl;
  }

  virtual ~goNibbleArray() {
    array.resize(0);
  }

  /*!
   * @param i Index of the nibble to be addressed
   * @param nibble Value to set the nibble to 
   * (only the lower 4 bits may be used).
   */
  inline void set(goSize_t i, goUInt8 nibble);
  /*!
   * @param nibbleNumber Index of the nibble to be returned
   * @return Value of the nibble with index <code>nibbleNumber</code>.
   * Only the lower 4 bits of the return value are used.
   */
  inline goUInt8 get(goSize_t nibbleNumber);
  /*!
   * @param nibbleNumber Index of the nibble to be returned
   * @return Value of the nibble with index <code>nibbleNumber</code>.
   * Only the lower 4 bits of the return value are used.
   */
  inline goUInt8 operator[] (goSize_t nibbleNumber);

  inline void fill (goUInt8 f);

  inline void OR (goUInt8 value, goSize_t nibbleNumber);

  inline goArray<goNibbleArray_t>& getArray();
 private:
  goSize_t nibblesPerWord;
  goSize_t nibblesPerWordLog;
  goArray<goNibbleArray_t> array;
};

inline goArray<goNibbleArray_t>& goNibbleArray::getArray ()
{
	return array;
}

inline void goNibbleArray::fill (goUInt8 f)
{
	goUInt8 f2 = f;
	f2 &= 15;  // delete upper 4 bits
	f2 |= (f << 4);  // copy lower 4 bits of f into higher 4 bits
	memset ((void*)array.getPtr(), (int)f2, sizeof(goInt8) * array.getSize());    
}

inline void goNibbleArray::OR (goUInt8 value, goSize_t nibbleNumber)
{
	goUInt8 temp = get(nibbleNumber);
	set ( nibbleNumber, ((value & 15) | temp) );
}

inline void goNibbleArray::set (goSize_t i, goUInt8 nibble)
{
  goSize_t idx = i >> nibblesPerWordLog;
  goSize_t nidx = i & (nibblesPerWord - 1);
  goNibbleArray_t n = nibble;
  n <<= (nidx << 2);
  goNibbleArray_t n2 = ~(15 << (nidx << 2));
  array[idx] &= n2;
  array[idx] |= n;
}

inline goUInt8 goNibbleArray::operator[] (goSize_t nibbleNumber)
{
  goSize_t idx = nibbleNumber >> nibblesPerWordLog;
  goSize_t nidx = nibbleNumber & (nibblesPerWord - 1);
  goUInt8 retval = (goUInt8)((array[idx] >> (nidx << 2)) & 15);
  return retval;
}

inline goUInt8 goNibbleArray::get (goSize_t nibbleNumber)
{
  return (*this)[nibbleNumber];
}


#endif
