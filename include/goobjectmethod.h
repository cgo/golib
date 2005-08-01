/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#ifndef GOOBJECTMETHOD_H
#define GOOBJECTMETHOD_H

#ifndef GOARRAY_H
# include <goarray.h>
#endif

enum goObjectMethodID
{
    GO_OBJECTMETHOD_USER = 0xfff00000
};

/*! \addtogroup misc
 * @{
 */
/**
 * @brief Parameter container for goObjectBase::callObjectMethod()
 *
 * @todo Add goObjectBase* parameters?
 **/
class
goObjectMethodParameters
{
	public:
	    goObjectMethodParameters () 
         : myVoidPointers(), 
           myIntegers(), 
           myUnsignedIntegers(), 
           myFloats(), 
           myDoubles() {};
        ~goObjectMethodParameters () {};
 
        /// Void pointers
        goArray<void*>    myVoidPointers;
        /// Integer parameters
        goArray<goInt32>  myIntegers;
        /// UInt parameters
        goArray<goUInt32> myUnsignedIntegers;
        /// Float parameters
        goArray<goFloat>  myFloats;
        /// Double parameters
        goArray<goDouble> myDoubles;
};
/*! @} */
#endif

