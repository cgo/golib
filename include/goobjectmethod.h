/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: goobjectmethod.h,v 1.1.1.1 2006/04/19 15:27:03 gosch Exp $
 */

#ifndef GOOBJECTMETHOD_H
#define GOOBJECTMETHOD_H

#ifndef GOARRAY_H
# include <goarray.h>
#endif
#ifndef GOTHREAD_H
# include <gothread.h>
#endif

enum goObjectMethodID
{
    GO_OBJECTMETHOD_MATLAB = 0xffefff00,
    GO_OBJECTMETHOD_USER =   0xfff00000
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
           myDoubles(),
           semaphore(),
           blocking(false) {};

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

        goSemaphore semaphore;
        bool        blocking;
};
/*! @} */
#endif

