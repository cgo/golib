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
 * $Id$
 */

#ifndef GO_OBJECTMESSAGE_H
#define GO_OBJECTMESSAGE_H

class goObjectBase;

/*!
 * \addtogroup misc
 * @{
 */
/*!
 * Message IDs for goObjectBase object messages.
 */
enum goObjectMessageID
{
    /// Default message
    GO_OBJECTMESSAGE_NONE = 0,
    /// Sent when the destructor of an object is called (FIXME)
    GO_OBJECTMESSAGE_DESTRUCTING,
    GO_OBJECTMESSAGE_CHANGED,
    /// Sent when an object changed its name (getObjectName()).
    GO_OBJECTMESSAGE_NAME_CHANGED,

    /*! Start of message space to be used by applications using goLib
     *  and deriving from goObjectBase
     */
    GO_OBJECTMESSAGE_USER = 0x0000FFFF
};

/**
 * @brief Message class for goObjectBase
 *
 * This is the class carrying messages sent by the obejct message mechanism
 * in goObjectBase.
 **/
class
goObjectMessage
{
    public:
        goObjectMessage ();
        ~goObjectMessage ();

	public:
        /// Pointer to the sending object
        goObjectBase*     mySender;
        /// ID (see goObjectMessageID)
        int               myMessageID;
        // goObjectMessageID myMessageID;
        /// Optional string
        char*             myMessageString;
        void*             myData;
};
/*!
 * @}
 */


#endif

