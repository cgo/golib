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
 */
/*!
 * @{
 */
/*!
 * Message IDs for goObjectBase object messages.
 */
enum goObjectMessageID
{
    /// Default message
    GO_OBJECTMESSAGE_NONE,
    /// Sent when the destructor of an object is called (FIXME)
    GO_OBJECTMESSAGE_DESTRUCTING
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
        goObjectMessageID myMessageID;
        /// Optional string
        char*             myMessageString;
};
/*!
 * @}
 */


#endif

