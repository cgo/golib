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

enum goObjectMessageID
{
    GO_OBJECTMESSAGE_NONE,
    GO_OBJECTMESSAGE_DESTRUCTING
};

class
goObjectMessage
{
    public:
        goObjectMessage ();
        ~goObjectMessage ();

	public:
        goObjectBase*     mySender;
        goObjectMessageID myMessageID;
        char*             myMessageString;
};


#endif

