#include <goobjectmessage.h>

goObjectMessage::goObjectMessage ()
    :
    mySender        (0),
    myMessageID     (GO_OBJECTMESSAGE_NONE),
    myMessageString (0)
{
}

goObjectMessage::~goObjectMessage ()
{
    if (myMessageString)
    {
        delete[] myMessageString;
    }
}
