#include <goobjectmessage.h>

/**
 * @brief Constructor
 **/
goObjectMessage::goObjectMessage ()
    :
    mySender        (0),
    myMessageID     (GO_OBJECTMESSAGE_NONE),
    myMessageString (0)
{
}

/**
 * @brief Destructor
 **/
goObjectMessage::~goObjectMessage ()
{
    if (myMessageString)
    {
        delete[] myMessageString;
    }
}
