#include <golist.h>
#include <golist.hpp>
#include <goobjectbase.h>
#include <iostream>

/*! \brief Constructor */
goObjectBase::goObjectBase ()
    :
    className            ("goObjectBase"),
    myConnectedObjects   ()
{
}

/*! \brief Destructor */
goObjectBase::~goObjectBase ()
{
    sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
}

/*! \brief Returns the class name. */
const char*
goObjectBase::getClassName()
{
    return className.toCharPtr();
}

/*! \brief Returns the size of this object or some measure of its memory
 * consumption.
 *
 * Overload this function as you please and as it makes sense.
 * @return Size in bytes of this object.
 */
goSize_t 
goObjectBase::memoryUsage ()
{
    return sizeof (goObjectBase);
}

/*! \brief Sets the class name */
void
goObjectBase::setClassName (const char* name)
{
    className = name;
}

/*! \brief Sets the class name */
void
goObjectBase::setClassName (goString& name)
{
    className = name;
}

/*! \brief Prints an informational message to the calling console.
 *
 * Prints messages of the form "<classname> message: <your message>".
 */
void
goObjectBase::printClassMessage (const char* msg)
{
    std::cout << getClassName() << " message: " << msg << std::endl;
}

/*! \brief Prints an informational message to the calling console.
 *
 * Prints messages of the form "<classname> message: <your message>".
 */
void
goObjectBase::printClassMessage (goString& msg)
{
    printClassMessage (msg.toCharPtr());
}

/*! \brief Connects an object to this object.
 *
 * Connected objects will receive messages sent by this object.
 * \todo It may be necessary to only allow bi-directional connections.
 */
void
goObjectBase::connectObject (goObjectBase* object)
{
    if (!object)
    {
        return;
    }
    goObjectBase* o = NULL;
    myConnectedObjects.resetToFront();
    if (!myConnectedObjects.isEmpty())
    {
        o = myConnectedObjects.getCurrent();
        for (; !myConnectedObjects.isTail(); o = myConnectedObjects.getNext())
        {
            o = myConnectedObjects.getCurrent();
            if (o == object)
                return;
        }
        if (object == myConnectedObjects.getTail())
            return;
    }
    myConnectedObjects.append (object);
}

/*! \brief Disconnects an object from this object. */
void
goObjectBase::disconnectObject (const goObjectBase* object)
{
    if (!object)
    {
        return;
    }
    goObjectBase* o = NULL;
    myConnectedObjects.resetToFront();
    o = myConnectedObjects.getCurrent();
    for (; !myConnectedObjects.isTail(); o = myConnectedObjects.getNext())
    {
        o = myConnectedObjects.getCurrent();
        if (o == object)
        {
            myConnectedObjects.remove();
            return;
        }
    }
    if (object == myConnectedObjects.getCurrent())
    {
        myConnectedObjects.remove();
        return;
    }
}

/*! \brief Sends a message to all connected objects. */
void
goObjectBase::sendObjectMessage (goObjectMessageID messageID)
{
    if (myConnectedObjects.isEmpty())
    {
        return;
    }
    myConnectedObjects.resetToFront();
    goObjectBase* o = NULL;
    goObjectMessage message;
    message.mySender        = this;
    message.myMessageID     = messageID;
    message.myMessageString = NULL;
    while (!myConnectedObjects.isTail())
    {
        o = myConnectedObjects.getCurrent();
        if (o)
        {
            o->receiveObjectMessage (message);
        }
        myConnectedObjects.getNext();
    }
    o = myConnectedObjects.getCurrent();
    if (o)
    {
        o->receiveObjectMessage (message);
    }
}

/*! \brief Sends a message to a specific object. */
void
goObjectBase::sendObjectMessage (goObjectBase* object, goObjectMessageID messageID) 
{
    if (!object)
    {
        return;
    }
    goObjectMessage message;
    message.mySender        = this;
    message.myMessageID     = messageID;
    message.myMessageString = NULL;
    object->receiveObjectMessage (message);
}

/*! \brief Receive a message.
 * 
 * This function gets called each time another object "sends" a message
 * to this object.
 * Overload as you please.
 */
void
goObjectBase::receiveObjectMessage (const goObjectMessage& message)
{
    std::cout << "Class " << getClassName() << " received message " << message.myMessageID << "\n";
}
