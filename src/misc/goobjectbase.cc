#include <golist.h>
#include <golist.hpp>
#include <gostring.h>
#include <goobjectbase.h>
#include <iostream>
#include <assert.h>

class goObjectBasePrivate
{
    public:
        goObjectBasePrivate ();
        ~goObjectBasePrivate ();

        goString              className;
        goString              objectName;
        goList<goObjectBase*> connectedObjects;
};

goObjectBasePrivate::goObjectBasePrivate ()
    :
    className        ("goObjectBase"),
    objectName       ("NO NAME"),
    connectedObjects ()
{
}

goObjectBasePrivate::~goObjectBasePrivate ()
{
}

/*! \brief Constructor */
goObjectBase::goObjectBase ()
    :
    myPrivate (NULL)
{
    myPrivate = new goObjectBasePrivate;
    assert (myPrivate);
}

/*! \brief Destructor 
 *
 *  The destructor sends a GO_OBJECTMESSAGE_DESTRUCTING message to all connected objects.
 */
goObjectBase::~goObjectBase ()
{
    sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = NULL;
    }
}

/*! \brief Returns the class name. 
 *
 *  \return Name string for this class.
 */
const char*
goObjectBase::getClassName()
{
    return myPrivate->className.toCharPtr();
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

/**
 * @brief Set name string for an object.
 *
 * @param name  Name of the object.
 **/
void
goObjectBase::setObjectName (const char* name)
{
    myPrivate->objectName = name;
}

/**
 * @brief Set name string for an object.
 *
 * @param name  Name of the object.
 **/
void
goObjectBase::setObjectName (const goString& name)
{
    myPrivate->objectName = name;
}

/**
 * @brief Get the object name.
 *
 * @return Name of this object.
 * \see setObjectName()
 **/
const goString&
goObjectBase::getObjectName () const
{
    return myPrivate->objectName;
}

/*! \brief Sets the class name */
void
goObjectBase::setClassName (const char* name)
{
    myPrivate->className = name;
}

/*! \brief Sets the class name */
void
goObjectBase::setClassName (goString& name)
{
    myPrivate->className = name;
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
 * Connected objects will <b>receive</b> messages sent by this object.
 * So when you say <br>
 * <code>object1.connectObject (anotherobject)</code> <br>
 * then anotherobject will receive messages from object1.
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
    myPrivate->connectedObjects.resetToFront();
    if (!myPrivate->connectedObjects.isEmpty())
    {
        o = myPrivate->connectedObjects.getCurrent();
        for (; !myPrivate->connectedObjects.isTail(); o = myPrivate->connectedObjects.getNext())
        {
            o = myPrivate->connectedObjects.getCurrent();
            if (o == object)
                return;
        }
        if (object == myPrivate->connectedObjects.getTail())
            return;
    }
    myPrivate->connectedObjects.append (object);
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
    myPrivate->connectedObjects.resetToFront();
    o = myPrivate->connectedObjects.getCurrent();
    for (; !myPrivate->connectedObjects.isTail(); o = myPrivate->connectedObjects.getNext())
    {
        o = myPrivate->connectedObjects.getCurrent();
        if (o == object)
        {
            myPrivate->connectedObjects.remove();
            return;
        }
    }
    if (object == myPrivate->connectedObjects.getCurrent())
    {
        myPrivate->connectedObjects.remove();
        return;
    }
}

/*! \brief Sends a message to all connected objects. */
void
goObjectBase::sendObjectMessage (goObjectMessageID messageID)
{
    if (myPrivate->connectedObjects.isEmpty())
    {
        return;
    }
    myPrivate->connectedObjects.resetToFront();
    goObjectBase* o = NULL;
    goObjectMessage message;
    message.mySender        = this;
    message.myMessageID     = messageID;
    message.myMessageString = NULL;
    while (!myPrivate->connectedObjects.isTail())
    {
        o = myPrivate->connectedObjects.getCurrent();
        if (o)
        {
            o->receiveObjectMessage (message);
        }
        myPrivate->connectedObjects.getNext();
    }
    o = myPrivate->connectedObjects.getCurrent();
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
 * Reimplement this in order to allow derived classes to react to messages.
 */
void
goObjectBase::receiveObjectMessage (const goObjectMessage& message)
{
    std::cout << "Class " << getClassName() << " received message " << message.myMessageID << " from object \"" << message.mySender->getObjectName() << "\" of class " << message.mySender->getClassName() << "\n";
}
