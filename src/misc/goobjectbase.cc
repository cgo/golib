#include <goobjectbase.h>
#include <iostream>

goObjectBase::goObjectBase ()
    :
    className            ("goObjectBase"),
    myConnectedObjects   ()
{
}

goObjectBase::~goObjectBase ()
{
    sendObjectMessage (GO_OBJECTMESSAGE_DESTRUCTING);
}

const char*
goObjectBase::getClassName()
{
    return className.toCharPtr();
}

/*!
 * @return Size in bytes of this object.
 */
goSize_t 
goObjectBase::memoryUsage ()
{
    return sizeof (goObjectBase);
}

void
goObjectBase::setClassName (const char* name)
{
    className = name;
}

void
goObjectBase::setClassName (goString& name)
{
    className = name;
}

void
goObjectBase::printClassMessage (const char* msg)
{
    std::cout << getClassName() << " message: " << msg << std::endl;
}

void
goObjectBase::printClassMessage (goString& msg)
{
    printClassMessage (msg.toCharPtr());
}

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

void
goObjectBase::receiveObjectMessage (const goObjectMessage& message)
{
    std::cout << "Class " << getClassName() << " received message " << message.myMessageID << "\n";
}
