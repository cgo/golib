/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <golist.h>
#include <gostring.h>
#ifndef GODEFS_H
# include <godefs.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOTHREAD_H
# include <gothread.h>
#endif
#include <iostream>
#include <assert.h>
#include <stdio.h>
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif
#include <goglobal.h>

class goObjectBasePrivate
{
    public:
        goObjectBasePrivate ();
        ~goObjectBasePrivate ();

        void cleanupConnectedObjects ();

        int                              classID;
        goString                         objectName;
        goMutex                          connectedObjectsMutex;
        bool                             connectedObjectsLocked;
        bool                             connectedObjectsCleanupNeeded;
        goList<goObjectBase*>            connectedObjects;

        goMutex                           queuedMethodsMutex;
        goList<int>                       queuedMethods;
        goList<goObjectMethodParameters*> queuedMethodParams;
};

goObjectBasePrivate::goObjectBasePrivate ()
    :
    classID            (GO_OBJECTBASE),
    objectName         (""),
    connectedObjectsMutex (),
    connectedObjectsLocked (false),
    connectedObjectsCleanupNeeded (false),
    connectedObjects   (),
    queuedMethodsMutex (),
    queuedMethods      (),
    queuedMethodParams ()
{
}

goObjectBasePrivate::~goObjectBasePrivate ()
{
}

/*
 * @brief Remove all connected objects from list which are 
 *    NULL. Do nothing if the list is locked.
 */
void goObjectBasePrivate::cleanupConnectedObjects ()
{
    if (this->connectedObjectsLocked || !this->connectedObjectsCleanupNeeded)
    {
        //= Do nothing if list is locked.
        return;
    }

    this->connectedObjectsLocked = true;
    this->connectedObjectsMutex.lock();
    
    goList<goObjectBase*>::Element* el = this->connectedObjects.getFrontElement ();
    while (el)
    {
        if (el->elem == 0)
        {
            el = this->connectedObjects.remove (el);
        }
        else
        {
            el = el->next;
        }
    }

    this->connectedObjectsCleanupNeeded = false;
    this->connectedObjectsMutex.unlock();
    this->connectedObjectsLocked = false;
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
    myPrivate->connectedObjects.erase();
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
goObjectBase::getClassName() const
{
    // return myPrivate->className.toCharPtr();  // DEPRECATED. Use IDs instead.
    return goGlobal::classNames[this->getClassID()].toCharPtr();
}

int
goObjectBase::getClassID() const
{
    return myPrivate->classID;
}

/*! \brief Returns the size of this object or some measure of its memory
 * consumption.
 *
 * Overload this function as you please and as it makes sense.
 * @return Size in bytes of this object.
 */
goSize_t 
goObjectBase::memoryUsage () const
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
    this->sendObjectMessage (GO_OBJECTMESSAGE_NAME_CHANGED);
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
    this->sendObjectMessage (GO_OBJECTMESSAGE_NAME_CHANGED);
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

/**
* @brief Write object to a file.
*
* Reimplement this in sub-classes.
* 
* @param f  Valid C file pointer.
*
* @return True if successful, false otherwise.
**/
bool goObjectBase::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    if (!goFileIO::writeASCII (f, this->getObjectName()))
        return false;
    const char cnull = 0;
    fwrite (&cnull, sizeof(char), 1, f);
    if (!goFileIO::writeASCII (f, "goObjectBase"))
        return false;
    fwrite (&cnull, sizeof(char), 1, f);
    return true;
}

/**
* @brief Read object from a file.
*
* Reimplement this in sub-classes.
* 
* @param f  Valid C file pointer.
*
* @return True if successful, false otherwise.
**/
bool goObjectBase::readObjectFile (FILE* f)
{
    if (!f)
        return false;
    goString name;
    if (!goFileIO::readASCII (f, name))
        return false;
    this->setObjectName (name);
    //if (!goFileIO::writeASCII (f, "goObjectBase"))
    //    return false;
    return true;
}

/*! \brief Sets the class name */
//void
//goObjectBase::setClassName (const char* name)
//{
//    myPrivate->className = name;
//}

void 
goObjectBase::setClassID (int id)
{
    myPrivate->classID = id;
}

/*! \brief Sets the class name */
//void
//goObjectBase::setClassName (goString& name)
//{
//    myPrivate->className = name;
//}

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
    if (!myPrivate->connectedObjects.isEmpty())
    {
        goList<goObjectBase*>::Element* el = myPrivate->connectedObjects.getFrontElement();
        while (el)
        {
            if (object == el->elem)
            {
                //= Already connected.
                return;
            }
            el = el->next;
        }
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
    if (myPrivate->connectedObjects.isEmpty())
    {
        return;
    }
    goList<goObjectBase*>::Element* el = myPrivate->connectedObjects.getFrontElement();
    while (el)
    {
        if (el->elem == object)
        {
            if (myPrivate->connectedObjectsLocked)
            {
                //= Do not alter the list, just set element to NULL.
                myPrivate->connectedObjectsCleanupNeeded = true;
                el->elem = 0;
            }
            else
            {
                el = myPrivate->connectedObjects.remove (el);
            }
        }
        else
        {
            el = el->next;
        }
    }
}

/**
* @brief Call an object method by identifier.
*
* See the file goobjectmessage.h for messages and add messages there
* if needed. 
* External applications building on this class should use
* identifiers starting with GO_OBJECTMETHOD_USER.
* 
* @note Reimplement this in sub-classes as needed.
* The method should return true if the method call was successful.
* Values can also be returned through the goObjectMethodParameters* param.
* 
* @param methodID  ID of the method. See goobjectmethod.h 
* @param param  Pointer to parameters for the method, if any.
*
* @return True if successful, false otherwise.
**/
bool 
goObjectBase::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return false;
}

/** 
 * @brief  Enqueue a method call to an internal list of methods.
 *
 * The queued methods can be called in one go by a call to callQueuedMethods().
 * @note This method is thread-safe.
 * 
 * @param methodID ID of the method. See goobjectmethod.h 
 * @param param    Pointer to a parameter structure, if any. The parameters
 *                 will be deep-copied, so there is no need to worry about 
 *                 keeping them after the method returned.
 * @return True if successful, false otherwise.
 */
bool
goObjectBase::queueObjectMethod (int methodID, goObjectMethodParameters* param, bool blocking)
{
    myPrivate->queuedMethodsMutex.lock();
        myPrivate->queuedMethods.append(methodID);
        goObjectMethodParameters* newParm = 0;
        if (param)
        {
            newParm = new goObjectMethodParameters;
            *newParm = *param;
            newParm->blocking = blocking;
        }
        else
        {
            if (blocking)
            {
                newParm = new goObjectMethodParameters;
                newParm->blocking = true;
            }
        }
        myPrivate->queuedMethodParams.append(newParm);
    myPrivate->queuedMethodsMutex.unlock();
    if (blocking && newParm)
    {
        printf ("Waiting for queued method to return ...\n");
        newParm->semaphore.dec();
        printf ("Queued method returned!\n");
    }
    return true;
}

/** 
 * @brief Call all queued methods.
 *
 * Calls all methods queued with queueObjectMethod().
 * @note This method is thread-safe.
 * 
 * @return True if successful, false otherwise. 
 *         If one of the methods failed, this method returns false.
 */ 
bool
goObjectBase::callQueuedMethods ()
{
    myPrivate->queuedMethodsMutex.lock();
    bool ok = true;
    {
        assert (myPrivate->queuedMethods.getSize() == myPrivate->queuedMethodParams.getSize());
        goList<int>::Element* el = myPrivate->queuedMethods.getFrontElement();
        goList<goObjectMethodParameters*>::Element* paramEl = myPrivate->queuedMethodParams.getFrontElement();
        while (el && paramEl)
        {
            printf ("Calling queued method %d\n", el->elem);
            printf ("\tQueued method is %s.\n", (paramEl->elem && paramEl->elem->blocking) ? "blocking" : "not blocking");
            ok = ok && this->callObjectMethod (el->elem, paramEl->elem);
            if (paramEl->elem)
            {
                if (paramEl->elem->blocking)
                {
                    paramEl->elem->semaphore.inc();
                }
                delete paramEl->elem;
            }
            el = myPrivate->queuedMethods.remove (el);
            paramEl = myPrivate->queuedMethodParams.remove (paramEl);
        }
    }
    myPrivate->queuedMethodsMutex.unlock();
    return ok;
}

/*! \brief Sends a message to all connected objects. */
void
goObjectBase::sendObjectMessage (int messageID, void* data)
{
    if (myPrivate->connectedObjects.isEmpty())
    {
        return;
    }

    bool locked = false;

    if (!myPrivate->connectedObjectsLocked)
    {
        myPrivate->connectedObjectsMutex.lock();
        myPrivate->connectedObjectsLocked = true;
        locked = true;
    }

    goList<goObjectBase*>::Element* el = myPrivate->connectedObjects.getFrontElement();
    goObjectBase* o = NULL;
    goObjectMessage message;
    message.mySender        = this;
    message.myMessageID     = messageID;
    message.myMessageString = NULL;
    message.myData          = data;
    assert (!myPrivate->connectedObjects.isClosed());
    goSize_t sz = myPrivate->connectedObjects.getSize();
    goSize_t i = 0;
    while (el && i < sz)
    {
        o = el->elem;
        if (o)
        {
            o->receiveObjectMessage (message);
        }
        el = el->next;
        ++i;
    }

    if (locked)
    {
        myPrivate->connectedObjectsMutex.unlock();
        myPrivate->connectedObjectsLocked = false;
    }

    //= Check if cleanup is needed and do so.
    myPrivate->cleanupConnectedObjects ();
}

/*! \brief Sends a message to a specific object. */
void
goObjectBase::sendObjectMessage (goObjectBase* object, int messageID, void* data) 
{
    if (!object)
    {
        return;
    }
    goObjectMessage message;
    message.mySender        = this;
    message.myMessageID     = messageID;
    message.myMessageString = NULL;
    message.myData          = data;
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
    if (message.myMessageID == GO_OBJECTMESSAGE_DESTRUCTING)
    {
        // std::cout << "goObjectBase: disconnected object " << std::hex << message.mySender << " (object was destroyed)\n";
        // Ensure this object does not try to send anything to mySender, 
        // in case the other direction is also connected.
        // This may be slow, but it is safer.
        this->disconnectObject (message.mySender);
    }
    // std::cout << "Class " << getClassName() << " received message " << message.myMessageID << " from object \"" << message.mySender->getObjectName() << "\" of class " << message.mySender->getClassName() << "\n" << std::endl;
}

//#include <golist.hpp>
//template class goList<goObjectBase*>;
//template class goList<goObjectMethodParameters*>;
