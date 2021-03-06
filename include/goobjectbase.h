/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOOBJECTBASE_H
#define GOOBJECTBASE_H

#include <goqueue.h>
#include <goobjectmessage.h>
#include <goobjectmethod.h>
#include <gotypes.h>

class goString;
class goObjectBasePrivate;

/*! 
 * \addtogroup misc
 * @{
 */
/*! 
 * \brief Base class for golib objects.
 * 
 * This is a base for all objects that we felt needed a common base.
 * It implements class names, class message printing, 
 * and inter-object communication.
 *
 * @note The communication takes place by "message" functions.
 * Each object can be connected to other objects using
 * connectObject() and disconnectObject() calls.
 * There are problems arising internally which I will describe now.
 * The connected objects are maintained as a list of goObjectBase pointers.
 * Problems arise in cases when sendObjectMessage() traverses the list
 * and calls receiveObjectMessage() for each connected object.
 * When somewhere during the call of receiveObjectMessage(), which can be
 * arbitrarily complicated, the internal list gets changed with disconnectObject(),
 * the initial list traversal will produce undefined results (i.e. segfault).
 * To avoid this, the list is protected with a mutex together with a flag, and
 * if the mutex is locked and an object gets disconnected, its pointer
 * is simply set to NULL. Later, in case the list mutex is unlocked,
 * the list gets cleaned by another internal function. See source code
 * for details.
 * However, if undefined behaviour should still appear, this is a possible source
 * for problems.
 * connectObject() should not be as problematic, because it simply appends
 * to the end of the list.
 *
 * \author Christian Gosch
 *
 * \todo The object messaging must be stable against re-entries and changing connections during message sending.
 * There is no way to control what connected classes do during a receive call, so this is the place where everything must
 * be stable. Already hardened against disconnecting while receiving, see sendObjectMessage() in the source code.
 * Mutexes alone are of little help since they will lead to deadlocks here. The pointer in the connection list is therefore
 * first set to NULL and later the list is cleaned up. Test this on multi-processor machines.
 *
 * \todo Do writeObjectFile/readObjectFile as ASCII. Better to port/debug/change.
 *       Check if libxml is nice enough to use for this.
 *
 * \author Christian Gosch
 */
class
goObjectBase
{
    public:
        goObjectBase ();
        virtual ~goObjectBase ();

 public:
    const char* getClassName () const;
    int         getClassID   () const;
    virtual goSize_t memoryUsage() const;

    void            setObjectName (const char* name);
    void            setObjectName (const goString& name);
    const goString& getObjectName () const;

    virtual bool writeObjectFile (FILE*) const;
    virtual bool readObjectFile  (FILE*);

 protected:
    void setClassID(int id);
    void printClassMessage (const char* msg);
    void printClassMessage (goString& msg);

    // API for sending messages from object to object
 public:
    void connectObject        (goObjectBase*          object);
    void disconnectObject     (const goObjectBase*    object);

    // Generic API for calling methods (reimplement in derived classes)
 public:
    virtual bool callObjectMethod  (int methodID, goObjectMethodParameters* param = NULL);
    virtual bool queueObjectMethod (int methodID, goObjectMethodParameters* param = NULL, bool blocking = false);
    bool         callQueuedMethods ();
            
 protected:
    void sendObjectMessage    (int messageID, void* data = NULL);
    void sendObjectMessage    (goObjectBase* object, int messageID, void* data = NULL);
    virtual void receiveObjectMessage (const goObjectMessage& message);
    
 private:
    goObjectBasePrivate*     myPrivate;
};
/*!
 * @}
 */

#endif
