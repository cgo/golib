#ifndef GOOBJECTBASE_H
#define GOOBJECTBASE_H

#include <goqueue.h>
#include <goobjectmessage.h>
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
 * \author Christian Gosch
 * \todo Test object communication
 * \todo Object communication has some problems (deleting objects and 
 *       sending the last 'dying' message)
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
    const char* getClassName ();
    virtual goSize_t memoryUsage();

    void            setObjectName (const char* name);
    void            setObjectName (const goString& name);
    const goString& getObjectName () const;

 protected:
    void setClassName(const char* name);
    void setClassName(goString& name);
    void printClassMessage (const char* msg);
    void printClassMessage (goString& msg);

    // API for sending messages from object to object
 public:
    void connectObject        (goObjectBase*          object);
    void disconnectObject     (const goObjectBase*    object);
 protected:
    void sendObjectMessage    (goObjectMessageID messageID);
    void sendObjectMessage    (goObjectBase* object, goObjectMessageID messageID);
    virtual void receiveObjectMessage (const goObjectMessage& message);
    
 private:
    goObjectBasePrivate*     myPrivate;
};
/*!
 * @}
 */

#endif
