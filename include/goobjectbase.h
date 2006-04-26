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
 * \author Christian Gosch
 * \todo Find a replacement for the class name string. Takes too much 
 *       space and time to allocate for every object!
 *       Some static solution would be better, or resort to enums.
 * \todo Do writeObjectFile/readObjectFile as ASCII. Better to port/debug/change.
 *       Check if libxml is nice enough to use for this.
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
    const char* getClassName () const;
    virtual goSize_t memoryUsage() const;

    void            setObjectName (const char* name);
    void            setObjectName (const goString& name);
    const goString& getObjectName () const;

    virtual bool writeObjectFile (FILE*) const;
    virtual bool readObjectFile  (FILE*);

 protected:
    void setClassName(const char* name);
    void setClassName(goString& name);
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
