#ifndef GOOBJECTBASE_H
#define GOOBJECTBASE_H

#include <gostring.h>
#include <goqueue.h>
#include <golist.h>
#include <goobjectmessage.h>
#include <gotypes.h>

class
goObjectBase
{
    public:
        goObjectBase ();
        virtual ~goObjectBase ();

 public:
    const char* getClassName ();
    virtual goSize_t memoryUsage();

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
    goString                 className;
    goList<goObjectBase*>    myConnectedObjects;
};

#endif
