#ifndef GOOBJECTBASE_H
#define GOOBJECTBASE_H

#include <gostring.h>

class
goObjectBase
{

 public:
    const char* getClassName ();

 protected:
    void setClassName(const char* name);
    void setClassName(goString& name);
    void printClassMessage (const char* msg);
    void printClassMessage (goString& msg);

 private:
    goString className;
};

#endif
